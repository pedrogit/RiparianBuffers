# Exact means polygon/cell intersections in the grid CRS; full-cell area is the
# denominator, even where studyArea occupies only part of a planning cell.
exactTileFractions <- function(template, footprint) {
  # Allocate one coverage value per template cell; zero is the natural value
  # for cells with no footprint intersection.
  answer <- numeric(terra::ncell(template))
  # Avoid invoking the extractor when this tile has no polygonal numerator.
  if (is.null(footprint) || !nrow(footprint)) {
    return(answer)
  }
  # Fail explicitly rather than substituting an approximate raster operation.
  if (!requireNamespace("exactextractr", quietly = TRUE)) {
    stop("Exact planar polygon coverage requires package 'exactextractr'.")
  }
  # Raster values are irrelevant: coverage and cell IDs come from geometry.
  grid <- terra::rast(template)
  # Supply a non-missing dummy value so exact_extract returns each intersection.
  terra::values(grid) <- 1
  # Request native cell IDs to put extracted coverage back in raster order.
  hits <- exactextractr::exact_extract(
    grid, sf::st_as_sf(footprint), include_cell = TRUE, progress = FALSE
  )
  hits <- do.call(rbind, hits)
  # No returned intersections means the initialized all-zero answer is complete.
  if (is.null(hits) || !nrow(hits)) {
    return(answer)
  }
  cellId <- hits$cell
  fraction <- hits$coverage_fraction
  # Ignore zero-area contacts while retaining all positive polygon/cell overlaps.
  valid <- is.finite(fraction) & fraction > 0
  if (any(!is.finite(fraction))) {
    stop(
      "Exact polygon coverage is outside [0,1]; non-finite intersection area."
    )
  }
  if (any(valid)) {
    # Sum fragments by cell because one cell may intersect several polygons.
    area <- rowsum(fraction[valid], cellId[valid], reorder = FALSE)
    answer[as.integer(rownames(area))] <- area[, 1]
  }
  tolerance <- 1e-6
  # Overlap should have been removed before extraction; allow only floating noise.
  if (
    any(!is.finite(answer)) || any(answer < -tolerance | answer > 1 + tolerance)
  ) {
    stop(
      "Exact polygon coverage is outside [0,1] by more than 1e-6; check geometry/overlaps."
    )
  }
  # Only roundoff-sized excursions may be clamped.
  pmin(1, pmax(0, answer))
}

#' Tiled exact combined lake-and-riparian coverage, with staged publication
#'
#' Planar cell coverage requires exactextractr; sf provides vector export because
#' terra 1.9.27 cannot append to an existing file with writeVector(insert=TRUE),
#' while sf's GDAL append also supports empty layers. The vector is a single
#' GeoPackage (`riparian_buffers.gpkg`, layer `riparian_buffers`), which has no
#' practical size limit, unlike a Shapefile (2 GiB per component).
#' Output geometry comprises disjoint tile cores; seams are processing artifacts.
#'
#' Tiles are independent, so with `workers > 1` they are computed in that many
#' separate R processes (see `runJobs`) while this process writes the raster in
#' row order. With `resume = TRUE` every finished tile is also saved under
#' `scratchDir/riparian_progress`; after a failure or interruption, running again
#' with the same inputs skips those tiles. The saved tiles are deleted when the
#' final files are published. Resuming is off when the vector is exported, since
#' footprints are not saved. A failing tile is reported by number, position and
#' extent.
fractionalCoverage <- function(
  studyArea,
  PlanningGrid,
  Jurisdiction,
  policy,
  streams,
  lakes,
  outputDir,
  scratchDir,
  tileCells = 128L,
  exportVector = FALSE,
  workers = 1L,
  resume = TRUE
) {
  # Validate the tile dimension before using it as a sequence increment or size.
  if (
    length(tileCells) != 1L ||
      !is.numeric(tileCells) ||
      is.na(tileCells) ||
      !is.finite(tileCells) ||
      tileCells < 1 ||
      tileCells != floor(tileCells) ||
      tileCells > .Machine$integer.max
  ) {
    stop("tileCells must be one positive integer.")
  }
  # Vector publication is optional, but its flag must not be coerced implicitly.
  if (
    !is.logical(exportVector) ||
      length(exportVector) != 1L ||
      is.na(exportVector)
  ) {
    stop("exportVector must be one non-missing logical value.")
  }
  if (
    !is.numeric(workers) ||
      length(workers) != 1L ||
      is.na(workers) ||
      workers < 1 ||
      workers != floor(workers)
  ) {
    stop("workers must be one positive integer.")
  }
  if (!is.logical(resume) || length(resume) != 1L || is.na(resume)) {
    stop("resume must be one non-missing logical value.")
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Exact planar coverage requires package 'sf'; install it explicitly.")
  }
  # Writing assumes a single-layer grid whose geometry defines every output cell.
  if (
    !inherits(PlanningGrid, "SpatRaster") || terra::nlyr(PlanningGrid) != 1L
  ) {
    stop("PlanningGrid must be a single-layer SpatRaster.")
  }
  widths <- c("small_stream", "large_stream", "small_lake", "large_lake")
  # Require one complete, unambiguous policy row for every jurisdiction key.
  if (
    !is.data.frame(policy) ||
      !all(c("jurisdiction", widths) %in% names(policy)) ||
      !nrow(policy) ||
      anyNA(policy$jurisdiction) ||
      anyDuplicated(policy$jurisdiction) ||
      !all(vapply(policy[, widths, drop = FALSE], is.numeric, logical(1)))
  ) {
    stop(
      "policy must have unique non-missing jurisdiction keys and four numeric width columns."
    )
  }
  w <- as.matrix(policy[, widths, drop = FALSE])
  # Negative or non-finite buffer distances have no spatial interpretation.
  if (any(!is.finite(w)) || any(w < 0)) {
    stop("Policy widths must be finite and nonnegative.")
  }
  # Every tile uses the same validated maximum width for whole-feature selection.
  maxWidth <- max(w)
  for (path in c(outputDir, scratchDir)) {
    # Ensure output and staging locations are usable before any processing begins.
    if (length(path) != 1L || is.na(path) || !nzchar(path)) {
      stop("Output/scratch paths must be nonempty.")
    }
    if (!dir.exists(path) && !dir.create(path, recursive = TRUE)) {
      stop("Cannot create directory: ", path)
    }
  }
  rasterName <- "riparian_fraction.tif"
  vectorName <- "riparian_buffers.gpkg"
  vectorLayer <- "riparian_buffers"
  # Name the final deliverables once so overwrite protection and publication agree.
  targets <- c(
    file.path(outputDir, rasterName),
    if (exportVector) file.path(outputDir, vectorName)
  )
  existing <- list.files(outputDir, full.names = TRUE)
  # Match base names to protect related files as well as named deliverables.
  protected <- tools::file_path_sans_ext(basename(existing)) %in%
    tools::file_path_sans_ext(basename(targets))
  # Also protect raster sidecars such as .tif.aux.xml.
  protected <- protected |
    startsWith(basename(existing), paste0(rasterName, "."))
  if (any(protected)) {
    stop(
      "Refusing to overwrite existing output: ",
      paste(existing[protected], collapse = ", ")
    )
  }
  # Build complete files in scratch space, so the output directory is published atomically.
  stage <- tempfile(pattern = "riparian_stage_", tmpdir = scratchDir)
  if (!dir.create(stage)) {
    stop("Cannot create staging directory: ", stage)
  }
  stagedRaster <- file.path(stage, rasterName)
  stagedVector <- file.path(stage, vectorName)
  completed <- FALSE
  opened <- FALSE
  # Copy the planning grid geometry and give its single output layer a stable name.
  writer <- terra::rast(PlanningGrid)
  names(writer) <- "riparianFraction"
  on.exit(
    {
      # Close a partially open GDAL writer during errors or interrupts.
      if (opened) {
        try(terra::writeStop(writer), silent = TRUE)
      }
      if (completed) {
        # Remove staging only after every final file has been successfully published.
        unlink(stage, recursive = TRUE)
      } else {
        # Preserve failed staging output for diagnosis instead of silently deleting it.
        warning(
          "Incomplete riparian export retained at: ",
          stage,
          call. = FALSE
        )
      }
    },
    add = TRUE
  )
  terra::writeStart(
    writer,
    stagedRaster,
    overwrite = FALSE,
    datatype = "FLT8S",
    gdal = c("COMPRESS=DEFLATE", "BIGTIFF=IF_SAFER")
  )
  # Remember that on.exit must close this writer until writeStop succeeds.
  opened <- TRUE
  nr <- terra::nrow(PlanningGrid)
  nc <- terra::ncol(PlanningGrid)
  tileCells <- as.integer(tileCells)
  # One row per tile, numbered stripe by stripe (left to right, top to bottom).
  tiles <- do.call(
    rbind,
    lapply(seq.int(1L, nr, by = tileCells), function(row) {
      cols <- seq.int(1L, nc, by = tileCells)
      data.frame(
        row = row,
        col = cols,
        nrows = min(tileCells, nr - row + 1L),
        ncols = pmin(tileCells, nc - cols + 1L)
      )
    })
  )
  tiles$id <- seq_len(nrow(tiles))
  tiles$stripe <- match(tiles$row, unique(tiles$row))
  totalTiles <- nrow(tiles)
  ctx <- list(
    studyArea = studyArea,
    Jurisdiction = Jurisdiction,
    policy = policy,
    streams = streams,
    lakes = lakes,
    maxWidth = maxWidth,
    gridExtent = as.vector(terra::ext(PlanningGrid)),
    resolution = terra::res(PlanningGrid),
    dim = c(nr, nc),
    crs = terra::crs(PlanningGrid),
    exportVector = exportVector
  )
  tileFunctions <- list(
    processTile,
    buildPolicyFootprints,
    exactTileFractions,
    clipToDestination,
    selectIntersecting,
    intersectPolygons,
    unionPolygons,
    polygonOnly
  )
  # Footprints are not saved, so a run that exports the vector cannot resume.
  progressDir <- if (resume && !exportVector) {
    file.path(scratchDir, "riparian_progress")
  }
  if (resume && exportVector) {
    message(
      "resume = TRUE is ignored because exportVector = TRUE: tile footprints are ",
      "not saved, so a failed run restarts from the first tile."
    )
  }
  saved <- integer()
  if (!is.null(progressDir)) {
    saved <- openTileProgress(
      progressDir,
      tileFingerprint(ctx, tileCells, tileFunctions)
    )
    saved <- saved[saved %in% tiles$id]
    if (length(saved)) {
      message(
        "Resuming: ", length(saved), " of ", totalTiles,
        " tiles were finished by an earlier run."
      )
    }
  }
  stripeIds <- unique(tiles$stripe)
  stripeValues <- list()
  stripeFootprints <- list()
  have <- logical(totalTiles)
  have[saved] <- TRUE
  nextStripe <- 1L
  vectorStarted <- FALSE
  done <- length(saved)
  # Write every stripe that is complete and next in line (GDAL needs row order).
  writeReadyStripes <- function() {
    while (nextStripe <= length(stripeIds)) {
      inStripe <- tiles[tiles$stripe == nextStripe, ]
      if (!all(have[inStripe$id])) {
        return(invisible())
      }
      stripe <- matrix(NA_real_, nrow = inStripe$nrows[1], ncol = nc)
      for (i in seq_len(nrow(inStripe))) {
        id <- inStripe$id[i]
        values <- stripeValues[[as.character(id)]]
        if (is.null(values)) {
          values <- readRDS(tileProgressFile(progressDir, id))
        }
        stripe[, seq.int(inStripe$col[i], length.out = inStripe$ncols[i])] <-
          values
        footprint <- stripeFootprints[[as.character(id)]]
        if (!is.null(footprint)) {
          footprint <- terra::unwrap(footprint)
          footprint$tile_id <- as.integer(id)
          # Append independent tile-core pieces; tile IDs retain their processing origin.
          # One geometry type for the whole layer, whatever each tile produces.
          sf::st_write(
            sf::st_cast(sf::st_as_sf(footprint), "MULTIPOLYGON"),
            stagedVector,
            layer = vectorLayer,
            quiet = TRUE,
            append = vectorStarted
          )
          vectorStarted <<- TRUE
        }
        stripeValues[[as.character(id)]] <<- NULL
        stripeFootprints[[as.character(id)]] <<- NULL
      }
      terra::writeValues(
        writer,
        as.vector(t(stripe)),
        start = inStripe$row[1],
        nrows = inStripe$nrows[1]
      )
      nextStripe <<- nextStripe + 1L
    }
    invisible()
  }
  writeReadyStripes()
  todo <- tiles[!have, ]
  runJobs(
    jobs = lapply(seq_len(nrow(todo)), function(i) as.list(todo[i, ])),
    run = processTile,
    ctx = ctx,
    helpers = c(
      "buildPolicyFootprints", "exactTileFractions", "clipToDestination",
      "selectIntersecting", "intersectPolygons", "unionPolygons",
      "polygonOnly"
    ),
    workers = workers,
    tmpDir = scratchDir,
    what = "tile",
    onResult = function(i, res) {
      id <- todo$id[i]
      key <- as.character(id)
      stripeValues[[key]] <<- res$values
      if (!is.null(res$footprint)) {
        stripeFootprints[[key]] <<- res$footprint
      }
      if (!is.null(progressDir)) {
        saveTileProgress(progressDir, id, res$values)
      }
      have[id] <<- TRUE
      done <<- done + 1L
      message("riparian tile ", done, "/", totalTiles, " (tile ", id, ")")
      writeReadyStripes()
    }
  )
  terra::writeStop(writer)
  opened <- FALSE
  if (exportVector) {
    if (!vectorStarted) {
      # Publish a valid empty layer when no tile produced a footprint.
      empty <- sf::st_sf(
        tile_id = integer(),
        geometry = sf::st_sfc(
          sf::st_multipolygon(),
          crs = terra::crs(PlanningGrid)
        )[FALSE]
      )
      sf::st_write(empty, stagedVector, layer = vectorLayer, quiet = TRUE)
    }
    # Confirm that GDAL produced the file before publication.
    if (!file.exists(stagedVector)) {
      stop("Staged GeoPackage was not created.")
    }
    # Querying the layer list via GDAL avoids reading a continental vector back.
    info <- sf::st_layers(stagedVector)
    if (!(vectorLayer %in% info$name)) {
      stop("Staged GeoPackage is not readable.")
    }
  }
  staged <- list.files(stage, full.names = TRUE)
  # Map every staged raster/vector sidecar into the requested output directory.
  destination <- file.path(outputDir, basename(staged))
  if (any(file.exists(destination))) {
    stop("Outputs appeared during processing; refusing publication overwrite.")
  }
  published <- character()
  for (i in seq_along(staged)) {
    # Copy without overwrite; remove already copied siblings if publication fails midway.
    if (!file.copy(staged[i], destination[i], overwrite = FALSE)) {
      unlink(published)
      stop(
        "Output publication failed; completed staged files retained at: ",
        stage
      )
    }
    published <- c(published, destination[i])
  }
  result <- tryCatch(
    terra::rast(file.path(outputDir, rasterName)),
    error = function(e) {
      # Do not leave a published but unreadable raster if validation fails.
      unlink(published)
      stop(e)
    }
  )
  completed <- TRUE
  # Saved tiles are only useful until the final files exist.
  if (!is.null(progressDir)) unlink(progressDir, recursive = TRUE)
  # Return both the opened raster and the paths consumers need for subsequent use.
  list(
    riparianFraction = result,
    rasterFile = file.path(outputDir, rasterName),
    vectorBufferFile = if (exportVector) {
      file.path(outputDir, vectorName)
    } else {
      NULL
    }
  )
}

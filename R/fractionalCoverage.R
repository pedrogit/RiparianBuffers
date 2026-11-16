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

checkShapefileSize <- function(filename, additionalBytes = 0) {
  # Check the three Shapefile components that can independently hit size limits.
  stem <- tools::file_path_sans_ext(filename)
  paths <- paste0(stem, c(".shp", ".shx", ".dbf"))
  sizes <- file.info(paths)$size
  sizes[is.na(sizes)] <- 0
  # Deliberately conservative portable Shapefile limit, including a safety margin.
  if (any(sizes + additionalBytes >= 2^31 - 1024^2)) {
    stop(
      "Shapefile is approaching the portable 2 GiB component limit. ",
      "Use a smaller study area or rerun with exportVector=FALSE; ",
      "no geometry has been silently truncated."
    )
  }
  invisible(TRUE)
}

#' Tiled exact combined lake-and-riparian coverage, with staged publication
#'
#' Planar cell coverage requires exactextractr; sf provides vector export because
#' terra 1.9.27 cannot append to an existing Shapefile
#' with writeVector(insert=TRUE); sf's GDAL append also supports empty polygons.
#' Output geometry comprises disjoint tile cores; seams are processing artifacts.
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
  exportVector = TRUE
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
  vectorName <- "riparian_buffers.shp"
  # Name the final deliverables once so overwrite protection and publication agree.
  targets <- c(
    file.path(outputDir, rasterName),
    if (exportVector) file.path(outputDir, vectorName)
  )
  existing <- list.files(outputDir, full.names = TRUE)
  # Match base names to protect Shapefile sidecars as well as named deliverables.
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
  resolution <- terra::res(PlanningGrid)
  gridExtent <- as.vector(terra::ext(PlanningGrid))
  # Convert after validation so loop arithmetic uses ordinary integer tile sizes.
  tileCells <- as.integer(tileCells)
  tileId <- 0L
  vectorStarted <- FALSE
  totalTiles <- ceiling(nr / tileCells) * ceiling(nc / tileCells)
  # Process row stripes so only one stripe, rather than the whole raster, is in memory.
  for (row in seq.int(1L, nr, by = tileCells)) {
    nrows <- min(tileCells, nr - row + 1L)
    # A row stripe, not a full-grid matrix, is held for incremental GDAL writes.
    stripe <- matrix(NA_real_, nrow = nrows, ncol = nc)
    # Divide this stripe into grid-aligned tile cores.
    for (col in seq.int(1L, nc, by = tileCells)) {
      ncols <- min(tileCells, nc - col + 1L)
      tileId <- tileId + 1L
      ex <- terra::ext(
        gridExtent[1] + (col - 1) * resolution[1],
        gridExtent[1] + (col - 1 + ncols) * resolution[1],
        gridExtent[4] - (row - 1 + nrows) * resolution[2],
        gridExtent[4] - (row - 1) * resolution[2]
      )
      # Recreate the exact grid geometry for this tile, preserving CRS and resolution.
      template <- terra::rast(
        nrows = nrows,
        ncols = ncols,
        extent = ex,
        crs = terra::crs(PlanningGrid)
      )
      # Convert the tile rectangle to a polygon for destination and footprint clipping.
      core <- terra::as.polygons(ex, crs = terra::crs(template))
      studyCore <- intersectPolygons(core, studyArea)
      if (!is.null(studyCore)) {
        # Build destination-policy buffers only where this tile intersects the study area.
        footprint <- buildPolicyFootprints(
          core,
          studyArea,
          Jurisdiction,
          policy,
          streams,
          lakes,
          core = studyCore,
          maxWidth = maxWidth
        )
        fractions <- exactTileFractions(template, footprint)
        # Mark cells outside the study area as NoData, even if a buffer reaches them.
        inStudy <- exactTileFractions(
          template,
          unionPolygons(list(studyCore))
        ) >
          0
        fractions[!inStudy] <- NA_real_
        stripe[, seq.int(col, length.out = ncols)] <-
          matrix(fractions, nrow = nrows, ncol = ncols, byrow = TRUE)
        if (exportVector && !is.null(footprint)) {
          # Estimate before writing, then check actual component sizes afterward.
          coordinates <- terra::geom(footprint)
          checkShapefileSize(
            stagedVector,
            additionalBytes = nrow(coordinates) * 32 + nrow(footprint) * 1024
          )
          footprint$tile_id <- as.integer(tileId)
          # Append independent tile-core pieces; tile IDs retain their processing origin.
          sf::st_write(
            sf::st_as_sf(footprint),
            stagedVector,
            quiet = TRUE,
            append = vectorStarted,
            layer_options = c("SHPT=POLYGON", "2GB_LIMIT=YES")
          )
          vectorStarted <- TRUE
          checkShapefileSize(stagedVector)
        }
      }
      message("riparian tile ", tileId, "/", totalTiles)
    }
    # Write this completed stripe in raster cell order, then release it next iteration.
    terra::writeValues(writer, as.vector(t(stripe)), start = row, nrows = nrows)
  }
  terra::writeStop(writer)
  opened <- FALSE
  if (exportVector) {
    if (!vectorStarted) {
      # Publish a valid empty layer when no tile produced a footprint.
      empty <- sf::st_sf(
        tile_id = integer(),
        geometry = sf::st_sfc(sf::st_polygon(), crs = terra::crs(PlanningGrid))[
          FALSE
        ]
      )
      sf::st_write(
        empty,
        stagedVector,
        quiet = TRUE,
        layer_options = c("SHPT=POLYGON", "2GB_LIMIT=YES")
      )
    }
    required <- paste0(
      tools::file_path_sans_ext(stagedVector),
      c(".shp", ".shx", ".dbf", ".prj")
    )
    # Confirm that GDAL produced all mandatory Shapefile sidecars before publication.
    if (!all(file.exists(required))) {
      stop("Incomplete staged Shapefile sidecars.")
    }
    checkShapefileSize(stagedVector)
    # Querying schema/extent via GDAL avoids reading a continental vector back.
    info <- sf::st_layers(stagedVector)
    if (!length(info$name)) stop("Staged Shapefile is not readable.")
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

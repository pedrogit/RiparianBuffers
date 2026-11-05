# Exact means polygon/cell intersections in the grid CRS; full-cell area is the
# denominator, even where studyArea occupies only part of a planning cell.
exactTileFractions_sol6_1 <- function(template, footprint) {
  answer <- numeric(terra::ncell(template))
  if (is.null(footprint) || !nrow(footprint)) {
    return(answer)
  }
  if (!requireNamespace("exactextractr", quietly = TRUE)) {
    stop("Exact planar polygon coverage requires package 'exactextractr'.")
  }
  # Raster values are irrelevant: coverage and cell IDs come from geometry.
  grid <- terra::rast(template)
  terra::values(grid) <- 1
  hits <- exactextractr::exact_extract(
    grid, sf::st_as_sf(footprint), include_cell = TRUE, progress = FALSE
  )
  hits <- do.call(rbind, hits)
  if (is.null(hits) || !nrow(hits)) {
    return(answer)
  }
  cellId <- hits$cell
  fraction <- hits$coverage_fraction
  valid <- is.finite(fraction) & fraction > 0
  if (any(!is.finite(fraction))) {
    stop(
      "Exact polygon coverage is outside [0,1]; non-finite intersection area."
    )
  }
  if (any(valid)) {
    area <- rowsum(fraction[valid], cellId[valid], reorder = FALSE)
    answer[as.integer(rownames(area))] <- area[, 1]
  }
  tolerance <- 1e-6
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

checkShapefileSize_sol6_1 <- function(filename, additionalBytes = 0) {
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
fractionalCoverage_sol6_1 <- function(
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
  if (
    !inherits(PlanningGrid, "SpatRaster") || terra::nlyr(PlanningGrid) != 1L
  ) {
    stop("PlanningGrid must be a single-layer SpatRaster.")
  }
  widths <- c("small_stream", "large_stream", "small_lake", "large_lake")
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
  if (any(!is.finite(w)) || any(w < 0)) {
    stop("Policy widths must be finite and nonnegative.")
  }
  for (path in c(outputDir, scratchDir)) {
    if (length(path) != 1L || is.na(path) || !nzchar(path)) {
      stop("Output/scratch paths must be nonempty.")
    }
    if (!dir.exists(path) && !dir.create(path, recursive = TRUE)) {
      stop("Cannot create directory: ", path)
    }
  }
  rasterName <- "riparian_fraction_sol6_1.tif"
  vectorName <- "riparian_buffers_sol6_1.shp"
  targets <- c(
    file.path(outputDir, rasterName),
    if (exportVector) file.path(outputDir, vectorName)
  )
  existing <- list.files(outputDir, full.names = TRUE)
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
  stage <- tempfile(pattern = "riparian_sol6_1_stage_", tmpdir = scratchDir)
  if (!dir.create(stage)) {
    stop("Cannot create staging directory: ", stage)
  }
  stagedRaster <- file.path(stage, rasterName)
  stagedVector <- file.path(stage, vectorName)
  completed <- FALSE
  opened <- FALSE
  writer <- terra::rast(PlanningGrid)
  names(writer) <- "riparianFraction"
  on.exit(
    {
      if (opened) {
        try(terra::writeStop(writer), silent = TRUE)
      }
      if (completed) {
        unlink(stage, recursive = TRUE)
      } else {
        warning(
          "Incomplete riparian_sol6_1 export retained at: ",
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
  opened <- TRUE
  nr <- terra::nrow(PlanningGrid)
  nc <- terra::ncol(PlanningGrid)
  resolution <- terra::res(PlanningGrid)
  gridExtent <- as.vector(terra::ext(PlanningGrid))
  tileCells <- as.integer(tileCells)
  tileId <- 0L
  vectorStarted <- FALSE
  totalTiles <- ceiling(nr / tileCells) * ceiling(nc / tileCells)
  for (row in seq.int(1L, nr, by = tileCells)) {
    nrows <- min(tileCells, nr - row + 1L)
    # A row stripe, not a full-grid matrix, is held for incremental GDAL writes.
    stripe <- matrix(NA_real_, nrow = nrows, ncol = nc)
    for (col in seq.int(1L, nc, by = tileCells)) {
      ncols <- min(tileCells, nc - col + 1L)
      tileId <- tileId + 1L
      ex <- terra::ext(
        gridExtent[1] + (col - 1) * resolution[1],
        gridExtent[1] + (col - 1 + ncols) * resolution[1],
        gridExtent[4] - (row - 1 + nrows) * resolution[2],
        gridExtent[4] - (row - 1) * resolution[2]
      )
      template <- terra::rast(
        nrows = nrows,
        ncols = ncols,
        extent = ex,
        crs = terra::crs(PlanningGrid)
      )
      terra::values(template) <- 1
      core <- terra::as.polygons(ex, crs = terra::crs(template))
      studyCore <- intersectPolygons_sol6_1(core, studyArea)
      if (!is.null(studyCore)) {
        footprint <- buildPolicyFootprints_sol6_1(
          core,
          studyArea,
          Jurisdiction,
          policy,
          streams,
          lakes
        )
        fractions <- exactTileFractions_sol6_1(template, footprint)
        inStudy <- exactTileFractions_sol6_1(
          template,
          unionPolygons_sol6_1(list(studyCore))
        ) >
          0
        fractions[!inStudy] <- NA_real_
        stripe[, seq.int(col, length.out = ncols)] <-
          matrix(fractions, nrow = nrows, ncol = ncols, byrow = TRUE)
        if (exportVector && !is.null(footprint)) {
          # Estimate before writing, then check actual component sizes afterward.
          coordinates <- terra::geom(footprint)
          checkShapefileSize_sol6_1(
            stagedVector,
            additionalBytes = nrow(coordinates) * 32 + nrow(footprint) * 1024
          )
          footprint$tile_id <- as.integer(tileId)
          sf::st_write(
            sf::st_as_sf(footprint),
            stagedVector,
            quiet = TRUE,
            append = vectorStarted,
            layer_options = c("SHPT=POLYGON", "2GB_LIMIT=YES")
          )
          vectorStarted <- TRUE
          checkShapefileSize_sol6_1(stagedVector)
        }
      }
      message("riparian_sol6_1 tile ", tileId, "/", totalTiles)
    }
    terra::writeValues(writer, as.vector(t(stripe)), start = row, nrows = nrows)
  }
  terra::writeStop(writer)
  opened <- FALSE
  if (exportVector) {
    if (!vectorStarted) {
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
    if (!all(file.exists(required))) {
      stop("Incomplete staged Shapefile sidecars.")
    }
    checkShapefileSize_sol6_1(stagedVector)
    # Querying schema/extent via GDAL avoids reading a continental vector back.
    info <- sf::st_layers(stagedVector)
    if (!length(info$name)) stop("Staged Shapefile is not readable.")
  }
  staged <- list.files(stage, full.names = TRUE)
  destination <- file.path(outputDir, basename(staged))
  if (any(file.exists(destination))) {
    stop("Outputs appeared during processing; refusing publication overwrite.")
  }
  published <- character()
  for (i in seq_along(staged)) {
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
      unlink(published)
      stop(e)
    }
  )
  completed <- TRUE
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

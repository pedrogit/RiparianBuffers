# Offline synthetic demonstration for riparianBuffers_Sol6_1.
# It supplies every spatial input, so it never downloads data. Call explicitly:
# demo_riparianBuffers_sol6_1("path/you/control/scratch")

demo_riparianBuffers_sol6_1 <- function(
  scratch_path,
  project_root = getwd(),
  export_vector = TRUE
) {
  stopifnot(
    is.character(scratch_path),
    length(scratch_path) == 1L,
    nzchar(scratch_path),
    is.logical(export_vector),
    length(export_vector) == 1L,
    !is.na(export_vector)
  )
  if (
    !requireNamespace("SpaDES.core", quietly = TRUE) ||
      !requireNamespace("terra", quietly = TRUE) ||
      !requireNamespace("sf", quietly = TRUE)
  ) {
    stop("This demo requires SpaDES.core, terra, and sf.")
  }
  if (export_vector && !requireNamespace("sf", quietly = TRUE)) {
    stop("export_vector=TRUE requires sf.")
  }
  root <- normalizePath(scratch_path, winslash = "/", mustWork = FALSE)
  paths <- list(
    modulePath = file.path(project_root, "modules"),
    inputPath = file.path(root, "inputs_sol6_1"),
    outputPath = file.path(root, "outputs_sol6_1"),
    cachePath = file.path(root, "cache_sol6_1"),
    scratchPath = file.path(root, "scratch_sol6_1")
  )
  if (!dir.exists(paths$modulePath)) {
    stop("Cannot find modules under project_root: ", project_root)
  }
  for (p in paths) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
  }

  # EPSG:5070 uses metres. Two jurisdictions meet at x = 500; the stream lies
  # in ON but must use QC's destination width for its contribution eastward.
  crs <- "EPSG:5070"
  polygon <- function(coords) {
    terra::vect(sf::st_sf(
      geometry = sf::st_sfc(
        sf::st_polygon(list(as.matrix(coords))),
        crs = crs
      )
    ))
  }
  study_area <- polygon(rbind(
    c(0, 0),
    c(1000, 0),
    c(1000, 1000),
    c(0, 1000),
    c(0, 0)
  ))
  jurisdiction <- do.call(
    rbind,
    list(
      polygon(rbind(c(0, 0), c(500, 0), c(500, 1000), c(0, 1000), c(0, 0))),
      polygon(rbind(
        c(500, 0),
        c(1000, 0),
        c(1000, 1000),
        c(500, 1000),
        c(500, 0)
      ))
    )
  )
  jurisdiction$jurisdiction_code <- c("ON", "QC")
  grid <- terra::rast(
    xmin = 0,
    xmax = 1000,
    ymin = 0,
    ymax = 1000,
    resolution = 250,
    crs = crs
  )
  streams <- terra::vect(sf::st_sf(
    ORD_STRA = 4,
    geometry = sf::st_sfc(
      sf::st_linestring(rbind(c(475, 0), c(475, 1000))),
      crs = crs
    )
  ))
  lakes <- terra::vect(sf::st_sf(
    Lake_area = 0.5,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(650, 300),
        c(900, 300),
        c(900, 700),
        c(650, 700),
        c(650, 300)
      ))),
      crs = crs
    )
  ))
  policy <- data.frame(
    jurisdiction = c("default", "ON", "QC"),
    small_stream = c(30, 30, 20),
    large_stream = c(50, 50, 40),
    small_lake = c(30, 30, 20),
    large_lake = c(50, 50, 40)
  )

  sim <- SpaDES.core::simInit(
    times = list(start = 1, end = 1),
    modules = "riparianBuffers_Sol6_1",
    paths = paths,
    objects = list(
      studyArea = study_area,
      PlanningGrid = grid,
      Jurisdiction = jurisdiction,
      riparianBufferPolicy = policy,
      Hydrology_streams = streams,
      Hydrology_lakes = lakes
    ),
    params = list(
      riparianBuffers_Sol6_1 = list(
        tileCells_sol6_1 = 2L,
        exportVectorBuffer_sol6_1 = export_vector
      )
    )
  )
  sim <- SpaDES.core::spades(sim)
  stopifnot(
    terra::compareGeom(
      sim$Riparian_sol6_1$riparianFraction,
      grid,
      stopOnError = FALSE
    ),
    identical(sim$Riparian_sol6_1$lakeInteriorsExcluded, FALSE)
  )
  invisible(sim)
}

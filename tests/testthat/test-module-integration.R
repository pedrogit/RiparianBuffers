sol_sim_objects <- function() {
  f <- sol_fixture(20, lake = TRUE)
  f$studyArea <- sol_rect(0, 400, 0, 400)
  f$Jurisdiction <- terra::deepcopy(f$studyArea)
  f$Jurisdiction$jurisdiction_code <- "ON"
  f$Jurisdiction$NAME_En <- "Ontario"
  f$Jurisdiction$COUNTRY <- "Canada"
  f$PlanningGrid <- terra::rast(
    xmin = 0,
    xmax = 400,
    ymin = 0,
    ymax = 400,
    nrows = 2,
    ncols = 2,
    crs = "EPSG:5070"
  )
  terra::values(f$PlanningGrid) <- seq_len(4)
  f$streams <- rbind(sol_line(100, -100, 500, 2), sol_line(300, -100, 500, 4))
  f$lakes <- rbind(sol_rect(125, 175, 125, 175), sol_rect(225, 275, 225, 275))
  f$lakes$Lake_area <- c(0.5, 1)
  map <- terra::rast(f$PlanningGrid)
  terra::values(map) <- 1
  list(
    studyArea = f$studyArea,
    PlanningGrid = f$PlanningGrid,
    Jurisdiction = f$Jurisdiction,
    riparianBufferPolicy = f$policy,
    Hydrology_streams = f$streams,
    Hydrology_lakes = f$lakes,
    jurisdictionMap = map,
    jurisdictionLookup = data.frame(
      ID = 1,
      jurisdiction = "Ontario",
      nation = "Canada"
    )
  )
}

sol_sim <- function(modules, modulePath, objects, export = FALSE) {
  root <- sol_dir("simulation")
  paths <- list(
    modulePath = modulePath,
    inputPath = file.path(root, "input"),
    outputPath = file.path(root, "output"),
    cachePath = file.path(root, "cache"),
    scratchPath = file.path(root, "scratch")
  )
  for (p in paths) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
  }
  old <- options(
    reproducible.destinationPath = paths$inputPath,
    reproducible.useCache = FALSE,
    spades.checkModuleCode = FALSE
  )
  on.exit(options(old), add = TRUE)
  params <- list()
  if ("RiparianBuffers" %in% modules) {
    params$RiparianBuffers <-
      list(tileCells = 1L, exportVectorBuffer = export)
  }
  if ("RiparianBuffers" %in% modules) {
    params$RiparianBuffers <- list(hydroRaster_m = 20)
  }
  sim <- SpaDES.core::simInit(
    times = list(start = 1, end = 1),
    modules = modules,
    paths = paths,
    objects = objects,
    params = params
  )
  SpaDES.core::spades(sim, debug = FALSE)
}

testthat::test_that("actual simInit/spades produces isolated result and the vector export when requested", {
  objects <- sol_sim_objects()
  before <- lapply(objects, sol_signature)
  # Ask for the export explicitly: the metadata default is FALSE.
  root <- sol_dir("default_sim")
  paths <- list(
    modulePath = dirname(sol_module),
    inputPath = file.path(root, "input"),
    outputPath = file.path(root, "output"),
    cachePath = file.path(root, "cache"),
    scratchPath = file.path(root, "scratch")
  )
  for (p in paths) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
  }
  sim <- SpaDES.core::simInit(
    times = list(start = 1, end = 1),
    modules = "RiparianBuffers",
    objects = objects,
    paths = paths,
    params = list(RiparianBuffers = list(tileCells = 1L, exportVectorBuffer = TRUE))
  )
  sim <- SpaDES.core::spades(sim, debug = FALSE)
  r <- sim$Riparian
  testthat::expect_s4_class(r$riparianFraction, "SpatRaster")
  testthat::expect_equal(
    file.exists(c(r$rasterFile, r$vectorBufferFile)),
    c(TRUE, TRUE)
  )
  testthat::expect_identical(r$lakeInteriorsExcluded, FALSE)
  testthat::expect_identical(r$exportVectorBuffer, TRUE)
  testthat::expect_null(r$raster_m)
  testthat::expect_match(r$coverageConvention, "FULL planning-cell")
  testthat::expect_equal(
    terra::compareGeom(r$riparianFraction, objects$PlanningGrid),
    TRUE
  )
  testthat::expect_equal(
    nrow(sf::st_read(r$vectorBufferFile, quiet = TRUE)) > 0,
    TRUE
  )
  testthat::expect_identical(lapply(objects, sol_signature), before)
  for (name in names(objects)) {
    testthat::expect_identical(sol_signature(sim[[name]]), before[[name]])
  }
  # The only object this module adds is Riparian. Prepared copies of the
  # inputs stay local and are not published under any name.
  testthat::expect_true("Riparian" %in% names(sim))
  testthat::expect_setequal(
    setdiff(ls(sim), names(objects)),
    "Riparian"
  )
  deps <- SpaDES.core::inputObjects(sim)[["RiparianBuffers"]]
  testthat::expect_equal(any(grepl("$", deps$objectName)), FALSE)
})

testthat::test_that("standalone default grid is 240 m and supplied 1000 m grid wins", {
  objects <- sol_sim_objects()
  objects$PlanningGrid <- NULL
  sim <- sol_sim("RiparianBuffers", dirname(sol_module), objects)
  testthat::expect_equal(terra::res(sim$PlanningGrid), c(240, 240))
  testthat::expect_equal(
    unname(as.vector(terra::ext(sim$PlanningGrid))),
    c(0, 480, 0, 480)
  )
  objects <- sol_sim_objects()
  objects$PlanningGrid <- terra::rast(
    xmin = 0,
    xmax = 1000,
    ymin = 0,
    ymax = 1000,
    nrows = 1,
    ncols = 1,
    crs = "EPSG:5070"
  )
  sim <- sol_sim("RiparianBuffers", dirname(sol_module), objects)
  testthat::expect_equal(terra::res(sim$PlanningGrid), c(1000, 1000))
})

testthat::test_that("module parameters default as documented and invalid values are rejected", {
  mkpaths <- function() list(
    modulePath = dirname(sol_module),
    inputPath = sol_dir("p_in"), outputPath = sol_dir("p_out"),
    cachePath = sol_dir("p_cache"), scratchPath = sol_dir("p_scr")
  )
  defaults <- SpaDES.core::simInit(
    times = list(start = 1, end = 1),
    modules = "RiparianBuffers",
    objects = sol_sim_objects(),
    paths = mkpaths()
  )
  p <- SpaDES.core::P(defaults, module = "RiparianBuffers")
  testthat::expect_identical(p$jurisdictionMargin_m, 1e5)
  testthat::expect_identical(as.integer(p$workers), 4L)
  testthat::expect_true(p$resume)
  testthat::expect_false(p$exportVectorBuffer)
  for (bad in list(-1, NA_real_, c(1, 2), "a")) {
    sim <- SpaDES.core::simInit(
      times = list(start = 1, end = 1),
      modules = "RiparianBuffers",
      objects = sol_sim_objects(),
      paths = mkpaths(),
      params = list(RiparianBuffers = list(jurisdictionMargin_m = bad))
    )
    sol_error(
      suppressWarnings(SpaDES.core::spades(sim, debug = FALSE)),
      "jurisdictionMargin_m"
    )
  }
})

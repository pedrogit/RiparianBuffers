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
    inputPath = file.path(root, "input_sol6_1"),
    outputPath = file.path(root, "output_sol6_1"),
    cachePath = file.path(root, "cache_sol6_1"),
    scratchPath = file.path(root, "scratch_sol6_1")
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
  if ("riparianBuffers_Sol6_1" %in% modules) {
    params$riparianBuffers_Sol6_1 <-
      list(tileCells_sol6_1 = 1L, exportVectorBuffer_sol6_1 = export)
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

testthat::test_that("actual simInit/spades produces isolated result and default vector export", {
  objects <- sol_sim_objects()
  before <- lapply(objects, sol_signature)
  # Omit the export parameter to exercise the metadata default TRUE.
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
    modules = "riparianBuffers_Sol6_1",
    objects = objects,
    paths = paths,
    params = list(riparianBuffers_Sol6_1 = list(tileCells_sol6_1 = 1L))
  )
  sim <- SpaDES.core::spades(sim, debug = FALSE)
  r <- sim$Riparian_sol6_1
  testthat::expect_s4_class(r$riparianFraction, "SpatRaster")
  testthat::expect_equal(
    file.exists(c(r$rasterFile, r$vectorBufferFile)),
    c(TRUE, TRUE)
  )
  testthat::expect_identical(r$lakeInteriorsExcluded, FALSE)
  testthat::expect_identical(r$exportVectorBuffer_sol6_1, TRUE)
  testthat::expect_null(r$raster_m)
  testthat::expect_null(sim$Riparian)
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
  testthat::expect_equal(
    all(
      c(
        "Jurisdiction_sol6_1",
        "riparianBufferPolicy_sol6_1",
        "Hydrology_streams_sol6_1",
        "Hydrology_lakes_sol6_1"
      ) %in%
        names(sim)
    ),
    TRUE
  )
  deps <- SpaDES.core::inputObjects(sim)[["riparianBuffers_Sol6_1"]]
  testthat::expect_equal(any(grepl("_sol6_1$", deps$objectName)), FALSE)
})

testthat::test_that("standalone default grid is 240 m and supplied 1000 m grid wins", {
  objects <- sol_sim_objects()
  objects$PlanningGrid <- NULL
  sim <- sol_sim("riparianBuffers_Sol6_1", dirname(sol_module), objects)
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
  sim <- sol_sim("riparianBuffers_Sol6_1", dirname(sol_module), objects)
  testthat::expect_equal(terra::res(sim$PlanningGrid), c(1000, 1000))
})

testthat::test_that("old/new coexist in both orders and each matches its standalone baseline", {
  oldModule <- file.path(dirname(sol_module), "riparianBuffers")
  testthat::expect_equal(
    file.exists(file.path(oldModule, "RiparianBuffers.R")),
    TRUE
  )
  oldFiles <- c(
    file.path(oldModule, "RiparianBuffers.R"),
    list.files(file.path(oldModule, "R"), "\\.R$", full.names = TRUE)
  )
  originalHashes <- tools::md5sum(oldFiles)
  isolated <- sol_dir("joint_modules")
  copied <- file.path(isolated, "RiparianBuffers")
  dir.create(copied)
  # Copy only required source/data/documentation, never the nested .git or old tests.
  items <- list.files(oldModule, full.names = TRUE, all.files = FALSE)
  items <- items[basename(items) != "tests"]
  testthat::expect_equal(all(file.copy(items, copied, recursive = TRUE)), TRUE)
  copiedFiles <- c(
    file.path(copied, "RiparianBuffers.R"),
    list.files(file.path(copied, "R"), "\\.R$", full.names = TRUE)
  )
  for (path in copiedFiles) {
    lines <- readLines(path, warn = FALSE)
    writeLines(gsub("\\bbrowser\\(\\)", "invisible(NULL)", lines), path)
  }
  # New module is also copied so both are resolved from one modulePath.
  newCopy <- file.path(isolated, "riparianBuffers_Sol6_1")
  dir.create(newCopy)
  items <- list.files(sol_module, full.names = TRUE)
  items <- items[basename(items) != "tests"]
  testthat::expect_equal(all(file.copy(items, newCopy, recursive = TRUE)), TRUE)
  makeObjects <- function() sol_sim_objects()
  oldBaseline <- sol_sim("RiparianBuffers", isolated, makeObjects())
  newBaseline <- sol_sim("riparianBuffers_Sol6_1", isolated, makeObjects())
  for (order in list(
    c("RiparianBuffers", "riparianBuffers_Sol6_1"),
    c("riparianBuffers_Sol6_1", "RiparianBuffers")
  )) {
    objects <- makeObjects()
    before <- lapply(objects, sol_signature)
    joint <- sol_sim(order, isolated, objects)
    testthat::expect_type(joint$Riparian, "list")
    testthat::expect_type(joint$Riparian_sol6_1, "list")
    testthat::expect_equal(
      sol_vals(joint$Riparian$riparianFraction),
      sol_vals(oldBaseline$Riparian$riparianFraction),
      tolerance = 1e-10
    )
    testthat::expect_equal(
      sol_vals(joint$Riparian_sol6_1$riparianFraction),
      sol_vals(newBaseline$Riparian_sol6_1$riparianFraction),
      tolerance = 1e-10
    )
    for (name in names(objects)) {
      testthat::expect_identical(sol_signature(joint[[name]]), before[[name]])
    }
    testthat::expect_identical(lapply(objects, sol_signature), before)
    testthat::expect_equal(joint$Riparian$raster_m, 20)
    testthat::expect_null(joint$Riparian_sol6_1$raster_m)
    testthat::expect_equal(joint$Riparian$policy, objects$riparianBufferPolicy)
    testthat::expect_match(
      joint$Riparian_sol6_1$rasterFile,
      "riparianBuffers_sol6_1"
    )
    testthat::expect_null(joint$Riparian_sol6_1$vectorBufferFile)
  }
  testthat::expect_identical(tools::md5sum(oldFiles), originalHashes)
})

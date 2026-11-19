testthat::test_that("straight stream widths give analytical exact fractions", {
  for (width in c(0, 20, 30, 50)) {
    f <- sol_prepared(sol_fixture(width))
    r <- sol_run(f)
    testthat::expect_equal(
      sol_vals(r$riparianFraction),
      min(1, 2 * width / 100),
      tolerance = 1e-6
    )
    testthat::expect_equal(
      terra::compareGeom(r$riparianFraction, f$PlanningGrid),
      TRUE
    )
    testthat::expect_equal(terra::datatype(r$riparianFraction), "FLT8S")
  }
})

testthat::test_that("lake interiors, shoreline buffers and overlaps count once", {
  f <- sol_fixture(0)
  f$streams <- f$streams[FALSE, ]
  f$lakes <- sol_rect(0, 50, -100, 200)
  f$lakes$Lake_area <- 1
  testthat::expect_equal(
    sol_vals(sol_run(sol_prepared(f))$riparianFraction),
    0.5,
    tolerance = 1e-10
  )
  f$policy <- sol_policy(20)
  testthat::expect_equal(
    sol_vals(sol_run(sol_prepared(f))$riparianFraction),
    0.7,
    tolerance = 1e-6
  )
  f$streams <- sol_line(45)
  testthat::expect_equal(
    sol_vals(sol_run(sol_prepared(f))$riparianFraction),
    0.7,
    tolerance = 1e-6
  )
  f$policy <- sol_policy(0)
  f$lakes <- sol_rect(-1000, 1000, -1000, 1000)
  f$lakes$Lake_area <- 10
  f$Jurisdiction <- f$Jurisdiction[FALSE, ]
  testthat::expect_equal(sol_vals(sol_run(sol_prepared(f))$riparianFraction), 1)
})

testthat::test_that("receiving policy and halo features cross jurisdiction and study borders", {
  f <- sol_fixture()
  f$Jurisdiction <- rbind(sol_rect(0, 50), sol_rect(50, 100))
  f$Jurisdiction$jurisdiction_code <- c("ON", "QC")
  f$policy$small_stream <- c(20, 10, 30)
  f$streams <- sol_line(45)
  # ON gets x=35..50 (15 m); QC gets x=50..75 (25 m).
  testthat::expect_equal(
    sol_vals(sol_run(sol_prepared(f))$riparianFraction),
    0.4,
    tolerance = 1e-6
  )
  f$Jurisdiction <- sol_fixture()$Jurisdiction
  f$policy <- sol_policy(20)
  f$streams <- sol_line(-10)
  p <- sol_prepared(f)
  testthat::expect_equal(unname(terra::ext(p$streams)[1]), -10)
  testthat::expect_equal(
    sol_vals(sol_run(p)$riparianFraction),
    0.1,
    tolerance = 1e-8
  )
  testthat::expect_equal(p$haloWidth_m, 20)
  testthat::expect_match(p$sources$streams$coverage, "unknown")
})

testthat::test_that("partial study cells, non-square cells and tiled append agree", {
  f <- sol_fixture(20, lake = TRUE)
  f$studyArea <- sol_rect(0, 150, 0, 100)
  f$Jurisdiction <- terra::deepcopy(f$studyArea)
  f$Jurisdiction$jurisdiction_code <- "ON"
  f$PlanningGrid <- terra::rast(
    xmin = 0,
    xmax = 300,
    ymin = 0,
    ymax = 100,
    nrows = 2,
    ncols = 3,
    crs = "EPSG:5070"
  )
  p <- sol_prepared(f)
  tiled <- suppressWarnings(sol_run(p, tile = 1L, export = TRUE))
  whole <- sol_run(p, tile = 128L)
  testthat::expect_equal(
    sol_vals(tiled$riparianFraction),
    sol_vals(whole$riparianFraction),
    tolerance = 1e-8
  )
  testthat::expect_equal(
    terra::compareGeom(tiled$riparianFraction, f$PlanningGrid),
    TRUE
  )
  testthat::expect_equal(
    is.na(sol_vals(tiled$riparianFraction)),
    c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE)
  )
  shape <- terra::vect(tiled$vectorBufferFile)
  terra::values(f$PlanningGrid) <- 1
  testthat::expect_equal(terra::same.crs(shape, f$PlanningGrid), TRUE)
  testthat::expect_equal(names(shape), "tile_id")
  testthat::expect_gte(length(unique(shape$tile_id)), 2L)
  testthat::expect_equal(
    sol_vals(tiled$riparianFraction)[!is.na(sol_vals(tiled$riparianFraction))],
    exactTileFractions(f$PlanningGrid, shape)[
      !is.na(sol_vals(tiled$riparianFraction))
    ],
    tolerance = 1e-9
  )
  summed <- sum(terra::expanse(shape, transform = FALSE))
  dissolved <- sum(terra::expanse(terra::aggregate(shape), transform = FALSE))
  testthat::expect_equal(summed, dissolved, tolerance = 1e-9)
  testthat::expect_match(basename(tiled$vectorBufferFile), "[.]gpkg$")
  testthat::expect_true(file.exists(tiled$vectorBufferFile))
  testthat::expect_identical(sf::st_layers(tiled$vectorBufferFile)$name, "riparian_buffers")
  testthat::expect_null(whole$vectorBufferFile)
  testthat::expect_length(list.files(dirname(whole$rasterFile), "\\.gpkg$"), 0)
})

testthat::test_that("fully lake-covered non-square cells and partial study cells use full denominator", {
  f <- sol_fixture(0)
  f$studyArea <- sol_rect(0, 150, 0, 100)
  f$Jurisdiction <- terra::deepcopy(f$studyArea)
  f$Jurisdiction$jurisdiction_code <- "ON"
  f$PlanningGrid <- terra::rast(
    xmin = 0,
    xmax = 300,
    ymin = 0,
    ymax = 100,
    nrows = 2,
    ncols = 3,
    crs = "EPSG:5070"
  )
  f$lakes <- sol_rect(-100, 400, -100, 200)
  f$lakes$Lake_area <- 5
  result <- tryCatch(
    suppressWarnings(sol_run(sol_prepared(f))),
    error = identity
  )
  testthat::expect_identical(
    if (inherits(result, "error")) conditionMessage(result) else NULL,
    NULL,
    info = "Valid non-square full-cell water coverage must not be rejected."
  )
  if (!inherits(result, "error")) {
    testthat::expect_equal(
      sol_vals(result$riparianFraction),
      c(1, 0.5, NA, 1, 0.5, NA),
      tolerance = 1e-8
    )
  }
})

testthat::test_that("all four classes contribute their configured widths", {
  for (kind in c("streams", "lakes")) {
    for (large in c(FALSE, TRUE)) {
      f <- sol_fixture(0)
      f$streams <- f$streams[FALSE, ]
      field <- paste0(
        if (large) "large_" else "small_",
        if (kind == "streams") "stream" else "lake"
      )
      f$policy[[field]] <- 20
      if (kind == "streams") {
        f$streams <- sol_line(order = if (large) 4 else 3)
        expected <- 0.4
      } else {
        f$lakes <- sol_rect(0, 50, -100, 200)
        f$lakes$Lake_area <- if (large) 1 else 0.5
        expected <- 0.7
      }
      testthat::expect_equal(
        sol_vals(sol_run(sol_prepared(f))$riparianFraction),
        expected,
        tolerance = 1e-6
      )
    }
  }
})

testthat::test_that("empty exports are readable and existing outputs are protected", {
  f <- sol_fixture(0)
  f$streams <- f$streams[FALSE, ]
  p <- sol_prepared(f)
  r <- sol_run(p, export = TRUE)
  testthat::expect_equal(sol_vals(r$riparianFraction), 0)
  empty <- sf::st_read(r$vectorBufferFile, quiet = TRUE)
  testthat::expect_equal(nrow(empty), 0L)
  testthat::expect_equal(
    sf::st_crs(empty)$wkt,
    sf::st_crs(terra::crs(f$PlanningGrid))$wkt
  )
  hash <- tools::md5sum(list.files(dirname(r$rasterFile), full.names = TRUE))
  sol_error(sol_run(p, output = dirname(r$rasterFile)), "overwrite")
  testthat::expect_identical(tools::md5sum(names(hash)), hash)
  sidecar <- sol_dir("sidecar")
  writeLines(
    "protected",
    file.path(sidecar, "riparian_fraction.tif.aux.xml")
  )
  sol_error(sol_run(p, output = sidecar), "overwrite")
  for (bad in list(NA, 1, "TRUE", logical(), c(TRUE, FALSE))) {
    sol_error(sol_run(p, export = bad), "logical")
  }
  for (bad in list(0, -1, 1.5, Inf, NA_real_, c(1, 2), "2")) {
    sol_error(sol_run(p, tile = bad), "positive integer")
  }
})

testthat::test_that("exact API preserves cell IDs and rejects double coverage", {
  grid <- terra::rast(
    xmin = 0,
    xmax = 200,
    ymin = 0,
    ymax = 100,
    nrows = 1,
    ncols = 2,
    crs = "EPSG:5070"
  )
  terra::values(grid) <- 1
  testthat::expect_equal(
    exactTileFractions(grid, sol_rect(125, 175)),
    c(0, 0.5)
  )
  testthat::expect_equal(exactTileFractions(grid, NULL), c(0, 0))
  testthat::expect_equal(
    exactTileFractions(grid, sol_rect(300, 400)),
    c(0, 0)
  )
  sol_error(
    exactTileFractions(grid, rbind(sol_rect(), sol_rect())),
    "outside"
  )
})

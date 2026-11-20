testthat::test_that("jurisdictions are cropped near the study area without changing the result", {
  f <- sol_fixture(20, lake = TRUE)
  # A jurisdiction much larger than the study area, as in the real data.
  f$Jurisdiction <- sol_rect(-5000, 5000, -5000, 5000)
  f$Jurisdiction$jurisdiction_code <- "ON"
  f$Jurisdiction$NAME_En <- "Ontario"
  f$Jurisdiction$COUNTRY <- "Canada"
  full <- sol_prepared(f, jurisdictionMargin_m = 1e9)
  cropped <- sol_prepared(f, jurisdictionMargin_m = 200)
  testthat::expect_lt(
    sum(terra::expanse(cropped$Jurisdiction, transform = FALSE)),
    sum(terra::expanse(full$Jurisdiction, transform = FALSE))
  )
  a <- suppressMessages(sol_run(full, tile = 1L))
  b <- suppressMessages(sol_run(cropped, tile = 1L))
  testthat::expect_identical(sol_vals(a$riparianFraction), sol_vals(b$riparianFraction))
  # cropToRegion keeps the box and tolerates other CRSs and empty input.
  region <- sol_rect(0, 100)
  testthat::expect_equal(unname(as.vector(terra::ext(cropToRegion(f$Jurisdiction, region)))), c(0, 100, 0, 100))
  testthat::expect_identical(cropToRegion(f$Jurisdiction, NULL), f$Jurisdiction)
  testthat::expect_equal(nrow(cropToRegion(f$Jurisdiction[FALSE, ], region)), 0L)
  geo <- terra::project(f$Jurisdiction, "EPSG:4326")
  testthat::expect_gt(nrow(cropToRegion(geo, region)), 0L)
})

testthat::test_that("policy names, all province mappings and fallback are explicit", {
  j <- do.call(rbind, rep(list(sol_rect()), 8))
  j$NAME_En <- c(
    "Ontario",
    "Qu\u00e9bec",
    "New Brunswick",
    "Nova Scotia",
    "Prince Edward Island",
    "Newfoundland and Labrador",
    "Maine",
    "Unknown"
  )
  p <- sol_policy(30)
  p <- rbind(
    p,
    transform(p[1, ], jurisdiction = "NB"),
    transform(p[1, ], jurisdiction = "NS"),
    transform(p[1, ], jurisdiction = "PE"),
    transform(p[1, ], jurisdiction = "NL")
  )
  p$jurisdiction[1:3] <- c(" DEFAULT ", " on ", "qc")
  before <- sol_signature(j)
  r <- resolveRiparianPolicy_sol6_1(p, terra::deepcopy(j))
  testthat::expect_identical(
    r$Jurisdiction$jurisdiction_code,
    c("ON", "QC", "NB", "NS", "PE", "NL", "MAINE", "UNKNOWN")
  )
  testthat::expect_equal(r$diagnostics$requested_code, c("MAINE", "UNKNOWN"))
  testthat::expect_equal(r$policy$small_stream, rep(30, 9))
  testthat::expect_identical(sol_signature(j), before)
})

testthat::test_that("invalid policies and jurisdiction keys stop before geometry work", {
  f <- sol_fixture()
  invalid <- list(
    f$policy[-1, ],
    rbind(f$policy, f$policy[2, ]),
    transform(f$policy, small_stream = -1),
    transform(f$policy, large_stream = NA_real_),
    transform(f$policy, small_lake = Inf),
    transform(f$policy, large_lake = "20"),
    f$policy[, -2],
    transform(f$policy, jurisdiction = "")
  )
  for (p in invalid) {
    sol_error(resolveRiparianPolicy_sol6_1(p, f$Jurisdiction), "[Pp]olicy")
  }
  sol_error(resolveRiparianPolicy_sol6_1(f$policy, f$studyArea), "requires")
  f$Jurisdiction$jurisdiction_code <- NA_character_
  sol_error(resolveRiparianPolicy_sol6_1(f$policy, f$Jurisdiction), "missing")
})

testthat::test_that("classification thresholds and malformed attributes are validated", {
  s <- rbind(sol_line(20, order = 3), sol_line(80, order = 4))
  l <- rbind(sol_rect(0, 20), sol_rect(80, 100))
  l$Lake_area <- c(0.999, 1)
  testthat::expect_equal(
    classifyHydrology_sol6_1(s, "streams")$hydro_class,
    c("small_stream", "large_stream")
  )
  testthat::expect_equal(
    classifyHydrology_sol6_1(l, "lakes")$hydro_class,
    c("small_lake", "large_lake")
  )
  for (bad in list(NA_real_, -1, Inf, "4")) {
    x <- terra::deepcopy(s)
    x$ORD_STRA <- rep(bad, 2)
    sol_error(classifyHydrology_sol6_1(x, "streams"), "finite")
  }
  sol_error(classifyHydrology_sol6_1(s[, 0], "streams"), "requires")
  sol_error(classifyHydrology_sol6_1(l[, 0], "lakes"), "requires")
  s$hydro_class <- rep("large_stream", 2)
  sol_error(classifyHydrology_sol6_1(s, "streams"), "conflicts")
  s <- s[, "hydro_class"]
  testthat::expect_equal(
    classifyHydrology_sol6_1(s, "streams")$hydro_class,
    rep("large_stream", 2)
  )
  s$hydro_class <- c("invalid", NA_character_)
  sol_error(classifyHydrology_sol6_1(s, "streams"), "requires")
})

testthat::test_that("CRS, grid geometry, repair and jurisdiction coverage are checked", {
  f <- sol_fixture()
  sol_error(
    validateMetreCRS_sol6_1(terra::project(f$studyArea, "EPSG:4326"), "area"),
    "projected"
  )
  sol_error(
    validateMetreCRS_sol6_1(terra::project(f$studyArea, "EPSG:2263"), "area"),
    "metres"
  )
  noCRS <- terra::deepcopy(f$studyArea)
  terra::crs(noCRS) <- ""
  sol_error(validateVector_sol6_1(noCRS, "area", "polygons"), "no CRS")
  sol_error(validateVector_sol6_1(f$streams, "area", "polygons"), "polygons")
  sol_error(validateVector_sol6_1(1, "area", "polygons"), "SpatVector")
  sol_error(
    validatePlanningGrid_sol6_1(c(f$PlanningGrid, f$PlanningGrid), f$studyArea),
    "single-layer"
  )
  small <- terra::rast(
    xmin = 0,
    xmax = 50,
    ymin = 0,
    ymax = 100,
    nrows = 1,
    ncols = 1,
    crs = "EPSG:5070"
  )
  sol_error(validatePlanningGrid_sol6_1(small, f$studyArea), "extent")
  other <- terra::project(f$PlanningGrid, "EPSG:32618")
  sol_error(validatePlanningGrid_sol6_1(other, f$studyArea), "share")
  gap <- sol_rect(0, 40)
  sol_error(
    validateJurisdictionCoverage_sol6_1(gap, f$studyArea, f$lakes),
    "uncovered"
  )
  overlap <- rbind(sol_rect(0, 60), sol_rect(40, 100))
  sol_error(
    validateJurisdictionCoverage_sol6_1(overlap, f$studyArea, f$lakes),
    "overlap"
  )
  sol_error(
    validateJurisdictionCoverage_sol6_1(gap[FALSE, ], f$studyArea, f$lakes),
    "no polygons"
  )
  testthat::expect_identical(
    validateJurisdictionCoverage_sol6_1(gap[FALSE, ], f$studyArea, f$studyArea),
    TRUE
  )
  bow <- terra::vect(sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(0, 0),
      c(100, 100),
      c(0, 100),
      c(100, 0),
      c(0, 0)
    ))),
    crs = 5070
  ))
  before <- sol_signature(bow)
  repaired <- validateVector_sol6_1(bow, "bowtie", "polygons")
  testthat::expect_equal(repaired$repaired, 1)
  testthat::expect_equal(terra::is.valid(repaired$vector), TRUE)
  testthat::expect_identical(sol_signature(bow), before)
})

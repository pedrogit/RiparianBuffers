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
  r <- resolveRiparianPolicy(p, terra::deepcopy(j))
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
    sol_error(resolveRiparianPolicy(p, f$Jurisdiction), "[Pp]olicy")
  }
  sol_error(resolveRiparianPolicy(f$policy, f$studyArea), "requires")
  f$Jurisdiction$jurisdiction_code <- NA_character_
  sol_error(resolveRiparianPolicy(f$policy, f$Jurisdiction), "missing")
})

testthat::test_that("classification thresholds and malformed attributes are validated", {
  s <- rbind(sol_line(20, order = 3), sol_line(80, order = 4))
  l <- rbind(sol_rect(0, 20), sol_rect(80, 100))
  l$Lake_area <- c(0.999, 1)
  testthat::expect_equal(
    classifyHydrology(s, "streams")$hydro_class,
    c("small_stream", "large_stream")
  )
  testthat::expect_equal(
    classifyHydrology(l, "lakes")$hydro_class,
    c("small_lake", "large_lake")
  )
  for (bad in list(NA_real_, -1, Inf, "4")) {
    x <- terra::deepcopy(s)
    x$ORD_STRA <- rep(bad, 2)
    sol_error(classifyHydrology(x, "streams"), "finite")
  }
  sol_error(classifyHydrology(s[, 0], "streams"), "requires")
  sol_error(classifyHydrology(l[, 0], "lakes"), "requires")
  s$hydro_class <- rep("large_stream", 2)
  sol_error(classifyHydrology(s, "streams"), "conflicts")
  s <- s[, "hydro_class"]
  testthat::expect_equal(
    classifyHydrology(s, "streams")$hydro_class,
    rep("large_stream", 2)
  )
  s$hydro_class <- c("invalid", NA_character_)
  sol_error(classifyHydrology(s, "streams"), "requires")
})

testthat::test_that("CRS, grid geometry, repair and jurisdiction coverage are checked", {
  f <- sol_fixture()
  sol_error(
    validateMetreCRS(terra::project(f$studyArea, "EPSG:4326"), "area"),
    "projected"
  )
  sol_error(
    validateMetreCRS(terra::project(f$studyArea, "EPSG:2263"), "area"),
    "metres"
  )
  noCRS <- terra::deepcopy(f$studyArea)
  terra::crs(noCRS) <- ""
  sol_error(validateVector(noCRS, "area", "polygons"), "no CRS")
  sol_error(validateVector(f$streams, "area", "polygons"), "polygons")
  sol_error(validateVector(1, "area", "polygons"), "SpatVector")
  sol_error(
    validatePlanningGrid(c(f$PlanningGrid, f$PlanningGrid), f$studyArea),
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
  sol_error(validatePlanningGrid(small, f$studyArea), "extent")
  other <- terra::project(f$PlanningGrid, "EPSG:32618")
  sol_error(validatePlanningGrid(other, f$studyArea), "share")
  gap <- sol_rect(0, 40)
  sol_error(
    validateJurisdictionCoverage(gap, f$studyArea, f$lakes),
    "uncovered"
  )
  overlap <- rbind(sol_rect(0, 60), sol_rect(40, 100))
  sol_error(
    validateJurisdictionCoverage(overlap, f$studyArea, f$lakes),
    "overlap"
  )
  sol_error(
    validateJurisdictionCoverage(gap[FALSE, ], f$studyArea, f$lakes),
    "no polygons"
  )
  testthat::expect_identical(
    validateJurisdictionCoverage(gap[FALSE, ], f$studyArea, f$studyArea),
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
  repaired <- validateVector(bow, "bowtie", "polygons")
  testthat::expect_equal(repaired$repaired, 1)
  testthat::expect_equal(terra::is.valid(repaired$vector), TRUE)
  testthat::expect_identical(sol_signature(bow), before)
})

testthat::test_that("non-lake land area is tile-independent and handles overlapping lakes", {
  study <- sol_rect(0, 100)
  # Two overlapping lakes: summed areas would double-count the overlap.
  lakes <- rbind(sol_rect(10, 50, 10, 50), sol_rect(30, 70, 30, 70))
  expected <- 100 * 100 - (40 * 40 + 40 * 40 - 20 * 20)
  for (tile in c(7, 25, 100, 1000)) {
    testthat::expect_equal(
      nonLakeLandArea(study, lakes, tileSize = tile),
      expected,
      tolerance = 1e-9
    )
  }
  testthat::expect_equal(nonLakeLandArea(study, lakes[FALSE, ]), 1e4)
  testthat::expect_equal(nonLakeLandArea(study, study), 0, tolerance = 1e-9)
})

testthat::test_that("coverage gap is computed only against lakes that touch it", {
  study <- sol_rect(0, 100)
  jur <- sol_rect(0, 60)
  # A lake fills the entire uncovered strip, so no uncovered land remains.
  testthat::expect_identical(
    validateJurisdictionCoverage(jur, study, sol_rect(60, 100)),
    TRUE
  )
  # A distant lake must not hide the uncovered strip.
  sol_error(
    validateJurisdictionCoverage(jur, study, sol_rect(0, 20)),
    "uncovered"
  )
})

testthat::test_that("small uncovered fringes take the neighbour policy; large ones stop", {
  study <- sol_rect(0, 100)
  jur <- sol_rect(0, 60)
  jur$jurisdiction_code <- "ON"
  noLakes <- sol_rect(0, 1)[FALSE, ]
  # The 40 x 100 m strip is 0.004 km2: under the limit it joins its neighbour.
  out <- assignFringeToNeighbour(jur, study, noLakes, maxArea_km2 = 0.01)
  testthat::expect_equal(out$fringe$pieces, 1L)
  testthat::expect_equal(out$fringe$area_km2, 0.004, tolerance = 1e-9)
  testthat::expect_identical(out$fringe$jurisdiction_code, "ON")
  testthat::expect_equal(
    sum(terra::expanse(out$Jurisdiction, transform = FALSE)),
    1e4,
    tolerance = 1e-9
  )
  testthat::expect_identical(
    validateJurisdictionCoverage(out$Jurisdiction, study, noLakes),
    TRUE
  )
  # At or above the limit the piece is a genuine hole.
  sol_error(
    assignFringeToNeighbour(jur, study, noLakes, maxArea_km2 = 0.004),
    "uncovered non-lake land piece"
  )
  # Zero disables the step and leaves inputs unchanged.
  off <- assignFringeToNeighbour(jur, study, noLakes, maxArea_km2 = 0)
  testthat::expect_null(off$fringe)
  testthat::expect_equal(nrow(off$Jurisdiction), 1L)
})

testthat::test_that("fringe goes to the longest shared border or the nearest jurisdiction", {
  study <- sol_rect(0, 100)
  noLakes <- sol_rect(0, 1)[FALSE, ]
  # Two jurisdictions flank a gap; QC shares the longer edge with the 0-100 y gap.
  on <- sol_rect(0, 40, 0, 100); on$jurisdiction_code <- "ON"
  qc <- sol_rect(60, 100, 0, 100); qc$jurisdiction_code <- "QC"
  jur <- rbind(on, qc)
  # Gap strip x 40-60: equal borders, so a tie falls to one of the two.
  out <- assignFringeToNeighbour(jur, study, noLakes, maxArea_km2 = 1)
  testthat::expect_true(out$fringe$jurisdiction_code %in% c("ON", "QC"))
  # Asymmetric borders: QC touches only part of the gap's edge.
  qc2 <- sol_rect(60, 100, 0, 30); qc2$jurisdiction_code <- "QC"
  on2 <- sol_rect(0, 60, 0, 100); on2$jurisdiction_code <- "ON"
  gapStudy <- sol_rect(0, 100, 0, 100)
  out2 <- assignFringeToNeighbour(rbind(on2, qc2), gapStudy, noLakes, maxArea_km2 = 1)
  # The region x 60-100, y 30-100 touches ON along 70 m and QC along 40 m.
  testthat::expect_identical(out2$fringe$jurisdiction_code, "ON")
  # A piece separated from every jurisdiction goes to the nearest one.
  far <- sol_rect(0, 10, 0, 100); far$jurisdiction_code <- "ON"
  out3 <- assignFringeToNeighbour(far, sol_rect(0, 12, 0, 100), noLakes, maxArea_km2 = 1)
  testthat::expect_identical(out3$fringe$jurisdiction_code, "ON")
})

testthat::test_that("tiled neighbour assignment is tile-independent and reports timing", {
  on <- sol_rect(0, 40, 0, 100); on$jurisdiction_code <- "ON"
  qc <- sol_rect(60, 100, 0, 100); qc$jurisdiction_code <- "QC"
  jur <- rbind(on, qc)
  # Four pieces: two near ON, one near QC, one touching neither within 1 m.
  pieces <- rbind(
    sol_rect(40, 42, 10, 20), sol_rect(38, 40, 60, 70),
    sol_rect(58, 60, 30, 40), sol_rect(50, 52, 80, 90)
  )
  one <- assignPiecesToJurisdiction(pieces, jur, tileSize = 1000)
  small <- assignPiecesToJurisdiction(pieces, jur, tileSize = 15)
  testthat::expect_identical(one$row, small$row)
  testthat::expect_equal(one$row[1:3], c(1L, 1L, 2L))
  testthat::expect_true(all(c("component", "seconds", "peak_rss_MB") %in% names(one$timing)))
  # A piece straddling both neighbours goes to the one sharing the longer border.
  both <- sol_rect(40, 60, 0, 100)
  out <- assignPiecesToJurisdiction(both, jur)
  testthat::expect_true(out$row %in% 1:2)
})

testthat::test_that("uncovered non-lake land splits into pieces and removes only touching lakes", {
  study <- sol_rect(0, 100)
  # Two separate uncovered strips: x 0-10 and x 90-100.
  jur <- sol_rect(10, 90)
  # One lake straddles the left strip's edge; a distant lake lies inside the jurisdiction.
  lakes <- rbind(sol_rect(0, 20, 40, 60), sol_rect(40, 60, 40, 60))
  gap <- uncoveredNonLakeLand(jur, study, lakes)
  # Left strip 10 x 100 minus the 10 x 20 lake overlap, plus the full right strip.
  testthat::expect_equal(
    sum(terra::expanse(gap, transform = FALSE)),
    (10 * 100 - 10 * 20) + 10 * 100,
    tolerance = 1e-9
  )
  # With no lakes the remainder is returned unchanged in area.
  testthat::expect_equal(
    sum(terra::expanse(uncoveredNonLakeLand(jur, study, lakes[FALSE, ]), transform = FALSE)),
    2000,
    tolerance = 1e-9
  )
  # A lake covering one whole strip removes it; a full cover leaves nothing.
  full <- uncoveredNonLakeLand(jur, study, rbind(sol_rect(0, 10), sol_rect(90, 100)))
  testthat::expect_equal(sum(terra::expanse(full, transform = FALSE)), 0, tolerance = 1e-9)
})

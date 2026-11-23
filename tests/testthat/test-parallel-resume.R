sol_multi_fixture <- function() {
  f <- sol_fixture(20, lake = TRUE)
  f$studyArea <- sol_rect(0, 150, 0, 100)
  f$Jurisdiction <- terra::deepcopy(f$studyArea)
  f$Jurisdiction$jurisdiction_code <- "ON"
  f$PlanningGrid <- terra::rast(
    xmin = 0, xmax = 300, ymin = 0, ymax = 100,
    nrows = 4, ncols = 6, crs = "EPSG:5070"
  )
  sol_prepared(f)
}

sol_run_with <- function(p, ..., scratch = sol_dir("stage")) {
  args <- utils::modifyList(
    list(outputDir = sol_dir("output"), scratchDir = scratch, exportVector = FALSE),
    list(...)
  )
  do.call(
    fractionalCoverage,
    c(p[c("studyArea", "PlanningGrid", "Jurisdiction", "policy", "streams", "lakes")], args)
  )
}

testthat::test_that("workers give the same raster as a single process", {
  p <- sol_multi_fixture()
  one <- suppressWarnings(suppressMessages(sol_run_with(p, tileCells = 1L, workers = 1L, resume = FALSE)))
  many <- suppressWarnings(suppressMessages(sol_run_with(p, tileCells = 1L, workers = 3L, resume = FALSE)))
  testthat::expect_identical(sol_vals(many$riparianFraction), sol_vals(one$riparianFraction))
  # More workers than tiles is harmless.
  few <- suppressWarnings(suppressMessages(sol_run_with(p, tileCells = 4L, workers = 4L, resume = FALSE)))
  whole <- suppressWarnings(suppressMessages(sol_run_with(p, tileCells = 128L, workers = 1L, resume = FALSE)))
  testthat::expect_equal(sol_vals(few$riparianFraction), sol_vals(whole$riparianFraction), tolerance = 1e-12)
})

testthat::test_that("the vector export works with workers and matches a single process", {
  p <- sol_multi_fixture()
  one <- suppressWarnings(suppressMessages(
    sol_run_with(p, tileCells = 1L, workers = 1L, exportVector = TRUE)
  ))
  many <- suppressWarnings(suppressMessages(
    sol_run_with(p, tileCells = 1L, workers = 3L, exportVector = TRUE)
  ))
  a <- sf::st_read(one$vectorBufferFile, quiet = TRUE)
  b <- sf::st_read(many$vectorBufferFile, quiet = TRUE)
  testthat::expect_identical(a$tile_id, b$tile_id)
  testthat::expect_equal(as.numeric(sf::st_area(a)), as.numeric(sf::st_area(b)))
})

testthat::test_that("a failing tile is named, and a rerun resumes from saved tiles", {
  p <- sol_multi_fixture()
  scratch <- sol_dir("resume")
  reference <- suppressWarnings(suppressMessages(sol_run_with(p, tileCells = 1L, workers = 1L, resume = FALSE)))
  # Break the footprint builder for one tile only.
  env <- environment(fractionalCoverage)
  original <- get("exactTileFractions", env)
  calls <- 0L
  assign("exactTileFractions", function(template, footprint) {
    calls <<- calls + 1L
    if (calls == 6L) stop("simulated failure")
    original(template, footprint)
  }, env)
  failed <- tryCatch(
    suppressWarnings(suppressMessages(
      sol_run_with(p, tileCells = 1L, workers = 1L, scratch = scratch)
    )),
    error = identity
  )
  assign("exactTileFractions", original, env)
  testthat::expect_s3_class(failed, "error")
  testthat::expect_match(conditionMessage(failed), "tile [0-9]+ \\(row [0-9]+, column [0-9]+, extent .*simulated failure")
  progress <- file.path(scratch, "riparian_progress")
  # The patched function changed the code signature; restore the genuine one,
  # as it would be for a real failure that left the code unchanged.
  fingerprint <- readRDS(file.path(progress, "fingerprint.rds"))
  fingerprint$code <- codeSignature(list(
    processTile, buildPolicyFootprints, exactTileFractions, clipToDestination,
    selectIntersecting, intersectPolygons, unionPolygons, polygonOnly
  ))
  saveRDS(fingerprint, file.path(progress, "fingerprint.rds"))
  saved <- list.files(progress, "^tile_[0-9]+[.]rds$")
  testthat::expect_gt(length(saved), 0L)
  testthat::expect_lt(length(saved), 24L)
  # The rerun reports resuming, computes only the missing tiles, and cleans up.
  messages <- character()
  resumed <- withCallingHandlers(
    suppressWarnings(sol_run_with(p, tileCells = 1L, workers = 2L, scratch = scratch)),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  testthat::expect_true(any(grepl("Resuming: [0-9]+ of 24", messages)))
  testthat::expect_identical(sol_vals(resumed$riparianFraction), sol_vals(reference$riparianFraction))
  testthat::expect_false(dir.exists(progress))
})

testthat::test_that("saved tiles are discarded when the inputs change", {
  p <- sol_multi_fixture()
  scratch <- sol_dir("stale")
  fp <- tileFingerprint(
    list(gridExtent = c(0, 300, 0, 100), resolution = c(50, 25), dim = c(4L, 6L),
         crs = "x", policy = p$policy, maxWidth = 20,
         studyArea = p$studyArea, Jurisdiction = p$Jurisdiction,
         streams = p$streams, lakes = p$lakes),
    tileCells = 1L, functions = list(processTile)
  )
  dir <- file.path(scratch, "progress")
  testthat::expect_length(openTileProgress(dir, fp), 0L)
  saveTileProgress(dir, 3L, matrix(1))
  testthat::expect_identical(openTileProgress(dir, fp), 3L)
  changed <- fp
  changed$maxWidth <- 40
  testthat::expect_length(suppressMessages(openTileProgress(dir, changed)), 0L)
  testthat::expect_length(list.files(dir, "^tile_"), 0L)
})

testthat::test_that("resume is off with vector export, and invalid arguments are rejected", {
  p <- sol_multi_fixture()
  scratch <- sol_dir("noresume")
  out <- suppressWarnings(suppressMessages(
    sol_run_with(p, tileCells = 2L, exportVector = TRUE, scratch = scratch)
  ))
  testthat::expect_false(dir.exists(file.path(scratch, "riparian_progress")))
  sol_error(sol_run_with(p, workers = 0), "workers")
  sol_error(sol_run_with(p, workers = 1.5), "workers")
  sol_error(sol_run_with(p, resume = NA), "resume")
})

testthat::test_that("worker failures are reported with the job number", {
  testthat::expect_error(
    runJobs(
      jobs = list(list(x = 1), list(x = 2), list(x = 3)),
      run = function(ctx, job) if (job$x == 2) stop("bad job") else job$x,
      ctx = list(), workers = 2L, tmpDir = sol_dir("jobs"), onResult = function(i, v) NULL
    ),
    "failed on job 2: bad job"
  )
  got <- numeric()
  runJobs(
    jobs = lapply(1:7, function(i) list(x = i)),
    run = function(ctx, job) job$x * ctx$m,
    ctx = list(m = 10), workers = 3L, tmpDir = sol_dir("jobs"),
    onResult = function(i, v) got[i] <<- v
  )
  testthat::expect_identical(got, (1:7) * 10)
})

testthat::test_that("fringe assignment gives the same answer with workers", {
  on <- sol_rect(0, 40, 0, 100); on$jurisdiction_code <- "ON"
  qc <- sol_rect(60, 100, 0, 100); qc$jurisdiction_code <- "QC"
  jur <- rbind(on, qc)
  pieces <- rbind(
    sol_rect(40, 42, 10, 20), sol_rect(38, 40, 60, 70),
    sol_rect(58, 60, 30, 40), sol_rect(50, 52, 80, 90)
  )
  one <- assignPiecesToJurisdiction(pieces, jur, tileSize = 15, workers = 1L)
  many <- assignPiecesToJurisdiction(pieces, jur, tileSize = 15, workers = 3L, workDir = sol_dir("assign"))
  testthat::expect_identical(many$row, one$row)
  testthat::expect_identical(many$method, one$method)
  testthat::expect_true(all(c("component", "seconds", "peak_rss_MB") %in% names(many$timing)))
})

testthat::test_that("a message says resume is ignored when the vector is exported", {
  p <- sol_multi_fixture()
  said <- function(...) {
    messages <- character()
    withCallingHandlers(
      suppressWarnings(sol_run_with(p, tileCells = 2L, ...)),
      message = function(m) {
        messages <<- c(messages, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    any(grepl("resume = TRUE is ignored", messages))
  }
  testthat::expect_true(said(exportVector = TRUE, resume = TRUE))
  testthat::expect_false(said(exportVector = TRUE, resume = FALSE))
  testthat::expect_false(said(exportVector = FALSE, resume = TRUE))
})

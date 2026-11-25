testthat::test_that("four missing/supplied hydrology combinations prepare only missing sources", {
  f <- sol_fixture(20, lake = TRUE)
  rawDir <- sol_dir("raw")
  raw <- list()
  for (dataset in c("streams", "lakes", "jurisdiction")) {
    source <- riparianSource(dataset)
    x <- switch(
      dataset,
      streams = f$streams,
      lakes = f$lakes,
      jurisdiction = f$Jurisdiction
    )
    path <- file.path(rawDir, source$shape)
    terra::writeVector(x, path)
    raw[[dataset]] <- list(path = path, source = source, reused = TRUE)
  }
  for (missingS in c(FALSE, TRUE)) {
    for (missingL in c(FALSE, TRUE)) {
      supplied <- f
      if (missingS) {
        supplied$streams <- NULL
      }
      if (missingL) {
        supplied$lakes <- NULL
      }
      signatures <- lapply(supplied, sol_signature)
      calls <- character()
      obtain <- function(dataset, cacheDir, reuseDir) {
        calls <<- c(calls, dataset)
        raw[[dataset]]
      }
      p <- sol_prepared(supplied, obtainSource = obtain)
      expected <- c(if (missingS) "streams", if (missingL) "lakes")
      if (is.null(expected)) {
        expected <- character()
      }
      testthat::expect_identical(calls, expected)
      testthat::expect_equal(nrow(p$streams), 1L)
      testthat::expect_equal(nrow(p$lakes), 1L)
      testthat::expect_identical(lapply(supplied, sol_signature), signatures)
      testthat::expect_equal(
        p$sources$streams$origin,
        if (missingS) raw$streams$source$name else "supplied read-only input"
      )
    }
  }
  f$Jurisdiction <- NULL
  calls <- character()
  p <- sol_prepared(f, obtainSource = function(dataset, cacheDir, reuseDir) {
    calls <<- c(calls, dataset)
    raw[[dataset]]
  })
  testthat::expect_identical(calls, "jurisdiction")
  testthat::expect_equal(p$Jurisdiction$jurisdiction_code, "ON")
})

testthat::test_that("valid empty inputs do not download and invalid inputs do not silently fall back", {
  f <- sol_fixture()
  f$streams <- f$streams[FALSE, ]
  never <- function(...) stop("UNEXPECTED NETWORK")
  p <- sol_prepared(f, obtainSource = never)
  testthat::expect_equal(nrow(p$streams), 0L)
  testthat::expect_equal(nrow(p$lakes), 0L)
  f$streams <- sol_line()[, 0]
  sol_error(sol_prepared(f, obtainSource = never), "requires valid ORD_STRA")
  f$streams <- NULL
  sol_error(sol_prepared(f, obtainSource = never), "UNEXPECTED NETWORK")
  f <- sol_fixture()
  f$policy <- NULL
  p <- sol_prepared(f, obtainSource = never)
  testthat::expect_match(p$policySource, "not legal")
  testthat::expect_equal(p$policy$jurisdiction[1], "default")
})

testthat::test_that("offline downloader writes validated source and reuses manifest and raw archives", {
  testthat::expect_equal(nzchar(Sys.which("zip")), TRUE)
  f <- sol_fixture(20, lake = TRUE)
  for (dataset in c("streams", "lakes")) {
    source <- riparianSource(dataset)
    rawDir <- sol_dir("archive_input")
    path <- file.path(rawDir, source$shape)
    terra::writeVector(if (dataset == "streams") f$streams else f$lakes, path)
    archive <- file.path(sol_dir("archive"), source$archive)
    utils::zip(
      archive,
      files = list.files(rawDir, full.names = TRUE),
      flags = "-j -q"
    )
    requests <- character()
    fakeDownload <- function(url, destfile, ...) {
      requests <<- c(requests, url)
      file.copy(archive, destfile)
      0L
    }
    cache <- sol_dir("source_cache")
    first <- obtainRiparianSource(
      dataset,
      cache,
      download = fakeDownload
    )
    testthat::expect_identical(requests, source$url)
    testthat::expect_identical(first$reused, FALSE)
    testthat::expect_equal(file.exists(first$path), TRUE)
    second <- obtainRiparianSource(
      dataset,
      cache,
      download = function(...) stop("NETWORK")
    )
    testthat::expect_identical(second$reused, TRUE)
    testthat::expect_equal(
      second$path,
      normalizePath(first$path, winslash = "/")
    )
    originalHash <- tools::md5sum(archive)
    reused <- obtainRiparianSource(
      dataset,
      sol_dir("reuse_cache"),
      reuseDir = dirname(archive),
      download = function(...) stop("NETWORK")
    )
    testthat::expect_identical(reused$reused, TRUE)
    testthat::expect_identical(tools::md5sum(archive), originalHash)
    manifest <- readRDS(file.path(
      cache,
      dataset,
      "complete_source.rds"
    ))
    testthat::expect_identical(manifest$rawUncropped, TRUE)
    testthat::expect_equal(manifest$url, source$url)
    manifest$url <- "wrong provenance"
    saveRDS(
      manifest,
      file.path(cache, dataset, "complete_source.rds")
    )
    refreshed <- obtainRiparianSource(
      dataset,
      cache,
      download = function(...) stop("NETWORK")
    )
    testthat::expect_identical(refreshed$reused, TRUE)
    testthat::expect_equal(
      readRDS(file.path(
        cache,
        dataset,
        "complete_source.rds"
      ))$url,
      source$url
    )
    # A cached raw layer preserves feature endpoints outside studyArea.
    marginBox <- sol_rect(-20, 120, -20, 120)
    selected <- readRiparianMargin(
      second,
      marginBox,
      terra::crs(f$studyArea)
    )$vector
    testthat::expect_equal(
      sol_signature(selected)$geometry,
      sol_signature(if (dataset == "streams") f$streams else f$lakes)$geometry
    )
  }
})

testthat::test_that("download failures and damaged cache report URL and local location", {
  source <- riparianSource("streams")
  for (download in list(
    function(...) stop("offline fixture"),
    function(...) 1L,
    function(url, destfile, ...) {
      writeLines("not a zip", destfile)
      0L
    }
  )) {
    cache <- sol_dir("failed_download")
    error <- suppressWarnings(sol_error(
      obtainRiparianSource("streams", cache, download = download),
      "Input preparation failed"
    ))
    testthat::expect_match(conditionMessage(error), source$url, fixed = TRUE)
    testthat::expect_match(conditionMessage(error), cache, fixed = TRUE)
  }
  cache <- sol_dir("corrupt_cache")
  localDir <- file.path(cache, "streams")
  dir.create(localDir)
  writeLines("corrupt", file.path(localDir, source$archive))
  suppressWarnings(sol_error(
    obtainRiparianSource("streams", cache, download = function(...) {
      stop("NETWORK")
    }),
    "incomplete/corrupt"
  ))
  incomplete <- file.path(sol_dir("incomplete"), source$shape)
  writeLines("fixture", incomplete)
  sol_error(
    validateShapefile(incomplete, source),
    "Incomplete shapefile"
  )
  wrong <- file.path(sol_dir("wrong_schema"), source$shape)
  terra::writeVector(sol_line()[, 0], wrong)
  sol_error(validateShapefile(wrong, source), "missing CRS or ORD_STRA")
})

testthat::test_that("downloaded ZIP missing components is not stored as a complete cache", {
  source <- riparianSource("streams")
  raw <- sol_dir("incomplete_zip")
  writeLines("fixture", file.path(raw, source$shape))
  archive <- file.path(sol_dir("zip"), source$archive)
  utils::zip(archive, list.files(raw, full.names = TRUE), flags = "-j -q")
  cache <- sol_dir("incomplete_zip_cache")
  sol_error(
    obtainRiparianSource(
      "streams",
      cache,
      download = function(url, destfile, ...) {
        file.copy(archive, destfile)
        0L
      }
    ),
    "missing or empty shapefile components"
  )
  testthat::expect_equal(
    file.exists(file.path(cache, "streams", source$archive)),
    FALSE
  )
  testthat::expect_equal(
    file.exists(file.path(
      cache,
      "streams",
      "complete_source.rds"
    )),
    FALSE
  )
})

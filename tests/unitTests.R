# Source this file, then call unitTests(moduleDir, scratchDir).
unitTests <- function(moduleDir, scratchDir) {
  stopifnot(
    dir.exists(moduleDir),
    length(scratchDir) == 1L,
    !is.na(scratchDir),
    nzchar(scratchDir)
  )
  dir.create(scratchDir, recursive = TRUE, showWarnings = FALSE)
  old <- options(
    riparian.sol6_1.moduleDir = normalizePath(moduleDir, winslash = "/"),
    riparian.sol6_1.scratchDir = normalizePath(scratchDir, winslash = "/"),
    spades.useRequire = FALSE,
    Require.install = FALSE
  )
  on.exit(options(old), add = TRUE)
  testthat::test_dir(
    file.path(moduleDir, "tests", "testthat"),
    env = new.env(parent = globalenv()),
    reporter = "summary",
    stop_on_failure = FALSE
  )
}

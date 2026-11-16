sol_module <- getOption(
  "riparian.moduleDir",
  Sys.getenv("RIPARIAN_MODULE_DIR")
)
sol_scratch <- getOption(
  "riparian.scratchDir",
  Sys.getenv("RIPARIAN_SCRATCH_DIR")
)
if (!nzchar(sol_module) || !dir.exists(sol_module) || !nzchar(sol_scratch)) {
  stop(
    "Set riparian.moduleDir and riparian.scratchDir options (or RIPARIAN_MODULE_DIR/RIPARIAN_SCRATCH_DIR)."
  )
}
dir.create(sol_scratch, recursive = TRUE, showWarnings = FALSE)
withr::local_options(
  list(spades.useRequire = FALSE, Require.install = FALSE),
  .local_envir = environment()
)
testthat::local_mocked_bindings(
  download.file = function(...) {
    stop("Network downloads are forbidden in offline tests.")
  },
  .package = "utils",
  .env = environment()
)
for (sol_file in list.files(
  file.path(sol_module, "R"),
  "\\.R$",
  full.names = TRUE
)) {
  sys.source(sol_file, envir = environment())
}
sol_dir <- function(label) {
  path <- tempfile(paste0(label, "_"), tmpdir = sol_scratch)
  dir.create(path, recursive = TRUE)
  path
}
sol_rect <- function(xmin = 0, xmax = 100, ymin = 0, ymax = 100) {
  terra::as.polygons(terra::ext(xmin, xmax, ymin, ymax), crs = "EPSG:5070")
}
sol_line <- function(x = 50, ymin = -200, ymax = 300, order = 1) {
  terra::vect(sf::st_sf(
    ORD_STRA = order,
    geometry = sf::st_sfc(
      sf::st_linestring(rbind(c(x, ymin), c(x, ymax))),
      crs = 5070
    )
  ))
}
sol_policy <- function(width = 20) {
  data.frame(
    jurisdiction = c("default", "ON", "QC"),
    small_stream = width,
    large_stream = width,
    small_lake = width,
    large_lake = width
  )
}
sol_fixture <- function(width = 20, lake = FALSE) {
  area <- sol_rect()
  j <- terra::deepcopy(area)
  j$jurisdiction_code <- "ON"
  j$NAME_En <- "Ontario"
  j$COUNTRY <- "Canada"
  l <- sol_rect(65, 85, 30, 70)
  l$Lake_area <- 0.5
  if (!lake) {
    l <- l[FALSE, ]
  }
  list(
    studyArea = area,
    PlanningGrid = terra::rast(
      terra::ext(area),
      nrows = 1,
      ncols = 1,
      crs = terra::crs(area)
    ),
    Jurisdiction = j,
    policy = sol_policy(width),
    streams = sol_line(),
    lakes = l
  )
}
sol_prepared <- function(f = sol_fixture(), ...) {
  do.call(
    prepareRiparianInputs,
    c(f, list(cacheDir = sol_dir("cache"), moduleDir = sol_module), list(...))
  )
}
sol_run <- function(
  f,
  tile = 128L,
  export = FALSE,
  output = sol_dir("output")
) {
  do.call(
    fractionalCoverage,
    c(
      f[c(
        "studyArea",
        "PlanningGrid",
        "Jurisdiction",
        "policy",
        "streams",
        "lakes"
      )],
      list(
        outputDir = output,
        scratchDir = sol_dir("stage"),
        tileCells = tile,
        exportVector = export
      )
    )
  )
}
sol_vals <- function(x) as.numeric(terra::values(x))
sol_error <- function(expr, pattern) {
  error <- tryCatch(
    {
      force(expr)
      NULL
    },
    error = identity
  )
  testthat::expect_s3_class(error, "error")
  if (inherits(error, "error")) {
    testthat::expect_match(conditionMessage(error), pattern)
  }
  invisible(error)
}
sol_signature <- function(x) {
  if (inherits(x, "SpatVector")) {
    return(list(
      geometry = terra::geom(x),
      attributes = as.data.frame(x),
      crs = terra::crs(x)
    ))
  }
  if (inherits(x, "SpatRaster")) {
    return(list(
      extent = as.vector(terra::ext(x)),
      resolution = terra::res(x),
      dimensions = dim(x),
      crs = terra::crs(x),
      values = if (terra::hasValues(x)) sol_vals(x) else NULL
    ))
  }
  x
}

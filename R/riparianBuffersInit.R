riparianBuffersInit <- function(sim) {
  # Require spatial context before preparing any optional data or writing outputs.
  required <- c("studyArea", "PlanningGrid")
  for (name in required) {
    if (is.null(sim[[name]])) {
      stop(
        "RiparianBuffers requires ",
        name,
        " before init; explicitly supply a common grid or configure one upstream provider/load order."
      )
    }
  }
  # An upstream promise must be fulfilled; downloading a competing fallback is unsafe.
  optional <- c(
    "Jurisdiction",
    "riparianBufferPolicy",
    "Hydrology_streams",
    "Hydrology_lakes"
  )
  for (name in optional) {
    if (is.null(sim[[name]]) && SpaDES.core::suppliedElsewhere(name, sim)) {
      stop(
        "Promised upstream input ",
        name,
        " is still missing at RiparianBuffers init. ",
        "Configure provider/load ordering or explicitly supply it; no competing fallback was downloaded."
      )
    }
  }
  # Read only this module's processing controls, not the original module's parameters.
  params <- SpaDES.core::P(sim, module = "RiparianBuffers")
  tile <- params$tileCells
  export <- params$exportVectorBuffer
  # Reject invalid tile sizes before coercion to integer or allocation of raster stripes.
  if (
    !is.numeric(tile) ||
      length(tile) != 1L ||
      is.na(tile) ||
      !is.finite(tile) ||
      tile < 1 ||
      tile != floor(tile) ||
      tile > .Machine$integer.max
  ) {
    stop("tileCells must be one finite positive integer.")
  }
  # Export is optional, but the decision must be one explicit TRUE or FALSE.
  if (!is.logical(export) || length(export) != 1L || is.na(export)) {
    stop("exportVectorBuffer must be one non-missing logical value.")
  }
  # Zero disables fringe assignment and keeps the strict coverage check.
  fringeMax <- params$fringeMaxArea_km2
  if (
    !is.numeric(fringeMax) ||
      length(fringeMax) != 1L ||
      is.na(fringeMax) ||
      !is.finite(fringeMax) ||
      fringeMax < 0
  ) {
    stop("fringeMaxArea_km2 must be one finite nonnegative number.")
  }
  # Isolate all writable products from the original module's files and caches.
  paths <- SpaDES.core::paths(sim)
  input <- SpaDES.core::inputPath(sim)
  output <- file.path(SpaDES.core::outputPath(sim), "RiparianBuffers")
  scratchRoot <- paths[["scratchPath"]]
  # Provide a module-specific staging location when no scratch path was configured.
  if (is.null(scratchRoot) || !length(scratchRoot) || !nzchar(scratchRoot)) {
    scratchRoot <- file.path(output, "scratch")
  }
  scratch <- file.path(scratchRoot, "RiparianBuffers")
  cache <- file.path(input, "RiparianBuffers")
  moduleDir <- file.path(SpaDES.core::modulePath(sim), "RiparianBuffers")
  # Validate supplied inputs, obtain missing sources, and select whole halo features.
  prepared <- prepareRiparianInputs(
    studyArea = sim$studyArea,
    PlanningGrid = sim$PlanningGrid,
    Jurisdiction = sim$Jurisdiction,
    policy = sim$riparianBufferPolicy,
    streams = sim$Hydrology_streams,
    lakes = sim$Hydrology_lakes,
    cacheDir = cache,
    moduleDir = moduleDir,
    reuseDir = input,
    fringeMaxArea_km2 = fringeMax
  )
  # Create destination directories only after the input contract has been checked.
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  dir.create(scratch, recursive = TRUE, showWarnings = FALSE)
  # Construct and union destination-policy footprints, then calculate tiled cell coverage.
  result <- fractionalCoverage(
    studyArea = prepared$studyArea,
    PlanningGrid = prepared$PlanningGrid,
    Jurisdiction = prepared$Jurisdiction,
    policy = prepared$policy,
    streams = prepared$streams,
    lakes = prepared$lakes,
    outputDir = output,
    scratchDir = scratch,
    tileCells = as.integer(tile),
    exportVector = export
  )
  # Prepared copies of the inputs stay local; common input objects are unchanged.
  # Attach scientific conventions and provenance to the raster and export paths.
  sim$Riparian <- c(
    result,
    list(
      policy = prepared$policy,
      method = "exactextractr projected polygon-cell coverage, grid-aligned sequential tiles",
      coverageTolerance = 1e-6,
      coverageConvention = paste(
        "Union of mapped lake interiors and destination-policy buffers,",
        "clipped to studyArea, divided by FULL planning-cell area including water; outside cells NA.",
        "This is combined lake-and-riparian-buffer coverage, not riparian land alone or harvestable land."
      ),
      lakeInteriorsExcluded = FALSE,
      borderRule = "Destination-jurisdiction policy; cross-border hydrology included through maximum-width halo",
      streamConvention = "Stream centrelines do not define explicit river water-surface polygons",
      sourceCRS = prepared$sourceCRS,
      policyFallback = prepared$policyFallback,
      fringeAssigned = prepared$fringeAssigned,
      fringeMaxArea_km2 = fringeMax,
      geometryRepairs = prepared$repairs,
      sources = prepared$sources,
      haloWidth_m = prepared$haloWidth_m,
      haloExtent = prepared$haloExtent,
      policyAssumption = prepared$policySource,
      projectionConvention = "Areas and widths are planar in the study-area CRS; projection distortion is not corrected",
      tileCells = as.integer(tile),
      exportVectorBuffer = export
    )
  )
  # Return the updated simulation without printing its potentially large contents.
  invisible(sim)
}

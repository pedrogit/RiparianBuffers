riparianBuffersInit_sol6_1 <- function(sim) {
  required <- c("studyArea", "PlanningGrid")
  for (name in required) {
    if (is.null(sim[[name]])) {
      stop(
        "riparianBuffers_Sol6_1 requires ",
        name,
        " before init; explicitly supply a common grid or configure one upstream provider/load order."
      )
    }
  }
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
        " is still missing at riparianBuffers_Sol6_1 init. ",
        "Configure provider/load ordering or explicitly supply it; no competing fallback was downloaded."
      )
    }
  }
  params <- SpaDES.core::P(sim, module = "riparianBuffers_Sol6_1")
  tile <- params$tileCells_sol6_1
  export <- params$exportVectorBuffer_sol6_1
  if (
    !is.numeric(tile) ||
      length(tile) != 1L ||
      is.na(tile) ||
      !is.finite(tile) ||
      tile < 1 ||
      tile != floor(tile) ||
      tile > .Machine$integer.max
  ) {
    stop("tileCells_sol6_1 must be one finite positive integer.")
  }
  if (!is.logical(export) || length(export) != 1L || is.na(export)) {
    stop("exportVectorBuffer_sol6_1 must be one non-missing logical value.")
  }
  paths <- SpaDES.core::paths(sim)
  input <- SpaDES.core::inputPath(sim)
  output <- file.path(SpaDES.core::outputPath(sim), "riparianBuffers_sol6_1")
  scratchRoot <- paths[["scratchPath"]]
  if (is.null(scratchRoot) || !length(scratchRoot) || !nzchar(scratchRoot)) {
    scratchRoot <- file.path(output, "scratch_sol6_1")
  }
  scratch <- file.path(scratchRoot, "riparianBuffers_sol6_1")
  cache <- file.path(input, "riparianBuffers_sol6_1")
  moduleDir <- file.path(SpaDES.core::modulePath(sim), "riparianBuffers_Sol6_1")
  prepared <- prepareRiparianInputs_sol6_1(
    studyArea = sim$studyArea,
    PlanningGrid = sim$PlanningGrid,
    Jurisdiction = sim$Jurisdiction,
    policy = sim$riparianBufferPolicy,
    streams = sim$Hydrology_streams,
    lakes = sim$Hydrology_lakes,
    cacheDir = cache,
    moduleDir = moduleDir,
    reuseDir = input
  )
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  dir.create(scratch, recursive = TRUE, showWarnings = FALSE)
  result <- fractionalCoverage_sol6_1(
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
  sim$Jurisdiction_sol6_1 <- prepared$Jurisdiction
  sim$riparianBufferPolicy_sol6_1 <- prepared$policy
  sim$Hydrology_streams_sol6_1 <- prepared$streams
  sim$Hydrology_lakes_sol6_1 <- prepared$lakes
  sim$Riparian_sol6_1 <- c(
    result,
    list(
      policy = prepared$policy,
      method = "exactextractr projected polygon-cell coverage, grid-aligned sequential tiles",
      coverageTolerance_sol6_1 = 1e-6,
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
      geometryRepairs = prepared$repairs,
      sources = prepared$sources,
      haloWidth_m = prepared$haloWidth_m,
      haloExtent = prepared$haloExtent,
      policyAssumption = prepared$policySource,
      projectionConvention = "Areas and widths are planar in the study-area CRS; projection distortion is not corrected",
      tileCells_sol6_1 = as.integer(tile),
      exportVectorBuffer_sol6_1 = export
    )
  )
  invisible(sim)
}

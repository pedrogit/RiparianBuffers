## Functions in R/ are module-scoped by SpaDES; shared inputs are read-only.
defineModule(
  sim,
  list(
    name = "riparianBuffers_Sol6_1",
    description = paste(
      "Exact destination-jurisdiction combined lake-and-riparian-buffer coverage.",
      "Mapped lake interiors are included; the denominator is full planning-cell area.",
      "Outside-study cells are NA. This is not a classification of harvestable land."
    ),
    keywords = c("hydrology", "riparian", "exact coverage"),
    authors = structure(
      list(list(
        given = "Shirin",
        family = "Varkouhi",
        role = c("aut", "cre"),
        email = "Shirin.varkuhi@gmail.com"
      )),
      class = "person"
    ),
    childModules = character(0),
    version = list(riparianBuffers_Sol6_1 = "0.1.0"),
    timeframe = as.POSIXlt(c(NA, NA)),
    timeunit = "year",
    citation = list(),
    documentation = list("README.md", "riparianBuffers_Sol6_1.Rmd", "NEWS.md"),
    reqdPkgs = list("SpaDES.core", "terra", "sf"),
    parameters = bindrows(
      defineParameter(
        "tileCells_sol6_1",
        "numeric",
        128L,
        1,
        NA,
        "Positive integer number of planning cells on each tile side."
      ),
      defineParameter(
        "exportVectorBuffer_sol6_1",
        "logical",
        TRUE,
        NA,
        NA,
        "Export final combined lake-and-buffer footprint as Shapefile alongside GeoTIFF."
      )
    ),
    inputObjects = bindrows(
      expectsInput(
        "studyArea",
        c("SpatVector", "sf"),
        "Required polygon study area in projected metre CRS."
      ),
      expectsInput(
        "PlanningGrid",
        "SpatRaster",
        "Read-only common grid; 240 m standalone fallback only if not supplied or promised upstream."
      ),
      expectsInput(
        "Jurisdiction",
        c("SpatVector", "sf"),
        "Optional receiving-jurisdiction polygons; downloaded when missing."
      ),
      expectsInput(
        "riparianBufferPolicy",
        "data.frame",
        "Optional widths in metres; bundled simplified analytical assumptions when missing."
      ),
      expectsInput(
        "Hydrology_streams",
        c("SpatVector", "sf"),
        "Optional halo-complete streams with ORD_STRA or validated hydro_class; North American HydroRIVERS when missing."
      ),
      expectsInput(
        "Hydrology_lakes",
        c("SpatVector", "sf"),
        "Optional halo-complete lakes with Lake_area (km2) or validated hydro_class; global HydroLAKES when missing."
      )
    ),
    outputObjects = bindrows(
      createsOutput(
        "Riparian_sol6_1",
        "list",
        "riparianFraction: combined lake-and-buffer coverage/full cell area; lakeInteriorsExcluded=FALSE; lake-water overlaps counted once. Includes policy and provenance; never replaces Riparian."
      ),
      createsOutput(
        "Jurisdiction_sol6_1",
        "SpatVector",
        "Prepared projected destination polygons with jurisdiction_code; common input unchanged."
      ),
      createsOutput(
        "riparianBufferPolicy_sol6_1",
        "data.frame",
        "Validated module-local policy in metres."
      ),
      createsOutput(
        "Hydrology_streams_sol6_1",
        "SpatVector",
        "Projected halo-selected original streams with validated hydro_class."
      ),
      createsOutput(
        "Hydrology_lakes_sol6_1",
        "SpatVector",
        "Projected halo-selected full lake polygons including interiors, with validated hydro_class."
      )
    )
  )
)

doEvent.riparianBuffers_Sol6_1 <- function(sim, eventTime, eventType) {
  if (identical(eventType, "init")) {
    sim <- riparianBuffersInit_sol6_1(sim)
  } else {
    stop("Undefined event type in riparianBuffers_Sol6_1: ", eventType)
  }
  invisible(sim)
}

.inputObjects <- function(sim) {
  if (is.null(sim$studyArea)) {
    stop("riparianBuffers_Sol6_1 requires studyArea.")
  }
  ## Do not compete with an upstream provider, including one whose init has not run.
  if (
    is.null(sim$PlanningGrid) &&
      !SpaDES.core::suppliedElsewhere("PlanningGrid", sim)
  ) {
    area <- validateVector_sol6_1(sim$studyArea, "studyArea", "polygons")$vector
    validateMetreCRS_sol6_1(area, "studyArea")
    e <- terra::ext(area)
    sim$PlanningGrid <- terra::rast(
      xmin = e[1],
      xmax = e[1] + ceiling((e[2] - e[1]) / 240) * 240,
      ymin = e[3],
      ymax = e[3] + ceiling((e[4] - e[3]) / 240) * 240,
      resolution = 240,
      crs = terra::crs(area)
    )
  }
  ## Other fallbacks are prepared in init, after promised upstream inputs exist.
  invisible(sim)
}

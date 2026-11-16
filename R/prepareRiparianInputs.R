riparianSource <- function(dataset) {
  # Map each supported dataset to its immutable distribution metadata and the
  # attribute/geometry contract expected after extraction.
  switch(
    dataset,
    streams = list(
      name = "HydroRIVERS North America",
      url = "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_na_shp.zip",
      archive = "HydroRIVERS_v10_na_shp.zip",
      shape = "HydroRIVERS_v10_na.shp",
      field = "ORD_STRA",
      type = "lines"
    ),
    lakes = list(
      name = "HydroLAKES global",
      url = "https://data.hydrosheds.org/file/hydrolakes/HydroLAKES_polys_v10_shp.zip",
      archive = "HydroLAKES_polys_v10_shp.zip",
      shape = "HydroLAKES_polys_v10.shp",
      field = "Lake_area",
      type = "polygons"
    ),
    jurisdiction = list(
      name = "North American political divisions 2021 v3",
      url = "https://drive.google.com/uc?export=download&id=1rJQCUJXN3m0pGBGo-bmf4qDfiZbCAg1p",
      archive = "politicalboundaries.zip",
      shape = "boundaries_p_2021_v3.shp",
      field = "NAME_En",
      type = "polygons"
    ),
    stop("Unknown riparian source: ", dataset)
  )
}

validateShapefile <- function(path, source) {
  # A shapefile is a multi-file dataset: derive the common stem before checking
  # its mandatory geometry, index, attributes, and projection sidecars.
  stem <- sub("\\.shp$", "", path, ignore.case = TRUE)
  sidecars <- paste0(stem, c(".shp", ".shx", ".dbf", ".prj"))
  # Reject absent or zero-byte components rather than letting downstream reads
  # fail with less specific spatial-data errors.
  if (!all(file.exists(sidecars)) || any(file.info(sidecars)$size <= 0)) {
    stop(
      "Incomplete shapefile at ",
      path,
      "; require nonempty .shp/.shx/.dbf/.prj components."
    )
  }
  # Read only the header to validate the cache without loading all features.
  header <- terra::vect(path, proxy = TRUE)
  # Require a declared CRS, the source-specific classification field, and the
  # advertised feature type before considering cached data trustworthy.
  if (
    !nzchar(terra::crs(header)) ||
      !source$field %in% names(header) ||
      !identical(terra::geomtype(header), source$type)
  ) {
    stop(
      "Invalid ",
      source$name,
      " cache at ",
      path,
      ": missing CRS or ",
      source$field,
      "."
    )
  }
  invisible(TRUE)
}

zipShapefileProblem <- function(contents, source, archive) {
  # Return a diagnostic so reuse can skip bad archives while new downloads fail clearly.
  main <- contents$Name[basename(contents$Name) == source$shape]
  if (length(main) != 1L) {
    return(paste("Downloaded ZIP lacks expected shapefile", source$shape))
  }
  required <- paste0(sub("\\.shp$", "", main), c(".shp", ".shx", ".dbf", ".prj"))
  if (!all(required %in% contents$Name) ||
      any(contents$Length[match(required, contents$Name)] <= 0)) {
    return(paste("Downloaded ZIP has missing or empty shapefile components:", archive))
  }
  NULL
}

obtainRiparianSource <- function(
  dataset,
  cacheDir,
  reuseDir = NULL,
  download = utils::download.file
) {
  # Resolve the selected source once so all cache, archive, and validation work
  # uses the same source definition.
  source <- riparianSource(dataset)
  # Keep each raw source in a module-specific cache namespace.
  localDir <- file.path(cacheDir, dataset)
  dir.create(localDir, recursive = TRUE, showWarnings = FALSE)
  archive <- file.path(localDir, source$archive)
  extraction <- file.path(localDir, "raw")
  marker <- file.path(localDir, "complete_source.rds")
  # Retain enough context to make failures actionable when propagated below.
  context <- paste0(source$name, " [", source$url, "] at ", localDir)
  tryCatch(
    {
      # Only trust our complete raw extraction when its URL/provenance matches.
      if (file.exists(marker)) {
        # A marker records a previously validated, uncropped extraction.
        manifest <- readRDS(marker)
        if (
          identical(manifest$url, source$url) &&
            isTRUE(manifest$rawUncropped) &&
            length(manifest$shape) == 1L &&
            file.exists(manifest$shape)
        ) {
          validateShapefile(manifest$shape, source)
          return(list(path = manifest$shape, source = source, reused = TRUE))
        }
      }
      # Old-module archives are read-only sources, never old cropped working layers.
      # Prefer the local archive, then look for an exact matching archive in the
      # optional reuse directory; never reuse an extracted derivative.
      candidates <- unique(c(
        archive,
        if (!is.null(reuseDir) && dir.exists(reuseDir)) {
          list.files(
            reuseDir,
            pattern = paste0("^", gsub("\\.", "\\\\.", source$archive), "$"),
            recursive = TRUE,
            full.names = TRUE
          )
        }
      ))
      archiveToUse <- NULL
      contents <- NULL
      # Inspect candidate ZIP directories first so corrupt or incomplete files
      # are not extracted into the working cache.
      for (candidate in candidates[file.exists(candidates)]) {
        listing <- tryCatch(
          utils::unzip(candidate, list = TRUE),
          error = function(e) NULL
        )
        if (!is.null(listing) && is.null(zipShapefileProblem(listing, source, candidate))) {
          archiveToUse <- candidate
          contents <- listing
          break
        }
      }
      # Track whether the raw archive was found locally versus downloaded now.
      reused <- !is.null(archiveToUse)
      if (is.null(archiveToUse)) {
        # Never overwrite a damaged cache silently; leave it for diagnosis.
        if (file.exists(archive)) {
          stop(
            "Cached archive is incomplete/corrupt; remove or replace it explicitly: ",
            archive
          )
        }
        # Download to a temporary name so a failed transfer cannot masquerade
        # as a valid cached archive on a later run.
        partial <- paste0(archive, ".partial")
        message(
          "Downloading ",
          source$name,
          " (large archive; requires internet and local storage): ",
          source$url
        )
        status <- download(
          source$url,
          destfile = partial,
          mode = "wb",
          quiet = FALSE
        )
        if (!identical(as.integer(status), 0L) || !file.exists(partial)) {
          stop("Download did not complete: ", partial)
        }
        # Verify ZIP contents before atomically publishing the completed file.
        contents <- utils::unzip(partial, list = TRUE)
        problem <- zipShapefileProblem(contents, source, partial)
        if (!is.null(problem)) stop(problem)
        if (!file.rename(partial, archive)) {
          stop("Cannot publish completed archive: ", archive)
        }
        archiveToUse <- archive
      }
      # Base-R unzip avoids reproducible's platform-dependent ZIP extraction path.
      # Refuse absolute paths and parent-directory traversal in archive members.
      if (
        any(grepl(
          "(^|[/\\\\])\\.\\.([/\\\\]|$)|^[/\\\\]|^[A-Za-z]:",
          contents$Name
        ))
      ) {
        stop("Unsafe path in source ZIP: ", archiveToUse)
      }
      # Extract only after structural checks have made the archive safe to use.
      dir.create(extraction, recursive = TRUE, showWarnings = FALSE)
      utils::unzip(archiveToUse, exdir = extraction)
      shapes <- list.files(
        extraction,
        pattern = "\\.shp$",
        recursive = TRUE,
        full.names = TRUE
      )
      # The source contract permits exactly one main shapefile after extraction.
      shape <- shapes[basename(shapes) == source$shape]
      if (length(shape) != 1L) {
        stop("Expected exactly one ", source$shape, " in ", extraction)
      }
      # Confirm the extracted vector itself still meets the source contract.
      validateShapefile(shape, source)
      # Record provenance and the uncropped status required for future reuse.
      saveRDS(
        list(
          url = source$url,
          shape = normalizePath(shape, winslash = "/"),
          archive = normalizePath(archiveToUse, winslash = "/"),
          rawUncropped = TRUE
        ),
        marker
      )
      list(path = shape, source = source, reused = reused)
    },
    error = function(e) {
      stop(
        "Input preparation failed for ",
        context,
        ": ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

readRiparianHalo <- function(raw, halo, targetCRS) {
  # OGR spatial filter selects WHOLE source features; do not crop shorelines/endpoints.
  header <- terra::vect(raw$path, proxy = TRUE)
  # Densify projected envelope edges before transforming: four corners alone can
  # underestimate its extent under curved transformations. Use a conservative
  # native-CRS bounding box; final selection occurs in the working projection.
  interval <- max(
    diff(as.vector(terra::ext(halo))[1:2]),
    diff(as.vector(terra::ext(halo))[3:4])
  ) /
    256
  # Add vertices along the projected halo before CRS transformation so curved
  # edges are represented conservatively in the source coordinate system.
  dense <- terra::densify(terra::deepcopy(halo), interval = interval)
  native <- terra::project(dense, terra::crs(header))
  nativeExt <- terra::ext(native)
  # Pad by one densification interval to protect against transformation and
  # envelope-rounding edge effects during the coarse source filter.
  margin <- max(nativeExt[2] - nativeExt[1], nativeExt[4] - nativeExt[3]) / 256
  filter <- terra::as.polygons(
    terra::ext(
      nativeExt[1] - margin,
      nativeExt[2] + margin,
      nativeExt[3] - margin,
      nativeExt[4] + margin
    ),
    crs = terra::crs(header)
  )
  # Read candidates using the native-CRS filter; features are intentionally
  # retained whole and are selected precisely after projection.
  v <- terra::vect(raw$path, filter = filter)
  validateVector(v, raw$source$name, raw$source$type, targetCRS)
}

selectRiparianHalo <- function(x, halo) {
  # Preserve an explicitly supplied empty vector without calling spatial
  # predicates that may not support zero features.
  if (!nrow(x)) {
    return(x)
  }
  # Keep whole features that intersect the buffered study envelope.
  index <- which(as.vector(terra::relate(x, halo, "intersects")))
  x[index, ]
}

prepareRiparianInputs <- function(
  studyArea,
  PlanningGrid,
  Jurisdiction = NULL,
  policy = NULL,
  streams = NULL,
  lakes = NULL,
  cacheDir,
  moduleDir,
  reuseDir = NULL,
  obtainSource = obtainRiparianSource
) {
  # Capture original CRS declarations for diagnostics before inputs are copied,
  # repaired, or projected to the PlanningGrid working CRS.
  sourceCRS <- list(
    studyArea = terra::crs(
      if (inherits(studyArea, "SpatVector")) {
        studyArea
      } else {
        terra::vect(studyArea)
      }
    ),
    PlanningGrid = terra::crs(PlanningGrid)
  )
  # Validate and locally repair the study boundary before deriving extents.
  a <- validateVector(studyArea, "studyArea", "polygons")
  area <- a$vector
  if (!nrow(area)) {
    stop("studyArea must contain at least one polygon.")
  }
  # Buffer widths are distance-based, so both study area and output grid must
  # use the same projected metre CRS.
  validateMetreCRS(area, "studyArea")
  validatePlanningGrid(PlanningGrid, area)
  target <- terra::crs(PlanningGrid)
  # State whether assumptions came from bundled analytical defaults or caller
  # input; neither source represents independently verified legal policy.
  policySource <- if (is.null(policy)) {
    "bundled simplified analytical assumptions (not legal US/Canadian policy)"
  } else {
    "supplied policy table (not independently legally verified)"
  }
  if (is.null(policy)) {
    # Load the module's default policy only when no policy table was supplied.
    file <- file.path(moduleDir, "data", "riparianBufferPolicy.csv")
    if (!file.exists(file)) {
      stop("Bundled analytical policy not found: ", file)
    }
    policy <- utils::read.csv(file, stringsAsFactors = FALSE)
  }
  # Validate policy before using any widths to prepare geographic input data.
  # Give resolution a placeholder jurisdiction so policy schema and numeric
  # widths are checked independently of boundary-source availability.
  dummy <- terra::deepcopy(area)
  dummy$jurisdiction_code <- rep("default", nrow(dummy))
  checked <- resolveRiparianPolicy(policy, dummy)$policy
  # The largest class-specific width defines a conservative metre-based halo
  # around the study extent for all hydrologic input reads.
  haloWidth <- max(as.matrix(checked[, c(
    "small_stream",
    "large_stream",
    "small_lake",
    "large_lake"
  )]))
  box <- terra::ext(area)
  # Construct the rectangular halo in the PlanningGrid CRS and metre units.
  halo <- terra::as.polygons(
    terra::ext(
      box[1] - haloWidth,
      box[2] + haloWidth,
      box[3] - haloWidth,
      box[4] + haloWidth
    ),
    crs = target
  )
  repairs <- list(studyArea = a$repaired)
  provenance <- list()
  # Normalise either downloaded raw data or caller data to a validated,
  # projected whole-feature layer for one required input dataset.
  prepare <- function(x, dataset, type) {
    if (is.null(x)) {
      # Obtain raw global/continental data, then read only halo candidates.
      raw <- obtainSource(dataset, cacheDir, reuseDir)
      sourceCRS[[dataset]] <<- terra::crs(terra::vect(raw$path, proxy = TRUE))
      prepared <- readRiparianHalo(raw, halo, target)
      provenance[[dataset]] <<- list(
        origin = raw$source$name,
        url = raw$source$url,
        rawFile = raw$path,
        cacheReused = raw$reused,
        coverage = "raw source spatially filtered to halo; source mapping completeness not guaranteed"
      )
    } else {
      # Caller-supplied data remain read-only: validation operates on a copy.
      sourceCRS[[dataset]] <<- terra::crs(
        if (inherits(x, "SpatVector")) x else terra::vect(x)
      )
      prepared <- validateVector(
        x,
        paste0("supplied ", dataset),
        type,
        target
      )
      provenance[[dataset]] <<- list(
        origin = "supplied read-only input",
        coverage = "unknown; complete halo coverage cannot be guaranteed from feature extents alone"
      )
    }
    # Record any local geometry repairs performed during validation.
    repairs[[dataset]] <<- prepared$repaired
    v <- prepared$vector
    if (dataset %in% c("streams", "lakes")) {
      # Assign the two policy classes from source attributes or supplied class.
      v <- classifyHydrology(v, dataset)
    }
    # Apply the exact working-CRS halo predicate after any coarse source filter.
    selectRiparianHalo(v, halo)
  }
  # Jurisdictions establish applicable policy before hydrology is prepared.
  j <- prepare(Jurisdiction, "jurisdiction", "polygons")
  resolved <- resolveRiparianPolicy(policy, j)
  # Prepare both feature types needed to generate stream and lake buffers.
  s <- prepare(streams, "streams", "lines")
  l <- prepare(lakes, "lakes", "polygons")
  # Require non-lake land to have one non-overlapping policy jurisdiction.
  validateJurisdictionCoverage(resolved$Jurisdiction, area, l)
  # Return validated inputs plus auditable policy, CRS, repair, and source data.
  list(
    studyArea = area,
    PlanningGrid = PlanningGrid,
    Jurisdiction = resolved$Jurisdiction,
    policy = resolved$policy,
    streams = s,
    lakes = l,
    policyFallback = resolved$diagnostics,
    sourceCRS = sourceCRS,
    repairs = repairs,
    sources = provenance,
    haloWidth_m = haloWidth,
    haloExtent = as.vector(terra::ext(halo)),
    policySource = policySource
  )
}

riparianSource_sol6_1 <- function(dataset) {
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
      archive = "politicalboundaries_sol6_1.zip",
      shape = "boundaries_p_2021_v3.shp",
      field = "NAME_En",
      type = "polygons"
    ),
    stop("Unknown riparian source: ", dataset)
  )
}

validateShapefile_sol6_1 <- function(path, source) {
  stem <- sub("\\.shp$", "", path, ignore.case = TRUE)
  sidecars <- paste0(stem, c(".shp", ".shx", ".dbf", ".prj"))
  if (!all(file.exists(sidecars)) || any(file.info(sidecars)$size <= 0)) {
    stop(
      "Incomplete shapefile at ",
      path,
      "; require nonempty .shp/.shx/.dbf/.prj components."
    )
  }
  header <- terra::vect(path, proxy = TRUE)
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

obtainRiparianSource_sol6_1 <- function(
  dataset,
  cacheDir,
  reuseDir = NULL,
  download = utils::download.file
) {
  source <- riparianSource_sol6_1(dataset)
  localDir <- file.path(cacheDir, paste0(dataset, "_sol6_1"))
  dir.create(localDir, recursive = TRUE, showWarnings = FALSE)
  archive <- file.path(localDir, source$archive)
  extraction <- file.path(localDir, "raw_sol6_1")
  marker <- file.path(localDir, "complete_source_sol6_1.rds")
  context <- paste0(source$name, " [", source$url, "] at ", localDir)
  tryCatch(
    {
      # Only trust our complete raw extraction when its URL/provenance matches.
      if (file.exists(marker)) {
        manifest <- readRDS(marker)
        if (
          identical(manifest$url, source$url) &&
            isTRUE(manifest$rawUncropped) &&
            length(manifest$shape) == 1L &&
            file.exists(manifest$shape)
        ) {
          validateShapefile_sol6_1(manifest$shape, source)
          return(list(path = manifest$shape, source = source, reused = TRUE))
        }
      }
      # Old-module archives are read-only sources, never old cropped working layers.
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
      for (candidate in candidates[file.exists(candidates)]) {
        listing <- tryCatch(
          utils::unzip(candidate, list = TRUE),
          error = function(e) NULL
        )
        if (!is.null(listing)) {
          main <- listing$Name[basename(listing$Name) == source$shape]
          if (length(main) == 1L) {
            required <- paste0(
              sub("\\.shp$", "", main),
              c(".shp", ".shx", ".dbf", ".prj")
            )
            if (
              all(required %in% listing$Name) &&
                all(listing$Length[match(required, listing$Name)] > 0)
            ) {
              archiveToUse <- candidate
              contents <- listing
              break
            }
          }
        }
      }
      reused <- !is.null(archiveToUse)
      if (is.null(archiveToUse)) {
        # Never overwrite a damaged cache silently; leave it for diagnosis.
        if (file.exists(archive)) {
          stop(
            "Cached archive is incomplete/corrupt; remove or replace it explicitly: ",
            archive
          )
        }
        partial <- paste0(archive, ".partial_sol6_1")
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
        contents <- utils::unzip(partial, list = TRUE)
        main <- contents$Name[basename(contents$Name) == source$shape]
        if (length(main) != 1L) {
          stop("Downloaded ZIP lacks expected shapefile ", source$shape)
        }
        required <- paste0(
          sub("\\.shp$", "", main),
          c(".shp", ".shx", ".dbf", ".prj")
        )
        if (
          !all(required %in% contents$Name) ||
            any(contents$Length[match(required, contents$Name)] <= 0)
        ) {
          stop(
            "Downloaded ZIP has missing or empty shapefile components: ",
            partial
          )
        }
        if (!file.rename(partial, archive)) {
          stop("Cannot publish completed archive: ", archive)
        }
        archiveToUse <- archive
      }
      # Base-R unzip avoids reproducible's platform-dependent ZIP extraction path.
      if (
        any(grepl(
          "(^|[/\\\\])\\.\\.([/\\\\]|$)|^[/\\\\]|^[A-Za-z]:",
          contents$Name
        ))
      ) {
        stop("Unsafe path in source ZIP: ", archiveToUse)
      }
      dir.create(extraction, recursive = TRUE, showWarnings = FALSE)
      utils::unzip(archiveToUse, exdir = extraction)
      shapes <- list.files(
        extraction,
        pattern = "\\.shp$",
        recursive = TRUE,
        full.names = TRUE
      )
      shape <- shapes[basename(shapes) == source$shape]
      if (length(shape) != 1L) {
        stop("Expected exactly one ", source$shape, " in ", extraction)
      }
      validateShapefile_sol6_1(shape, source)
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

readRiparianHalo_sol6_1 <- function(raw, halo, targetCRS) {
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
  dense <- terra::densify(terra::deepcopy(halo), interval = interval)
  native <- terra::project(dense, terra::crs(header))
  nativeExt <- terra::ext(native)
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
  v <- terra::vect(raw$path, filter = filter)
  validateVector_sol6_1(v, raw$source$name, raw$source$type, targetCRS)
}

selectRiparianHalo_sol6_1 <- function(x, halo) {
  if (!nrow(x)) {
    return(x)
  }
  index <- which(as.vector(terra::relate(x, halo, "intersects")))
  x[index, ]
}

prepareRiparianInputs_sol6_1 <- function(
  studyArea,
  PlanningGrid,
  Jurisdiction = NULL,
  policy = NULL,
  streams = NULL,
  lakes = NULL,
  cacheDir,
  moduleDir,
  reuseDir = NULL,
  obtainSource = obtainRiparianSource_sol6_1
) {
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
  a <- validateVector_sol6_1(studyArea, "studyArea", "polygons")
  area <- a$vector
  if (!nrow(area)) {
    stop("studyArea must contain at least one polygon.")
  }
  validateMetreCRS_sol6_1(area, "studyArea")
  validatePlanningGrid_sol6_1(PlanningGrid, area)
  target <- terra::crs(PlanningGrid)
  policySource <- if (is.null(policy)) {
    "bundled simplified analytical assumptions (not legal US/Canadian policy)"
  } else {
    "supplied policy table (not independently legally verified)"
  }
  if (is.null(policy)) {
    file <- file.path(moduleDir, "data", "riparianBufferPolicy.csv")
    if (!file.exists(file)) {
      stop("Bundled analytical policy not found: ", file)
    }
    policy <- utils::read.csv(file, stringsAsFactors = FALSE)
  }
  # Validate policy before using any widths to prepare geographic input data.
  dummy <- terra::deepcopy(area)
  dummy$jurisdiction_code <- rep("default", nrow(dummy))
  checked <- resolveRiparianPolicy_sol6_1(policy, dummy)$policy
  haloWidth <- max(as.matrix(checked[, c(
    "small_stream",
    "large_stream",
    "small_lake",
    "large_lake"
  )]))
  box <- terra::ext(area)
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
  prepare <- function(x, dataset, type) {
    if (is.null(x)) {
      raw <- obtainSource(dataset, cacheDir, reuseDir)
      sourceCRS[[dataset]] <<- terra::crs(terra::vect(raw$path, proxy = TRUE))
      prepared <- readRiparianHalo_sol6_1(raw, halo, target)
      provenance[[dataset]] <<- list(
        origin = raw$source$name,
        url = raw$source$url,
        rawFile = raw$path,
        cacheReused = raw$reused,
        coverage = "raw source spatially filtered to halo; source mapping completeness not guaranteed"
      )
    } else {
      sourceCRS[[dataset]] <<- terra::crs(
        if (inherits(x, "SpatVector")) x else terra::vect(x)
      )
      prepared <- validateVector_sol6_1(
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
    repairs[[dataset]] <<- prepared$repaired
    v <- prepared$vector
    if (dataset %in% c("streams", "lakes")) {
      v <- classifyHydrology_sol6_1(v, dataset)
    }
    selectRiparianHalo_sol6_1(v, halo)
  }
  j <- prepare(Jurisdiction, "jurisdiction", "polygons")
  resolved <- resolveRiparianPolicy_sol6_1(policy, j)
  s <- prepare(streams, "streams", "lines")
  l <- prepare(lakes, "lakes", "polygons")
  validateJurisdictionCoverage_sol6_1(resolved$Jurisdiction, area, l)
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

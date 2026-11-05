validateMetreCRS_sol6_1 <- function(x, label) {
  wkt <- terra::crs(x)
  if (
    !nzchar(wkt) || terra::is.lonlat(x) || !grepl("PROJCRS\\[|PROJCS\\[", wkt)
  ) {
    stop(
      label,
      " must have a projected metre-based CRS; no implicit output-grid reprojection."
    )
  }
  # WKT2 also contains angular units; test the Cartesian coordinate-system units.
  cartesian <- sub(".*CS\\[Cartesian,[0-9]+\\]", "", wkt)
  if (
    !grepl(
      '(LENGTHUNIT|UNIT)\\["(metre|meter)",[[:space:]]*1([,.]|\\])',
      cartesian,
      ignore.case = TRUE
    ) ||
      grepl(
        '(LENGTHUNIT|UNIT)\\["[^\"]*(foot|feet)',
        cartesian,
        ignore.case = TRUE
      )
  ) {
    stop(label, " CRS must use metres, not feet or degrees.")
  }
  invisible(TRUE)
}

validateVector_sol6_1 <- function(x, label, type, targetCRS = NULL) {
  if (!inherits(x, c("SpatVector", "sf", "sfc"))) {
    stop(label, " must be a SpatVector or sf object.")
  }
  # terra::deepcopy is essential: SpatVectors hold external pointers.
  x <- if (inherits(x, "SpatVector")) terra::deepcopy(x) else terra::vect(x)
  if (!nzchar(terra::crs(x))) {
    stop(label, " has no CRS.")
  }
  if (nrow(x) && !identical(terra::geomtype(x), type)) {
    stop(label, " must contain ", type, ".")
  }
  repaired <- 0L
  if (nrow(x)) {
    valid <- terra::is.valid(x)
    if (anyNA(valid) || any(!valid)) {
      repaired <- sum(is.na(valid) | !valid)
      message(
        label,
        ": explicitly repairing ",
        repaired,
        " invalid geometries on a local copy."
      )
      x <- terra::makeValid(x)
      if (anyNA(terra::is.valid(x)) || any(!terra::is.valid(x))) {
        stop(label, " remains invalid after geometry repair.")
      }
    }
    if (any(terra::is.empty(x))) {
      stop(
        label,
        " contains empty geometries; supply a valid zero-feature vector instead."
      )
    }
  }
  if (!is.null(targetCRS) && !terra::same.crs(x, targetCRS)) {
    if (nrow(x)) {
      x <- terra::project(x, targetCRS)
    } else {
      # There are no coordinates to transform; project() can reject empty vectors.
      terra::crs(x) <- targetCRS
    }
    if (nrow(x) && (anyNA(terra::is.valid(x)) || any(!terra::is.valid(x)))) {
      stop(label, " became invalid during projection.")
    }
  }
  list(vector = x, repaired = repaired)
}

classifyHydrology_sol6_1 <- function(x, kind) {
  stream <- identical(kind, "streams")
  field <- if (stream) "ORD_STRA" else "Lake_area"
  classes <- if (stream) {
    c("small_stream", "large_stream")
  } else {
    c("small_lake", "large_lake")
  }
  # Empty supplied data are valid and never trigger a download.
  if (!nrow(x)) {
    x$hydro_class <- character(0)
    return(x)
  }
  if (field %in% names(x)) {
    value <- x[[field]][, 1]
    if (!is.numeric(value) || any(!is.finite(value)) || any(value < 0)) {
      stop(
        "Hydrology_",
        kind,
        "$",
        field,
        " must be finite, nonnegative numeric values (no NA)."
      )
    }
    cls <- ifelse(value >= if (stream) 4 else 1, classes[2], classes[1])
    if (
      "hydro_class" %in%
        names(x) &&
        (anyNA(x$hydro_class) || any(as.character(x$hydro_class) != cls))
    ) {
      stop(
        "Hydrology_",
        kind,
        "$hydro_class conflicts with ",
        field,
        " classification."
      )
    }
    x$hydro_class <- cls
  } else {
    if (
      !"hydro_class" %in% names(x) ||
        anyNA(x$hydro_class) ||
        any(!as.character(x$hydro_class) %in% classes)
    ) {
      stop(
        "Hydrology_",
        kind,
        " requires valid ",
        field,
        " or hydro_class (",
        paste(classes, collapse = ", "),
        ")."
      )
    }
  }
  x
}

validatePlanningGrid_sol6_1 <- function(grid, area) {
  if (
    !inherits(grid, "SpatRaster") ||
      terra::nlyr(grid) != 1L ||
      terra::ncell(grid) < 1 ||
      any(!is.finite(terra::res(grid))) ||
      any(terra::res(grid) <= 0)
  ) {
    stop(
      "PlanningGrid must be a nonempty single-layer SpatRaster with positive finite resolution."
    )
  }
  validateMetreCRS_sol6_1(grid, "PlanningGrid")
  # The study CRS is the specified working projection; do not silently select another.
  if (!terra::same.crs(grid, area)) {
    stop("PlanningGrid and studyArea must share their projected metre CRS.")
  }
  e <- as.vector(terra::ext(grid))
  a <- as.vector(terra::ext(area))
  tol <- max(terra::res(grid)) * 1e-8
  if (
    e[1] > a[1] + tol ||
      e[2] < a[2] - tol ||
      e[3] > a[3] + tol ||
      e[4] < a[4] - tol
  ) {
    stop("PlanningGrid extent must cover the entire studyArea.")
  }
  invisible(TRUE)
}

validateJurisdictionCoverage_sol6_1 <- function(
  Jurisdiction,
  studyArea,
  lakes,
  areaTolerance = 1e-6
) {
  # Geometric area in the chosen projection, not geodesic expanse's default.
  area <- function(x) {
    if (is.null(x) || !nrow(x)) 0 else sum(terra::expanse(x, transform = FALSE))
  }
  land <- terra::aggregate(studyArea)
  if (nrow(lakes)) {
    land <- terra::erase(land, terra::aggregate(lakes))
  }
  if (!nrow(Jurisdiction)) {
    if (area(land) > areaTolerance) {
      stop("Jurisdiction has no polygons covering study-area non-lake land.")
    }
    return(invisible(TRUE))
  }
  clipped <- terra::intersect(Jurisdiction, terra::aggregate(studyArea))
  if (nrow(clipped)) {
    # Dissolving can only reduce summed area; a positive reduction identifies overlap.
    overlap <- area(clipped) - area(terra::aggregate(clipped))
    if (overlap > max(areaTolerance, area(clipped) * 1e-10)) {
      stop(
        "Jurisdiction polygons overlap within studyArea (",
        signif(overlap, 8),
        " m2); resolve overlaps explicitly before running."
      )
    }
  }
  gap <- if (nrow(land)) {
    terra::erase(land, terra::aggregate(Jurisdiction))
  } else {
    land
  }
  if (area(gap) > max(areaTolerance, area(land) * 1e-10)) {
    stop(
      "Jurisdiction polygons leave ",
      signif(area(gap), 8),
      " m2 of non-lake study-area land uncovered; supply complete polygon boundaries."
    )
  }
  invisible(TRUE)
}

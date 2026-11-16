validateMetreCRS <- function(x, label) {
  # Inspect the CRS WKT because buffer widths and areas in this module are
  # interpreted in projected Cartesian metres.
  wkt <- terra::crs(x)
  # Reject absent, geographic, and non-projected CRSs; no implicit reprojection
  # is permitted because the PlanningGrid defines the working projection.
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
  # Accept metre/meter units with scale one, while explicitly rejecting feet.
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

validateVector <- function(x, label, type, targetCRS = NULL) {
  # Accept only in-memory vector formats that terra can safely copy and inspect.
  if (!inherits(x, c("SpatVector", "sf", "sfc"))) {
    stop(label, " must be a SpatVector or sf object.")
  }
  # terra::deepcopy is essential: SpatVectors hold external pointers.
  x <- if (inherits(x, "SpatVector")) terra::deepcopy(x) else terra::vect(x)
  # Spatial operations require an explicit coordinate reference system.
  if (!nzchar(terra::crs(x))) {
    stop(label, " has no CRS.")
  }
  # Enforce the expected geometry family when features are present; empty inputs
  # are allowed to represent intentional absence of a data class.
  if (nrow(x) && !identical(terra::geomtype(x), type)) {
    stop(label, " must contain ", type, ".")
  }
  repaired <- 0L
  if (nrow(x)) {
    # Repair invalid topology on the private copy and report how many features
    # required intervention, preserving the caller's original object.
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
    # Empty geometries carry no usable spatial support and are not equivalent to
    # a valid zero-feature vector.
    if (any(terra::is.empty(x))) {
      stop(
        label,
        " contains empty geometries; supply a valid zero-feature vector instead."
      )
    }
  }
  if (!is.null(targetCRS) && !terra::same.crs(x, targetCRS)) {
    # Transform nonempty features into the specified working CRS only after
    # validity checks; empty vectors receive the CRS declaration directly.
    if (nrow(x)) {
      x <- terra::project(x, targetCRS)
    } else {
      # There are no coordinates to transform; project() can reject empty vectors.
      terra::crs(x) <- targetCRS
    }
    # Projection can introduce topology defects, so verify it did not do so.
    if (nrow(x) && (anyNA(terra::is.valid(x)) || any(!terra::is.valid(x)))) {
      stop(label, " became invalid during projection.")
    }
  }
  # Return both the normalised vector and an audit count for the caller.
  list(vector = x, repaired = repaired)
}

classifyHydrology <- function(x, kind) {
  # Select the authoritative source attribute and the two policy classes for
  # streams (order) or lakes (area).
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
    # Use the source measurement when present; classifications need finite,
    # nonnegative values so every feature has an unambiguous threshold result.
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
    # Stream order >= 4 and lake area >= 1 receive the corresponding large
    # class; values below those scientific/policy thresholds are small.
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
    # Materialise the derived class for downstream policy-width lookup.
    x$hydro_class <- cls
  } else {
    # Without the source measurement, require a complete caller-provided class
    # from the same controlled vocabulary.
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

validatePlanningGrid <- function(grid, area) {
  # The output grid must be a usable single layer with finite, positive cell
  # dimensions before it can define the analysis projection and extent.
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
  # Require metre units because all buffer widths are expressed in metres.
  validateMetreCRS(grid, "PlanningGrid")
  # The study CRS is the specified working projection; do not silently select another.
  if (!terra::same.crs(grid, area)) {
    stop("PlanningGrid and studyArea must share their projected metre CRS.")
  }
  # Compare extents with a resolution-scaled tolerance for floating-point
  # rounding while requiring every study-area edge to lie within the grid.
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

nonLakeLandArea <- function(studyArea, lakes, tileSize = 128000) {
  # Returns area(studyArea minus lakes) in m2. It exists because dissolving all
  # lakes in one region-wide union is very slow (about 46 min for ~440k lakes),
  # while the same area can be obtained exactly from many small local unions.
  # The result is identical for any tileSize because the tiles partition the
  # study area, so their areas add without double counting.
  #
  # Geometric (planar) area in the projected CRS, matching the validation below.
  area <- function(x) {
    if (is.null(x) || !nrow(x)) 0 else sum(terra::expanse(x, transform = FALSE))
  }
  # Dissolve study features so a point shared by two features is counted once.
  study <- terra::aggregate(studyArea)
  # With no lakes, all study-area land is non-lake land; no tiling is needed.
  if (!nrow(lakes)) {
    return(area(study))
  }
  # Build tile edge coordinates from the study extent, stepping by tileSize.
  e <- as.vector(terra::ext(study))
  xs <- seq(e[1], e[2], by = tileSize)
  ys <- seq(e[3], e[4], by = tileSize)
  # seq() stops at the last full step; append the extent edge so the final
  # partial column/row of tiles still covers the whole study area.
  if (tail(xs, 1) < e[2]) xs <- c(xs, e[2])
  if (tail(ys, 1) < e[4]) ys <- c(ys, e[4])
  total <- 0
  for (i in seq_len(length(xs) - 1L)) {
    for (j in seq_len(length(ys) - 1L)) {
      # One rectangular tile in the study-area CRS.
      tile <- terra::as.polygons(
        terra::ext(xs[i], xs[i + 1L], ys[j], ys[j + 1L]),
        crs = terra::crs(study)
      )
      # Skip tiles outside the study area; intersect() warns on empty results,
      # and irregular study areas leave many bounding-box tiles empty.
      if (!any(as.vector(terra::relate(study, tile, "intersects")))) {
        next
      }
      # Study-area piece inside this tile; tiles partition the study area.
      piece <- terra::intersect(study, tile)
      if (!nrow(piece)) {
        next
      }
      # Only lakes touching the piece can change its non-lake area, so the
      # dissolve below sees a small subset instead of every lake in the region.
      near <- lakes[which(as.vector(terra::relate(lakes, piece, "intersects"))), ]
      # Dissolving the nearby lakes before erasing removes double counting where
      # lakes overlap; the erase also clips lakes that extend past the tile.
      total <- total +
        if (nrow(near)) {
          area(terra::erase(piece, terra::aggregate(near)))
        } else {
          area(piece)
        }
    }
  }
  total
}

validateJurisdictionCoverage <- function(
  Jurisdiction,
  studyArea,
  lakes,
  areaTolerance = 1e-6
) {
  # Geometric area in the chosen projection, not geodesic expanse's default.
  # Treat NULL and zero-feature vectors as zero area for the edge cases below.
  area <- function(x) {
    if (is.null(x) || !nrow(x)) 0 else sum(terra::expanse(x, transform = FALSE))
  }
  # Dissolve study polygons once; reused by the overlap and gap checks. Lakes
  # are no longer erased from the study area here (see the gap step below).
  study <- terra::aggregate(studyArea)
  if (!nrow(Jurisdiction)) {
    # An empty jurisdiction layer is acceptable only when no non-lake land
    # remains, such as a study area wholly occupied by lakes. The tiled helper
    # gives the non-lake land area without a region-wide lake dissolve.
    if (nonLakeLandArea(studyArea, lakes) > areaTolerance) {
      stop("Jurisdiction has no polygons covering study-area non-lake land.")
    }
    return(invisible(TRUE))
  }
  # Intersect with the study area before detecting overlaps, since overlaps
  # outside the area cannot affect the current analysis.
  clipped <- terra::intersect(Jurisdiction, study)
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
  # Unassigned non-lake land is (study minus jurisdictions) minus lakes. Set
  # difference is order-independent, so removing jurisdictions first gives the
  # same region as the former (study minus lakes) minus jurisdictions, but only
  # this small remainder ever has to meet the lake layer.
  uncovered <- terra::erase(study, terra::aggregate(Jurisdiction))
  gap <- uncovered
  if (nrow(uncovered) && nrow(lakes)) {
    # Lakes not touching the uncovered remainder cannot reduce it, so only
    # touching lakes are dissolved and erased. Lakes lying wholly inside
    # jurisdictions are irrelevant here by construction.
    near <- lakes[
      which(as.vector(terra::relate(lakes, uncovered, "intersects"))),
    ]
    if (nrow(near)) {
      gap <- terra::erase(uncovered, terra::aggregate(near))
    }
  }
  gapArea <- area(gap)
  # The failure threshold is max(areaTolerance, 1e-10 * land area), so it is
  # never below areaTolerance. The land area is therefore needed only when the
  # gap exceeds that floor, which avoids the tiled computation in the common
  # fully covered case.
  if (gapArea > areaTolerance) {
    landArea <- nonLakeLandArea(studyArea, lakes)
    if (gapArea > max(areaTolerance, landArea * 1e-10)) {
      stop(
        "Jurisdiction polygons leave ",
        signif(gapArea, 8),
        " m2 of non-lake study-area land uncovered; supply complete polygon boundaries."
      )
    }
  }
  invisible(TRUE)
}

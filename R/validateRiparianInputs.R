#' Require a projected CRS measured in metres
#'
#' Buffer widths and areas are computed as planar distances, so geographic
#' (degree) and foot-based CRSs are rejected rather than silently reprojected.
#' `label` names the input in the error message. Returns TRUE invisibly.
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

#' Validate, repair and optionally project a vector input
#'
#' Works on a private copy so the caller's object is never changed. Checks the
#' class, CRS and geometry type (`type` is "polygons" or "lines"), repairs
#' invalid geometries (reporting how many), rejects empty geometries, and
#' projects to `targetCRS` when given. With `cropTo` (a polygon in the working
#' CRS) the features are first cropped to its bounding box, before repair. Returns list(vector, repaired), where
#' `repaired` is the number of geometries that needed repair.
validateVector <- function(x, label, type, targetCRS = NULL, cropTo = NULL) {
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
  # Crop before repair: repairing a continental layer costs minutes, and only
  # the part near the study area is ever used.
  x <- cropToRegion(x, cropTo)
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

#' Add the `hydro_class` column that selects the buffer width
#'
#' `kind` is "streams" or "lakes". Streams are classed by Strahler order
#' (`ORD_STRA`): below 4 is "small_stream", 4 or more "large_stream". Lakes are
#' classed by whole-lake area (`Lake_area`, km2): below 1 is "small_lake", 1 or
#' more "large_lake". If the source attribute is absent, a complete
#' `hydro_class` supplied by the caller is accepted instead. Stops on missing,
#' negative or conflicting values.
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

#' Check that the output grid is usable for this study area
#'
#' `grid` must be a non-empty single-layer raster with positive resolution, a
#' metre CRS identical to `area`'s, and an extent covering the whole study area
#' (within a tiny rounding tolerance). Stops otherwise; returns TRUE invisibly.
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

#' Area of study-area land that is not lake, in square metres
#'
#' Used to scale the coverage tolerance. Done tile by tile (`tileSize` metres)
#' because dissolving all lakes at once is very slow; the tiles partition the
#' study area, so the total is the same for any tile size.
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

#' Non-lake land that no jurisdiction polygon covers
#'
#' Returns the part of `study` (already dissolved) left after removing all
#' jurisdictions and then all lakes that touch what remains. This is the "gap"
#' used by the coverage check and the fringe assignment. The remainder is split
#' into connected pieces so that lake matching uses a spatial index instead of
#' testing every lake against one multi-million-vertex polygon.
uncoveredNonLakeLand <- function(Jurisdiction, study, lakes) {
  # Unassigned non-lake land is (study minus jurisdictions) minus lakes. Set
  # difference is order-independent, so removing jurisdictions first gives the
  # same region as the former (study minus lakes) minus jurisdictions, but only
  # this small remainder ever has to meet the lake layer. `study` must already
  # be dissolved. An empty jurisdiction layer leaves the whole study area.
  uncovered <- if (nrow(Jurisdiction)) {
    terra::erase(study, terra::aggregate(Jurisdiction))
  } else {
    study
  }
  if (!nrow(uncovered) || !nrow(lakes)) {
    return(uncovered)
  }
  # The remainder is usually one multipart polygon with millions of vertices
  # (every coastline and boundary). Testing each lake against that single shape
  # costs seconds per lake, hours for a continental layer. Split it into its
  # connected pieces and use the pieces as the query instead: sf builds a
  # spatial index over the lakes, so each small piece meets only nearby lakes.
  pieces <- terra::disagg(uncovered)
  hits <- sf::st_intersects(
    sf::st_geometry(sf::st_as_sf(pieces)),
    sf::st_geometry(sf::st_as_sf(lakes))
  )
  # Lakes not touching any piece cannot reduce the remainder, so only touching
  # lakes are dissolved and erased. Lakes lying wholly inside jurisdictions are
  # irrelevant here by construction.
  touching <- sort(unique(unlist(hits)))
  if (!length(touching)) {
    return(pieces)
  }
  terra::erase(pieces, terra::aggregate(lakes[touching, ]))
}


#' Find the neighbouring jurisdiction of each uncovered piece
#'
#' `pieces` are the small uncovered polygons. Returns list(row, method, timing):
#' `row` is the index of the chosen jurisdiction feature for each piece, `method`
#' says whether it was decided by "vote" or by "border length", and `timing`
#' gives time and memory per step. Pieces are grouped by 128 km tile and each
#' group is compared with a small crop of the jurisdictions (never the whole
#' layer). Each piece vertex votes for the jurisdiction owning the nearest
#' jurisdiction vertex; the winner has the most votes. Where several
#' jurisdictions touch a piece, the one sharing the longest border is chosen.
assignPiecesToJurisdiction <- function(
  pieces,
  Jurisdiction,
  tileSize = 128000,
  margin = 2000,
  vertexSpacing_m = 1000,
  borderTolerance = 1,
  log = FALSE
) {
  # Returns, for every piece, the row of the neighbouring Jurisdiction feature,
  # plus per-component timing and memory. A piece's neighbour is found by a vertex
  # vote: edges are densified to vertexSpacing_m, each piece vertex votes for the
  # jurisdiction owning its nearest jurisdiction vertex, and the jurisdiction with
  # most votes wins (the one sharing the longest border, approximately). A piece
  # whose vertices vote for several jurisdictions that all lie within
  # borderTolerance of it is decided exactly by shared border length. Pieces pair
  # with a small crop of the jurisdictions, never the whole layer.
  rss <- function() {
    if (requireNamespace("ps", quietly = TRUE)) {
      round(unname(ps::ps_memory_info()[["rss"]]) / 2^20)
    } else {
      NA_real_
    }
  }
  clock <- list()
  tick <- function(name, since) {
    clock[[name]] <<- c(
      seconds = sum(clock[[name]][["seconds"]], proc.time()[["elapsed"]] - since),
      rssMB = max(clock[[name]][["rssMB"]], rss(), na.rm = TRUE)
    )
    proc.time()[["elapsed"]]
  }
  t0 <- proc.time()[["elapsed"]]
  rows <- Jurisdiction[, 0]
  rows$.row <- seq_len(nrow(Jurisdiction))
  centres <- terra::crds(terra::centroids(pieces))
  key <- paste(floor(centres[, 1] / tileSize), floor(centres[, 2] / tileSize))
  groups <- split(seq_len(nrow(pieces)), key)
  t0 <- tick("setup (centroids, tile keys)", t0)
  row <- rep(NA_integer_, nrow(pieces))
  method <- rep(NA_character_, nrow(pieces))
  # Planar vertices of an sf polygon set, tagged with their feature index.
  vertices <- function(g) {
    xy <- sf::st_coordinates(sf::st_cast(g, "MULTIPOLYGON"))
    data.frame(x = xy[, 1], y = xy[, 2], id = xy[, ncol(xy)])
  }
  for (n in seq_along(groups)) {
    k <- groups[[n]]
    pt <- pieces[k]
    pg <- sf::st_geometry(sf::st_as_sf(pt))
    pv <- vertices(sf::st_segmentize(pg, vertexSpacing_m))
    pp <- sf::st_as_sf(pv, coords = c("x", "y"), crs = sf::st_crs(pg))
    reach <- margin
    repeat {
      # Crop the jurisdictions to this tile's pieces plus a reach, growing the
      # reach when pieces sit far from any jurisdiction vertex, since a crop
      # boundary is not a real jurisdiction edge.
      t0 <- proc.time()[["elapsed"]]
      jc <- terra::crop(rows, terra::ext(pt) + reach)
      t0 <- tick("crop jurisdictions to tile", t0)
      if (nrow(jc)) {
        jg <- sf::st_geometry(sf::st_as_sf(jc))
        jv <- vertices(sf::st_segmentize(jg, vertexSpacing_m))
        jp <- sf::st_as_sf(jv, coords = c("x", "y"), crs = sf::st_crs(jg))
        nn <- sf::st_nearest_feature(pp, jp)
        gap <- as.numeric(sf::st_distance(pp, jp[nn, ], by_element = TRUE))
        worst <- max(tapply(gap, pv$id, min))
      } else {
        worst <- Inf
      }
      t0 <- tick("vertex vote (nearest vertices)", t0)
      if (worst <= reach / 2 || reach > 1e7) {
        break
      }
      reach <- reach * 5
    }
    if (!nrow(jc)) {
      stop("No jurisdiction polygon found near uncovered land pieces.")
    }
    owner <- jc$.row[jv$id[nn]]
    votes <- table(factor(pv$id, seq_along(k)), factor(owner, unique(owner)))
    pick <- as.integer(colnames(votes)[max.col(votes, ties.method = "first")])
    how <- rep("vote", length(k))
    # Exact shared border length only where several jurisdictions touch a piece.
    multi <- which(rowSums(votes > 0) > 1L)
    if (length(multi)) {
      near <- sf::st_is_within_distance(pg[multi], jg, dist = borderTolerance)
      for (m in seq_along(multi)) {
        cand <- near[[m]]
        if (length(cand) < 2L) {
          next
        }
        edge <- sf::st_boundary(pg[multi[m]])
        shared <- vapply(
          cand,
          function(j) {
            sum(as.numeric(sf::st_length(sf::st_intersection(
              edge,
              sf::st_buffer(jg[j], borderTolerance)
            ))))
          },
          numeric(1)
        )
        total <- tapply(shared, jc$.row[cand], sum)
        pick[multi[m]] <- as.integer(names(total)[which.max(total)])
        how[multi[m]] <- "border length"
      }
    }
    t0 <- tick("exact border length", t0)
    row[k] <- pick
    method[k] <- how
    if (log) {
      message(
        "tile ", n, "/", length(groups), ": ", length(k), " pieces, rss ",
        rss(), " MB"
      )
    }
  }
  timing <- data.frame(
    component = names(clock),
    seconds = vapply(clock, function(x) x[["seconds"]], numeric(1)),
    peak_rss_MB = vapply(clock, function(x) x[["rssMB"]], numeric(1)),
    row.names = NULL
  )
  list(row = row, method = method, timing = timing)
}

#' Give small uncovered land pieces their neighbour's policy
#'
#' Coastlines and boundary mismatches leave many tiny pieces of land that no
#' jurisdiction polygon covers. Pieces smaller than `maxArea_km2` are assigned to
#' a neighbouring jurisdiction (see `assignPiecesToJurisdiction`) and added to
#' the jurisdiction layer as extra multipart features, one per receiving code,
#' flagged `fringe_assigned`. Any piece at or above the limit stops the run, as
#' a real hole. `maxArea_km2 = 0` disables the step. Returns list(Jurisdiction,
#' timing, fringe), where `fringe` summarises pieces and area per jurisdiction
#' (NULL when nothing was assigned).
assignFringeToNeighbour <- function(
  Jurisdiction,
  studyArea,
  lakes,
  maxArea_km2,
  vertexSpacing_m = 1000,
  areaTolerance = 1e-6
) {
  # Shoreline and boundary-mismatch fringes are many tiny pieces of non-lake
  # land that no jurisdiction polygon covers. Each piece smaller than
  # maxArea_km2 receives the policy of its neighbouring jurisdiction (the one
  # sharing the longest border when several touch it, otherwise the nearest).
  # Any piece at or above the limit is treated as a genuine hole and stops the
  # run. maxArea_km2 = 0 disables this step and keeps the strict check.
  unchanged <- list(Jurisdiction = Jurisdiction, fringe = NULL)
  if (maxArea_km2 <= 0 || !nrow(Jurisdiction)) {
    return(unchanged)
  }
  study <- terra::aggregate(studyArea)
  gap <- uncoveredNonLakeLand(Jurisdiction, study, lakes)
  if (!nrow(gap)) {
    return(unchanged)
  }
  pieces <- terra::disagg(gap)
  areas <- terra::expanse(pieces, transform = FALSE)
  if (sum(areas) <= areaTolerance) {
    return(unchanged)
  }
  large <- areas / 1e6 >= maxArea_km2
  if (any(large)) {
    stop(
      "Jurisdiction polygons leave ",
      sum(large),
      " uncovered non-lake land piece(s) of at least ",
      maxArea_km2,
      " km2 (largest ",
      signif(max(areas) / 1e6, 5),
      " km2); supply complete polygon boundaries."
    )
  }
  # Neighbour search is tiled so that no step ever meets the full multi-million
  # vertex jurisdiction layer (an untiled search exhausted memory).
  timing <- NULL
  assignment <- assignPiecesToJurisdiction(
    pieces,
    Jurisdiction,
    vertexSpacing_m = vertexSpacing_m
  )
  chosen <- assignment$row
  timing <- assignment$timing
  ps <- sf::st_geometry(sf::st_as_sf(pieces))
  code <- as.character(Jurisdiction$jurisdiction_code)[chosen]
  # One multipart feature per receiving code keeps the footprint builder's
  # per-feature loop short even with very many fringe pieces. The pieces come
  # from one disaggregation, so they overlap nowhere and combining them without
  # a dissolve is exact and far cheaper than a union. Each new feature copies
  # the receiving jurisdiction's attributes so the layer stays uniform.
  codes <- sort(unique(code))
  extra <- Jurisdiction[match(codes, Jurisdiction$jurisdiction_code), ]
  geometry <- do.call(
    c,
    lapply(codes, function(k) sf::st_combine(ps[code == k]))
  )
  extra <- terra::vect(sf::st_sf(
    as.data.frame(extra),
    geometry = geometry,
    crs = sf::st_crs(terra::crs(Jurisdiction))
  ))
  Jurisdiction$fringe_assigned <- FALSE
  extra$fringe_assigned <- TRUE
  list(
    Jurisdiction = rbind(Jurisdiction, extra),
    timing = timing,
    fringe = data.frame(
      jurisdiction_code = codes,
      pieces = as.integer(table(code)[codes]),
      area_km2 = as.numeric(tapply(areas / 1e6, code, sum)[codes]),
      largest_km2 = as.numeric(tapply(areas / 1e6, code, max)[codes]),
      stringsAsFactors = FALSE
    )
  )
}

#' Require that jurisdictions cover all non-lake study-area land exactly once
#'
#' Stops when jurisdiction polygons overlap inside the study area, or when
#' non-lake land is left uncovered by more than `max(areaTolerance, 1e-10 *
#' land area)` square metres. An empty jurisdiction layer is accepted only when
#' no non-lake land exists. Runs after the fringe assignment, so small gaps have
#' already been dealt with. Returns TRUE invisibly.
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
  gap <- uncoveredNonLakeLand(Jurisdiction, study, lakes)
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

#' Keep only the part of `x` near a region (a coarse crop by bounding box)
#'
#' `region` is a polygon in the working CRS. When `x` uses another CRS the region
#' is densified, projected and padded by 1% so the crop box is conservative.
#' Used on the jurisdiction layer, whose parts far from the study area cannot
#' affect the result but make repair, dissolving and erasing very slow.
#' Returns `x` unchanged when `region` is NULL or `x` has no features.
cropToRegion <- function(x, region) {
  if (is.null(region) || !nrow(x)) {
    return(x)
  }
  if (terra::same.crs(x, region)) {
    box <- terra::ext(region)
  } else {
    e <- as.vector(terra::ext(region))
    dense <- terra::densify(
      terra::deepcopy(region),
      interval = max(e[2] - e[1], e[4] - e[3]) / 256
    )
    b <- as.vector(terra::ext(terra::project(dense, terra::crs(x))))
    pad <- max(b[2] - b[1], b[4] - b[3]) / 100
    box <- terra::ext(b[1] - pad, b[2] + pad, b[3] - pad, b[4] + pad)
  }
  terra::crop(x, box)
}

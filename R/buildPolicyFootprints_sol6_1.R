# Geometry helpers deliberately retain original hydrology until after buffering.
# NULL denotes an empty polygon footprint; supplied vectors are never modified.
polygonOnly_sol6_1 <- function(x) {
  # Treat missing or zero-feature inputs uniformly as an absent polygon result.
  if (is.null(x) || !nrow(x)) {
    return(NULL)
  }
  # Drop all attributes so geometry operations cannot propagate irrelevant fields.
  x[, 0]
}

intersectPolygons_sol6_1 <- function(x, y) {
  # Short-circuit empty inputs because terra intersection needs actual features.
  if (is.null(x) || is.null(y) || !nrow(x) || !nrow(y)) {
    return(NULL)
  }
  # Restrict both operands to polygons and discard attributes from the result.
  polygonOnly_sol6_1(terra::intersect(
    polygonOnly_sol6_1(x),
    polygonOnly_sol6_1(y)
  ))
}

unionPolygons_sol6_1 <- function(parts) {
  # Remove absent pieces collected from empty classes, tiles, or intersections.
  parts <- Filter(function(x) !is.null(x) && nrow(x) > 0L, parts)
  if (!length(parts)) {
    return(NULL)
  }
  # Dissolve all remaining overlaps so coverage fractions cannot double count area.
  polygonOnly_sol6_1(terra::aggregate(do.call(rbind, parts), dissolve = TRUE))
}

selectIntersecting_sol6_1 <- function(x, region) {
  # An empty candidate set or region has no spatially related features to select.
  if (is.null(x) || !nrow(x) || is.null(region) || !nrow(region)) {
    return(NULL)
  }
  # Use an inexpensive relationship predicate before later exact geometry work.
  keep <- terra::is.related(x, region, "intersects")
  if (!any(keep)) {
    return(NULL)
  }
  # Preserve original attributes here because hydro_class is needed downstream.
  x[keep, ]
}

#' Construct a tile's destination-policy lake-and-buffer numerator
#'
#' tileCore is a grid-aligned polygon. Inputs have the same projected metre CRS.
#' Jurisdiction has jurisdiction_code; policy has jurisdiction and the four
#' hydro_class columns. Empty classes are valid. Preparation resolves fallbacks.
#' Buffers use GEOS polygonal circular arcs (30 segments per quadrant), not a
#' distance raster. "Exact" coverage refers to the resulting vector polygons.
buildPolicyFootprints_sol6_1 <- function(
  tileCore,
  studyArea,
  Jurisdiction,
  policy,
  streams,
  lakes,
  core = intersectPolygons_sol6_1(tileCore, studyArea),
  maxWidth = max(as.matrix(policy[, c("small_stream", "large_stream", "small_lake", "large_lake")]))
) {
  # Direct callers get the same defaults; tiled callers reuse their prepared core and width.
  if (is.null(core)) {
    return(NULL)
  }
  receiving <- selectIntersecting_sol6_1(Jurisdiction, core)
  # These names pair policy columns with the corresponding hydrology classes.
  widths <- c("small_stream", "large_stream", "small_lake", "large_lake")
  # A bounding-box halo only selects whole features: it does not manufacture
  # line endpoints or lake shorelines by clipping the source geometries.
  e <- as.vector(terra::ext(tileCore)) +
    c(-maxWidth, maxWidth, -maxWidth, maxWidth)
  halo <- terra::as.polygons(terra::ext(e), crs = terra::crs(tileCore))
  # Retain all source features that could reach the tile after the widest buffer.
  nearStreams <- selectIntersecting_sol6_1(streams, halo)
  nearLakes <- selectIntersecting_sol6_1(lakes, halo)
  pieces <- list()
  if (!is.null(receiving)) {
    # Match each receiving destination to its unique policy row by jurisdiction code.
    keys <- as.character(receiving$jurisdiction_code)
    rows <- match(keys, as.character(policy$jurisdiction))
    if (anyNA(rows)) {
      stop(
        "Unresolved jurisdiction policy in footprint builder: ",
        paste(unique(keys[is.na(rows)]), collapse = ", ")
      )
    }
    # Cache class/width results within this tile, including valid empty classes.
    cache <- new.env(parent = emptyenv())
    for (j in seq_len(nrow(receiving))) {
      # Destination clipping makes policy depend on where buffered hydrology lands.
      destination <- intersectPolygons_sol6_1(receiving[j, ], core)
      if (is.null(destination)) {
        next
      }
      for (cl in widths) {
        # A zero width contributes no buffered band for this class and destination.
        width <- policy[[cl]][rows[j]]
        if (width == 0) {
          next
        }
        key <- paste(cl, format(width, digits = 17), sep = ":")
        # Share identical class/width buffers across destinations within this tile.
        if (!exists(key, envir = cache, inherits = FALSE)) {
          source <- if (grepl("stream$", cl)) nearStreams else nearLakes
          # Select the requested source class without altering original hydrology.
          source <- if (is.null(source)) {
            NULL
          } else {
            source[source$hydro_class == cl, ]
          }
          buffered <- if (is.null(source) || !nrow(source)) {
            NULL
          } else {
            terra::buffer(
              polygonOnly_sol6_1(source),
              width = width,
              quadsegs = 30
            )
          }
          # Cache NULL too, so valid empty class/width combinations are not reprocessed.
          assign(key, buffered, envir = cache)
        }
        piece <- intersectPolygons_sol6_1(get(key, envir = cache), destination)
        # Keep only the buffered area assigned to this particular policy destination.
        if (!is.null(piece)) pieces[[length(pieces) + 1L]] <- piece
      }
    }
  }
  # Water surfaces are independent of policy and jurisdiction coverage. Even a
  # lake with its shoreline far outside the tile must fill its interior cells.
  water <- intersectPolygons_sol6_1(nearLakes, core)
  if (!is.null(water)) {
    pieces[[length(pieces) + 1L]] <- water
  }
  # Union policy bands and lake surfaces to return a non-overlapping tile numerator.
  unionPolygons_sol6_1(pieces)
}

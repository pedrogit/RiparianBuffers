# Geometry helpers deliberately retain original hydrology until after buffering.
# NULL denotes an empty polygon footprint; supplied vectors are never modified.
polygonOnly_sol6_1 <- function(x) {
  if (is.null(x) || !nrow(x)) {
    return(NULL)
  }
  x[, 0]
}

intersectPolygons_sol6_1 <- function(x, y) {
  if (is.null(x) || is.null(y) || !nrow(x) || !nrow(y)) {
    return(NULL)
  }
  polygonOnly_sol6_1(terra::intersect(
    polygonOnly_sol6_1(x),
    polygonOnly_sol6_1(y)
  ))
}

unionPolygons_sol6_1 <- function(parts) {
  parts <- Filter(function(x) !is.null(x) && nrow(x) > 0L, parts)
  if (!length(parts)) {
    return(NULL)
  }
  polygonOnly_sol6_1(terra::aggregate(do.call(rbind, parts), dissolve = TRUE))
}

selectIntersecting_sol6_1 <- function(x, region) {
  if (is.null(x) || !nrow(x) || is.null(region) || !nrow(region)) {
    return(NULL)
  }
  keep <- terra::is.related(x, region, "intersects")
  if (!any(keep)) {
    return(NULL)
  }
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
  lakes
) {
  core <- intersectPolygons_sol6_1(tileCore, studyArea)
  if (is.null(core)) {
    return(NULL)
  }
  receiving <- selectIntersecting_sol6_1(Jurisdiction, core)
  widths <- c("small_stream", "large_stream", "small_lake", "large_lake")
  maxWidth <- max(as.matrix(policy[, widths, drop = FALSE]))
  # A bounding-box halo only selects whole features: it does not manufacture
  # line endpoints or lake shorelines by clipping the source geometries.
  e <- as.vector(terra::ext(tileCore)) +
    c(-maxWidth, maxWidth, -maxWidth, maxWidth)
  halo <- terra::as.polygons(terra::ext(e), crs = terra::crs(tileCore))
  nearStreams <- selectIntersecting_sol6_1(streams, halo)
  nearLakes <- selectIntersecting_sol6_1(lakes, halo)
  pieces <- list()
  if (!is.null(receiving)) {
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
      destination <- intersectPolygons_sol6_1(receiving[j, ], core)
      if (is.null(destination)) {
        next
      }
      for (cl in widths) {
        width <- policy[[cl]][rows[j]]
        if (width == 0) {
          next
        }
        key <- paste(cl, format(width, digits = 17), sep = ":")
        if (!exists(key, envir = cache, inherits = FALSE)) {
          source <- if (grepl("stream$", cl)) nearStreams else nearLakes
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
          assign(key, buffered, envir = cache)
        }
        piece <- intersectPolygons_sol6_1(get(key, envir = cache), destination)
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
  unionPolygons_sol6_1(pieces)
}

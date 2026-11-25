# Geometry helpers retain the original streams and lakes; buffering uses a trimmed copy only.
# NULL denotes an empty lake-and-buffer area; supplied vectors are never modified.
polygonOnly <- function(x) {
  # Treat missing or zero-feature inputs uniformly as an absent polygon result.
  if (is.null(x) || !nrow(x)) {
    return(NULL)
  }
  # Drop all attributes so geometry operations cannot propagate irrelevant fields.
  x[, 0]
}

#' Geometric intersection of two polygon vectors
#'
#' Returns the area shared by `x` and `y` as geometry only (no attributes), or
#' NULL when either input is NULL/empty or they do not overlap. The NULL result
#' is how the module represents "no polygon here".
intersectPolygons <- function(x, y) {
  # Short-circuit empty inputs because terra intersection needs actual features.
  if (is.null(x) || is.null(y) || !nrow(x) || !nrow(y)) {
    return(NULL)
  }
  # Restrict both operands to polygons and discard attributes from the result.
  polygonOnly(terra::intersect(
    polygonOnly(x),
    polygonOnly(y)
  ))
}

#' Dissolve a list of polygon vectors into one non-overlapping area
#'
#' `parts` is a list whose elements may be NULL or empty (they are dropped).
#' Overlapping pieces are merged so that no area is counted twice when cell
#' coverage is later computed. Returns geometry only, or NULL if nothing remains.
unionPolygons <- function(parts) {
  # Remove absent pieces collected from empty classes, tiles, or intersections.
  parts <- Filter(function(x) !is.null(x) && nrow(x) > 0L, parts)
  if (!length(parts)) {
    return(NULL)
  }
  # Dissolve all remaining overlaps so coverage fractions cannot double count area.
  polygonOnly(terra::aggregate(do.call(rbind, parts), dissolve = TRUE))
}

#' Clip buffers to one jurisdiction
#'
#' Returns the part of `buffered` (a SpatVector of buffer polygons) that lies
#' inside `jurisdiction` (one jurisdiction polygon, already trimmed to the tile), or
#' NULL if nothing remains. The result is not dissolved: overlapping pieces are
#' merged later by `unionPolygons`.
#'
#' Why it exists: a precise polygon intersection is the most expensive step in a
#' tile when the jurisdiction has a detailed boundary (hundreds of thousands of
#' vertices). Most buffers lie entirely inside the jurisdiction, and for those the
#' intersection returns the buffer unchanged. Testing "within" first is cheap
#' and lets those buffers skip the costly operation, with an identical result.
clipToJurisdiction <- function(buffered, jurisdiction) {
  # Nothing to clip: either no buffers or no jurisdiction area in this tile.
  if (is.null(buffered) || is.null(jurisdiction)) {
    return(NULL)
  }
  # One TRUE/FALSE per buffer: is it wholly inside the jurisdiction polygon?
  inside <- terra::is.related(buffered, jurisdiction, "within")
  parts <- Filter(
    Negate(is.null),
    list(
      # Buffers wholly inside are kept as they are (attributes dropped).
      if (any(inside)) polygonOnly(buffered[inside, ]),
      # Buffers that cross the boundary, or lie outside it, get the precise clip.
      # Outside buffers simply produce no piece here.
      if (!all(inside)) intersectPolygons(buffered[!inside, ], jurisdiction)
    )
  )
  # Every buffer fell outside the jurisdiction, so there is no contribution.
  if (!length(parts)) {
    return(NULL)
  }
  # Stack the kept and clipped pieces into one vector; unname avoids rbind
  # treating the list names as extra arguments.
  do.call(rbind, unname(parts))
}

#' Keep the features of `x` that touch `region`
#'
#' Whole features are returned with their attributes (hydro_class is needed
#' later); nothing is trimmed. Returns NULL when `x` or `region` is empty or no
#' feature intersects. Used to pick the streams and lakes that could affect a tile.
selectIntersecting <- function(x, region) {
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

#' Build the lake-and-buffer area of one tile
#'
#' tileBox is a grid-aligned polygon. Inputs have the same projected metre CRS.
#' Jurisdiction has jurisdiction_code; policy has jurisdiction and the four
#' hydro_class columns. Empty classes are valid. Preparation resolves fallbacks.
#' Buffers use GEOS polygonal circular arcs (30 segments per quadrant), not a
#' distance raster. "Exact" coverage refers to the resulting vector polygons.
buildLakeBufferArea <- function(
  tileBox,
  studyArea,
  Jurisdiction,
  policy,
  streams,
  lakes,
  tileInStudy = intersectPolygons(tileBox, studyArea),
  maxWidth = max(as.matrix(policy[, c("small_stream", "large_stream", "small_lake", "large_lake")]))
) {
  # Direct callers get the same defaults; tiled callers reuse their prepared tile area and width.
  if (is.null(tileInStudy)) {
    return(NULL)
  }
  receiving <- selectIntersecting(Jurisdiction, tileInStudy)
  # These names pair policy columns with the corresponding stream and lake classes.
  widths <- c("small_stream", "large_stream", "small_lake", "large_lake")
  # A bounding-box margin selects whole features. Buffering later uses a trimmed copy
  # confined to the area around the jurisdictions (see below); the trim edge is
  # always beyond the buffer reach of every jurisdiction, so it cannot leave an
  # artificial shoreline or line endpoint inside a jurisdiction.
  e <- as.vector(terra::ext(tileBox)) +
    c(-maxWidth, maxWidth, -maxWidth, maxWidth)
  marginBox <- terra::as.polygons(terra::ext(e), crs = terra::crs(tileBox))
  # Retain all source features that could reach the tile after the widest buffer.
  nearStreams <- selectIntersecting(streams, marginBox)
  nearLakes <- selectIntersecting(lakes, marginBox)
  pieces <- list()
  if (!is.null(receiving)) {
    # Match each receiving jurisdiction to its unique policy row by jurisdiction code.
    keys <- as.character(receiving$jurisdiction_code)
    rows <- match(keys, as.character(policy$jurisdiction))
    if (anyNA(rows)) {
      stop(
        "Unresolved jurisdiction policy in lake-and-buffer builder: ",
        paste(unique(keys[is.na(rows)]), collapse = ", ")
      )
    }
    # One entry per receiving jurisdiction polygon, trimmed to this tile.
    # Clipping to each jurisdiction makes the width depend on where the buffer lands.
    tileJurisdictions <- lapply(seq_len(nrow(receiving)), function(j) {
      intersectPolygons(receiving[j, ], tileInStudy)
    })
    # Work class by class (small/large stream, small/large lake).
    for (cl in widths) {
      source <- if (grepl("stream$", cl)) nearStreams else nearLakes
      # Select the requested source class without altering the original streams and lakes.
      source <- if (is.null(source)) {
        NULL
      } else {
        source[source$hydro_class == cl, ]
      }
      if (is.null(source) || !nrow(source)) {
        next
      }
      # Width of this class in each jurisdiction. Jurisdictions that share a width
      # share one buffer, because buffering is the same whichever jurisdiction
      # the result is later assigned to. With the fringe pieces, a province can
      # appear as several jurisdictions, so sharing avoids repeating the work.
      classWidths <- policy[[cl]][rows]
      # A zero width contributes no buffered band for this class and jurisdiction.
      for (width in unique(classWidths[classWidths > 0])) {
        # Jurisdictions that use this width and have area in the tile.
        sameWidth <- which(classWidths == width & !vapply(tileJurisdictions, is.null, NA))
        if (!length(sameWidth)) {
          next
        }
        # Step 1, select: keep only features whose buffer can reach at least one
        # of these jurisdictions (feature touches the jurisdiction's bounding box
        # grown by the width). Features that cannot reach any are never buffered.
        # Extents are built from plain vectors: terra's ext + numeric shifts them.
        reachable <- Reduce(`|`, lapply(sameWidth, function(j) {
          reach <- terra::as.polygons(
            terra::ext(as.vector(terra::ext(tileJurisdictions[[j]])) +
              c(-width, width, -width, width)),
            crs = terra::crs(tileInStudy)
          )
          terra::is.related(source, reach, "intersects")
        }))
        if (!any(reachable)) {
          next
        }
        # Step 2, trim: very large lakes (such as the Great Lakes) have hundreds of
        # thousands of vertices, and buffering cost grows with vertex count. Trim
        # the selected features to the bounding box of the jurisdictions that use
        # this width, grown by twice the width, then buffer the trimmed copy.
        # Why the result inside the jurisdictions is unchanged: a buffer at a point
        # depends only on source within one width of that point. The trim edge
        # lies at least two widths beyond every jurisdiction, so no point in a
        # jurisdiction sees it. The extra width is a safety margin.
        sameWidthExtents <- do.call(rbind, lapply(sameWidth, function(j) {
          as.vector(terra::ext(tileJurisdictions[[j]]))
        }))
        trimBox <- terra::ext(
          min(sameWidthExtents[, 1]) - 2 * width,
          max(sameWidthExtents[, 2]) + 2 * width,
          min(sameWidthExtents[, 3]) - 2 * width,
          max(sameWidthExtents[, 4]) + 2 * width
        )
        trimmed <- terra::crop(polygonOnly(source[reachable, ]), trimBox)
        # Step 3, buffer once per class and width, shared by all the
        # jurisdictions in sameWidth below.
        buffered <- terra::buffer(
          trimmed,
          width = width,
          quadsegs = 30
        )
        # Step 4, clip: assign the buffered area to each jurisdiction (see
        # clipToJurisdiction for why most buffers skip the precise clip).
        for (j in sameWidth) {
          piece <- clipToJurisdiction(buffered, tileJurisdictions[[j]])
          # Keep only the buffered area assigned to this jurisdiction.
          if (!is.null(piece)) pieces[[length(pieces) + 1L]] <- piece
        }
      }
    }
  }
  # Water surfaces are independent of policy and jurisdiction coverage. Even a
  # lake with its shoreline far outside the tile must fill its interior cells.
  water <- intersectPolygons(nearLakes, tileInStudy)
  if (!is.null(water)) {
    pieces[[length(pieces) + 1L]] <- water
  }
  # Union policy bands and lake surfaces to return the non-overlapping lake-and-buffer area of the tile.
  unionPolygons(pieces)
}

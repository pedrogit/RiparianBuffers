#' Resident memory of this R process in MB (NA when package ps is missing)
residentMemoryMB <- function() {
  if (requireNamespace("ps", quietly = TRUE)) {
    round(unname(ps::ps_memory_info()[["rss"]]) / 2^20)
  } else {
    NA_real_
  }
}

#' Find the neighbouring jurisdiction of one group of uncovered pieces
#'
#' One unit of work for `assignPiecesToJurisdiction`: `job$k` holds the row
#' numbers (in `ctx$pieces`) of the pieces of one 128 km tile. `ctx` holds
#' `pieces`, `rows` (jurisdictions with a `.row` index), `vertexSpacing_m`,
#' `margin` and `borderTolerance`. Returns list(k, pick, how, clock): the chosen
#' jurisdiction row and the deciding method for every piece, plus the time and
#' memory used per step.
processPieceGroup <- function(ctx, job) {
  k <- job$k
  pieces <- ctx$pieces
  rows <- ctx$rows
  vertexSpacing_m <- ctx$vertexSpacing_m
  borderTolerance <- ctx$borderTolerance
  clock <- list()
  tick <- function(name, since) {
    clock[[name]] <<- c(
      seconds = sum(clock[[name]][["seconds"]], proc.time()[["elapsed"]] - since),
      rssMB = max(clock[[name]][["rssMB"]], residentMemoryMB(), na.rm = TRUE)
    )
    proc.time()[["elapsed"]]
  }
  # Planar vertices of an sf polygon set, tagged with their feature index.
  vertices <- function(g) {
    xy <- sf::st_coordinates(sf::st_cast(g, "MULTIPOLYGON"))
    data.frame(x = xy[, 1], y = xy[, 2], id = xy[, ncol(xy)])
  }
  pt <- pieces[k]
  pg <- sf::st_geometry(sf::st_as_sf(pt))
  pv <- vertices(sf::st_segmentize(pg, vertexSpacing_m))
  pp <- sf::st_as_sf(pv, coords = c("x", "y"), crs = sf::st_crs(pg))
  reach <- ctx$margin
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
    t0 <- tick("find nearest jurisdiction for each outline point", t0)
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
  tick("exact border length", t0)
  list(k = k, pick = pick, how = how, clock = clock)
}

#' Find the neighbouring jurisdiction of each uncovered piece
#'
#' `pieces` are the small uncovered polygons. Returns list(row, method, timing):
#' `row` is the index of the chosen jurisdiction feature for each piece, `method`
#' says whether it was decided by "vote" or by "border length", and `timing`
#' gives time and memory per step. Pieces are grouped by 128 km tile and each
#' group is compared with a small crop of the jurisdictions (never the whole
#' layer). Each point on a piece's outline picks the jurisdiction owning the
#' nearest jurisdiction outline point (its "vote"); the jurisdiction picked most
#' often wins. Where several
#' jurisdictions touch a piece, the one sharing the longest border is chosen.
#' The groups are independent, so they are spread over `workers` R processes
#' (see `runJobs`); the answer does not depend on the number of workers. In the
#' timing table the seconds are summed over all workers, so they can exceed the
#' elapsed time.
assignPiecesToJurisdiction <- function(
  pieces,
  Jurisdiction,
  tileSize = 128000,
  margin = 2000,
  vertexSpacing_m = 1000,
  borderTolerance = 1,
  log = FALSE,
  workers = 1L,
  workDir = tempdir()
) {
  # Returns, for every piece, the row of the neighbouring Jurisdiction feature,
  # plus per-component timing and memory. A piece's neighbour is found by
  # counting nearest neighbours: edges are densified to vertexSpacing_m, each piece
  # vertex picks the jurisdiction owning its nearest jurisdiction vertex, and the
  # jurisdiction picked most often wins (the one sharing the longest border,
  # approximately). A piece whose vertices pick several jurisdictions that all lie within
  # borderTolerance of it is decided exactly by shared border length. Pieces pair
  # with a small crop of the jurisdictions, never the whole layer.
  t0 <- proc.time()[["elapsed"]]
  rows <- Jurisdiction[, 0]
  rows$.row <- seq_len(nrow(Jurisdiction))
  centres <- terra::crds(terra::centroids(pieces))
  key <- paste(floor(centres[, 1] / tileSize), floor(centres[, 2] / tileSize))
  groups <- split(seq_len(nrow(pieces)), key)
  clock <- list(
    "setup (centroids, tile keys)" = c(
      seconds = proc.time()[["elapsed"]] - t0,
      rssMB = residentMemoryMB()
    )
  )
  row <- rep(NA_integer_, nrow(pieces))
  method <- rep(NA_character_, nrow(pieces))
  done <- 0L
  runJobs(
    jobs = lapply(groups, function(k) list(k = k)),
    run = processPieceGroup,
    ctx = list(
      pieces = pieces,
      rows = rows,
      vertexSpacing_m = vertexSpacing_m,
      margin = margin,
      borderTolerance = borderTolerance
    ),
    helpers = "residentMemoryMB",
    workers = workers,
    tmpDir = workDir,
    what = "piece group",
    onResult = function(i, res) {
      row[res$k] <<- res$pick
      method[res$k] <<- res$how
      for (name in names(res$clock)) {
        old <- clock[[name]]
        clock[[name]] <<- c(
          seconds = sum(old[["seconds"]], res$clock[[name]][["seconds"]]),
          rssMB = max(old[["rssMB"]], res$clock[[name]][["rssMB"]], na.rm = TRUE)
        )
      }
      done <<- done + 1L
      if (log) {
        message(
          "tile ", done, "/", length(groups), ": ", length(res$k),
          " pieces, rss ", residentMemoryMB(), " MB"
        )
      }
    }
  )
  timing <- data.frame(
    component = names(clock),
    seconds = vapply(clock, function(x) x[["seconds"]], numeric(1)),
    peak_rss_MB = vapply(clock, function(x) x[["rssMB"]], numeric(1)),
    row.names = NULL
  )
  list(row = row, method = method, timing = timing)
}

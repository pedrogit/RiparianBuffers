#' Compute one tile: exact coverage values and, optionally, its footprint
#'
#' One unit of work for `fractionalCoverage`. `job` gives the tile (`id`, first
#' `row` and `col`, size `nrows` x `ncols`); `ctx` holds the prepared layers and
#' grid description. Returns list(values, footprint): a `nrows` x `ncols` matrix
#' (NA outside the study area) and the tile's footprint as a packed vector (NULL
#' when there is none or when `ctx$exportVector` is FALSE). Any error is
#' re-raised with the tile number, position and extent so the failing tile can
#' be found and re-run alone.
processTile <- function(ctx, job) {
  ex <- terra::ext(
    ctx$gridExtent[1] + (job$col - 1) * ctx$resolution[1],
    ctx$gridExtent[1] + (job$col - 1 + job$ncols) * ctx$resolution[1],
    ctx$gridExtent[4] - (job$row - 1 + job$nrows) * ctx$resolution[2],
    ctx$gridExtent[4] - (job$row - 1) * ctx$resolution[2]
  )
  tryCatch(
    {
      # Recreate the exact grid geometry for this tile, preserving CRS and resolution.
      template <- terra::rast(
        nrows = job$nrows,
        ncols = job$ncols,
        extent = ex,
        crs = ctx$crs
      )
      # Convert the tile rectangle to a polygon for destination and footprint clipping.
      core <- terra::as.polygons(ex, crs = ctx$crs)
      studyCore <- intersectPolygons(core, ctx$studyArea)
      values <- matrix(NA_real_, nrow = job$nrows, ncol = job$ncols)
      footprint <- NULL
      if (!is.null(studyCore)) {
        # Build destination-policy buffers only where this tile intersects the study area.
        footprint <- buildPolicyFootprints(
          core,
          ctx$studyArea,
          ctx$Jurisdiction,
          ctx$policy,
          ctx$streams,
          ctx$lakes,
          core = studyCore,
          maxWidth = ctx$maxWidth
        )
        fractions <- exactTileFractions(template, footprint)
        # Mark cells outside the study area as NoData, even if a buffer reaches them.
        inStudy <- exactTileFractions(
          template,
          unionPolygons(list(studyCore))
        ) >
          0
        fractions[!inStudy] <- NA_real_
        values <- matrix(
          fractions,
          nrow = job$nrows,
          ncol = job$ncols,
          byrow = TRUE
        )
      }
      list(
        values = values,
        footprint = if (isTRUE(ctx$exportVector) && !is.null(footprint)) {
          terra::wrap(footprint)
        }
      )
    },
    error = function(e) {
      stop(
        "tile ", job$id, " (row ", job$row, ", column ", job$col,
        ", extent ", paste(signif(as.vector(ex), 10), collapse = " "), "): ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

#' Summarise a vector layer so a changed input can be detected
#'
#' Feature count, extent and total area (polygons) or length (lines).
layerSignature <- function(x) {
  if (is.null(x) || !nrow(x)) {
    return(list(n = 0L))
  }
  size <- if (identical(terra::geomtype(x), "polygons")) {
    sum(terra::expanse(x, transform = FALSE))
  } else {
    sum(terra::perim(x))
  }
  list(n = nrow(x), extent = as.vector(terra::ext(x)), size = size)
}

#' Hash of the code that computes a tile
#'
#' Part of the resume fingerprint: progress saved by different code is not reused.
codeSignature <- function(functions) {
  text <- vapply(
    functions,
    function(f) paste(deparse(f), collapse = "\n"),
    character(1)
  )
  file <- tempfile("codesig_")
  on.exit(unlink(file))
  writeLines(text, file)
  unname(tools::md5sum(file))
}

#' Describe everything that determines the tile values
#'
#' Saved progress is reused only when this description is identical.
tileFingerprint <- function(ctx, tileCells, functions) {
  list(
    grid = list(
      extent = ctx$gridExtent,
      resolution = ctx$resolution,
      dim = ctx$dim,
      crs = ctx$crs
    ),
    tileCells = as.integer(tileCells),
    policy = ctx$policy,
    maxWidth = ctx$maxWidth,
    layers = lapply(
      ctx[c("studyArea", "Jurisdiction", "streams", "lakes")],
      layerSignature
    ),
    code = codeSignature(functions)
  )
}

#' Open (or start) the folder where finished tiles are saved
#'
#' Returns the set of tile numbers already saved. A folder made with a different
#' fingerprint is emptied first, so stale tiles are never mixed in.
openTileProgress <- function(dir, fingerprint) {
  file <- file.path(dir, "fingerprint.rds")
  if (dir.exists(dir)) {
    same <- file.exists(file) && identical(readRDS(file), fingerprint)
    if (!same) {
      message("Saved riparian progress does not match these inputs; starting over.")
      unlink(dir, recursive = TRUE)
    }
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(file)) {
    saveRDS(fingerprint, file)
  }
  saved <- list.files(dir, pattern = "^tile_[0-9]+[.]rds$")
  as.integer(sub("^tile_([0-9]+)[.]rds$", "\\1", saved))
}

tileProgressFile <- function(dir, id) {
  file.path(dir, sprintf("tile_%06d.rds", as.integer(id)))
}

#' Save one finished tile; the rename makes a half-written file impossible
saveTileProgress <- function(dir, id, values) {
  target <- tileProgressFile(dir, id)
  partial <- paste0(target, ".partial")
  saveRDS(values, partial)
  file.rename(partial, target)
  invisible(target)
}

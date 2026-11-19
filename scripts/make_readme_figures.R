# Draws the schematic figures used in README.md (written to docs/).
# All shapes are invented for illustration and use arbitrary units; nothing here
# reads real data. Usage: source this file, then make_readme_figures(moduleDir).
# Needs sf and ggplot2 (4.0 or later: geom_label(linewidth = 0)).

make_readme_figures <- function(moduleDir) {
  stopifnot(requireNamespace("sf", quietly = TRUE), requireNamespace("ggplot2", quietly = TRUE))
  library(sf)
  library(ggplot2)
  out <- file.path(moduleDir, "docs")
  dir.create(out, showWarnings = FALSE)

  col <- c(
    lake = "#BFD7EA", lakeLine = "#2E6F9E", stream = "#1F5C99",
    buffer = "#F2B880", bufferLine = "#C77B2B", foot = "#E8A33D",
    A = "#9CCBA8", B = "#C7B6E8", Adark = "#4C9A62", Bdark = "#7A5CC0",
    gap = "#D64545", grey = "grey80", greyLine = "grey55", study = "grey25"
  )

  # --- small geometry helpers -------------------------------------------------
  poly <- function(m) st_polygon(list(rbind(m, m[1, ])))
  box <- function(x0, y0, x1, y1) poly(rbind(c(x0, y0), c(x1, y0), c(x1, y1), c(x0, y1)))
  circ <- function(x, y, r) st_buffer(st_point(c(x, y)), r, nQuadSegs = 16)
  buf <- function(g, w) st_buffer(g, w, nQuadSegs = 16)
  # One sf layer tagged with the facet panel it belongs to.
  L <- function(g, panel) {
    if (!inherits(g, "sfc")) g <- st_sfc(g)
    st_sf(panel = rep(panel, length(g)), geometry = g)
  }
  # The same geometry drawn in several panels.
  LP <- function(g, panels) do.call(rbind, lapply(panels, function(p) L(g, p)))
  area <- function(g) as.numeric(st_area(g))
  inter <- function(a, b) suppressWarnings(st_intersection(a, b))
  txt <- function(x, y, label, panel) data.frame(x = x, y = y, label = label, panel = panel)

  theme_fig <- function() {
    theme_void(base_size = 10) +
      theme(
        strip.text = element_text(face = "bold", hjust = 0, size = 9.5, margin = margin(b = 3)),
        panel.border = element_rect(colour = "grey70", fill = NA, linewidth = 0.4),
        plot.margin = margin(4, 6, 4, 6), legend.position = "none"
      )
  }
  save <- function(p, name, w, h) {
    ggsave(file.path(out, name), p, width = w, height = h, dpi = 130, bg = "white")
  }
  # Plain text, and text on a translucent white background (readable over shapes).
  lab <- function(d, size = 3, ...) geom_text(data = d, aes(x, y, label = label), size = size, lineheight = 0.95, ...)
  labb <- function(d, size = 3, ...) {
    geom_label(
      data = d, aes(x, y, label = label), size = size, lineheight = 0.95,
      fill = alpha("white", 0.78), linewidth = 0, label.padding = unit(0.15, "lines"), ...
    )
  }

  # =============================================================================
  # 1. From hydrology to cell fractions
  # =============================================================================
  lake <- st_sfc(poly(rbind(c(55, 30), c(70, 26), c(82, 34), c(79, 48), c(65, 53), c(54, 44))))
  stream <- st_sfc(st_linestring(rbind(c(4, 8), c(24, 16), c(38, 14), c(48, 26), c(58, 33))))
  w <- 5
  lakeBuf <- buf(lake, w)
  streamBuf <- buf(stream, w)
  foot <- st_union(c(lake, lakeBuf, streamBuf))
  grid <- st_make_grid(st_bbox(c(xmin = 0, ymin = 0, xmax = 100, ymax = 60)), cellsize = 20)
  frac <- vapply(seq_along(grid), function(i) {
    x <- inter(grid[i], foot)
    if (!length(x)) 0 else sum(area(x)) / area(grid[i])
  }, 1)
  ctr <- st_coordinates(st_centroid(grid))
  p <- c("1. Mapped hydrology", "2. Buffer each feature", "3. Union (overlap counted once)", "4. Fraction of each cell")
  g1 <- ggplot() +
    geom_sf(data = L(lake, p[1]), fill = col["lake"], colour = col["lakeLine"]) +
    geom_sf(data = L(stream, p[1]), colour = col["stream"], linewidth = 0.9) +
    geom_sf(data = rbind(L(lakeBuf, p[2]), L(streamBuf, p[2])), fill = col["buffer"], colour = col["bufferLine"], alpha = 0.6) +
    geom_sf(data = L(lake, p[2]), fill = col["lake"], colour = col["lakeLine"]) +
    geom_sf(data = L(stream, p[2]), colour = col["stream"], linewidth = 0.9) +
    geom_sf(data = L(foot, p[3]), fill = col["foot"], colour = col["bufferLine"]) +
    geom_sf(data = LP(grid, p[4]), fill = NA, colour = "grey55", linewidth = 0.3) +
    geom_sf(data = L(foot, p[4]), fill = col["foot"], colour = NA, alpha = 0.8) +
    labb(txt(ctr[, 1], ctr[, 2], ifelse(frac > 0, sprintf("%.2f", frac), ""), p[4]), size = 3) +
    labb(txt(c(22, 66), c(28, 14), c("stream buffer", "lake + its buffer"), p[2]), size = 2.7) +
    facet_wrap(vars(panel), nrow = 1) +
    coord_sf(xlim = c(0, 100), ylim = c(0, 60), expand = FALSE) +
    theme_fig()
  save(g1, "fig_coverage.png", 11, 2.9)

  # =============================================================================
  # 2. The halo
  # =============================================================================
  study <- st_sfc(poly(rbind(c(30, 20), c(55, 12), c(85, 22), c(92, 45), c(70, 62), c(40, 58), c(24, 40))))
  bb <- st_bbox(study)
  wd <- 12
  bbPoly <- st_sfc(box(bb[1], bb[2], bb[3], bb[4]))
  halo <- st_sfc(box(bb[1] - wd, bb[2] - wd, bb[3] + wd, bb[4] + wd))
  lakes <- c(st_sfc(circ(55, 38, 6)), st_sfc(circ(103, 40, 8)), st_sfc(circ(128, 14, 7)), st_sfc(circ(18, 8, 4)))
  streams <- c(
    st_sfc(st_linestring(rbind(c(72, 38), c(92, 46), c(112, 55)))),
    st_sfc(st_linestring(rbind(c(4, 80), c(30, 88), c(50, 96))))
  )
  lakeSel <- lengths(st_intersects(lakes, halo)) > 0
  strSel <- lengths(st_intersects(streams, halo)) > 0
  g2 <- ggplot() +
    geom_sf(data = L(halo, "h"), fill = "#FFF4E0", colour = col["bufferLine"], linetype = "dashed") +
    geom_sf(data = L(bbPoly, "h"), fill = NA, colour = "grey50", linetype = "dotted") +
    geom_sf(data = L(study, "h"), fill = "grey92", colour = col["study"], linewidth = 0.7) +
    geom_sf(data = L(lakes[!lakeSel], "h"), fill = col["grey"], colour = col["greyLine"]) +
    geom_sf(data = L(lakes[lakeSel], "h"), fill = col["lake"], colour = col["lakeLine"]) +
    geom_sf(data = L(streams[!strSel], "h"), colour = col["greyLine"], linewidth = 0.8) +
    geom_sf(data = L(streams[strSel], "h"), colour = col["stream"], linewidth = 0.9) +
    labb(txt(
      c(40, 76, 58, 114, 128, 22), c(49, 7, 69, 63, 3, 98),
      c("study area", "bounding box (dotted)", "halo (dashed) = bounding box grown\nby the widest buffer width",
        "kept whole,\nnot cut at the halo", "too far:\nnot used", "too far: not used"), "h"
    ), size = 2.9) +
    coord_sf(xlim = c(0, 138), ylim = c(0, 100), expand = FALSE) +
    theme_fig()
  save(g2, "fig_halo.png", 6.5, 4.6)

  # =============================================================================
  # 3. Destination policy: the width depends on where the buffer lands
  # =============================================================================
  A <- st_sfc(box(0, 0, 50, 60))
  B <- st_sfc(box(50, 0, 100, 60))
  str3 <- st_sfc(st_linestring(rbind(c(4, 18), c(30, 26), c(48, 22), c(66, 30), c(96, 24))))
  lake3 <- st_sfc(circ(42, 44, 6))
  wA <- 4
  wB <- 10
  p3 <- c("1. Two jurisdictions, one stream and one lake", "2. Each side uses its own width")
  g3 <- ggplot() +
    geom_sf(data = LP(A, p3), fill = col["A"], colour = NA, alpha = 0.35) +
    geom_sf(data = LP(B, p3), fill = col["B"], colour = NA, alpha = 0.35) +
    geom_sf(data = L(inter(buf(str3, wA), A), p3[2]), fill = col["Adark"], colour = NA, alpha = 0.8) +
    geom_sf(data = L(inter(buf(lake3, wA), A), p3[2]), fill = col["Adark"], colour = NA, alpha = 0.8) +
    geom_sf(data = L(inter(buf(str3, wB), B), p3[2]), fill = col["Bdark"], colour = NA, alpha = 0.8) +
    geom_sf(data = L(inter(buf(lake3, wB), B), p3[2]), fill = col["Bdark"], colour = NA, alpha = 0.8) +
    geom_sf(data = LP(lake3, p3), fill = col["lake"], colour = col["lakeLine"]) +
    geom_sf(data = LP(str3, p3), colour = col["stream"], linewidth = 0.8) +
    geom_sf(data = LP(st_sfc(st_linestring(rbind(c(50, 0), c(50, 60)))), p3), colour = "grey30", linewidth = 0.6) +
    lab(txt(
      rep(c(25, 75), 2), rep(56, 4),
      c("jurisdiction A", "jurisdiction B", "A: width 4", "B: width 10"), rep(p3, each = 2)
    ), size = 3.1) +
    facet_wrap(vars(panel), nrow = 1) +
    coord_sf(xlim = c(0, 100), ylim = c(0, 60), expand = FALSE) +
    theme_fig()
  save(g3, "fig_destination.png", 9, 3.4)

  # =============================================================================
  # 4. Fringes: small gaps take a neighbour's policy, large holes stop the run
  # =============================================================================
  study4 <- st_union(st_sfc(box(0, 0, 100, 60)), st_sfc(box(10, 60, 35, 64)))
  A4 <- st_sfc(box(0, 0, 50, 60))
  B4 <- st_difference(st_difference(st_sfc(box(50, 0, 100, 60)), st_sfc(box(50, 30, 54, 42))), st_sfc(box(70, 15, 86, 35)))
  gap <- st_difference(study4, st_union(c(A4, B4)))
  pieces <- st_cast(st_sfc(gap), "POLYGON")
  pa <- area(pieces)
  limit <- 200
  # Shared border length of each piece with each jurisdiction (within a hair).
  shared <- sapply(list(A4, B4), function(j) vapply(seq_along(pieces), function(i) {
    sum(as.numeric(st_length(inter(st_boundary(pieces[i]), buf(j, 0.01)))))
  }, 1))
  pick <- ifelse(pa >= limit, NA, max.col(shared, ties.method = "first"))
  pcx <- st_coordinates(st_centroid(pieces))
  p4 <- c("1. Jurisdictions leave gaps (red)", "2. Small gaps are assigned; a large hole stops the run")
  small <- !is.na(pick)
  g4 <- ggplot() +
    geom_sf(data = LP(A4, p4), fill = col["A"], colour = "grey30") +
    geom_sf(data = LP(B4, p4), fill = col["B"], colour = "grey30") +
    geom_sf(data = L(pieces, p4[1]), fill = col["gap"], colour = col["gap"], linewidth = 0.6) +
    geom_sf(data = L(pieces[small & pick == 1], p4[2]), fill = col["Adark"], colour = "grey20", linewidth = 0.5) +
    geom_sf(data = L(pieces[small & pick == 2], p4[2]), fill = col["Bdark"], colour = "grey20", linewidth = 0.5) +
    geom_sf(data = L(pieces[!small], p4[2]), fill = col["gap"], colour = "grey20", linewidth = 0.5) +
    lab(txt(c(25, 75), c(30, 52), c("A", "B"), p4[1]), size = 4)
  # Labels are placed after the pieces are classified, so they match the data.
  lbl <- do.call(rbind, lapply(seq_along(pieces), function(i) {
    if (!small[i]) {
      data.frame(x = pcx[i, 1], y = pcx[i, 2], label = sprintf("area %.0f >= limit %d\nstops the run", pa[i], limit), panel = p4[2])
    } else if (pcx[i, 2] > 60) {
      data.frame(x = pcx[i, 1], y = pcx[i, 2] + 5, label = sprintf("area %.0f -> %s", pa[i], c("A", "B")[pick[i]]), panel = p4[2])
    } else {
      data.frame(
        x = pcx[i, 1] + 4, y = pcx[i, 2] + 14,
        label = sprintf("area %.0f\nborder shared: A %.0f, B %.0f\n-> %s", pa[i], shared[i, 1], shared[i, 2], c("A", "B")[pick[i]]),
        panel = p4[2]
      )
    }
  }))
  g4 <- g4 + labb(lbl, size = 2.6, hjust = 0.5) +
    coord_sf(xlim = c(-2, 102), ylim = c(-2, 72), expand = FALSE) +
    facet_wrap(vars(panel), nrow = 1) +
    theme_fig()
  save(g4, "fig_fringe.png", 10, 3.9)

  # =============================================================================
  # 5. Tiles
  # =============================================================================
  study5 <- st_sfc(poly(rbind(c(38, 18), c(80, 14), c(104, 40), c(92, 72), c(50, 80), c(24, 50))))
  tiles <- st_make_grid(st_bbox(c(xmin = 0, ymin = 0, xmax = 120, ymax = 90)), cellsize = 30)
  hit <- lengths(st_intersects(tiles, study5)) > 0
  chosen <- st_sfc(box(30, 30, 60, 60))
  str5 <- st_sfc(st_linestring(rbind(c(24, 32), c(27, 42), c(26, 52), c(29, 62), c(36, 68))))
  lake5 <- st_sfc(circ(48, 62, 5))
  foot5 <- st_union(c(buf(str5, 6), buf(lake5, 6), lake5))
  cells5 <- st_make_grid(st_bbox(c(xmin = 30, ymin = 30, xmax = 60, ymax = 60)), cellsize = 5)
  # Two separate plots (the second is zoomed) placed side by side with grid.
  ttl <- theme(plot.title = element_text(face = "bold", size = 9.5, hjust = 0))
  gA <- ggplot() +
    geom_sf(data = L(tiles[!hit], "a"), fill = "grey88", colour = "grey60", linewidth = 0.3) +
    geom_sf(data = L(study5, "a"), fill = "#E6EEF7", colour = col["study"], alpha = 0.85) +
    geom_sf(data = L(tiles[hit], "a"), fill = NA, colour = "grey40", linewidth = 0.4) +
    geom_sf(data = L(chosen, "a"), fill = NA, colour = col["bufferLine"], linewidth = 1.3) +
    labb(txt(c(45, 14), c(45, 8), c("the tile\nin panel 2", "grey tiles: outside the\nstudy area, skipped"), "a"), size = 2.8) +
    coord_sf(xlim = c(0, 120), ylim = c(0, 90), expand = FALSE) +
    labs(title = "1. The grid is cut into tiles") +
    theme_fig() + ttl
  gB <- ggplot() +
    geom_sf(data = L(foot5, "b"), fill = col["foot"], colour = NA, alpha = 0.3) +
    geom_sf(data = L(inter(foot5, chosen), "b"), fill = col["foot"], colour = NA, alpha = 0.9) +
    geom_sf(data = L(cells5, "b"), fill = NA, colour = "grey60", linewidth = 0.25) +
    geom_sf(data = L(chosen, "b"), fill = NA, colour = col["bufferLine"], linewidth = 1.3) +
    geom_sf(data = L(lake5, "b"), fill = col["lake"], colour = col["lakeLine"]) +
    geom_sf(data = L(str5, "b"), colour = col["stream"], linewidth = 0.9) +
    labb(txt(c(22, 45), c(28, 27), c("stream starts\noutside the tile", "tile: 6 x 6 cells"), "b"), size = 2.7) +
    coord_sf(xlim = c(14, 66), ylim = c(24, 76), expand = FALSE) +
    labs(title = "2. One tile: features just outside it still count") +
    theme_fig() + ttl
  png(file.path(out, "fig_tiles.png"), width = 10, height = 4, units = "in", res = 130, bg = "white")
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2)))
  print(gA, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(gB, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
  dev.off()
  g5 <- list(gA, gB)

  # =============================================================================
  # 6. Inside one tile: select, cut, buffer once, clip
  # =============================================================================
  w6 <- 5
  D <- st_sfc(poly(rbind(c(20, 20), c(60, 20), c(60, 45), c(45, 60), c(20, 60))))
  grown <- st_sfc(box(20 - w6, 20 - w6, 60 + w6, 60 + w6))
  cutbox <- st_sfc(box(20 - 2 * w6, 20 - 2 * w6, 60 + 2 * w6, 60 + 2 * w6))
  big <- st_sfc(poly(rbind(c(-30, -20), c(140, -20), c(140, 32), c(100, 26), c(70, 30), c(30, 28), c(0, 34), c(-30, 30))))
  smalls <- c(st_sfc(circ(35, 50, 3)), st_sfc(circ(55, 53, 3)), st_sfc(circ(67, 38, 3)), st_sfc(circ(100, 78, 4)), st_sfc(circ(4, 72, 4)))
  lakes6 <- c(big, smalls)
  sel6 <- lengths(st_intersects(lakes6, grown)) > 0
  bigCut <- inter(big, cutbox)
  used <- c(bigCut, smalls[sel6[-1]])
  bufs <- lapply(seq_along(used), function(i) buf(used[i], w6))
  p6 <- c("1. Select", "2. Cut", "3. Buffer once", "4. Clip")
  keep <- list(); drop <- list(); cross <- list()
  for (b in bufs) {
    if (isTRUE(st_within(b, D, sparse = FALSE)[1, 1])) {
      keep[[length(keep) + 1]] <- b
    } else {
      cross[[length(cross) + 1]] <- inter(b, D)
      drop[[length(drop) + 1]] <- st_difference(b, D)
    }
  }
  cmb <- function(l) do.call(c, l)
  g6 <- ggplot() +
    geom_sf(data = LP(D, p6), fill = col["A"], colour = "grey30", alpha = 0.55) +
    # 1. select
    geom_sf(data = L(grown, p6[1]), fill = NA, colour = col["bufferLine"], linetype = "dashed") +
    geom_sf(data = L(lakes6[!sel6], p6[1]), fill = col["grey"], colour = col["greyLine"]) +
    geom_sf(data = L(lakes6[sel6], p6[1]), fill = col["lake"], colour = col["lakeLine"]) +
    # 2. cut
    geom_sf(data = L(big, p6[2]), fill = NA, colour = col["greyLine"], linetype = "dotted") +
    geom_sf(data = L(cutbox, p6[2]), fill = NA, colour = col["gap"], linetype = "dashed") +
    geom_sf(data = L(bigCut, p6[2]), fill = col["lake"], colour = col["lakeLine"]) +
    geom_sf(data = L(smalls[sel6[-1]], p6[2]), fill = col["lake"], colour = col["lakeLine"]) +
    geom_sf(data = L(circ(20, 20, w6), p6[2]), fill = NA, colour = "grey20", linetype = "dashed") +
    # 3. buffer once
    geom_sf(data = L(cmb(bufs), p6[3]), fill = col["buffer"], colour = col["bufferLine"], alpha = 0.7) +
    geom_sf(data = L(used, p6[3]), fill = col["lake"], colour = col["lakeLine"]) +
    # 4. clip
    geom_sf(data = L(cmb(drop), p6[4]), fill = col["grey"], colour = col["greyLine"]) +
    geom_sf(data = L(cmb(keep), p6[4]), fill = "#8DC26F", colour = "grey30") +
    geom_sf(data = L(cmb(cross), p6[4]), fill = col["foot"], colour = "grey30") +
    geom_sf(data = L(D, p6[4]), fill = NA, colour = "grey20", linewidth = 0.6) +
    labb(txt(c(40, 40, 30), c(74, 76, 11), c("dashed: destination box\ngrown by one width",
         "red dashed: cut edge,\ntwo widths out", "reach of one width"), c(p6[1], p6[2], p6[2])), size = 2.5) +
    labb(txt(88, 74, "kept as is", p6[4]), size = 2.6, colour = "#3E7D1F") +
    labb(txt(88, 67, "exact clip", p6[4]), size = 2.6, colour = col["bufferLine"]) +
    labb(txt(88, 60, "dropped", p6[4]), size = 2.6, colour = "grey35") +
    facet_wrap(vars(panel), nrow = 1) +
    coord_sf(xlim = c(-8, 108), ylim = c(-8, 82), expand = FALSE) +
    theme_fig()
  save(g6, "fig_tile_steps.png", 12, 3.5)

  # =============================================================================
  # 7. Cells at the study-area edge
  # =============================================================================
  study7 <- st_sfc(poly(rbind(c(0, 0), c(100, 0), c(100, 40), c(60, 80), c(0, 80))))
  grid7 <- st_make_grid(st_bbox(c(xmin = 0, ymin = 0, xmax = 100, ymax = 80)), cellsize = 20)
  lake7 <- st_sfc(circ(78, 44, 7))
  str7 <- st_sfc(st_linestring(rbind(c(8, 70), c(30, 62), c(50, 66), c(70, 52))))
  foot7 <- st_union(c(lake7, buf(lake7, 5), buf(str7, 5)))
  foot7s <- inter(foot7, study7)
  val7 <- vapply(seq_along(grid7), function(i) {
    inStudy <- length(inter(grid7[i], study7)) > 0 && sum(area(inter(grid7[i], study7))) > 0
    if (!inStudy) return(NA_real_)
    x <- inter(grid7[i], foot7s)
    if (!length(x)) 0 else sum(area(x)) / area(grid7[i])
  }, 1)
  c7 <- st_coordinates(st_centroid(grid7))
  # Labels sit in the lower-left corner of each cell so they stay off the shapes.
  g7 <- ggplot() +
    geom_sf(data = L(grid7[is.na(val7)], "x"), fill = "grey88", colour = "grey60", linewidth = 0.3) +
    geom_sf(data = L(study7, "x"), fill = "#F4F8FC", colour = col["study"], linewidth = 0.8) +
    geom_sf(data = L(foot7, "x"), fill = col["foot"], colour = NA, alpha = 0.25) +
    geom_sf(data = L(foot7s, "x"), fill = col["foot"], colour = NA, alpha = 0.85) +
    geom_sf(data = L(grid7, "x"), fill = NA, colour = "grey55", linewidth = 0.3) +
    geom_sf(data = L(lake7, "x"), fill = col["lake"], colour = col["lakeLine"]) +
    geom_sf(data = L(str7, "x"), colour = col["stream"], linewidth = 0.8) +
    labb(txt(c7[, 1] - 5, c7[, 2] - 7, ifelse(is.na(val7), "NA", ifelse(val7 > 0, sprintf("%.2f", val7), "0")), "x"), size = 3) +
    coord_sf(xlim = c(0, 100), ylim = c(0, 80), expand = FALSE) +
    theme_fig() +
    theme(strip.text = element_blank())
  save(g7, "fig_study_edge.png", 6, 4.8)

  invisible(list(pieces = data.frame(area = pa, pick = pick), shared = shared, fractions = frac, edge = val7))
}

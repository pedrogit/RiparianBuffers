sol_sf_reference <- function(grid, footprint) {
  polygons <- sf::st_set_crs(sf::st_geometry(sf::st_as_sf(footprint)), NA)
  e <- unname(as.vector(terra::ext(grid)))
  cells <- sf::st_make_grid(
    sf::st_as_sfc(sf::st_bbox(c(xmin = e[1], ymin = e[3], xmax = e[2], ymax = e[4]))),
    n = c(terra::ncol(grid), terra::nrow(grid))
  )
  hits <- sf::st_intersection(cells, polygons)
  answer <- numeric(terra::ncell(grid))
  if (!length(hits)) return(answer)
  index <- attr(hits, "idx")[, 1]
  nc <- terra::ncol(grid)
  cell <- (terra::nrow(grid) - 1 - (index - 1) %/% nc) * nc + (index - 1) %% nc + 1
  areas <- rowsum(as.numeric(sf::st_area(hits)) / prod(terra::res(grid)), cell, reorder = FALSE)
  answer[as.integer(rownames(areas))] <- areas[, 1]
  answer
}

testthat::test_that("exactextractr agrees with sf for curves, holes and non-square cells", {
  grid <- terra::rast(xmin = 0, xmax = 400, ymin = 0, ymax = 300,
                      nrows = 6, ncols = 4, crs = "EPSG:5070")
  centre <- terra::vect(matrix(c(175, 125), ncol = 2), crs = terra::crs(grid))
  curved <- terra::buffer(centre, 117, quadsegs = 30)
  holed <- terra::erase(sol_rect(15, 285, 10, 290), sol_rect(65, 135, 85, 165))
  for (shape in list(curved, holed, rbind(sol_rect(5, 65, 10, 90), sol_rect(205, 365, 160, 280)))) {
    reference <- sol_sf_reference(grid, shape)
    result <- exactTileFractions_sol6_1(grid, shape)
    testthat::expect_lte(max(abs(result - reference)), 1e-6)
    # Coverage must not depend on raster values or NA values.
    terra::values(grid) <- NA_real_
    testthat::expect_identical(exactTileFractions_sol6_1(grid, shape), result)
    tiled <- numeric(terra::ncell(grid))
    for (row in c(1L, 4L)) for (col in c(1L, 3L)) {
      subgrid <- terra::rast(xmin = (col - 1) * 100, xmax = (col + 1) * 100,
                             ymin = 300 - (row + 2) * 50, ymax = 300 - (row - 1) * 50,
                             nrows = 3, ncols = 2, crs = terra::crs(grid))
      ids <- terra::cellFromXY(grid, terra::xyFromCell(subgrid, seq_len(terra::ncell(subgrid))))
      tiled[ids] <- exactTileFractions_sol6_1(subgrid, shape)
    }
    testthat::expect_lte(max(abs(tiled - result)), 1e-6)
  }
})

testthat::test_that("disjoint features in one cell are summed, overlap is not concealed", {
  grid <- terra::rast(xmin = 0, xmax = 100, ymin = 0, ymax = 100,
                      nrows = 1, ncols = 1, crs = "EPSG:5070")
  pieces <- rbind(sol_rect(0, 20), sol_rect(80, 100))
  testthat::expect_lte(abs(exactTileFractions_sol6_1(grid, pieces) - 0.4), 1e-6)
  testthat::expect_error(exactTileFractions_sol6_1(grid, rbind(sol_rect(), sol_rect())), "outside")
})

testthat::test_that("precomputed tile geometry preserves footprint and empty behavior", {
  f <- sol_prepared(sol_fixture(20, lake = TRUE))
  tile <- terra::as.polygons(terra::ext(f$PlanningGrid), crs = terra::crs(f$PlanningGrid))
  args <- list(tileCore = tile, studyArea = f$studyArea, Jurisdiction = f$Jurisdiction,
               policy = f$policy, streams = f$streams, lakes = f$lakes)
  baseline <- do.call(buildPolicyFootprints_sol6_1, args)
  reused <- do.call(buildPolicyFootprints_sol6_1, c(args, list(
    core = intersectPolygons_sol6_1(tile, f$studyArea), maxWidth = f$haloWidth_m)))
  testthat::expect_identical(terra::geom(reused), terra::geom(baseline))
  testthat::expect_null(do.call(buildPolicyFootprints_sol6_1, c(args, list(core = NULL))))
})

testthat::test_that("shared archive validation recognizes complete and incomplete sources", {
  source <- riparianSource_sol6_1("streams")
  members <- paste0("nested/HydroRIVERS_v10_na", c(".shp", ".shx", ".dbf", ".prj"))
  listing <- data.frame(Name = members, Length = 100)
  testthat::expect_null(zipShapefileProblem_sol6_1(listing, source, "test.zip"))
  testthat::expect_match(zipShapefileProblem_sol6_1(listing[-1, ], source, "test.zip"), "lacks expected")
  testthat::expect_match(zipShapefileProblem_sol6_1(listing[-4, ], source, "test.zip"), "missing or empty")
  listing$Length[2] <- 0
  testthat::expect_match(zipShapefileProblem_sol6_1(listing, source, "test.zip"), "missing or empty")
})

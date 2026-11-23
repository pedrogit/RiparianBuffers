# RiparianBuffers news

## Unreleased

- **Jurisdictions are cropped before repair.** Only jurisdiction land within
  `jurisdictionMargin_m` (default 100 km) of the study extent is repaired,
  dissolved and used; on the ON/QC 200 km test this cut preparation from 278 s to
  4.8 s with an identical raster.
- **Parallel processing and resumable tile loop.** New parameter `workers`
  (default 4) runs the fringe assignment (`assignPiecesToJurisdiction`) and the
  tile loop in separate R processes (package callr; see `runJobs`). The raster
  is identical for any number of workers. New parameter `resume` (default TRUE)
  saves each finished tile under `riparian_progress` in the scratch folder, and a
  rerun with identical inputs and code skips them. A failing tile is reported by
  number, row, column and extent. Resuming is off when the vector is exported.
- **Faster tile loop (about 109 to 59 min on the Eastern Canada area).** Buffers
  are made once per class and width and shared across destinations; buffers
  wholly inside one destination skip the exact clip (`clipToDestination`);
  hydrology is cut to the users' extent grown by twice the width before
  buffering. Footprints match the previous code to about 1e-4 m2.
- Header comments added to every function.
- **Breaking: the vector export is now a GeoPackage.** `riparian_buffers.gpkg`
  (layer `riparian_buffers`) replaces `riparian_buffers.shp`. A Shapefile stops
  at 2 GiB per component, which the full Eastern Canada area could exceed; a
  GeoPackage has no practical limit. `checkShapefileSize` is removed, so the
  export no longer stops at a size limit. `vectorBufferFile` now points to the
  `.gpkg`. Earlier NEWS entries below still refer to the Shapefile.
- README rewritten in plain language with schematic figures (`docs/`,
  `scripts/make_readme_figures.R`).
- **Small uncovered fringes take a neighbouring jurisdiction's policy.** New
  parameter `fringeMaxArea_km2` (default 50; 0 restores the strict check). Each
  piece of uncovered non-lake land smaller than the limit is assigned to the
  jurisdiction sharing the longest border with it, or to the nearest one if it
  touches none. Any piece at or above the limit still stops the run. The count,
  area and largest piece are reported in a message and in
  `Riparian$fringeAssigned`. On the full Eastern Canada study area this
  assigned 136,649 pieces (1,817.6 km2, largest 47.4 km2), which previously made
  the coverage check stop.
- The neighbour search is tiled (128 km tiles, vertex vote, exact shared-border
  length for pieces touching several jurisdictions) because an untiled search
  ran out of memory on the full area.
- `uncoveredNonLakeLand` now splits the uncovered area into pieces and queries
  the lakes with the pieces, so a spatial index is used. Testing every lake
  against one multipart polygon took about 22 hours on the full area; the helper
  now takes about 9 minutes and gives identical areas on the Ontario-Quebec
  test.
- **Breaking: names internal to the module were shortened.**
  Parameters are now `tileCells`, `exportVectorBuffer` and `fringeMaxArea_km2`.
  Output files are `riparian_fraction.tif` and `riparian_buffers.shp`. Helper
  functions, R/ file names, options (`riparian.moduleDir`), environment
  variables (`RIPARIAN_MODULE_DIR`) and cache and scratch names were shortened
  the same way. Existing source caches under the old names are not reused. Scripts
  that pass the old parameter names must be updated.
- **Breaking: only `Riparian` is published.** Prepared copies of
  `Jurisdiction`, `riparianBufferPolicy`, `Hydrology_streams` and
  `Hydrology_lakes` are no longer created. No module in this repository used
  them, and they kept every prepared stream and lake in the `simList`
  after `init`. Prepared data stay local; common inputs are never modified.

## 0.1.1

- Reuse tile-study intersections and the validated maximum policy width, remove
  redundant raster initialization, and share cached/downloaded ZIP validation.
- Replace explicit sf/GEOS polygon-cell intersections with `exactextractr`
  coverage fractions, preserving full-cell denominators and outside-study NA.
- Add `exactextractr` as a runtime dependency and record the agreed absolute
  coverage tolerance of 1e-6 in output metadata. Retain overlap/range checks.
- Add regression comparisons against independent planar sf intersections and
  tiled coverage checks, including holes, multiple features and non-square cells.

## 0.1.0

- Initial isolated module. It writes `Riparian` and uses the common read-only
  `PlanningGrid` when supplied.
- Replaced the original fine-raster approximation with projected vector
  buffers and exact polygon--planning-cell intersections.
- Added destination-jurisdiction policy assignment, cross-border hydrology
  halo selection, automatic HydroRIVERS/HydroLAKES preparation when either
  optional hydrology input is absent, and a 240 m standalone grid fallback.
- **Lake interiors are included.** The numerator is the union of mapped lake
  water and policy buffer footprints; it is not a land-only riparian result.
- Added a default-on Shapefile export of the final numerator footprint. Tile
  seams are processing boundaries and are not ecological boundaries.

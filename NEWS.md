# riparianBuffers_Sol6_1 news

## Unreleased

- **Small uncovered fringes take a neighbouring jurisdiction's policy.** New
  parameter `fringeMaxArea_km2` (default 50; 0 restores the strict check). Each
  piece of uncovered non-lake land smaller than the limit is assigned to the
  jurisdiction sharing the longest border with it, or to the nearest one if it
  touches none. Any piece at or above the limit still stops the run. The count,
  area and largest piece are reported in a message and in
  `Riparian_sol6_1$fringeAssigned`. On the full Eastern Canada study area this
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
- **Breaking: names internal to the module no longer carry `_sol6_1`.**
  Parameters are now `tileCells`, `exportVectorBuffer` and `fringeMaxArea_km2`.
  Output files are `riparian_fraction.tif` and `riparian_buffers.shp`. Helper
  functions, R/ file names, options (`riparian.moduleDir`), environment
  variables (`RIPARIAN_MODULE_DIR`) and cache and scratch names were renamed the
  same way. Existing source caches under the old names are not reused. Scripts
  that pass the old parameter names must be updated.
- **Breaking: only `Riparian_sol6_1` is published.** `Jurisdiction_sol6_1`,
  `riparianBufferPolicy_sol6_1`, `Hydrology_streams_sol6_1` and
  `Hydrology_lakes_sol6_1` are no longer created. No module in this repository
  used them, and they kept every prepared stream and lake in the `simList`
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

- Initial isolated solution-6.1 module.  It writes `Riparian_sol6_1`, never
  replaces the original module's `Riparian` output, and uses the common
  read-only `PlanningGrid` when supplied.
- Replaced the original fine-raster approximation with projected vector
  buffers and exact polygon--planning-cell intersections.
- Added destination-jurisdiction policy assignment, cross-border hydrology
  halo selection, automatic HydroRIVERS/HydroLAKES preparation when either
  optional hydrology input is absent, and a 240 m standalone grid fallback.
- **Lake interiors are included.** The numerator is the union of mapped lake
  water and policy buffer footprints; it is not a land-only riparian result.
- Added a default-on Shapefile export of the final numerator footprint. Tile
  seams are processing boundaries and are not ecological boundaries.

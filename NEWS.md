# riparianBuffers_Sol6_1 news

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

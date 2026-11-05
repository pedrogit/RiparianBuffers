# riparianBuffers_Sol6_1

`riparianBuffers_Sol6_1` produces `sim$Riparian_sol6_1$riparianFraction`, a
single-layer `terra::SpatRaster` aligned to `PlanningGrid`. It is an isolated
alternative to `RiparianBuffers`: it does **not** overwrite, alias, or feed
the original `sim$Riparian` result.

## Coverage definition

> The output measures combined lake-and-riparian-buffer coverage, not
> riparian land alone. Mapped lake water surfaces are included along with
> policy-width buffers on surrounding land. A planning cell entirely inside a
> mapped lake and the study area has `riparianFraction = 1`. Lake water
> overlapped by stream or lake buffers is counted once, not added repeatedly.
> The denominator is the full planning-cell area, including water. Cells
> entirely outside the study area are `NA`. These values are a hydrological
> influence/coverage signal, not a classification of harvestable land.

For example, 10 ha of buffered land plus 5 ha of mapped lake water in a 100 ha
planning cell gives $15 / 100 = 0.15$, after unioning any overlap. Stream
centreline data do not define river water-surface polygons: only mapped lake
polygons and constructed buffer footprints contribute.

```text
stream centreline ──────── buffer ────────
                         ┌───────────────┐
shoreline ───────────────┤ mapped lake   │
                         │ interior      │
                         └───────────────┘
              union: buffer ∪ lake interior (overlap counted once)
```

Fractions are exact vector polygon--cell area intersections in the selected
projected grid CRS, within numerical tolerance. They are not geodesically
exact, do not correct projection distortion, and do not establish completeness
of source hydrology or policy. Partial study-area cells still use their **full
cell** area as denominator.

## Inputs and preparation

Runtime packages are `SpaDES.core`, `terra`, `sf`, and `exactextractr`.
`exactextractr` supplies planar cell coverage; `sf` supplies geometry conversion
and Shapefile append/empty-export operations. The numerical acceptance tolerance
is an absolute coverage-fraction difference of **1e-6** (1 m² per 1000 m cell),
used for regression comparisons, tile consistency, and roundoff range checks.
The output records this as `coverageTolerance_sol6_1`.
Circular buffer edges are polygonal approximations with 30 segments per quadrant;
exact cell coverage applies to those resulting polygons, not ideal curved arcs.

Required input: projected metre-based polygon `studyArea`. Supply a projected
single-layer `PlanningGrid` to retain its exact geometry; otherwise standalone
preparation constructs a 240 m grid. A supplied grid, including Angela's
1000 m grid, always wins and is never modified.

Optional read-only inputs are `Jurisdiction`, `riparianBufferPolicy`,
`Hydrology_streams`, and `Hydrology_lakes`. Jurisdiction must be polygonal and
cover study-area non-lake land without overlaps. Policy buffers are assigned by
the **destination** jurisdiction, so features across a border can contribute
after clipping to the receiving jurisdiction. Missing policy keys use the
`default` row and are reported in `policyFallback`.

When a hydrology input is missing (and not promised by an upstream provider),
the module independently obtains only that source: North American HydroRIVERS
for streams and global HydroLAKES for lakes. These archives are large, require
internet access and local storage, and are cached under the module-specific
input directory. Download or validation failure stops with the source URL and
local location; a supplied valid empty vector does not trigger a download.
Hydrology is selected with a maximum-policy-width halo, so supplied data with
unknown coverage cannot guarantee complete halo coverage.

The bundled CSV contains simplified analytical assumptions, **not legal
US/Canadian policy**. It preserves the original six province mappings and
default fallback. Large streams are `ORD_STRA >= 4`; large lakes are
`Lake_area >= 1` km². Classification attributes are validated rather than
invented.

## Outputs and metadata

`Riparian_sol6_1` contains `riparianFraction`, `policy`, `rasterFile`, and,
when enabled, `vectorBufferFile`; it also records method, coverage convention,
source CRS, geometry repairs, policy fallback diagnostics, halo provenance,
and `lakeInteriorsExcluded = FALSE`. There is deliberately no `raster_m`
field: this algorithm has no fine computation raster.

`exportVectorBuffer_sol6_1` is a non-missing logical parameter, default
`TRUE`. It writes `riparian_buffers_sol6_1.shp` beside
`riparian_fraction_sol6_1.tif`. The Shapefile is the final lake-and-buffer
numerator footprint, in the output CRS, not raw hydrology, a raster polygon,
or a land-only habitat boundary. Its short `tile_id` field identifies
non-overlapping tile-core pieces. Neighbouring pieces may visibly meet at tile
seams; these are processing boundaries, not ecological boundaries, and do not
duplicate area. Shapefile field names are short and its component files have a
portable approximately 2 GiB limit; export stops rather than truncating data.
The `.shp`, `.shx`, `.dbf`, and `.prj` sidecars must stay together.

Set the parameter to `FALSE` to skip **only** vector export: raster values are
unchanged and `vectorBufferFile` is `NULL`. Existing output names are protected
from silent overwrite.

## Coexistence

Both modules can run in one `simList` if they share explicitly supplied,
validated read-only inputs and one common `PlanningGrid`. Do not let both
modules independently make grids. The original module retains its existing
algorithm and output `Riparian`; downstream Landbase continues to select that
output unless separate, explicit integration selects `Riparian_sol6_1`.
Fallback hydrology prepared by this module stays suffixed/module-local and is
not published as the original module's shared hydrology output.

See `scripts/demo_riparianBuffers_Sol6_1.R` for an offline synthetic run and
`EasternCanadaForAngela/run_riparianBuffers_Sol6_1.R` for opt-in Angela setup.

## Validation and resource limits

Run the offline suite by sourcing `tests/unitTests.R`, then calling
`unitTests(moduleDir, scratchDir)` with an explicit scratch directory. Tests
cover synthetic analytical fractions, downloads mocked offline, lake interiors,
cross-border policies, tile equivalence, file exports and both modules together.
The original module's debugger calls are removed only in isolated test copies.

A 200 km Ottawa/Ontario–Quebec test at 1000 m resolution completed with vector
export in 106.71 seconds using exactextractr, versus 846.89 seconds with the
previous sf cell-intersection engine. Shared input preparation took a further
75.89 seconds in the rerun. Maximum fraction difference across the full test was
1.09e-7, with identical outside-study masks and no cells exceeding 1e-6.
These are single-run timings, not a reliable full-area estimate. Real-data repair
and vector geometry operations remain expensive. Start with a representative
subset; a full Angela-area run has not been performed.

`tileCells_sol6_1` defaults to 128 cells per tile side. Larger tiles can require
more memory and more complex geometry operations. No parallel processing or
additional buffer optimization is enabled.

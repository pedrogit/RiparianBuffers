# Plan: the new riparian buffer module

> **Naming.** "The new" is this module (`modules/RiparianBuffers`). "The original" is the earlier module, now in `modules/RiparianBuffersOriginal`. Both are named `RiparianBuffers` and both create `Riparian`, so they are never loaded in the same simulation.

## Goal and approved scientific rules

Create the new module without changing the original (`modules/RiparianBuffersOriginal`) or replacing existing runs/results.

User-selected design:
- Construct vector lake-and-buffer areas using the actual policy widths; calculate exact polygon–cell area fractions rather than averaging a fine binary raster.
- Use **policy of the jurisdiction the buffer lands in**: each jurisdiction applies its widths to nearby streams and lakes, including features across its boundary; clip the resulting lake-and-buffer area to the receiving jurisdiction.
- **Include mapped lake interiors**: union lake water surfaces with the policy-width buffer lake-and-buffer areas. A cell wholly inside a mapped lake and study area has fraction 1, consistent with the original module's normal lake-interior behavior. This supersedes the earlier land-only choice.

The fraction denominator remains the **full grid-cell area**, preserving the fractional-coverage interpretation while exposing the result as `Riparian$riparianFraction`. It is not the fraction of remaining dry land. Clip the numerator to study-area polygons; cells outside the study area are NA, and partially intersecting cells retain the full-cell denominator. Record this definition in output metadata and documentation.

### Required documentation: lake inclusion

State prominently in the README overview, module metadata/output descriptions, and module manual:

> The output measures combined lake-and-riparian-buffer coverage, not riparian land alone. Mapped lake water surfaces are included along with policy-width buffers on surrounding land. A grid cell entirely inside a mapped lake and the study area has riparianFraction = 1. Lake water overlapped by stream or lake buffers is counted once, not added repeatedly. The denominator is the full grid-cell area, including water. Cells entirely outside the study area are NA. These values are a hydrological influence/coverage signal, not a classification of harvestable land.

Explain that stream centreline data do not define explicit river water-surface polygons; only mapped lake polygons and constructed buffer lake-and-buffer areas contribute. Include a shoreline/stream-over-lake illustration and a worked example: 10 ha buffered land plus 5 ha mapped lake water in a 100 ha cell gives 0.15, with overlapping areas counted once. Explicitly document lake inclusion, which was implicit in the original implementation but not clearly specified in its manuals. Record `lakeInteriorsExcluded = FALSE` in output metadata. Remove superseded land-only wording from all new documentation and examples.

## 1. Scaffold and preserve the interface

Use the module name `RiparianBuffers` consistently in directory, main filename, metadata, event handler, and parameter namespace. Provide one `init` event, with no recurring events or debugger calls.

Expose `sim$Riparian$riparianFraction` as a single-layer `SpatRaster` aligned exactly to the selected target reference raster. The original module also writes `sim$Riparian`, so the two are never loaded together. Preserve the list's `policy` element; add method, coverage convention, source CRS, policy-fallback diagnostics, and lake/border-rule metadata. Do not retain a misleading `raster_m` field: the exact engine has no fine-raster resolution. Document the object-name and metadata compatibility differences.

Use the common shared object name `PlanningGrid`. Accept a supplied target reference raster; otherwise build a documented 240 m default during standalone input preparation. A supplied grid overrides this fallback, including the 1000 m grid used in the Angela example. Never replace or modify an existing or upstream-promised grid. Output resolution is independent of geometry processing. Use jurisdiction polygons, not a resampled coarse jurisdiction raster. Existing jurisdiction raster/lookup products alone are insufficient to recover precise boundaries; obtain polygons when missing.

### Naming and shared inputs

- External input names are: `studyArea`, `PlanningGrid`, `Jurisdiction`, `riparianBufferPolicy`, `Hydrology_streams`, and `Hydrology_lakes`. The new algorithm does not require `jurisdictionMap` or `jurisdictionLookup`.
- Reuse suitable explicitly supplied/upstream inputs; prepare missing data locally. Never overwrite, repair, project, classify, or extend an input in place. Keep modified or fallback geometries and policy tables module-local. The only output is `Riparian`.
- Do not store independently downloaded fallback streams and lakes under the input names. Do not assume cropped streams and lakes from another module supplies the margin; independently prepared fallback data remain isolated.
- Keep working variables module-local. Do not leak intermediate objects into the shared `sim` or `.GlobalEnv`.
- Isolate writable cache entries, prepared input files, temporary files, GeoTIFFs, policy exports, and logs in dedicated module directories. Raw source archives may be reused read-only after validation.
- Default downstream Landbase still consumes the original module's `Riparian`. Preserve the same nested fraction structure to allow a later explicit integration adapter.

## 2. Prepare and validate inputs

Keep standalone operation: require `studyArea`; accept upstream/supplied grid, jurisdiction polygons, policy and streams and lakes; prepare only missing objects. Reuse existing source URLs, caches and available `terra`/`reproducible` functionality, without hard-coded user-specific scratch paths.

### Automatic water-data downloads

Independently check the two optional inputs. If `Hydrology_streams` is missing and not promised by a configured upstream provider, automatically obtain North American HydroRIVERS from `https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_na_shp.zip`. If `Hydrology_lakes` is missing and not promised upstream, automatically obtain global HydroLAKES from `https://data.hydrosheds.org/file/hydrolakes/HydroLAKES_polys_v10_shp.zip`. When both are missing, obtain both; when only one is missing, obtain only that dataset. No external data-preparation module is required.

Reuse a validated complete local archive/extraction before downloading, validate required shapefile components and classification attributes, and prepare margin-aware module-local working copies as specified below. Supplied invalid inputs produce a clear validation error, not silent replacement. Download/extraction failures must stop with the dataset, URL and local path identified; do not silently substitute empty streams and lakes or a zero raster. Missing inputs must be distinguished from supplied valid empty datasets, which should not trigger downloads. Document internet access, download/storage costs and geographic coverage.

Test the four supplied/missing combinations with mocked download/preparation functions, verifying the correct sources, cache reuse, no unexpected downloads, and no writes to the original module's shared streams and lakes outputs. Test failures and valid empty supplied inputs separately.

Validate projected metre-based CRS, grid geometry, finite nonnegative policy widths, unique jurisdiction keys, default-policy availability, required jurisdiction/classification fields, and valid geometries. Normalize supplied lookup tables if used. Make geometry repair explicit and report it; never alter original input files.

Retain source classifications: `ORD_STRA >= 4` for large streams; `Lake_area >= 1 km²` for large lakes. Validate missing/NA class attributes instead of silently assigning them. Retain the bundled simplified policy values, clearly labelled as analytical assumptions, not legal US/Canadian policy. Preserve the existing six-province mappings and default fallback for other names, with an explicit diagnostic identifying defaulted jurisdictions.

Prepare streams and lakes using a study-area envelope expanded by the largest applicable width. Preserve original feature attributes and geometries needed for buffering and lake-interior inclusion. Select lake polygons intersecting the processing area even when their shorelines are outside it, so cells deep inside large lakes are included. Do not crop to the unbuffered study-area boundary before buffering. If supplied streams and lakes coverage is unknown, document that complete margin coverage cannot be guaranteed from feature extents alone.

Check that jurisdiction polygons cover the study-area land where policy widths must be evaluated. Include mapped lake interiors independently of policy assignment, so offshore jurisdiction gaps do not turn wholly lake-covered cells into zero. Gaps on non-lake land remain validation errors. Handle overlapping jurisdiction polygons explicitly rather than allowing arbitrary overwrite.

Retain the base-R ZIP extraction workaround if required, and validate extracted files. Reuse existing complete raw downloads where feasible, but do not assume old cropped caches contain the margin. Check cached geometries/coverage/provenance before reuse; use separate cache keys for the new module.

## 3. Implement policy lake-and-buffer areas

For each grid-aligned tile:
1. Select relevant receiving jurisdictions intersecting the tile.
2. Select nearby streams/lakes from a margin at least as wide as the maximum policy width; avoid artificial endpoints or shorelines introduced by clipping before buffering.
3. For each feature class and applicable policy width, buffer original geometries by that width. Reuse matching class/width results locally where possible.
4. Clip lake-and-buffer areas to receiving-jurisdiction polygons and the tile and study area.
5. Union overlapping lake-and-buffer areas so area is counted once.
6. Union the lake-and-buffer area with **all mapped lake interiors** intersecting the tile and study area, including lakes with zero buffer widths or no assigned water-surface jurisdiction. Count water/stream/lake-buffer overlaps once. Lake interiors are not subtracted.

Zero widths contribute no surrounding land, but mapped lake interiors still contribute their full area. Empty feature classes are valid and yield no lake-and-buffer area of that class. No fixed 100 m pre-buffer, global distance raster, coarse jurisdiction resampling, or class-order overwrite remains.

Buffering and area calculations use the chosen projected CRS. “Exact” means exact geometric polygon–cell intersection within numerical tolerance, not geodesically exact ground area or proof that source streams and lakes/policy are complete. Document projection distortion and the study-area CRS limitation; do not silently reproject the user's output grid.

## 4. Exact coverage and scalable execution

First verify the installed `terra` exact polygon–cell coverage API on analytical fixtures, including fraction semantics, cell IDs, empty intersections, and datatype precision. Prefer existing dependencies; consider another exact-coverage dependency only if required and obtain approval before adding it. Do not substitute `rasterize(cover=TRUE)` and call it exact.

Compute coverage from locally dissolved non-overlapping geometry. Ensure each output cell is processed once, only tile pieces are written, and fraction values are finite and within [0,1] up to a stated floating-point tolerance. Do not conceal substantial errors by unconditional clamping.

Use one output-grid-aligned tiling parameter expressed in cells. Avoid a continental union or fine-resolution raster. Use disk-backed output and configured scratch paths; process sequentially initially. Validate the installed incremental raster-write API before implementing it. Tiled and untiled results must agree within numerical tolerance. Record tile progress and useful errors without storing a full simulation checkpoint per tile.

### Raster and optional vector export

Add module parameter `exportVectorBuffer` of class logical, default **FALSE** (changed from TRUE), validated as one non-missing boolean. In the same module run that writes the fraction GeoTIFF, export `riparian_buffers.gpkg` (a GeoPackage, layer `riparian_buffers`) when this parameter is TRUE. FALSE skips vector export only: the raster and its values must be identical. Use the configured output path and isolated filenames; never overwrite existing results silently.

Export the **final lake-and-buffer area used for the raster numerator**: unioned buffers plus mapped lake interiors, clipped to the study area. Apply receiving-jurisdiction clipping to policy buffers, not to the independently included lake interiors. Do not export raw streams and lakes, uncorrected buffer polygons, or a polygonization of raster cells. Use the same projected CRS as the output raster. Document that this layer represents combined lake-and-buffer coverage, not land-only riparian habitat or an actual ecological-boundary survey.

Support tiled processing without a global dissolve: append non-overlapping tile polygon pieces to the vector layer, using stable short field names such as `tile_id`. Adjacent polygons may share tile seams; document that these are processing boundaries, not ecological boundaries, and do not duplicate area. Validate the installed vector append API. Write a single GeoPackage, which has no practical size limit (a Shapefile is limited to 2 GiB per component); report the main path and store it as `Riparian$vectorBufferFile` (NULL when export is disabled). Also record the raster filename in the output metadata.

The export was first planned as a Shapefile with a 2 GiB size check; it was changed to a GeoPackage so the full Eastern Canada area can be exported. Build the files in module-specific scratch space and copy the completed outputs to the output folder together; identify incomplete exports clearly on failure. For empty lake-and-buffer areas, validate that a correctly projected empty polygon Shapefile can be written and read back; do not invent non-empty placeholder geometry.

Explain the boolean parameter, default FALSE, output location, required sidecars, lake inclusion, tile seams, and FALSE behavior in README/manual and examples.

## 5. Proposed files

Within `modules/RiparianBuffers/`:
- `RiparianBuffers.R`: metadata, standalone input preparation, event dispatcher.
- `R/riparianBuffersInit.R`: orchestration and output rules.
- `R/validateRiparianInputs.R`: validation and normalization.
- `R/resolveRiparianPolicy.R`: jurisdiction names, policy matching and diagnostics.
- `R/buildLakeBufferArea.R`: correct vector geometry using the width of the jurisdiction each buffer lands in.
- `R/fractionalCoverage.R`: exact coverage and tiled writing.
- `data/riparianBufferPolicy.csv`: documented initial policy table.
- `README.md`, `RiparianBuffers.Rmd`, `NEWS.md`.
- `tests/unitTests.R`, `tests/testthat/helper-fixtures.R`, and tests for policies, lake-and-buffer areas, coverage and module integration.
- `scripts/demo_RiparianBuffers.R`: offline small example.

Group small helpers rather than creating further files or adding unrelated infrastructure.

## 6. Tests and acceptance criteria

Run offline deterministic synthetic tests before remote data preparation:
- Straight streams with analytically known 20/30/50 m buffers and cell fractions, including zero width.
- All four streams and lakes classes and missing classification attributes.
- Lake shoreline buffers with lake interiors included; stream-over-lake areas counted once.
- A cell wholly inside a mapped lake and study area returns 1, including a cell far from the shoreline, with zero lake-buffer width, or without offshore jurisdiction coverage. A partly lake-covered cell counts lake area plus non-overlapping buffer area using full-cell area as its denominator. Documentation examples must match these tests.
- Overlapping stream/lake buffers do not double-count.
- Two adjacent jurisdictions with unequal widths, including features entirely across the border.
- Features just outside study-area/tile boundaries that contribute buffers inside.
- Jurisdiction coverage gaps/overlaps and invalid/duplicate/missing policies.
- Empty streams/lakes, no lake-and-buffer area, full-cell coverage, partial study-area cells, and outside-area NA values.
- Non-square grid cells and geometry alignment; no rounded resolution-ratio assumption.
- Exact-coverage fixture accuracy and tiled-versus-untiled equivalence across tile boundaries.
- Minimal `simInit()`/`spades()` integration using supplied offline inputs, verifying outputs and no hidden downloads.
- Compatibility smoke test of the nested fraction structure expected by Landbase, with any adapter explicit; do not modify Landbase.
- Test independent fallback preparation and supplied-input overrides; verify supplied inputs remain unchanged and no writable file/cache collisions or unexpected shared-state changes occur. Confirm the only output object is `Riparian`.
- Test default TRUE creates the raster and readable GeoPackage with expected CRS and layer name; FALSE creates no vector output and leaves raster values unchanged. Test tiled append, empty lake-and-buffer areas, pre-existing output protection, invalid boolean values, and agreement between exported polygon coverage and the raster fractions within numerical tolerance.

The old output is a comparison baseline only, not a correctness oracle: changed policy lake-and-buffer areas are deliberate.

## 7. Separate Angela demonstration and rollout

Add an opt-in `EasternCanadaForAngela/run_RiparianBuffers_new.R`, leaving the existing script and GeoTIFFs unchanged. Use the existing study area and 1000 m target reference raster, validate cached inputs, and isolate new outputs/cache under a location specific to the new module. Never load the original in the same simulation. Document that Landbase's existing explicit load-order reference and input still target the old module/result; selecting the new result needs separate explicit integration work.

After unit/integration tests, run a small representative real-data subset spanning a jurisdiction boundary, compare geometries/fractions, and measure tile runtime/memory. Report estimates before a full Angela-area execution. A full-area production run is **not part of initial implementation validation** and should be requested separately after reviewing the subset.

## Completion

Deliver the new isolated module, offline tests, documentation and opt-in runner. Report tests run, unresolved data/API limitations, expected differences from the old module, and the subset performance results. Do not modify the original module or automatically replace its existing outputs.

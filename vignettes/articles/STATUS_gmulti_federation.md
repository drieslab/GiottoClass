# Status — gmulti federation implementation

Companion to [DESIGN_gmulti_federation.md](DESIGN_gmulti_federation.md). This file tracks **only the four implementation phases currently in scope** — what's landed, what's planned, what's deferred.

**Branches** (both `feature/gmulti-federation-design`):
- GiottoClass: `/Users/george/Documents/GitHub/GiottoClass-federation-design` (off `feature/giotto-view`)
- GiottoVisuals: `/Users/george/Documents/GitHub/GiottoVisuals-federation-design` (off `gsource`)

**Test counts**:
- GiottoClass: 174 PASS on parent branch (before this work); 259 PASS as of phase 5 (+ 1 pre-existing snapshotSave failure unrelated to this work).
- GiottoVisuals: 12 PASS for the new dispatcher tests (phase 4); pre-existing tests for color palettes + save unchanged.

---

## Landed

### Phase 1 — `@mapping` slot + auto-discovery + accessor

Commits `057f5135` (slot + auto-discovery + full-list setter) and `fc0b631a` (axis-scoped + entry-scoped setters).

- New slot `gmulti@mapping`: two-list structure (`spat_unit` / `feat_type`), each holding per-sample-named char vectors mapping gmulti-level handle → child-level slot name. Declares which children participate in each (spat_unit / feat_type) and reconciles per-child name variation.
- `.gm_discover_mapping(children)`: auto-populates the symmetric trivial mapping from children's `@cell_ID` / `@feat_ID` slot keys at construction. User-edited mappings survive bare re-init; assigning `NULL` triggers fresh re-discovery.
- `gmultiMapping()` / `gmultiMapping<-` accessor (exported generics). Three setter forms:
  - `gmultiMapping(mg) <- full_list` — full replacement
  - `gmultiMapping(mg, "spat_unit") <- axis_list` — replace one axis
  - `gmultiMapping(mg, "spat_unit", "cell") <- c(B191 = "cell", B215 = "poly")` — replace one entry
- Setter validates per-sample names + child slot existence; rejects unknown entries with clear errors.
- Joint-slot invalidation on mapping mutation is **per-universe** — only the affected `(spat_unit, feat_type)` universe's joint state drops; unrelated universes survive byte-identically.
- `show()` displays a one-line summary per axis.

### Phase 2 — Federation consults `@mapping`

Commit `f1d7beb8`.

- New `.gm_resolve_axis(g, axis, handle)`: returns participating samples + per-sample child-level slot names. Primary path is `@mapping` lookup; legacy fallback scans child slots when the handle isn't declared.
- New `.gm_resolve_participation(g, spat_unit, feat_type)`: composes two axis resolutions into the per-sample `(su, ft)` federation plan.
- Three federation helpers refactored to delegate via the new helpers:
  - `.gm_assemble_expression`
  - `.gm_assemble_cell_metadata`
  - `.gm_assemble_feat_metadata`
- `@mapping` is now load-bearing — a user-edited mapping unifying e.g. B's `"transcripts"` feat_type under gmulti-level `"rna"` makes `getCellMetadata(mg, feat_type = "rna")` actually pull B's transcripts slot.
- Legacy gmulti objects without `@mapping` (mapping cleared) keep working via per-child default fallback.

### Phase 3 — Access layer: `sample =` arg + `"sample::name"` parser

Commit `71a8e6a5`.

- `.parse_sample_qualified_name(name)`: splits on the first `::` for optional sample prefix.
- `.gm_slice_to_sample(x, sample, gobject)`: per-subobject-class slicer for joint cmeta / expr / dimreduc / nnnet via the `sample::cell_id` namespacing convention.
- `getExpression(gmulti)`: adds `sample =` AND parses `values =` for `"sample::name"` prefix. Conflicting `sample =` + prefix errors clearly.
- `getCellMetadata(gmulti)` and `getFeatureMetadata(gmulti)`: add `sample =` arg. (featmeta sample is no-op slice + validation only since featIDs are passthrough.)
- Five spatial-domain getters (`getSpatialLocations`, `getSpatialNetwork`, `getPolygonInfo`, `getFeatureInfo`, `getGiottoImage`): `sample =` as canonical alias for the legacy `object =` arg. Conflicts error.

After phase 3, the access pattern looks like:

```r
# canonical
getCellMetadata(mg, sample = "B191")
getExpression(mg, sample = "B191", values = "raw")

# prefix shortcut (on getters with a name/values arg)
getExpression(mg, values = "B191::raw")

# spatial domain (sample is alias of legacy object=)
getSpatialLocations(mg, sample = "B191")
```

### Phase 4 — Dispatcher cleanup

GiottoVisuals commit `ba5a263` (on the GiottoVisuals worktree's `feature/gmulti-federation-design` branch).

- Reworked `.gg_multi_dispatch_spatial`:
  - New `samples =` param replaces the previous abuse of `space =` as a sample-name vector. `space =` is now strictly a defined-coord-frame reference.
  - New `.resolve_samples(gobject, samples, space, child_names)`: auto-injects samples from `names(space@samples)` when `samples = NULL` and a defined space is supplied. Errors on samples not in `@objects` or not in the space's participation set.
  - New `.gg_build_panel_child(gobject, sample)`: produces the per-panel child by starting from the gmulti's child and projecting joint-only `@cell_metadata` columns onto it via the phase 3 access layer (`getCellMetadata(mg, sample = ...)` + `addCellMetadata`). **No `:::` reach** — uses only exported GiottoClass APIs.
- Plot fn signatures across `vis_spatial_gg.R` + `vis_spatial_in_situ.R` gained `samples = NULL` formal alongside `view` / `space`; dispatch sites forward it. Affects spatPlot2D / spatDimPlot2D / dimPlot2D / spatDimFeatPlot / spatInSituPlotPoints / Density / Hex and family.
- 9 new `test_that` blocks in `tests/testthat/test_gmulti_dispatch.R` (GiottoVisuals). 12 PASS.

After phase 4:
- The dispatcher's previous `space = c("A","B")` abuse is gone.
- Cross-sample defined-space panels become expressible via `space = "atlas"` (which auto-derives samples). Their actual cross-panel rendering remains deferred (separate design).
- No `:::` into GiottoClass anywhere in GiottoVisuals.

### Phase 5 — `federatedReadHandle` wrapper class

Commit `0edc4504` (GiottoClass) + commit `6a966697` (phase 3 doc regen).

- New S4 class `federatedReadHandle` in `R/classes-federatedRead.R`: holds a per-sample list of fragments + a `combine` function + an `output_class` hint + opaque `meta`. Consumers call `materialize()` (or `[[`) when they need a concrete object.
- Public constructor `federatedReadHandle(substores, keys, output_class, combine, meta)`.
- Methods: `length()`, `names()`, `[[`, `show()`, `materialize()` with optional `as = ` override.
- Mirror of `unionParquetGeomStore` in GiottoDisk one rung up: any list of fragments + combine fn, not just parquet substores.
- 7 new `test_that` blocks. 259 PASS in the gmulti suite (was 242).

**Scope of phase 5 is structural only.** Wiring the existing federation helpers (`.gm_assemble_expression` etc.) to RETURN a `federatedReadHandle` rather than an eager combined object is opt-in for downstream work (e.g. duckdb / sedonadb lowering of expression queries). Existing eager paths keep their current behavior unchanged.

After phase 5, the class is ready for these follow-on uses:
1. Returning a federation handle when getters detect file-backed substores (atlas-scale parquet expression stores).
2. Lowering federation into a single SQL plan when consumed by duckdb / sedonadb.
3. Arrow-side concat at materialize time for in-memory consumers.

---

## Deferred — phase 6 (NOT in scope right now)

A real follow-on but not blocking. Land it when a concrete workflow needs it.

### Phase 6 — Pointer-class for `@spatial_info`

A `gmultiSpatialAlias` class for ad-hoc cross-sample groupings within `@spatial_info` that don't follow the federation pattern (e.g. `"tumor_focus_polys" = B191's tumor_roi + B215's epithelium_roi`).

Why deferred: `@mapping` covers the standard federation case for spat_unit / feat_type axes. Pointer-class only matters for content-level aliases that don't decompose along those axes — long-tail use case, not blocking.

---

## Out of scope for THIS implementation effort (separate design pass)

Items the design doc explicitly identifies as needing their own design — do not pull into this work:

- **Composable views** (`view1 + view2`). Memory line 125 in `project_giottoview_design_shape.md`.
- **Cross-sample aggregation infrastructure** (the "fan-out + reduce" dispatch shape — needed for atlas-frame polygon aggregation across samples).
- **GiottoLens consumption of `@mapping`** — downstream package, separate branch.
- **Migration helper** for existing saved gmulti objects without `@mapping`. Auto-init on load might cover it; defer the decision until a concrete migration case appears.
- **`@h5_file` slot deprecation cleanup** — adjacent but independent.

---

## Files touched so far

GiottoClass:
```
R/gmulti.R                         (+ ~510 lines net across phases 1-3)
R/classes-federatedRead.R          (new — phase 5)
NAMESPACE                          (+ exports: gmultiMapping, gmultiMapping<-, federatedReadHandle)
man/giottoMulti-class.Rd           (regenerated)
man/gmultiMapping.Rd               (new)
man/federatedReadHandle*.Rd        (new — phase 5)
man/get*.Rd                        (8 manpages regenerated for phase 3 sample= arg)
tests/testthat/test-gmulti.R       (+ 33 new test_that blocks across phases 1-5)
```

GiottoVisuals (phase 4):
```
R/gmulti.R                         (~218 lines net — full rewrite of dispatcher + new helpers)
R/vis_spatial_gg.R                 (+ samples= formal across plot fn signatures)
R/vis_spatial_in_situ.R            (+ samples= formal across plot fn signatures)
DESCRIPTION                        (Collate field updated)
tests/testthat/test_gmulti_dispatch.R  (new — 9 new test_that blocks, 12 PASS)
man/*.Rd                           (16 manpages regenerated)
```

---

## Pointers

- Full design rationale: [DESIGN_gmulti_federation.md](DESIGN_gmulti_federation.md)
- Memory entry that indexes both files: `project_gmulti_federation_design.md`
- Foundation memory (view/space split — load-bearing for §2 of the design doc): `project_giottoview_design_shape.md`

*Last updated after phase 5 (2026-06-14).*

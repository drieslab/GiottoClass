# Implementation — federation

Detail page. Shared context in the [hub](IMPLEMENTATION_gmulti.md). The view/space
recipe subsystem is a sibling: [IMPLEMENTATION_viewspace.md](IMPLEMENTATION_viewspace.md).

One section per implementation. Status in the header.

---

# Foundation

## 1. `gAny` virtual base + `giottoMulti` class — Complete

Defines the container: named list of child `giotto` objects plus joint slots mirroring
`giotto`'s shared-domain slots by name.

- `gAny` is virtual, with `giotto` and `giottoMulti` as its two implementations
- slot names align with `giotto` where semantics are shared; per-dataset spatial slots are **omitted, not inherited-empty**
- shared-domain methods written once on `gAny`; accessors promoted `"giotto"` → `"gAny"` one at a time
- children held in a `list`, not an `environment` — keeps value semantics. Disk-backed children get reference semantics from the storage layer anyway; avoiding it at the container level removes a class of surprise
- per-child defaults (active spat_unit / feat_type) derived live, not cached — a cache drifts when a child is mutated standalone

Why not `contains = "giotto"`: spatial-domain methods would fall through to empty slots
silently. With a virtual base they fail loudly via no-method dispatch.

---

## 2. `@id_map` registry + `@id_sig` invalidation — Complete

Namespaces cell/feature IDs across children into one global vocabulary.

- `@id_map$cells` / `$feats`: `data.table(object, local_id, global_id)`
- default `global_id` = `paste(object, local_id, sep = "::")`
- features legitimately overlap across datasets, so feature `global_id` often equals `local_id`; the map still records which features exist where
- `@id_sig` caches a child length-signature; `initialize()` rebuilds only on a difference, so bare re-init costs one comparison over N children

`@id_map` is the **identity registry** — ground truth for which globals exist. It is
*not* the narrowing channel; see [§17](#17-cell_id--feat_id-narrowing-contract--complete).

---

## 3. `@source` multi-level gsource — Complete

Gives cross-sample artifacts (joint PCA, joint NN graphs) their own project directory
while children keep per-sample sources. `.gm_resolve_source()` at init:

- all sourced children must share one source class — mixing parquet- and BPCells-backed children breaks union/cbind dispatch downstream, so it errors
- an explicit source must match that class, or is accepted as-is when no child has one
- no explicit source → adopt the first sourced child's

**This is why the whole stack targets `gsource`:** `giotto@source` doesn't exist on `dev`.

---

## 4. save / load round-trip — Partial

`saveGiotto` delegates to `GiottoDisk::snapshotSave` for sourced objects; the federated
round-trip got a dedicated fix so children reconnect.

- **`snapshotSave` has no `signature(src = "gDirSource", x = "giottoMulti")`** — round-trip errors on a sourced multi. GiottoDisk-side. The one remaining `test-gmulti.R` failure. Blocks PR 3
- any new slot needs a round-trip test, not an assumption — the S4 prototype should cover objects saved before the slot existed, but this load path has needed explicit fixing before

---

# Federation

## 5. `@mapping` declaration + auto-discovery — Complete

Declares which children participate in each spat_unit / feat_type, and reconciles
per-child naming at the declaration layer instead of by renaming data.

```r
gmulti@mapping <- list(
    spat_unit = list(
        cell    = c(B191 = "cell", B215 = "cell", B651 = "cell"),
        nucleus = c(B191 = "nucleus_v1", B215 = "nuc"),   # naming varies
        tile    = c(B191 = "tile_100um")                  # one sample participates
    ),
    feat_type = list(
        rna     = c(B191 = "rna", B215 = "transcripts"),
        protein = c(B651 = "protein")
    )
)
```

- two named lists keyed by gmulti-level handle; each entry a per-sample named char vector → child-level name
- partial coverage is fine — a sample lacking the modality is simply absent
- `.gm_discover_mapping()` auto-populates the symmetric trivial mapping at construction from children's `@cell_ID` / `@feat_ID` keys
- user edits survive bare re-init; assigning `NULL` triggers fresh discovery
- `gmultiMapping<-` has three setter forms (full / axis-scoped / entry-scoped), validating entries against real spat_units / feat_types in their named samples
- invalidation is **per-universe** — editing one `(spat_unit, feat_type)` drops only that universe's joint state

**Why the lift was small:** per-spat_unit cell_ID universes already existed at the
gmulti level, under-used because the implicit federation assumed one spat_unit. So
`@mapping` is a declaration of intent rather than a data-model change, and
`sample::cell_id` namespacing is unaffected — separate universes don't collide.

Open: when an existing entry's per-sample names change, warn-and-drop dependent joint
state, or block pending explicit opt-in? Currently drops.

---

## 6. Federation helpers consult `@mapping` — Complete

Makes `@mapping` load-bearing: assembling a joint slot pulls the right child-level name
per sample.

- `.gm_resolve_axis(g, axis, handle)` → participating samples + each one's child-level slot name. Prefers `@mapping`, falls back to scanning child slots for undeclared handles
- `.gm_resolve_participation(g, su, ft)` composes two axis resolutions into the per-sample plan
- `.gm_assemble_expression` / `_cell_metadata` / `_feat_metadata` delegate through these
- cell metadata assembly tags each row `list_ID = child name`, so sample identity is an ordinary column for grouping / splitting / colouring
- legacy multis with cleared `@mapping` keep working via the per-child fallback

Consequence: a mapping unifying sample B's `"transcripts"` under handle `"rna"` makes
`getCellMetadata(mg, feat_type = "rna")` actually pull B's transcripts slot.

---

## 7. Access layer — Complete

Address content by sample without constructing scratch objects.

```r
getCellMetadata(mg, samples = "B191")
getExpression(mg, samples = "B191", values = "raw")
getExpression(mg, values = "B191::raw")
getSpatialLocations(mg, samples = "B191")      # `object =` is the legacy alias
```

- `samples =` canonical on every gmulti-aware getter
- `"sample::name"` shortcut where a getter has `name` / `values`; each entry parses independently; a conflicting explicit `samples =` errors
- `.parse_sample_qualified_name()` splits on the first `::`
- `.gm_slice_to_samples()` slices joint cmeta / expression / dimreduc / nnnet via the `sample::cell_id` convention
- the five spatial-domain getters take `samples =` as canonical alias for legacy `object =`

Two choices worth keeping visible:

- **paired-vector API dropped** (`sample = c("A","B"), name = c("raw","filtered")`) — the prefix syntax covers it *and* the uniform-handle-across-samples case, which paired vectors can't express without repetition
- **`samples =` rather than always-prefix** — getters without a `name` arg have nowhere clean to put one, and `spat_unit = "B191::cell"` is wrong: spat_unit shouldn't carry sample identity

This is what removed the `:::` reach — per-panel slicing happens at the getter layer, so
`.gm_inject_joint_metadata` and the `GiottoClass:::` call are gone.

Open: output shape for `values = c(...)` mixing gmulti-level and sample-qualified
entries — list keyed by entry, or one combined object? Probably list; combination is
ill-defined in the heterogeneous case.

---

## 8. Joint slots as cache + ground truth — Complete

| trigger | behaviour |
|---|---|
| first access to a federated handle whose raw content is child-only | extract via `@mapping` → cbind/rbind into joint → store → slice and return |
| derived content computed at gmulti scope | lives in the joint slot natively; no child round-trip ever |
| subsequent access | joint slot only; children not consulted |

Implications, all intended:

- children are **frozen native contributions**, not write targets
- no gmulti getter call mutates a child
- `addCellMetadata(mg, ...)` targets joint cmeta only; child edits go through `addCellMetadata(mg@objects[[s]], ...)` — intentional friction
- a saved multi carries joint slots for whatever's been touched; raw child content is the disk-only source for unreached federations

**The trap.** These slots are a *lazily-populated* cache: empty on a fresh multi, and
empty until something explicitly writes one — notably `getExpression()` returns an
assembled result **without** caching it back. So any logic deriving keys or universes
from `names(@expression)` / `names(@cell_metadata)` silently sees nothing. That caused
the narrowing bug in [§17](#17-cell_id--feat_id-narrowing-contract--complete).

Open: what signals eager materialization? Lazy is the default; `materialize(mg, ...)` is
the presumed trigger but isn't formally the contract.

---

## 9. Carry-keys discipline — Partial

Prevents silent mis-labelling of per-cell / per-feature values.

**Why needed:** joint `@cell_metadata` row order and joint `@expression` column order
come from independent assembly paths — `rbindlist` over children vs
`unionParquetExprStore` stacking — so **positional alignment between them is not
guaranteed**. A value produced against one and written back against the other mislabels
rows.

- `addCellMetadata` / `addFeatMetadata` auto-detect a named vector or a table with a key column → key-based merge
- with neither: single `giotto` warns and falls back to positional cbind (back-compat); **`giottoMulti` errors**
- that makes every existing positional producer fail loudly on the next gmulti run

Guards applied at every audited producer, all loud-stop on ID mismatch:

| site | file | approach |
|---|---|---|
| `create_average_DT` / `_detection_DT` | `GiottoClass/R/auxilliary.R` | reorder before the per-cluster mask loop |
| `adjustGiottoMatrix` | `Giotto/R/auxiliary_giotto.R` | reorder before `limma::removeBatchEffect` |
| `runDWLSDeconv` | `Giotto/R/spatial_enrichment.R` | reorder before `enrich_deconvolution` / `spot_deconvolution` |
| `runGiottoHarmony` | `Giotto/R/dimension_reduction.R` | reorder metadata to dim_reduction rownames |
| `findScranMarkers` | `Giotto/R/differential_expression.R` | reorder cmeta rows to expression column order |
| `giottoToAnnData` | `GiottoClass/R/interoperability.R` | capture cell_IDs from `X`; reorder cmeta + spatial_locs |
| `giottoToSpatialExperiment` | `GiottoClass/R/interoperability.R` | capture IDs from first assay; reorder pData + spatial_locs |
| `giottoToAnnDataZarr` | `Giotto/R/interactivity.R` | reorder `obs` to `rownames(X)` |
| `cal_cell_niche_cluster_bin` | `Giotto/R/ONTraC_wrapper.R` | key-based — pair bins with `colnames(expr_values)`, join by cell_ID |

`findScranMarkers` is the clearest case: unaligned `groups` passed to
`scran::findMarkers` silently *inverts* the DE result rather than erroring. MAST and
Gini paths already aligned.

A canonical cross-slot ordering was considered and **rejected as unnecessary** — with
key discipline at the boundary, internal slot order is irrelevant to correctness.

Remaining, lower confidence the bug bites today: `addHMRF_V2`'s commented-out
`by_column = TRUE` branch, `specificCellCellcommunicationScores`' permutation block,
SPARK's `covariates` extraction.

---

## 10. `federatedReadHandle` — Partial

Lets a cross-sample read return per-sample fragments plus a recipe for folding them, so
the consumer decides when and whether to concatenate.

- slots: `substores`, `keys` (authoritative ordering), `output_class`, `combine(substores, keys, ...)`, `meta`
- methods: `length`, `names`, `[[`, `show`, `materialize(as =)`
- gobject-layer analogue of GiottoDisk's `unionParquetGeomStore` — any fragments plus a combine fn, one rung up

**Remaining: nothing consumes it** — zero references outside its own file. Wiring the
federation helpers to return a handle is opt-in. Payoff: for duckdb / sedonadb the
fragments lower to a **single SQL plan** instead of per-substore materialization;
in-memory consumers concat at Arrow level at use time.

---

## 11. `@groups` registered sample handles — Not started

Design settled, nothing written. **Supersedes design doc §7.**

Lets a name refer to several samples at once, usable anywhere a sample name is — so no
new parameter appears anywhere.

```r
gmultiGroup(mg, "tumor_pair") <- c("B191", "B215")   # register
gmultiGroup(mg, "tumor_pair")                         # -> c("B191", "B215")
gmultiGroups(mg)                                      # registered names
gmultiGroup(mg, "tumor_pair") <- NULL                 # drop

getCellMetadata(mg, samples = "tumor_pair")           # just works
spatPlot2D(mg,     samples = "tumor_pair")
```

**Why design doc §7 is unusable:** it stores a `gmultiSpatialAlias` at
`gmulti@spatial_info[["all_cells"]]`. The class has **no `@spatial_info` slot** — that
example errors, as does its `@spatial_info[["atlas_regions"]] <- polys` companion.

**Why not `@objects`:** 64 references assume every entry is a `giotto` — `initialize()`
asserts `types = "giotto"`; `names()` / `length()` would count groups as children;
`.gm_build_cell_idmap`, `.gm_build_feat_idmap`, `.gm_discover_mapping`, and
`.gm_compute_sig` all iterate children; `show()` sums cells and features. Storage stays
separate; only the *namespace* is shared, because only *resolution* is shared.

Resolution — `.gm_resolve_objects()` is nine lines and the sole place a name becomes a
child (`samples =`, `object =`, every getter, the dispatcher all funnel through it):

```r
.gm_resolve_objects <- function(x, object = NULL) {
    if (is.null(object)) return(names(x))
    checkmate::assert_character(object)
    object <- .gm_expand_groups(x, object)   # recursive, then unique()
    bad <- setdiff(object, names(x))
    if (length(bad) > 0L) stop("unknown object(s): ", ...)
    object
}
```

Lookup order: recursively expand against `@groups`, then resolve collected names against
`@objects`; names matching neither are reported.

| decision | settled as |
|---|---|
| nesting | allowed (`all_tumor = c("pair_a", "pair_b")`), recursive with a cycle guard |
| dedup | `unique()`, first-appearance order — overlapping groups don't double-read a child |
| `names(mg)` / `length(mg)` | remain the child list; groups via `gmultiGroups(mg)` |
| stale members | do **not** reset on child removal (destroys user intent); validate at resolution, error naming the missing member. The one registry not following the `@id_sig` reset rule — deliberate |
| value shape | named char vector; `names()` NULL = pure membership, named = per-child content handle. Same shape as a `@mapping` entry |
| collisions | reject group names colliding with a child on `gmultiGroup<-`; reject child names colliding with a group on `[[<-` / `names<-` |

Open before building — hub §5 Q1–Q3.

---

## 12. Combined defaults — Not started

A gmulti's default `spat_unit` / `feat_type` should be the **union of child defaults**,
not one child's.

- heterogeneous federation is supported — different modalities or spat_unit conventions across samples
- picking one child's default to stand for the whole multi silently misroutes calls when others disagree
- don't assume `set_default_spat_unit(mg)` agrees with `set_default_spat_unit(child)`
- fix: compute a combined default at init, propagate mapping-aware to per-child calls
- current paths work only because typical use is homogeneous

---

## 13. gmulti-level spatial content — Not started, unsolved

Polygons drawn in a cross-sample (atlas) frame belong to no child's `@spatial_info`, and
the multi has no spatial slot to hold them.

- `getPolygonInfo(mg)` is purely a per-child fan-out — no storage, no read path
- design doc §1 lists this as one of the five original problems; §7's proposed fix targets a slot that doesn't exist
- **distinct from `@groups`**, which solves *enumeration* ("this handle means these samples' existing content"). This is *concrete content at the multi level*

Options not yet evaluated: a dedicated gmulti-level spatial slot; a `:default:`-keyed
pseudo-child; or declining to support it and requiring atlas geometry to live in a child.
Blocks nothing today, but the atlas story is incomplete without it.

---

## 14. Joint `@spatial_network` — Not started

Move spatial networks from per-child slots to a joint slot. Two reasons:

- **restore child-immutability** — `createSpatialNetwork(mg, ...)` writes into each child, the lone analysis output that does. Expression, cell_metadata, dim_reduction, nn_network, and spatial_enrichment all live on joint slots
- **cross-sample edges have no home** — a Delaunay or kNN over combined-frame locations produces edges *between* samples, and no child's slot can hold those since each knows only its own cell_IDs

Sketch:

- add `@spatial_network`, nested by child name (sample-internal) or combined-space key (cross-sample)
- `createSpatialNetwork(mg, space = "sample_a")` → `mg@spatial_network[["sample_a"]]`, child untouched
- `createSpatialNetwork(mg, space = "combined")` → `sample::id` on both endpoints
- `getSpatialNetwork(mg)` federates the joint slot, not children

Wide accessor fan-out across GiottoClass / GiottoDisk / Giotto. Revisit once the
combined-space story is further along.

---

## 15. `getSpatialLocations` unification — Not started

Return one concatenated `spatLocsObj` with `sample::id` globals instead of a named list
of per-child objects with local IDs.

- matches joint `@cell_metadata`, `@expression`, `spatValues` — one flat object with `sample::id`
- reinforces "joint output is ground truth", keeping consumers off the per-child reach that previously caused an alignment / `list_ID` / leak chain
- substrate is already `data.table` — an `rbindlist` after the per-child fetch, rewriting each `cell_ID` to global form
- makes [§18](#18-additive-sample--view-composition--partial)'s filter redundant for spatial_locs

Consumer ripple:

- every GiottoVisuals caller iterating the list by sample: filter the unified table by prefix, or move to a sample-agnostic path
- `spatPlot2D` / `spatInSituPlotPoints` already accept a single locs object on the single-giotto path, so `.gg_build_panel_child` thins or disappears
- tests comparing `length(out)` (samples) vs `nrow(out[])` (cells) both flip

Related but separate: `getPolygonInfo` on `parquetGeomTileStore` needs cell_ID pushdown
into the parquet query, not in-memory narrowing. `getFeatureInfo` is on the feature axis.

---

## 16. Cross-sample aggregation — Not started

The fan-out-then-reduce dispatch shape: computing across samples in a shared frame —
region-based composition cohort-wide, atlas-frame aggregation.

Why it matters: spaces can *position* samples relative to one another, but nothing
computes across them, so the atlas use case isn't fully deliverable.

**Frame this as a substrate-readiness gap, not a design tradeoff.** The active work
pushes cross-sample scans down to the database substrate — `parquetExprStore`, sedona
predicates, the `parquetCoordinator` plan. Once that lands, atlas-scale aggregation is
SQL-level (group-by plus a spatial predicate over shared parquet) and the gobject-layer
dispatch shape stops mattering for performance. The orthogonal-axes design sits on top of
a shared substrate rather than competing with one, so the gap closes without the data
model changing.

Structurally different from the rest of this project; needs its own design pass.

---

# Narrowing

## 17. `@cell_ID` / `@feat_ID` narrowing contract — Complete

Records which cells and features are in scope at the multi level, without mutating
children and without losing the record of what exists.

**Contract:** `@cell_ID` / `@feat_ID` are the active narrowing, nested by spat_unit /
feat_type (mirroring the single-giotto convention). `NULL` = unfiltered. `@id_map` is the
registry and is **never** narrowed, so a narrowing can be widened or dropped without
having lost the population.

Rules:

- **children are never mutated** — narrowing lives at the multi level only. Besides child-immutability, this avoids breaking `unionParquetExprStore`'s "ops-clean substores" invariant that per-child `[`-subset state would violate, and keeps narrowing state from scattering across N children
- **unknown IDs are silently ignored**, intersected away — matching `subsetGiotto` on a single giotto
- **value semantics** — `subset()` returns a new multi; the original is untouched
- **structural change resets narrowing** — `initialize()` nulls both slots when `@id_sig` differs, because they record survivors of a filter over a *specific population*. `[[<-` defaults to `initialize = TRUE`; bulk callers pass `FALSE` and re-initialize once. The alternative — a new child silently inheriting the parent's narrowing — would report a filter that child never went through

How:

- `.subset_giotto` sets `is_multi` once at entry and guards the five per-child slot blocks with `!is_multi` (`.subset_spatial_locations`, `.subset_spatial_network`, the `@spatial_info` loop, the `@feat_info` block, the overlap cleanup) — these previously crashed on a multi
- joint-slot helpers run unchanged; joint slots have the same shape as on a single giotto
- survivors recorded on `@cell_ID` / `@feat_ID`, intersected with prior content
- `":all:"` keys come from `.gm_narrowing_keys()` — `@mapping` as the authoritative universe, unioning joint-slot and `@cell_ID` / `@feat_ID` names as a legacy fallback
- `spatIDs` / `featIDs` intersect `@id_map` with the narrowing, with optional `spat_unit` / `feat_type` for one universe's survivors
- `show()`'s view line reports visible-over-total from these, so it reflects real narrowing rather than registry size

> **Retired bug, worth keeping recorded.** `@cell_ID` was never written, because the
> `":all:"` keys were derived from `names(@expression)` / `names(@cell_metadata)` — the
> joint slots, which are a lazily-populated cache and empty on a fresh multi. `su_keys`
> was `character(0)`, so the recording loop never ran. That is the entire explanation for
> the long-standing "any edit to `subset(giottoMulti)` silently no-ops" mystery: the
> readers were correct and the writer never fired, so every reader-side experiment looked
> broken. Since `getExpression()` doesn't cache back into `@expression` either, this was
> never merely a first-access window.

**Landing note.** `origin/gmulti` predates this contract and narrows `@id_map`. Landing
that slice unchanged would publish the old contract and revoke it one PR later — so this
belongs in PR 1, not PR 3.

Remaining checks:

- `filterGiotto` should record narrowing identically (same `.subset_giotto` route), but only `subset()` / `subsetGiotto()` were exercised and `filterGiotto` is the path users hit
- `spatIDs(mg)` unions across spat_units when `spat_unit = NULL`, which with per-universe narrowing can over-report a cell filtered out of one universe but present in another

---

## 18. Additive sample × view composition — Partial

The three selectors compose rather than override.

| knob | mechanism | selects |
|---|---|---|
| sample | `samples =` / legacy `object =` | which children are read |
| view | `@cell_ID` / `@feat_ID` | which cells / features survive |
| space | `@spaces[[name]]` | which coordinate frame |

- a sample-scoped read returns that child's content with the active view narrowing on top
- children untouched — `mg@objects$a` / `mg[["a"]]` always return the raw child (the documented escape hatch)
- `.gm_resolve_per_child_arg()` reconciles `object =` and `samples =` (conflicts error), delegating to `.gm_resolve_objects()`
- `.gm_narrow_child_outputs()` applies the `@cell_ID` allow-list to the assembled per-child list, translating globals back to local IDs by prefix; returns the list untouched when no narrowing is active

**Remaining — the feature axis doesn't compose with points.**

- `.gm_narrow_child_outputs` is wired into `getSpatialLocations`, `getSpatialNetwork`, `getPolygonInfo` — **not** `getFeatureInfo`
- `.gm_subobj_filter_by_local_ids` handles `spatLocsObj`, `spatialNetworkObj`, `giottoPolygon` — **not** `giottoPoints`
- so `subset(mg, features = ...)` then a points read returns unnarrowed points
- `getGiottoImage` correctly doesn't narrow — no cell axis

---

# Consumers

## 19. GiottoVisuals dispatcher — Complete

Renders a multi as a panel per sample, honouring view, space, and sample selection.

- `.gg_multi_dispatch_spatial` takes `samples =` as canonical
- `.resolve_samples(gobject, samples, space, child_names)` auto-injects from `names(space@samples)` when `samples = NULL` and a defined space is given; errors on samples absent from `@objects` or outside the space's participation set
- `.gg_build_panel_child` builds each panel's child by projecting joint-only `@cell_metadata` columns through the access layer
- the auto-injection convention lives here, consumer-side — the recipe classes stay ignorant of who reads them

What it replaced: `space =` was typed as a character vector of sample names and actively
rejected real defined-space names with "not yet supported"; and the dispatcher reached
into `GiottoClass:::.gm_inject_joint_metadata` to build a scratch child per panel.

Cross-sample defined-space panels are now *expressible* (`space = "atlas"` derives its
own samples); rendering N samples into one viewport remains
[§16](#16-cross-sample-aggregation--not-started).

---

## 20. Per-panel sizing — Not started

`cowplot::plot_grid` gives every panel an equal grid cell regardless of the sample's
extent.

- `coord_fixed(1)` is right *within* each panel, so data aspect is preserved internally
- but scale *across* panels is wrong — wide- and narrow-extent samples render at the same width
- `rel_widths` / `rel_heights` are per-row/col, not per-cell
- options: restrict to single-row layouts with a width vector, or switch to `patchwork::wrap_plots()` (per-panel widths/heights, including MxN)

Prefer patchwork — heterogeneous extents are the norm at atlas scale, so uniform cells
are wrong by default. Until then, `cow_rel_w` / `cow_rel_h` can be passed manually.

---

## 21. GiottoLens consumption — Separate repo

On GiottoLens `feature/gmulti-spaces-views`, with its own design doc. Recorded here only
where it constrains this project.

- treats single-giotto as a gmulti with one child keyed `:default:` — no viewer-side branching on input class. Matches the `giottoSpace()@samples` sentinel, and is why hub §5 Q4 exists
- **not a consumer of server-side spaces** — applies per-sample 3×3 affines client-side via deck.gl `modelMatrix`, no tile reload. So `giottoSpace` serves R-side plotting and analysis; worth remembering when weighing [view/space §7](IMPLEMENTATION_viewspace.md#7-threading-through-generics--complete-scope-under-review)
- views are pre-rendered R-side: the viewer registers a resolved `cell_ids` payload and semi-joins it into live queries; the R-side resolver handles predicate-frame projection so the viewer stays frame-naive

Decided there, don't re-litigate: spaces and views stay orthogonal (no `view@space`
auto-flip in the viewer); joint dim reductions stored and queried at gmulti level;
joint-space rendering of N samples in one viewport is a v2 extension.

---

*Part of the [gmulti implementation plan](IMPLEMENTATION_gmulti.md). Created 2026-08-11.*

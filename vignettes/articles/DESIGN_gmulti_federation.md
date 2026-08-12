# Design — gmulti federation, sample addressing, `@mapping`

**Status**: design draft. Captures decisions from the 2026-06-12/13/14 design discussion. Implementation phased; nothing in here has shipped yet on this branch — the branch exists to hold the doc.

**Scope**: how the `giottoMulti` class federates child gobjects across spatial units, feature types, and samples; how callers address content; how the dispatcher and access layer cooperate; how this composes with the existing `giottoView` / `giottoSpace` recipe system.

**Out of scope** (deferred to follow-on design): composable view algebra (`view1 + view2`), cross-sample aggregation primitives, GiottoLens-side consumption of `@mapping`.

---

## 1. The design problem

Today's `giottoMulti` is incomplete in five ways:

1. **Implicit single-spat_unit / single-feat_type at the joint level.** Joint slots (`@cell_metadata`, `@expression`, `@dim_reduction`) silently flatten to one spat_unit and one feat_type because there's no declaration of which child names federate up. This backslides on Giotto's multi-modality / multi-scale value proposition.

2. **Dispatcher abuses `space` as a sample selector.** [`.gg_multi_dispatch_spatial`](../../../GiottoVisuals/R/gmulti.R) takes a `space` arg typed as a character vector of sample names. Lines 56–69 of that file actively reject *real* defined-space names with a "not yet supported" error — the parameter is doing the wrong job under the wrong name.

3. **`GiottoVisuals` reaches into GiottoClass internals via `:::`.** [GiottoVisuals/R/gmulti.R:99](../../../GiottoVisuals/R/gmulti.R#L99) calls `GiottoClass:::.gm_inject_joint_metadata` to project joint cmeta into a scratch child gobject before each per-panel plot. Workaround for getters that aren't sample-aware.

4. **No home for atlas-frame shared content.** Polygons drawn in an atlas (cross-sample) coordinate frame don't belong in any `@objects[child]@spatial_info`. There's no slot at the gmulti level that holds them.

5. **No mechanism for per-child name reconciliation.** Different pipelines name the same modality differently (`"rna"` / `"transcripts"` / `"gene_expression"`). Joint federation has no way to declare "these three things are the same modality" — so the user is stuck either renaming on disk or running a coercion step.

These five are interlocking: (1), (3), (4), and (5) all point at a missing federation layer, and (2) is the consequence of trying to do per-sample dispatch without that federation layer to lean on.

---

## 2. Axis decomposition: view, space, samples

Three orthogonal call-time concerns:

| param | role | shape | source |
|---|---|---|---|
| `view` | subset/filter recipe | character (named slot) or `giottoView` object | `gobject@view[[name]]` |
| `space` | coordinate-frame recipe | character (named slot) | `gobject@spaces[[name]]` |
| `samples` | ad-hoc multi-sample selection | character vector (or `":all:"` sentinel) | call-time only, not slotted |

`samples` is a **call-time parameter, not a slotted artifact**. No new slot. The character vector composes into the view pipeline as `selectSamples(...)` — exactly the existing view step — when the resolver is invoked. From the outside, `samples = c("A","B")` looks atomic; underneath it routes through the same mechanism that handles `view = "tumor_focus"` containing a `selectSamples` step.

### Why the axes are orthogonal

The alternative is folding sample identity into coordinate-frame membership, so that
"which sample" is answered by naming conventions or by per-sample frames rather than by
a structural axis. That was considered and rejected.

Most spatial-omics work is sample-keyed: QC, normalization, clustering, integration,
faceted visualization, and cohort statistics are all per-sample operations, and each of
them needs to name a sample directly. Cross-sample aggregation in a shared frame is the
one workflow that benefits from the merged model, and it is real but narrow. Giotto
keeps `@objects` as the first-class sample axis and adds the aggregation infrastructure
as a focused follow-on (separate design pass) rather than reshaping the data model
around the narrower case.

### Auto-injection convention (carries over from `giottoView` design)

When `space = "atlas"` is passed to a consumer, the consumer looks up `names(giottoSpace(g, "atlas")@samples)` and synthesises an ephemeral `selectSamples(...)` view from those keys. End users write `plot(g, space = "atlas")` and the sample narrowing happens for free.

This means **spaces still carry their participating samples via `@samples`** (existing design — `vignettes/view_and_space.Rmd`). They just default the `samples` arg; `samples` is the explicit override.

### Resolution rules (composition)

| `samples` | `space` | result |
|---|---|---|
| `NULL` | `NULL` | all `@objects`, native frames |
| `NULL` | `"atlas"` | `samples` auto-derives from `atlas@samples` keys; atlas frame |
| `c("A","B")` | `NULL` | A and B in their native frames |
| `"A"` | `"atlas"` | atlas frame, restricted to A (must be in `atlas@samples`; error if not) |
| with `view = "tumor_focus"` containing `selectSamples` | any | view's `selectSamples` intersects with explicit `samples` |

`space` rejects samples not in `@samples` (error rather than silent fallback). Preserves the "spaces enumerate their participating samples" contract.

### Why `samples` isn't promoted to a third recipe slot

`samples` is **ad-hoc** in a way `view` and `space` are not. `view` and `space` are nouns users build, slot, name, reference repeatedly. `samples` is a verb at the call site — "show me these samples now." Forcing it into the noun vocabulary either invents synthetic slotted recipes (`giottoSamples("just_A_B")`) for trivial selections, or pushes users to write `view = selectSamples(...)` every time and learn `selectSamples` as a primitive.

The closest analogue is `subset(g, predicate)`: it's sugar over `view = viewFilter(predicate)` and we accept the overlap because the call-site ergonomics matter. `samples` is sugar over `selectSamples` for the same reason.

---

## 3. `@mapping` slot — federation declaration

### Shape

```r
gmulti@mapping <- list(
    spat_unit = list(
        cell = c(B191 = "cell", B215 = "cell", B651 = "cell"),
        nucleus = c(B191 = "nucleus_v1", B215 = "nuc"),       # partial coverage, naming varies
        tile = c(B191 = "tile_100um")                          # only one sample participates
    ),
    feat_type = list(
        rna = c(B191 = "rna", B215 = "transcripts"),           # per-child name variation
        protein = c(B651 = "protein")                          # one sample, fine
    )
)
```

Two named lists (`spat_unit`, `feat_type`), each holding entries keyed by the gmulti-level handle. Each entry is a per-sample-named character vector mapping gmulti-level name → child-level name in that sample. Partial coverage is fine — a sample missing the modality is absent from the vector.

### Why this shape

- Captures **which children participate** in each modality / spatial unit.
- Captures **per-child name reconciliation** at the declaration layer, not via on-disk renaming or coercion steps.
- Captures **the set of gmulti-level handles** users address.
- Composes with existing slot model: joint `@cell_metadata` / `@expression` / `@dim_reduction` become keyed by (spat_unit, feat_type) drawn from `@mapping`'s top-level names; federation looks up which child name to pull from each participating sample.

The shape is a familiar one — declarations that link logical groupings to concrete addresses — but lifted out of per-element metadata into a first-class gmulti slot, so the declaration is addressable and mutable rather than implied by convention.

### Auto-init from children

At gmulti construction, `@mapping` is auto-discovered:

1. For every spat_unit name appearing in any child's `@cell_ID` keys → create an entry.
2. For every feat_type name appearing in any child's `@feat_ID` keys → create an entry.
3. Populate per-sample char vectors with the symmetric trivial mapping (`B191 = "cell"` if B191 has `"cell"`).

This handles ~80% of cases with no user intervention. Customisation (renaming, partial coverage, multiple gmulti-level handles drawing from the same child name) is explicit post-init via a setter.

### Cell_ID universes — already supported, just under-used

The infrastructure for **per-spat_unit cell_ID universes** already exists at the gmulti level (mirroring the child structure — `@cell_ID` is a list keyed by spat_unit). Today it's effectively single-universe because the implicit federation assumes one spat_unit.

`@mapping` doesn't change the cell_ID format. `sample::cell_id` namespacing continues to work because each spat_unit lives in a separate universe — a "cell" with id 42 in B191 and a "nucleus" with id 42 in B191 don't collide because they're in different universes.

**Implication**: `@mapping` is more declaration-of-intent than data-model change. Implementation lift is smaller than the user-facing impact suggests.

### Mutation and invalidation

`@mapping` is **mutable post-init**, scoped per-universe. A `gmultiMapping<-` setter handles:

| operation | invalidation |
|---|---|
| Add new spat_unit / feat_type entry | None — creates a new universe slot |
| Change which child names federate under an existing entry | Joint state for that universe only (cmeta + expression + dim_reduc for that spat_unit / feat_type) |
| Remove an entry | Drops that universe's joint state |

Per-universe scoping naturally bounds the blast radius of any single mapping edit. Standard "metadata edit + invalidation" pattern — same shape Giotto already uses elsewhere (e.g. instruction edits that invalidate cached state).

---

## 4. Access layer — `sample =` arg + `"sample::name"` prefix

### Canonical mechanism

`sample =` arg on every getter. Takes a single sample name or `NULL`. When non-`NULL`, the getter slices joint content (or the federated child contribution) to just that sample. The dispatcher uses `sample =` uniformly for per-panel slicing.

### Convenience parser

`"sample::name"` prefix in `name` params (where the getter has one). Each `name` entry is independently parsed; the prefix is decomposed into the equivalent `(sample, name)` slice. Vectorizable:

```r
getExpression(g, name = c("raw", "filtered"))                # two handles, all participating samples
getExpression(g, name = "B191::raw")                          # one sample, one handle
getExpression(g, name = c("B191::raw", "B215::filtered"))     # per-sample handle variation
getExpression(g, name = c("raw", "B191::experimental_v2"))    # mix gmulti-level + sample-qualified
```

### What this replaces

**Drop the paired-vector API** (`sample = c("A","B"), name = c("raw","filtered")`). The vectorized prefix syntax covers every paired-vector case plus the "uniform handle across samples" case (which paired vectors can't express without redundant repetition).

### Resolution precedence

When both joint and child-level content exist:

- **Joint is preferred** (gmulti as source of truth — see §5).
- **`"sample::name"`** is an explicit child-level slice request — bypasses joint lookup, goes directly to the child's contribution via the federation. (If joint exists, the slice is taken from joint, since joint federated from the child in the first place. If joint hasn't been materialized yet, federation happens lazily.)

### Federation through `@mapping`

```r
getExpression(g, sample = "B191", spat_unit = "cell", feat_type = "rna", name = "raw")
# Resolution:
#   1. @mapping[["feat_type"]][["rna"]][["B191"]] → "transcripts"
#   2. If joint @expression[["cell"]][["rna"]][["raw"]] exists → slice to B191
#   3. Else: pull from child B191's @expression[["cell"]][["transcripts"]][["raw"]],
#      federate into joint, return B191 slice.
```

The getter consults `@mapping` to know which child-level name to pull. Users call by the gmulti-level handle and trust the mapping to resolve.

### Why `sample =` instead of always-prefix

For getters that don't have a `name` arg (`getCellMetadata`, `getSpatialLocations`), there's nowhere clean to put a prefix. `spat_unit = "B191::cell"` is wrong — spat_unit conceptually shouldn't carry sample identity. So `sample =` is the universal mechanism, and `"sample::name"` is the ergonomic shortcut where it composes naturally with an existing `name` field.

---

## 5. Joint slots as cache + ground truth

Joint slots (`gmulti@expression`, `gmulti@cell_metadata`, `gmulti@dim_reduction`) are **both cache AND source of truth** post-first-access.

### Lifecycle

| trigger | behavior |
|---|---|
| First access to federated handle (raw content existing only at child level) | Extract child content via `@mapping` → cbind / rbind into joint → store in `gmulti@<slot>` → slice and return |
| Derived content (normalized expression, dim reduction, etc.) computed at gmulti scope | Lives in joint slot natively — no child round-trip ever needed |
| Subsequent access | Hits joint slot directly; child slots not consulted |

### Implications

- **Children become frozen native contributions**, not write targets. The carry-keys discipline (memory: `project_gmulti_carry_keys_discipline.md`) already pushes this direction; this formalises it.
- **No mutation of child gobjects by gmulti getter calls.** Mutation lives at the joint slot, where it belongs.
- **Saved gmulti has joint slots populated** for whatever's been touched; raw child content is the disk-only source for unreached federations.
- **`addCellMetadata(g, ...)` etc. target joint cmeta only.** Child-level metadata edits go through `addCellMetadata(g@objects[[s]], ...)` on the child directly (intentional friction — child edits are private, not routine).

### Why this matters for the `:::` problem

With access-layer slicing via `sample = ` or `"sample::name"` prefix, the dispatcher's "inject joint cmeta into a scratch child before plotting" step disappears. The plot helper calls `getCellMetadata(gmulti, sample = "B191", ...)` and gets the right slice — no scratch child, no mutation, no internal helper reach.

`.gm_inject_joint_metadata` becomes unnecessary and can be deleted (along with the `:::` call in GiottoVisuals).

---

## 6. Federated-read wrapper class

For getters that pull from multiple samples (e.g. `getExpression(g, sample = NULL)` returning federated content across all samples), the return value is a **federated read handle** rather than a forced concat.

### Shape (v1)

```r
setClass("federatedReadHandle",
    slots = list(
        substores = "list",     # per-sample list of substores / matrices / arrow queries
        keys = "character",     # sample names matching substores
        output_class = "character",  # what to materialize to ("matrix", "data.table", "arrow")
        combine = "function"    # how to fold substores into one output_class instance
    )
)
```

External API: behaves like the requested output class when consumed. Internal: stays a list-of-substores until consumer triggers materialization.

### Why a wrapper class

- Defers the **concat vs keep-as-list** decision to the consumer.
- For duckdb / sedonadb queries, the list-of-substores **lowers to a single SQL plan** when the consumer is a query operation — no per-substore arrow materialization step.
- For arrow / in-memory consumers, the wrapper materializes to a combined object at use time.
- Mirrors the existing `unionParquetGeomStore` pattern in GiottoDisk — federated wrapper across multiple parquet substores presented as one logical entity.

### Deferred

Implementing the federated-read class as a real S4 class is optional for the initial rollout. v1 can return `list_of_substores` directly with a documented convention; the class wrapper is the upgrade path when a specific query benefits from lazy lowering.

---

## 7. Pointer-class for `@spatial_info` (orthogonal to `@mapping`)

`@mapping` handles federation at the **indexing axes** (spat_unit, feat_type). The pointer-class handles **content-level cross-sample groupings within `@spatial_info`** that don't follow the federation pattern.

### Use case

```r
# Concrete frame-anchored polygons (no federation needed)
gmulti@spatial_info[["atlas_regions"]] <- atlas_polygons_giottoPolygon

# Cross-sample alias (federation by enumeration, not by @mapping)
gmulti@spatial_info[["all_cells"]] <- new("gmultiSpatialAlias",
    refs = list(B191 = "cell", B215 = "cell", B651 = "cell"),
    space = "atlas"   # or NULL for native-frame mixed
)
```

The alias is a pointer that resolves to per-child polygons via the named refs. Distinct from `@mapping` because:

- `@mapping` declares "this gmulti-level handle federates with these child-level handles" — declared at the indexing axes.
- Pointer-class declares "this gmulti-level handle aliases this specific set of per-child content within `@spatial_info`" — declared at the content layer for non-standard groupings.

Both can coexist. Most users hit `@mapping`. Pointer-class is for the long tail (e.g. "tumor_focus_polys = B191's tumor_roi + B215's epithelium_roi" — content that doesn't naturally federate under a spat_unit name).

### Precedent

`unionParquetGeomStore` in GiottoDisk is the lower-level analogue (federates parquet substores). `gmultiSpatialAlias` is the gobject-layer equivalent for `@spatial_info`.

### Scope

Pointer-class is **deferred** in implementation order — only landed when there's a concrete workflow that needs it. `@mapping` covers the standard federation case.

---

## 8. Dispatcher cleanup

### Today

[`.gg_multi_dispatch_spatial`](../../../GiottoVisuals/R/gmulti.R) (GiottoVisuals):

- Takes `space` arg typed as character vector of sample names (semantic misnomer).
- Materializes view at gmulti level, iterates per child.
- Calls `GiottoClass:::.gm_inject_joint_metadata` to project joint cmeta into a scratch child per panel.
- Throws "not yet supported" if user passes a real defined-space name.

### After this design

- Takes `samples =` arg (canonical name).
- `space =` accepts only real defined-space names from `@spaces`.
- Auto-injection: if `space = "atlas"` and `samples = NULL`, derive `samples` from `atlas@samples`.
- No scratch-child injection — per-panel slicing happens at getter layer via `sample =` arg.
- No `:::` reach into GiottoClass internals.

### Dispatcher reduces to:

```r
.gg_multi_dispatch_spatial <- function(plot_fn, named, dots, gobject, view, space, samples) {
    samples <- .resolve_samples(gobject, samples, space)  # auto-inject from space if needed
    if (!is.null(view)) {
        gobject <- materialize(gobject, view, space = NULL)
    }
    plots <- lapply(samples, function(s) {
        a <- named
        a$gobject <- gobject  # gmulti, not a child — getters slice via sample =
        a$sample <- s
        a$samples <- NULL  # consumed
        a$view <- NULL     # already applied
        do.call(plot_fn, c(a, dots))
    })
    composite <- if (length(plots) == 1L) plots[[1L]] else cowplot::plot_grid(...)
    plot_output_handler(...)
}
```

The plot fn receives the gmulti + a single `sample` and uses ordinary getters. Sample identity flows through the access layer, not through scratch object construction.

---

## 9. Implementation phases

In dependency order. Each phase is independently shippable and useful.

### Phase 1 — `@mapping` slot + auto-discovery

- Add `@mapping` slot to `giottoMulti` class definition.
- Auto-populate at construction via `.gm_discover_mapping(children)`.
- `gmultiMapping()` getter + `gmultiMapping<-` setter (setter handles invalidation).
- Validation: per-sample char vector entries must reference real spat_units / feat_types in their named samples.

**Touches**: `R/gmulti.R` (class def), `R/methods-show.R` (display), `R/create.R` (init).

### Phase 2 — Federation in joint slot population consults `@mapping`

- Update internal joint cmeta / expression / dim_reduction federation to read from `@mapping` rather than assuming child-name == gmulti-name.
- Per-universe federation: joint slot key (spat_unit, feat_type) drives `@mapping` lookup.

**Touches**: wherever joint slot lazy-materialization lives (likely `R/gmulti.R`, `R/slot_accessors.R`).

### Phase 3 — Getter `sample =` arg + `"sample::name"` parser

- Add `sample = NULL` to every gmulti-aware getter signature.
- Implement `.parse_sample_qualified_name(name)` helper.
- Getters that have a `name` arg use the parser; those that don't use `sample =` directly.
- Slicing logic: lookup → federation via `@mapping` if joint absent → slice to sample → return.

**Touches**: `R/slot_accessors.R` for each affected getter (getCellMetadata, getExpression, getSpatialLocations, getPolygonInfo, getDimReduction, getSpatialEnrichment, etc.).

### Phase 4 — Dispatcher `samples =` + drop `:::`

- Rename `space` → `samples` in `.gg_multi_dispatch_spatial`.
- Implement `.resolve_samples(gobject, samples, space)` (auto-inject from space).
- Per-panel call uses `sample =` arg instead of scratch-child injection.
- Delete `.gm_inject_joint_metadata` and the `:::` reference in GiottoVisuals.

**Touches**: `GiottoVisuals/R/gmulti.R` (dispatcher), all spatial plot fns that accept `space =` for sample selection (rename to `samples =`).

### Phase 5 — Federated-read wrapper class (when needed)

- Implement `federatedReadHandle` S4 class.
- Update getters that return cross-sample federations to wrap their list-of-substores in this class.
- Add `combine()` / materialize-on-demand semantics.

**Touches**: new `R/class-federatedReadHandle.R`; consumer-side opt-in.

### Phase 6 — Pointer-class for `@spatial_info` (when needed)

- Implement `gmultiSpatialAlias` S4 class.
- Update `getPolygonInfo` / `getPointsInfo` to resolve aliases.

**Touches**: new `R/class-gmultiSpatialAlias.R`; `R/slot_accessors.R` getters.

---

## 10. What's NOT in this design

Explicitly out of scope, to avoid scope creep:

- **Composable views** (`view1 + view2`). Real demand, but composition semantics (intersect filters, union crops, predicate-frame conflicts) deserve their own design pass. Currently errors with "not yet implemented" — leave it.
- **Merging samples into coordinate systems** rather than keeping a structural sample axis. Considered and rejected — see §2.
- **Cross-sample aggregation infrastructure** (the "fan-out + reduce" dispatch shape). Adjacent to this design but structurally different — separate document needed.
- **GiottoLens consumption of `@mapping`**. The viewer needs to learn how to read `@mapping`-aware federations; that's a downstream package change.
- **Migration helper for existing gmulti objects**. If `@mapping` lands as a required slot, existing saved gmulti's need either auto-population on load or an explicit migration call. Decide closer to landing.
- **`@h5_file` slot interaction**. Memory notes the slot is deprecated and audit-ready for removal; this design doesn't depend on it.

---

## 11. Open questions to resolve at implementation time

1. **`gmultiMapping<-` setter semantics for changing an existing entry's per-sample names**: warn + drop dependent joint state, or block until user explicitly opts in?
2. **Lazy federation vs eager materialization on first joint slot access**: lazy by default (matches the carry-keys discipline + minimizes startup cost). What signals eager materialization (`materialize(gmulti, ...)`)?
3. **Auto-inject from space**: error or silent fallback when `samples` is given and doesn't intersect `space@samples`? Lean error (per §2 resolution rules). Confirm.
4. **`name = c(...)` output shape** when entries mix gmulti-level handles and sample-qualified ones: list keyed by entry, or one combined object when reasonably possible? Probably list — combination is hard to define in the heterogeneous case.
5. **Where does `:default:` sentinel live?** GiottoLens convention treats single-giotto as `giottoMulti` with one child keyed `":default:"`. Does the gmulti class itself need to participate, or is `:default:` strictly an export-side convention?
6. **Should `samples = ":all:"` be the explicit default value, or is `samples = NULL` (with `:all:` implied by NULL) the cleaner API?** Affects function signature noise. Lean toward NULL-as-default, `":all:"` as documented sentinel for users who want to be explicit.

---

## 12. Related memory entries

- `project_giottoview_design_shape.md` — view/space split, predicate-frame vs output-frame contract, auto-injection convention. Foundation this builds on.
- `project_giottolens_gmulti_spaces_views.md` — GiottoLens side, `:default:` sample sentinel pattern, three orthogonal capabilities (sample / space / view).
- `project_gmulti_combined_defaults.md` — heterogeneous federation: gmulti defaults should union child defaults. `@mapping` operationalizes this.
- `project_gmulti_carry_keys_discipline.md` — joint level as source of truth; this design extends that to access-layer slicing.
- `project_gmulti_joint_spatial_network.md` — spatial_network slot lift from per-child to joint (pending). Adjacent to `@mapping` since networks would also federate.
- `feedback_giottodisk_output_no_materialize.md` — federated-read wrapper philosophy (don't force materialization at getter boundaries).

## 13. Related code references

- `R/gmulti.R` — `giottoMulti` class def, dispatcher hooks, `.gm_inject_joint_metadata` (to be deleted in phase 4).
- `R/methods-view.R` — view / space / selectSamples machinery (foundation).
- `R/methods-resolver.R` — `.surviving_cell_ids`, `.cached_surviving_cell_ids` (resolver this design layers on).
- `../GiottoVisuals/R/gmulti.R` — dispatcher (phase 4 touches this).
- `../GiottoDisk-giotto-view/R/methods-storeRead.R` — `unionParquetGeomStore` (federated-read precedent).

---

*Last updated: 2026-06-14.*

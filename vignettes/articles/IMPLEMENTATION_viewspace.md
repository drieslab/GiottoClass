# Implementation — view / space

`giottoView` and `giottoSpace`: two non-destructive recipe layers over any gobject. A
*view* narrows which cells and features are in scope; a *space* defines an alternate
coordinate frame. Both dispatch on `gAny`, so both work on a single `giotto` as well
as a `giottoMulti` — **this is its own subsystem, not part of gmulti.**

Each section is one implementation: what it does, how, and what remains. Status is in
the header.

Where this meets gmulti — sample-keyed spaces, `selectSamples`, groups as space keys,
and the multi-panel dispatcher — see the
[gmulti implementation plan](IMPLEMENTATION_gmulti.md).

---

# The two recipe layers

## 1. Recipe classes + resolver engine — Complete

Two standalone, composable, slottable recipe objects plus one engine that resolves them
against a gobject.

- **view** — read-only narrowing: which cells and features are in scope
- **space** — coordinate frame: where the data sits. *Not* read-only; running an analysis in a non-native frame is fine, the coordinates just differ, and mutations still target the underlying data in its native frame

**Class layout.**

```
viewStep (virtual)
├── viewFilter         predicate, env, scope_args
├── viewCrop           extent — meaningful in @space's frame
└── viewSampleSelect   samples character vector

spaceTransform         op, args

giottoView             steps, space, name, source, misc
giottoSpace            samples (named list of step lists), name, source, misc
```

- neither class mutates a gobject — `materialize()` returns a new one, or a recipe is passed per-call to a getter. Detaching is not passing it
- slotted by name into `gobject@view` / `gobject@spaces`, so they persist with the object
- **`gobject@spaces` *is* the multi-space registry** — several frames falls out of slotting several spaces, no separate mechanism
- **read-only enforced by signature** — functions taking `view =` return a result; mutating functions don't take it. The contract is the argument list, not a runtime check
- **lazy** on a disk-backed gobject: predicates and crops push into the backend query plan; the pull happens when a downstream call collects
- files: `classes-view.R` (194), `classes-space.R` (147), `methods-view.R` (706), `methods-space.R` (354), resolver `classes-resolver.R` (95) + `methods-resolver.R` (629)

**Why two classes rather than one.** An earlier unified `giottoView` carried transforms,
filters, crops, and sample selection together, leaving four things unresolved:

- `calculateOverlap(v)` couldn't tell whether a view's transforms or its filters were the meaningful part for the artifact
- crop extents were ambiguous — "in what frame?"
- the read-only contract applied uniformly, even to transforms-only views with no need of it
- "view" did two jobs in the vocabulary at once: selection and positioning

**Why transforms are centralized rather than owned per subobject.** Each slotted
space is already an alternate frame, so multi-space works without giving every
subobject its own transform state. The alternative would mean extending
`giottoAffineImage`'s affine slot to every spatial subobject *and* keying multiple
affines by frame name. Centralizing matches Giotto's sample-level alignment workflow
without that cost, and can be extended later if multi-modal microscopy pushes for it.

---

## 2. View steps — Complete

Records the narrowing operations a view can carry.

| step | records |
|---|---|
| `viewFilter` | a metadata predicate (what `subset()` sugars over) |
| `viewCrop` | a spatial region — extent, WKT, or an arbitrary polygon |
| `viewSampleSelect` | which children participate (gmulti only) |

- steps are pure data appended in order — no closures — so a view survives serialization and travels to parallel workers
- the resolver folds them into a surviving-ID set, cached via `.surviving_cell_ids` / `.cached_surviving_cell_ids` so downstream per-child calls reuse one computation
- **cell-keyed propagation is automatic** — a `subset()` predicate evaluates against `spatValues(g, feats = <names>)` at resolution, and surviving cell_IDs reach every cell-keyed slot through the existing relational structure, no per-slot wiring
- **`selectSamples()` resolves first** when a view is consumed, before the other steps

`selectSamples` is the view-layer form of sample selection;
[federation §11](IMPLEMENTATION_gmulti_federation.md#11-groups-registered-sample-handles--not-started)'s
`@groups` is the registered form, and `samples =` is the ad-hoc form. All three
narrow the same axis.

**Why `samples` is an argument and not a third recipe slot.** Views and spaces are
nouns users build, name, and reference repeatedly. `samples` is a verb at the call
site — "show me these samples now." Forcing it into the noun vocabulary would mean
inventing synthetic slotted recipes for trivial selections, or making users write
`view = selectSamples(...)` every time. The precedent is `subset(g, predicate)`,
which sugars over `viewFilter` for the same ergonomic reason. `@groups` answers the
case where a selection *stops* being ad-hoc because it's reused — completing this
reasoning rather than contradicting it.

**Remaining — the crop region is untyped at the substrate boundary.** `viewCrop`
serializes its region as either `numeric(4)` (an AABB) or `character` (WKT).
`.materialize_crop_region()` (`methods-resolver.R:201`, four call sites) deserializes
WKT into a `SpatVector` but returns the numeric unchanged, so every substrate
normalizes it independently: `.cells_in_region` reads it as an AABB directly, terra's
`crop`/`ext`/`intersect` apply the terra `(xmin, xmax, ymin, ymax)` convention, and
`parquetGeomTileStore` calls `terra::as.polygons(terra::ext(y))` for affine
back-projection.

The convention is therefore implicit and assumed in several places at once. sf and
most GIS tooling use `(xmin, ymin, xmax, ymax)`, so any sedona/duckdb-spatial path has
to remember to reorder, and anyone reading a saved recipe has to know which order it
is. The fix is to convert numeric → `terra::ext()` or a polygon `SpatVector` inside
`.materialize_crop_region`, so substrates always receive a typed object and the
convention lives in one place — costing a few microseconds per resolve.

Not urgent: the recipe is correct and serializable, and the AABB path is the fastest.
Do it when a non-terra substrate needs the other axis ordering (otherwise the
convention gets duplicated), or when someone hits an off-by-axis bug from misreading
the 4-vector.

---

## 3. Predicate frame vs output frame — Complete

Lets a spatial predicate be evaluated in one coordinate frame while the result is
returned in another.

Why it matters: an ROI hand-drawn on a rotated or registered image is defined in *that*
frame's coordinates, but the cells it selects should usually come back in native
coordinates. Without the split you must choose between drawing in native space and
permanently transforming the data.

The two concerns are strictly separate:

| concern | source | consumed by |
|---|---|---|
| **predicate frame** — how the crop region is interpreted | `view@space`, consulted at the crop step | `.surviving_cell_ids`, `.surviving_cell_ids_arrow`, `.push_view_to_dt`, `.push_view_to_pstore` |
| **output frame** — what frame returned coordinates live in | the explicit `space =` argument only, no fallback | `.apply_space_to_subobj` |

```r
getSpatialLocations(g, view = "test")
#   native-frame coords, narrowed to cells satisfying the crop in view@space's frame
getSpatialLocations(g, view = "test", space = "rotate")
#   rotated-frame coords, same cells
```

- `.resolve_space` is explicit-only — deliberately **no** `view@space` fallback for the output frame
- crop-step helpers consult `view@space` directly for the predicate frame
- `.project_region_between_spaces` pushes the region's WKT into the output frame when the two differ

---

## 4. Centroid routing for backed-polygon crops — Complete, refactor pending

Guarantees a view recipe narrows identically no matter which slot you read it through.

- `viewCrop` against the backed parquet polygon store always narrows via the centroid-derived cell_ID set (`.surviving_cell_ids_arrow`), regardless of caller path
- that keeps **one usage layer per predicate** — narrowing is the same across spatial locations, cell metadata, polygons, and expression
- the polygon-geom `spat_relate` path is reserved for `giottoPoints` (not cell-aggregatable) and for a future explicit polygon-vs-polygon step ([§11](#11-viewspatrelate-step-type--not-started))

**Remaining — the mechanism is a workaround.** Routing is currently decided by
*target storage kind* and implemented through cache allocation: the polygon's
`resolveSubobject` allocates a one-shot cache environment when none is provided,
which is what forces `.push_view_to_pstore` down the centroid pathway. Two patches
ride on this — force-cache for polygons, force-`NULL`-cache for points.

The clean refactor decouples semantic routing from cache memoization: decide per step
on `(predicate relation, polygon source availability)`, and let the cache sit
underneath the centroid path as pure optimization. That drops both patches. A TODO in
the method points at it.

---

## 5. Space steps — sample-keyed transforms — Complete

Records deferred spatial transforms, keyed by sample.

- a `spaceTransform` step captures a call to an existing transform generic — `affine`, `spin`, `spatShift`, `flip`, `rescale`, `shear`, `zoom`
- at resolution the receiving object is spliced in as the first argument and `do.call` dispatches to the method that already exists, so spaces add **no new transform implementations**
- `@samples` is a named list: sample name → ordered step list
- `:default:` is the sentinel for sample-anonymous (single-giotto) construction
- the constructor binds exactly one sample name per call

Properties that follow from the storage shape:

- **the space owns the transforms, not the objects** — nothing is written to the data. That's what lets one object participate in several frames at once and makes per-object composition well-defined
- **a key with no steps still participates** — `giottoSpace("a")` creates `@samples[["a"]]` with an empty step list, so `a` is in the space at identity. The vignette's atlas example relies on this: one sample untransformed, another shifted
- **participation is `names(space@samples)`** — consumers auto-derive the sample narrowing from those keys, so `plot(mg, space = "atlas")` needs nothing else. Samples outside the key set error rather than silently falling back, preserving the "spaces enumerate their participating samples" contract
- **anchor defaults to `(0, 0)`** for `spin` / `affine` recorded onto a space, not the data's centre, so a recorded rotation is reproducible independent of the extent. Overridable per call
- **sample-uniform scope, deliberately** — within a sample, cells, polygons, points, images and spatlocs all move together. Per-element overrides are unsupported; the documented path is `materialize()` plus per-element transforms afterwards. This does put image-versus-polygon registration, the hard alignment problem, out of scope

---

## 6. Composition and the broadcast rule — Complete, undocumented

Combines space recipes. `+` merges two: same-sample keys concatenate their step lists in
order, different-sample keys merge into one multi-sample space.

**The undocumented part.** `.space_record` appends a step to **every sample currently
keyed in the space**, not just one. That makes composition **order-sensitive**:

```r
# transform BEFORE merge — scoped to one child
(giottoSpace("a") |> spin(30)) + giottoSpace("b")
#   a: spin        b: (none)

# transform AFTER merge — broadcast to the group
grp <- giottoSpace("a") + giottoSpace("b")
grp |> affine(M)
#   a: affine      b: affine

# group transform then per-child transform composes
(grp |> affine(M)) + (giottoSpace("a") |> spatShift(dx = 10))
#   a: affine -> spatShift        b: affine
```

That last form is the group-plus-individual composition the design intends, and it
works. But the rule making it work lives only in a code comment on `.space_record`.
Nothing user-facing states it — the vignette's design notes say only "every space step
is scoped to one or more named samples," which hints at the behaviour without stating
either the broadcast or the ordering.

**Remaining.** Document the rule in the vignette's `+` section. Doc-only work,
independent of any PR, and the kind of load-bearing subtlety that gets re-litigated.

---

## 7. Threading through generics — Complete, scope under review

Exposes `view =` and `space =` on the ordinary API, so recipes are built and applied
through the natural verbs rather than only through `materialize()`.

```r
g <- subset(g, leiden_clus == 1, view = "cluster1")   # records a viewFilter
g <- crop(g, roi, view = "stripe", space = "atlas")   # records a viewCrop, binds view@space
g <- spin(g, 30, space = "rotated")                   # appends a step to space "rotated"
```

- **all gobject ops stay eager by default** — `spin(g, 30)` is unchanged; the presence of `space =` is what switches from acting to recording. Dispatch plus argument is the signal
- **`space =` is overloaded on purpose, consistently** — on `spin`/`affine`/`spatShift`/`rescale`/`flip` it *appends a step* to the named space; on `crop` it *binds the view's `@space` reference* to it. Same referent, two operations, the verb decides which. The readings look contradictory until you see the target is identical
- **current reach:** 57 formals across 20 files (30 GiottoClass, 27 GiottoVisuals), 71 man pages

**Under review.** Whether `space =` earns that surface. Most sites are pass-through
plumbing nobody will call with `space =`, and a getter-level `space =` is largely
redundant with `materialize(g, space = ...)` followed by ordinary accessors — the
pattern the vignette already presents as primary. Trimming to `materialize()` plus the
plot entry points would remove most of the maintenance and doc burden while keeping
the capability. `view =` is the stronger case, since narrowing is reused and expensive
to recompute.

This is hub §5 question 4, and it wants deciding before PR 2 lands the surface —
removing formals afterwards is a breaking change.

---

## 8. `materialize()` — Complete

Applies a view and/or space and returns a new gobject where ordinary accessors see the
narrowed, reframed data — no recipe argument needed downstream.

- `materialize(g, view, space)` resolves the recipes, applies the surviving-ID narrowing and the transform chain
- `slots =` limits which slots are materialized; the `combine*()` family routes through it so one resolution is reused rather than recomputed
- on a `giottoMulti` it walks participating children and applies each one's step list
- its resolved surviving-ID set is what GiottoLens pre-renders as a view payload

**Open.** Image-slot semantics: warp at materialize time, or keep images as
references with the transform applied at render? Unresolved, and it matters for
anything that exports a materialized gobject.

---

## 9. Groups as space keys — Not started

Let a registered group name key a space, so one transform declaratively lands on every
member and still composes with per-child steps.

```r
giottoSpace(group = "tumor_pair") |> affine(M)
```

**Why.** Today the only group-transform mechanism is the order-dependent broadcast in
[§6](#6-composition-and-the-broadcast-rule--complete-undocumented). Naming the group
makes the intent explicit and stable rather than emergent from construction order.

**Depends on** [federation §11](IMPLEMENTATION_gmulti_federation.md#11-groups-registered-sample-handles--not-started).

**The design fork** (hub §5 question 3): a standalone `giottoSpace()` has no gobject,
so a group name can't be expanded at construction. Either the constructor starts
requiring a gobject, or the space stores the group name symbolically and expands at
resolution. Symbolic is more consistent with "recipes, not state" and lets group edits
propagate to every space referencing them — but then `names(space@samples)` is no
longer the participation set by itself, so `.resolve_samples` has to expand too.

---

## 10. Composable views — Not started

`view1 + view2`. Deliberately errors with "not yet implemented" — spaces compose freely,
views don't.

Deferred because the semantics need their own pass: intersecting filters is obvious,
union-versus-intersection for crops is not, and two views binding different predicate
frames conflict in ways that need a rule. Chaining `subset()` / `crop()` on a single view
covers current needs.

---

## 11. `viewSpatRelate` step type — Not started

Record a polygon-versus-polygon spatial predicate as a view step, with an indirect form
`spatRelate(g, ..., view = ...)`.

**Why.** [§4](#4-centroid-routing-for-backed-polygon-crops--complete-refactor-pending)
narrows backed-polygon crops by centroid, which is right for `intersects`-style
questions on cell-aggregatable content but wrong for predicates that genuinely need
geometry: `within`, `contains`, `covers`, `overlaps`, `touches`, `crosses`. Those need
the geom path, and a distinct step type is how a view declares that it means the
geometric predicate rather than the centroid approximation.

**Related.** The `spatRelate` generic and its `(giottoSpatial, giottoSpatial)` method
are already upstream on both `dev` and `gsource`, as is `spatQuery` for the
gobject-level multi-filter pipeline — so the predicate machinery exists and this is
about recording it as a recipe step. Note GiottoClass's `spatRelate` currently has
only the one signature, while GiottoDisk carries seven `parquetGeomBase` y-forms
(WKT `character`, `SpatVector`, `sf`, `giottoPolygon`, `giottoPoints`, `spatLocsObj`,
`parquetGeomBase`); the in-memory side would want widening to match before a view step
leans on it.

---

## 12. Sedonadb lowering for view recipes — Not started

Compile a whole view recipe into a single SQL plan rather than resolving steps one at a
time.

**Why.** Views already push into the backend's lazy query plan, but the predicate
folding happens step-by-step. Lowering the recipe wholesale lets the SQL engine plan
across steps — and it pairs with
[federation §10](IMPLEMENTATION_gmulti_federation.md#10-federatedreadhandle--partial),
where a cross-sample read's fragments can lower into one plan instead of materializing
per substore. Together those are what make an atlas-scale narrowing a single query.

**Also on this list, further out:** ephemeral analysis steps inside a view — e.g.
`with_network(method, k)` — so a recipe can carry a derived structure rather than only
a narrowing.

---

## 13. `attach_derived()` — Not started

Reattach a column-style derivation computed *under a view* — cluster assignments, module
scores, QC metrics — back onto the parent gobject, stored alongside existing metadata and
tagged with the view name so provenance is explicit.

**Why it's needed.** `materialize()` is the read direction: it projects a view into a
new gobject where derived outputs live. `attach_derived()` is the write direction —
without it, anything computed on a materialized subset is stranded there, and the only
way back to the parent is a manual key-based join. That join is exactly the operation
[federation §9](IMPLEMENTATION_gmulti_federation.md#9-carry-keys-discipline--partial)
says must carry keys, so this is the natural place to enforce it rather than leaving
each caller to get it right.

**Status.** The generic and a `giotto` method exist; the method body is
`stop("attach_derived(): not yet implemented.")`.

> **Two documentation traps here.** The source groups this under a banner reading
> `# Mutation escape hatches (stubs) ####` and gives `materialize()` a roxygen
> `\strong{Status:} stub. The actual resolution engine lands in a follow-up.` That is
> **stale** — `materialize()` has three real methods (`gAny`, `giottoMulti`,
> `federatedReadHandle`) and is exercised 24 times in `test-view-space.R`. Only
> `attach_derived` is still a stub. The banner and that roxygen line should be
> corrected. It's a clean illustration of why *status* claims decay when kept next to
> code — nothing falsifies them when the code advances — whereas the invariant comment
> on `.space_record` ([§6](#6-composition-and-the-broadcast-rule--complete-undocumented))
> has stayed correct because editing the loop would force editing the comment.

---


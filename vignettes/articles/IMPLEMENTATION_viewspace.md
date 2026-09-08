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

**Recipe layout.** Q7 made the steps plain tagged lists; Q8 made the containers follow,
so nothing here is S4:

```
view    list(steps = list(<step>, ...), space = NA_character_, misc = list())
space   list(samples = list("<sample>" = list(<step>, ...)), misc = list())

step (view)   list(type = "filter",    predicate = , scope_args = )
              list(type = "crop",      region = , relation = , geom = )
              list(type = "samples",   samples = )
step (space)  list(type = "transform", op = , args = )
```

The dropped S4 slots were `name` (redundant — the name is the key under `@view` /
`@spaces`) and `source` (documented as reserved, never read). Validation moved with the
shape rather than disappearing: `.validate_view()` / `.validate_space()` and the
per-step validators run in the recorder and in the `giottoView<-` / `giottoSpace<-`
setters, which is where a hand-built or copied-in recipe gets checked.

- resolving a recipe does not mutate a gobject — `materialize()` returns a new one, or a name is passed per-call to a getter. Detaching is not passing it. Recording a step is the one write, and it touches only the recipe
- **created by recording, not construction** — the first `subset()` / `crop()` / `selectSamples()` call naming a view creates it; likewise a transform verb naming a space. There is no constructor
- keyed by name in `gobject@view` / `gobject@spaces`, so they persist with the object
- **`gobject@spaces` *is* the multi-space registry** — several frames falls out of slotting several spaces, no separate mechanism
- **read-only enforced by signature** — functions taking `view =` return a result. Q8 nuance: `subset(g, ..., view = )` and friends *do* return a modified gobject, because recording is a write to the recipe. The data is never touched either way
- **lazy** on a disk-backed gobject: predicates and crops push into the backend query plan; the pull happens when a downstream call collects
- files (after Q8): `classes-view.R` (339), `classes-space.R` (208), `methods-view.R` (585), `methods-space.R` (317), resolver `classes-resolver.R` (95) + `methods-resolver.R` (715)

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

| step `type` | records |
|---|---|
| `"filter"` | a metadata predicate, deparsed (`subset(g, ..., view = )`) |
| `"crop"` | a spatial region as WKT, plus `relation` and `geom` |
| `"samples"` | which children participate (gmulti only) |

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

**Resolved in GiottoClass — the step declares it.** The refactor decouples semantic
routing from cache memoization by making the choice a recorded field: a crop step
carries `geom = "centroid" | "poly"`, and the resolver reads it instead of deriving
anything. The cache sits underneath the centroid arm as pure optimization.

An earlier attempt *inferred* the choice from the relation name, via an internal
predicate that was then going to be exported so GiottoDisk could share the inference.
That was the wrong shape twice over: the earlier prescription here — "decide per step
on `(predicate relation, polygon source availability)`" — reads as licence to infer,
and inference means a recorded recipe cannot state which question it asks. Declaration
also removes the shared-contract problem entirely, since there is nothing to share:
GiottoDisk reads the field.

Still owed on the GiottoDisk side: both patches go away by replacing the
`is.null(.cache)` test in `.push_view_to_pstore` with `identical(step$geom,
"centroid")` — eager `id_filter` for the centroid arm, the existing lazy
`spat_relate` op for the poly arm. Note the patches are currently *class*-keyed
(polygon → forced cache → centroid; points → forced NULL cache → geom), so a backed
`giottoPolygon` crop cannot honour `geom = "poly"` until this lands.

---

## 5. Space steps — sample-keyed transforms — Complete

Records deferred spatial transforms, keyed by sample.

- a `spaceTransform` step captures a call to an existing transform generic — `affine`, `spin`, `spatShift`, `flip`, `rescale`, `shear`, `zoom`
- at resolution the receiving object is spliced in as the first argument and `do.call` dispatches to the method that already exists, so spaces add **no new transform implementations**
- `samples` is a named list: sample name → ordered step list
- `:default:` is the sentinel for sample-anonymous (single-giotto) recording
- **Q8 update:** there is no constructor. A space is created by recording a transform onto a
  name (`spin(g, 30, space = "s")`), and `samples =` on the `giottoMulti` methods says which
  children a step applies to. The sentinel is added only when a step is recorded with no
  `samples` scope, so a per-sample space carries no stray empty key

Properties that follow from the storage shape:

- **the space owns the transforms, not the objects** — nothing is written to the data. That's what lets one object participate in several frames at once and makes per-object composition well-defined
- **participation is `names(space$samples)`** — consumers auto-derive the sample narrowing from those keys, so `plot(mg, space = "atlas")` needs nothing else. Samples outside the key set error rather than silently falling back, preserving the "spaces enumerate their participating samples" contract. Q8 note: a child that should sit untransformed in the space is no longer expressed by adding an empty `giottoSpace("a")`, since there is no constructor — leave it unkeyed and it resolves at identity
- **anchor defaults to `(0, 0)`** for `spin` / `affine` recorded onto a space, not the data's centre, so a recorded rotation is reproducible independent of the extent. Overridable per call
- **sample-uniform scope, deliberately** — within a sample, cells, polygons, points, images and spatlocs all move together. Per-element overrides are unsupported; the documented path is `materialize()` plus per-element transforms afterwards. This does put image-versus-polygon registration, the hard alignment problem, out of scope

**Remaining — collapse the chain into one transform before applying it.**
Recording stays stepwise; the *application* should not be.

`.apply_space_to_subobj()` (`methods-resolver.R:234`) is the only site that applies a
space step, and it loops:

```r
for (step in steps) {
    subobj <- do.call(step$op, c(list(x = subobj), step$args))
}
```

So an N-step space is N sequential eager dispatches **per subobject**, each rewriting
that subobject's coordinates before the next reads them. A 5-transform space resolving
spatlocs + polygons + points + an image is 20 passes where one composed affine would be
4. There is no fold anywhere: the resolver and the space recorder contain no `affine2d`,
no `%*%`, and no compose step.

The machinery already exists. `affine2d` *is* a composition accumulator —
`spin(affine2d)` does `.aff_linear_2d(new_aff) <- .aff_linear_2d(new_aff) %*% rotate_m`
— and there are `affine2d` methods for the affine-representable verbs. All of `spin`,
`spatShift`, `affine`, `flip`, `rescale` and `shear` are affine; only `zoom` is not
(it is an extent crop, and is a deprecation candidate anyway).

**Shape.** Fold the affine-representable prefix of the chain into one `affine2d` before
the loop, apply that in a single pass, and break the chain at the first non-affine op —
folding each affine run between them. The recorded steps are never rewritten, so a
recipe stays inspectable and hand-editable (`v$steps[[2]] <- NULL` still drops a step);
this is purely a resolve-time optimisation.

Composition is exact rather than approximate: a user-supplied `x0`/`y0` anchor is a
translate–rotate–translate triple, itself affine, so it composes into the same product.
The `(0, 0)` anchor default above is what keeps the fold independent of intermediate
extents.

**Worth checking when this is implemented:** whether the image path resamples per
transform. If it does, folding is not only ~N× cheaper but *more faithful* — one
resample instead of N — which would make this a correctness improvement for images
rather than a pure optimisation.

---

## 6. Composition and the broadcast rule — resolved by Q8

**Superseded.** `+` is gone, and with it the ordering subtlety this section was written
to document.

The problem it described: `.space_record()` appended a step to **every sample currently
keyed in the space**, so composition was order-sensitive —
`(giottoSpace("a") |> spin(30)) + giottoSpace("b")` spun only `a`, while
`(giottoSpace("a") + giottoSpace("b")) |> spin(30)` spun both. The group-plus-individual
composition the design wanted did work, but only via a rule that lived in one code
comment; nothing user-facing stated it, and the same expression meant different things
depending on how much of the recipe had been merged before it.

Q8 replaced `+` with `samples =` on the `giottoMulti` transform methods, which states
the scope at each call instead of inheriting it from build order:

```r
mg <- spin(mg, 30, space = "atlas", samples = "a")          # a only
mg <- spatShift(mg, dx = 8000, space = "atlas", samples = "b")  # b only
mg <- affine(mg, M, space = "atlas")                        # every keyed sample
mg <- spin(mg, 30, space = "atlas", samples = c("a", "b"))  # both, explicitly
```

The broadcast case survives as `samples = NULL` (omitted), which still means "every
sample this space already keys" — but it is now the *only* implicit form, and it reads
at the call site rather than depending on history. A mistyped child name is rejected at
record time rather than silently creating a chain nothing resolves against.

**Remaining.** Nothing. The rule is stated in the vignette's "Recording a space"
section and enforced by tests.

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
[§6](#6-composition-and-the-broadcast-rule--resolved-by-q8). Naming the group
makes the intent explicit and stable rather than emergent from construction order.

**Depends on** [federation §11](IMPLEMENTATION_gmulti_federation.md#11-groups-registered-sample-handles--not-started).

**The design fork is resolved by Q8, not by a choice.** The fork was: a standalone
`giottoSpace()` has no gobject, so a group name could not be expanded at construction —
either the constructor takes a gobject, or the space stores the name symbolically and
expands at resolution. There is no constructor any more: every space is recorded through
a gobject, so the gobject is always in hand.

Late binding remains the better of the two, and Q8 keeps it: the step records the group
name and `.gm_resolve_sample_names()` expands it at resolve time, so group edits
propagate to every space referencing them. This is consistent with filter predicates,
which also resolve against current object state. The cost is that
`names(space$samples)` is no longer the participation set by itself, so the consumer-side
sample narrowing has to expand too — and that a group edit silently changes a saved
layout, which argues for surfacing group-derived scope wherever a space is summarised.

Three guards the expansion needs, recorded in
[PLAN_gmulti2_port.md](PLAN_gmulti2_port.md) Q8: terminate on cyclic group definitions
(dedupe against already-expanded *group* names, not against the output); reject a group
whose name collides with a sample's at assignment time rather than shadowing the child at
read time; and error when a scope resolves to zero samples, which otherwise looks like a
step that worked.

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

**Why — the original motivation is now handled elsewhere, see below.**
[§4](#4-centroid-routing-for-backed-polygon-crops--complete-refactor-pending)
narrowed crops by centroid, which is right for `intersects`-style questions on
cell-aggregatable content but wrong for predicates that genuinely need geometry.
A distinct step type was the proposed way for a view to declare that it means the
geometric predicate rather than the centroid approximation.

**That declaration now lives on the crop step itself** as `geom = "centroid" | "poly"`
(§4), which is a cheaper answer to the same requirement and does not need a second
step type. Two corrections to the reasoning above, both measured against terra rather
than assumed: `within` and `touches` *are* well defined on a centroid (strict interior
and boundary-only respectively), so they were never in the geometry-only set; and only
`contains`, `covers`, `overlaps`, `crosses` are always `FALSE` against a point.
`covered_by` is not a terra predicate at all.

**What is left of this item** is the narrower case it also mentions: recording a
general polygon-versus-polygon `spatRelate` as a view step, for relating arbitrary
geometry rather than narrowing cells by a region. Still deferred, and no longer
blocking geometry-accurate crops. The 1-signature-vs-8 prerequisite below is partly
closed — GiottoClass's `spatRelate` now takes `character` (WKT), `SpatVector` and `sf`
on the y side, mirroring GiottoDisk's cascade in reverse (SpatVector is canonical in
memory, WKT on disk, each side canonicalizing to what its engine consumes).

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
> on `.space_record` ([§6](#6-composition-and-the-broadcast-rule--resolved-by-q8))
> has stayed correct because editing the loop would force editing the comment.

---


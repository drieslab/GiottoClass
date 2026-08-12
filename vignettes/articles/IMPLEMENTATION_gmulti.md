# Implementation plan — `giottoMulti` federation + view/space

Hub document. This page holds the shared context — the class, the branch situation,
the landing order — and indexes the implementations. Every implementation is listed
once, with its status in its own section header.

| page | covers |
|---|---|
| **this page** | data model, status index, landing plan, cross-cutting questions |
| [Federation](IMPLEMENTATION_gmulti_federation.md) | foundation, federation, narrowing, consumers |
| [View / space](IMPLEMENTATION_viewspace.md) | recipe classes, resolver, transforms, composition, threading |

The recipe layers are their own subsystem — they dispatch on `gAny` and work on a plain
`giotto` too — so they get their own page. This plan covers where the two meet
(sample-keyed spaces, `selectSamples`, groups as space keys) and otherwise defers there.

Related: [DESIGN_gmulti_federation.md](DESIGN_gmulti_federation.md) is the design
*record* — read it for rationale and rejected alternatives.
[STATUS_gmulti_federation.md](STATUS_gmulti_federation.md) is an older phase-landing
log, superseded by the status index below.

[design.Rmd](design.Rmd) is the pre-existing GiottoClass architecture article — the
two-tier object model, the schema, `initialize()`, accessors, analysis verbs. It
predates this work and mentions gmulti nowhere. It is the framework description this
plan must stay consistent with, and it will need a gmulti section before this lands.

Where this plan and the design doc disagree, this plan is current. The design doc
predates implementation and has drifted in one known place, flagged in §7.

**Provenance.** These docs describe implementations as they stood on
`feature/gmulti-federation-design` at checkpoint `4f58d861`, not as they stand on this
branch — this branch starts from `gsource` with none of it ported. Read every
"Complete" as "complete over there, pending review here". The user-facing vignettes
(`giottoMulti.Rmd`, `view_and_space.Rmd`) also live on that checkpoint only.

---

## 1. What the project is

A data coordination harness for multiple Giotto objects and samples. Two pieces.

### 1.1 Federation

Presents N child `giotto` objects as one analysable unit.

- Non-spatial content is joint: expression, metadata, dim reductions across samples,
  with per-child naming reconciled by declaration rather than by renaming data.
- Spatial content is **not** joint by default — see [§3](#3-data-model) for what the
  class does and doesn't hold, and
  [federation §13](IMPLEMENTATION_gmulti_federation.md#13-gmulti-level-spatial-content--not-started-unsolved)
  for the unsolved case.
- Gives multi-sample analysis a backbone without paying the cost of joining.

### 1.2 View / space / sample

Three knobs that modify how data is queried, to be standardised in naming and usage
across the functions that expose them. **They are additive** — each narrows on top of
the others; none overrides another.

- **view** and **space** are non-destructive recipe layers over any gobject. Both are
  named, slotted, and composable, and both work on a single `giotto` as well as a multi.
  - *view* narrows which cells and features are in scope
  - *space* defines a coordinate frame that one or more samples are mapped into
- **sample** serves two purposes:
  - an ad-hoc filter on top of *view*
  - the key for *space* — designating which children participate, and their
    sample-specific transforms if any

Where each knob is available is gated by whether a given function exposes it as a
parameter; the recipe classes themselves are documented in
[IMPLEMENTATION_viewspace.md](IMPLEMENTATION_viewspace.md).

---

## 2. Status index

**Complete** = implemented and tested on the branch. **Partial** = usable with a
stated gap. **Not started** = designed only, or not yet designed.

### Foundation

| implementation | status |
|---|---|
| [`gAny` virtual base + `giottoMulti` class](IMPLEMENTATION_gmulti_federation.md#1-gany-virtual-base--giottomulti-class--complete) | Complete |
| [`@id_map` registry + `@id_sig` invalidation](IMPLEMENTATION_gmulti_federation.md#2-id_map-registry--id_sig-invalidation--complete) | Complete |
| [`@source` multi-level gsource](IMPLEMENTATION_gmulti_federation.md#3-source-multi-level-gsource--complete) | Complete |
| [save / load round-trip](IMPLEMENTATION_gmulti_federation.md#4-save--load-round-trip--partial) | Partial — `snapshotSave` gap |

### Federation

| implementation | status |
|---|---|
| [`@mapping` declaration + auto-discovery](IMPLEMENTATION_gmulti_federation.md#5-mapping-declaration--auto-discovery--complete) | Complete |
| [Federation helpers consult `@mapping`](IMPLEMENTATION_gmulti_federation.md#6-federation-helpers-consult-mapping--complete) | Complete |
| [Access layer — `samples =` + `sample::name`](IMPLEMENTATION_gmulti_federation.md#7-access-layer--complete) | Complete |
| [Joint slots as cache + ground truth](IMPLEMENTATION_gmulti_federation.md#8-joint-slots-as-cache--ground-truth--complete) | Complete |
| [Carry-keys discipline](IMPLEMENTATION_gmulti_federation.md#9-carry-keys-discipline--partial) | Partial — 3 sites unreviewed |
| [`federatedReadHandle`](IMPLEMENTATION_gmulti_federation.md#10-federatedreadhandle--partial) | Partial — structural, unwired |
| [`@groups` registered sample handles](IMPLEMENTATION_gmulti_federation.md#11-groups-registered-sample-handles--not-started) | Not started — design settled |
| [Combined defaults](IMPLEMENTATION_gmulti_federation.md#12-combined-defaults--not-started) | Not started |
| [gmulti-level spatial content](IMPLEMENTATION_gmulti_federation.md#13-gmulti-level-spatial-content--not-started-unsolved) | Not started, unsolved |
| [Joint `@spatial_network`](IMPLEMENTATION_gmulti_federation.md#14-joint-spatial_network--not-started) | Not started |
| [`getSpatialLocations` unification](IMPLEMENTATION_gmulti_federation.md#15-getspatiallocations-unification--not-started) | Not started |
| [Cross-sample aggregation](IMPLEMENTATION_gmulti_federation.md#16-cross-sample-aggregation--not-started) | Not started |

### Narrowing

| implementation | status |
|---|---|
| [`@cell_ID` / `@feat_ID` narrowing contract](IMPLEMENTATION_gmulti_federation.md#17-cell_id--feat_id-narrowing-contract--complete) | Complete |
| [Additive sample × view composition](IMPLEMENTATION_gmulti_federation.md#18-additive-sample--view-composition--partial) | Partial — feature axis on points |

### Consumers

| implementation | status |
|---|---|
| [GiottoVisuals dispatcher](IMPLEMENTATION_gmulti_federation.md#19-giottovisuals-dispatcher--complete) | Complete |
| [Per-panel sizing](IMPLEMENTATION_gmulti_federation.md#20-per-panel-sizing--not-started) | Not started |
| [GiottoLens consumption](IMPLEMENTATION_gmulti_federation.md#21-giottolens-consumption--separate-repo) | Separate repo |

The recipe subsystem has its own index — see
[IMPLEMENTATION_viewspace.md](IMPLEMENTATION_viewspace.md). The entries that touch
gmulti directly are sample-keyed space steps, `selectSamples`, and groups as space
keys.

---

## 3. Data model

```
gAny (virtual)
├── giotto
└── giottoMulti
```

`giottoMulti` does **not** inherit from `giotto`. The virtual base is deliberate:
gmulti's spatial slots are absent, and inheriting would let spatial-domain methods
fall through to empty slots silently. With `gAny`, an undefined method fails loudly
via no-method dispatch, and shared-domain methods are written once.

### Slots

```
objects  id_map  id_sig  mapping
expression  expression_feat  cell_metadata  feat_metadata  cell_ID  feat_ID
spatial_enrichment  dimension_reduction  nn_network  multiomics
instructions  parameters  versions  misc  source  view  spaces
```

| slot | role |
|---|---|
| `@objects` | named list of child `giotto` objects — the first-class sample axis |
| `@id_map` | `list(cells, feats)`, each `data.table(object, local_id, global_id)`. Identity **registry**; never narrowed |
| `@id_sig` | child length-signature; drives cache invalidation in `initialize()` |
| `@mapping` | federation declaration for the indexing axes |
| `@cell_ID` / `@feat_ID` | **active narrowing**, nested by spat_unit / feat_type; `NULL` = unfiltered |
| joint slots | `@expression`, `@cell_metadata`, `@feat_metadata`, `@dimension_reduction`, `@nn_network`, `@spatial_enrichment` — cache *and* source of truth |
| `@source` | one multi-level `gsource` for cross-sample artifacts; children keep their own |
| `@view` / `@spaces` | named recipe slots, owned by the recipe subsystem |

### Deliberately absent

`@spatial_locs`, `@spatial_info`, `@spatial_network`, `@spatial_grid`, `@feat_info`,
`@images`, `@join_info`, `@offset_file`, `@h5_file` — all per-dataset, all on
children. Spatial-domain access is a per-sample fan-out.

This has a consequence that is **not** yet solved: there is nowhere for gmulti-level
spatial content to live. See
[federation §13](IMPLEMENTATION_gmulti_federation.md#13-gmulti-level-spatial-content--not-started-unsolved).

### Three declaration layers

Easily confused with one another:

| layer | declares | scope |
|---|---|---|
| `@mapping` | "this gmulti handle federates with these child handles" | indexing axes (spat_unit / feat_type) |
| `@groups` | "this name refers to these samples" | sample membership, by enumeration |
| `@spaces` | a coordinate frame, keyed by sample | frame only |

---

## 4. Branch situation and landing plan

Nothing is upstream. Neither `dev` nor `gsource` contains `R/gmulti.R` or any
view/space file.

| repo | ahead of `upstream/gsource` | behind |
|---|---|---|
| GiottoClass | 59 (+9,219 lines) | 6 |
| GiottoVisuals | 10 | 3 |
| Giotto | 2 | 19 |
| GiottoData | 1 | — |

Test state on the branch: `test-gmulti.R` 181 pass / 0 fail; `test-view-space.R`
green; full GiottoClass suite 1,413 pass / 0 fail with 3 errors, all traced to the
two gaps in §6 below.

### Why the target is `gsource`, not `dev`

`giotto@source` exists only on `gsource`, and `gmulti.R` references it. Retargeting to
`dev` would mean either merging `gsource` → `dev` first (102 commits) or amputating
`@source` and re-adding it. The stack stays on `gsource`.

### Why not restart from a fresh branch

A dry-run merge of `gsource` into the federation branch produces a clean tree with no
conflicts — the only overlapping files are three regenerated `.Rd`s, and the six
missing commits touch `R/NN_network.R` and `adr/`, which the branch never touched.
There is no drift. Re-cutting would replay ~9k lines to buy only tidier history, and a
previous replay onto this branch left pieces behind. If clean PR history is wanted,
apply the content diff onto a fresh branch and split it into commits — don't re-derive
the work.

### Order

The recipe subsystem depends on the gmulti foundation, not the reverse: `giottoView`
dispatches on `gAny` and `materialize` has a `giottoMulti` signature — 27 references to
`gAny`/`giottoMulti` across the recipe files against 1 the other way.

| PR | content | base |
|---|---|---|
| 1 | foundation: `gAny`, class, `@id_map`, `@source`, save/load, **plus the narrowing contract** | `gsource` |
| 2 | view / space | after 1 |
| 3 | federation: `@mapping`, access layer, joint slots, GiottoVisuals dispatcher | after 1 |

`origin/gmulti` is a ready-made foundation slice — 27 commits, no view/space files, a
strict ancestor of the current branch. It needs the 6-commit merge forward.

**One deliberate deviation from that slice:** the narrowing contract must be pulled
into PR 1. `origin/gmulti` predates the contract change, so landing it as-is would
publish `@id_map`-narrowing and then revoke it one PR later. See
[federation §17](IMPLEMENTATION_gmulti_federation.md#17-cell_id--feat_id-narrowing-contract--complete).

---

## 5. Cross-cutting open questions

Questions whose answers change more than one implementation. Implementation-local
questions live with their sections; recipe-subsystem questions live in that doc.

1. **`@groups` value shape — membership now, or named immediately?** The named form
   (`c(B191 = "tumor_roi", B215 = "epithelium_roi")`) would also cover content-level
   aliasing, but nothing consumes those names: sample resolution needs membership
   only, and content resolution is a second resolver that doesn't exist. Leaning
   membership-only with the shape reserved.
2. **Can a group name key a `@mapping` entry?** `c(tumor_pair = "rna")` is convenient
   but doubles the resolution paths inside federation. Leaning no — groups resolve at
   the sample axis only.
3. **Does a group-keyed space expand eagerly or stay symbolic?** A standalone
   `giottoSpace()` has no gobject, so a group can't be expanded at construction.
   Symbolic keeps spaces true recipes and lets group edits propagate, but then
   `names(space@samples)` stops being the participation set on its own and
   `.resolve_samples` must expand too. Leaning symbolic.
4. **Does the class participate in the `:default:` sentinel,** or is it strictly a
   GiottoLens export-side convention?
5. **`samples = ":all:"` explicit, or `NULL` with `":all:"` implied?** Affects
   signature noise. Leaning `NULL` default, `":all:"` documented.
6. **How does `joinGiottoObjects` relate to gmulti?** Current read from the class
   header: siblings, not nested — gmulti preserves separate spaces, `join` merges into
   one giotto. Unconfirmed.
7. **Where do integration-method parameters live** (harmony, anchor-based
   integration) — `@parameters`, or a dedicated slot if it grows?

---

## 6. Prerequisites outside this project

Two things block or distort testing and are not gmulti work.

**The shared library's GiottoDisk is stale and broken.** `gmulti_libs` holds a build
from GiottoDisk `feature/giotto-view`, 48 commits behind `dev`. On it,
`createGiottoObject(expression = mat, backend = gdir)` returns 4 cell IDs and 0
features instead of 20 and 50, which is the cause of two `test-networks.R` errors and
may be masking others. Rebuilding also risks pulling in `dev`-side changes this branch
hasn't seen, so it wants doing deliberately rather than mid-PR. Note the local
GiottoDisk `feature/giotto-view` is 14 commits ahead of its origin and unpushed, and
carries a 55-line spatial AABB pre-cull that never reached `dev`.

**`snapshotSave` has no `(gDirSource, giottoMulti)` method.** GiottoDisk-side gap;
blocks save/load round-trip on a sourced multi. See
[federation §4](IMPLEMENTATION_gmulti_federation.md#4-save--load-round-trip--partial).

---

## 7. Documentation debt

- **Design doc §7 cannot be implemented as written** — it stores a pointer class in
  `gmulti@spatial_info`, a slot the class does not have. Superseded by
  [federation §11](IMPLEMENTATION_gmulti_federation.md#11-groups-registered-sample-handles--not-started).
- **No NEWS entries** anywhere for ~9k lines of new API.
- **These docs are `.md`, so they won't render on a pkgdown site.** They live in
  `vignettes/articles/` (the pkgdown convention) and that directory is
  `.Rbuildignore`d, so they never ship in the tarball. Vignette-engine detection only
  looks at files directly in `vignettes/`, so the subdirectory is what keeps R CMD
  check quiet. To make any of these public, convert to `.Rmd` with YAML front matter
  and add an `articles:` entry to `_pkgdown.yml` — GiottoClass has none yet.
- **Several open questions are ADR-shaped.** GiottoClass already carries ADRs
  0001–0004 on `gsource`. Once §5's questions are decided, the decisions belong there —
  one numbered file each — rather than as prose in this plan.

### Superseded memory notes

Ten notes were folded into these pages and deleted. The remaining pointer note lists
what deliberately stays outside the repo: local worktree layout, cross-repo state, and
positioning material.

---

*Created 2026-08-11.*

# Port plan — `feature/gmulti2`

Working plan for reviewing and porting the gmulti / view-space work onto a fresh branch
cut from `GiottoClass@gsource`, `Giotto@gsource`, `GiottoDisk@dev`.

The [implementation plan](IMPLEMENTATION_gmulti.md) describes **what exists on the
checkpoint**. This page decides **what comes across, in what order, and what gets
re-argued first**. Where the two disagree, this page wins.

Source of truth for the old work: `feature/gmulti-federation-design` @ `4f58d861`
(GiottoClass), plus `feature/giotto-view` @ `1c08c9af` (gmulti-v2, reference only),
`wip/gmulti-visuals-checkpoint` @ `54f19e9` (GiottoVisuals).

---

## 1. Verified baseline

Checked against the branches as of 2026-08-12, not assumed.

| claim | verdict |
|---|---|
| `gsource` has no gmulti implementation | **confirmed** — 0 lines across all 7 implementation files; only 2 incidental comment mentions |
| `giotto@source` exists on `gsource`, not `dev` | **confirmed** — this is why the stack targets `gsource` |
| `spatRelate` + `spatQuery` already upstream | **confirmed** — 2 files each on `gsource` |
| carry-keys guards already upstream | **false** — none of the 9 sites are on `gsource`; all still to port |
| `snapshotSave(gDirSource, giottoMulti)` missing | **confirmed** — only `("gDirSource", "giotto")` is defined. Minor doc nit alongside it: the `@returns` prose names a `giottoMulti` method as the thing capturing the mutation (`methods-snapshotSave.R:15-18`, rendered at `man/snapshotSave.Rd:43`). Returning the mutated gobject is correct on its own — the adoption pass rewrites in-memory file handles, same as for a plain `giotto` on a `gDirSource` — so this is just a stale cross-reference, not a broken rationale |

**Drift is small in GiottoClass, real in Giotto.**

- GiottoClass diverged 2026-06-02; `gsource` +7 commits; only `R/methods-wrap.R` (+10) and `R/spatial_structures.R` (+4/−1) touched on both sides. The gmulti work sits on a base that barely moved.
- Giotto diverged 2026-05-24; `gsource` +16 commits; **11 files touched on both sides**, including `dimension_reduction.R` (+105) and `pca-param.R` (+78). The param-class refactor moved fast underneath.

So: the *framework* did not drift out from under gmulti. Giotto-side analysis code did.

## 2. Port surface

| file | lines |
|---|---|
| `R/gmulti.R` | 2,268 |
| `R/methods-view.R` | 706 |
| `R/methods-resolver.R` | 629 |
| `R/methods-space.R` | 354 |
| `R/classes-view.R` | 194 |
| `R/classes-space.R` | 147 |
| `R/classes-resolver.R` | 95 |
| **implementation total** | **4,393** |
| `test-gmulti.R` | 1,516 |
| `test-view-space.R` | 755 |
| `test-gmulti-structural-ops.R` *(gmulti-v2, never tracked)* | 419 |
| **test total** | **2,690** |

Plus 8 new `Collate:` entries in DESCRIPTION, and edits to `subset.R`,
`slot_accessors.R`, `interoperability.R`, `auxilliary.R`.

## 3. Disposition vocabulary

| disposition | meaning |
|---|---|
| **Port** | bring across, review for drift, keep the design |
| **Rework** | capability is right, implementation should change on the way over |
| **Re-decide** | design question is genuinely open — do not port code that bakes it in |
| **Build** | never written; write fresh here |
| **Defer** | out of scope for first landing; revisit after |
| **Drop** | do not bring |

---

## 4. Dispositions — federation

| # | implementation | old status | disposition | why |
|---|---|---|---|---|
| 1 | `gAny` + `giottoMulti` class | Complete | **Port** | Everything depends on it. The not-`contains="giotto"` argument holds — spatial methods failing loudly beats reading empty slots |
| 2 | `@id_map` + `@id_sig` | Complete | **Port** | Registry/narrowing split is the thing that made §17 correct |
| 3 | `@source` multi-level | Complete | **Port** | Verified base choice |
| 4 | save / load round-trip | Partial | **Port + GiottoDisk work** | Needs the `giottoMulti` method GiottoDisk's docs already claim. Small, well-scoped, unblocks the last test failure |
| 5 | `@mapping` + auto-discovery | Complete | **Port** | Declaration-over-renaming is the load-bearing idea. One open question (Q1) |
| 6 | helpers consult `@mapping` | Complete | **Port** | Inseparable from 5 |
| 7 | Access layer `samples =` | Complete | **Port** | Highest value/line ratio; removed the `:::` reach |
| 8 | joint slots as cache + ground truth | Complete | **Re-decide** | The dual role *is* the root cause of the §17 bug. See Q5 |
| 9 | carry-keys discipline | Partial | **Port, staged separately** | Confirmed absent upstream. Spans 3 repos / 9 sites, and makes existing positional producers hard-error. Not a gmulti PR |
| 10 | `federatedReadHandle` | Partial | **Drop** | Zero consumers; its own doc says so. Re-add with its first real consumer (viewspace §12) |
| 11 | `@groups` | Not started | **Build** | Design settled, ~9-line resolution point. Gated on Q1–Q3 |
| 12 | combined defaults | Not started | **Build** | Real correctness bug for heterogeneous federations; cheap |
| 13 | gmulti-level spatial content | Not started, unsolved | **Defer** | Genuinely unsolved. Needs its own pass; blocks nothing today |
| 14 | joint `@spatial_network` | Not started | **Defer** | Wide accessor fan-out; wants the combined-space story first |
| 15 | `getSpatialLocations` unification | Not started | **Re-decide** | Output-shape break with wide consumer ripple. See Q6 |
| 16 | cross-sample aggregation | Not started | **Defer** | Substrate-readiness, own design pass |
| 17 | `@cell_ID` / `@feat_ID` narrowing | Complete | **Port, first** | Landing note is right: shipping the old `@id_map`-narrowing contract and revoking it later is worse than porting this up front |
| 18 | additive sample × view | Partial | **Port + finish** | Close the `giottoPoints` / `getFeatureInfo` gap during the port, not after |
| 19 | GiottoVisuals dispatcher | Complete | **Defer (4th repo)** | Outside the three-repo scope. Note two divergent variants exist |
| 20 | per-panel sizing | Not started | **Defer** | Wants the patchwork switch |
| 21 | GiottoLens | Separate repo | **No action** | Constraint only |

## 5. Dispositions — view / space

| # | implementation | old status | disposition | why |
|---|---|---|---|---|
| 1 | recipe classes + resolver | Complete | **Port + rework** | 2,125 lines, foundational. Steps become plain tagged lists; containers stay S4. See Q7 |
| 2 | view steps | Complete | **Port + rework** | Subsumed by Q7 — normalizing the crop region is a prerequisite for serializable step records, and fixes the axis-convention duplication at the same time |
| 3 | predicate vs output frame | Complete | **Port** | Clean separation, explicit-only resolution. Keep as-is |
| 4 | centroid routing | Complete, refactor pending | **Port + rework** | Routing decided by cache allocation with two patches riding on it. Its own doc calls it a workaround — fix on the way over, not later |
| 5 | space steps | Complete | **Port** | Storage shape drives the good properties |
| 6 | composition / broadcast rule | Complete, undocumented | **Port + document** | Order-sensitivity lives only in a code comment. Doc-only remainder |
| 7 | threading `view =` / `space =` through generics | Complete, scope under review | **Re-decide before porting** | 57 formals / 20 files / 71 man pages. See Q4 — the single biggest scope decision here |
| 8 | `materialize()` | Complete | **Port + fix roxygen** | Three real methods, 24 test uses, roxygen still says "stub" |
| 9 | groups as space keys | Not started | **Defer** | Depends on federation §11 and Q3 |
| 10 | composable views | Not started | **Defer** | Intentional error; semantics need a pass |
| 11 | `viewSpatRelate` | Not started | **Defer** | Prerequisite: GiottoClass `spatRelate` has 1 signature vs GiottoDisk's 7 |
| 12 | sedonadb lowering | Not started | **Defer** | Pairs with federation §10; revisit together |
| 13 | `attach_derived()` | Not started | **Drop the stub** | Body is `stop(...)`. Don't port a stub — implement when a caller needs it. Do fix the stale banner and roxygen |

**Tally:** Port 13 · Port+rework/finish 4 · Build 2 · Re-decide 3 · Defer 9 · Drop 2 ·
No action 1.

---

## 6. Decisions that gate work

Resolve before writing the code that assumes an answer.

| Q | question | gates |
|---|---|---|
| Q1 | `@mapping` entry rename — warn-and-drop dependent joint state, or block pending opt-in? Currently drops | fed 5, 11 |
| Q2 | `@groups` name collision handling — reject at registration, at resolution, or both? | fed 11 |
| Q3 | Does `giottoSpace(group=)` expand at construction (needs a gobject) or stay symbolic and expand at resolution? Symbolic breaks `names(space@samples)` as the participation set | fed 11, vs 9 |
| Q4 | **Does `space =` earn 57 formals?** Porting the wide surface then trimming is a breaking change; porting narrow then widening is not | vs 7 — port scope |
| Q5 | Should joint slots stay both lazy cache *and* ground truth? The duality caused the §17 no-op — see below for what the audit found | fed 8, 12, 17 |
| Q6 | `getSpatialLocations(mg)` — named per-child list, or one `sample::id` table? Consistency with the other joint getters vs a consumer-wide break | fed 15, 18 |
| Q7 | **Recipe steps as plain tagged lists instead of S4 step classes?** Recommend yes — see below | vs 1, 2, 4, 5, 6 |

Q4, Q5 and Q7 are the three to settle first. Q4 sets how much surface the port carries;
Q5 is where the old implementation has a demonstrated defect rather than an open
preference; Q7 is cheap now and breaking later.

### Q5 — joint slots: ground truth, lazily populated

Audited on the checkpoint. The design is "joint slots are ground truth, raw content
assembled on first access." Sound in the abstract; the implementation has no way to
police intent.

**`@mapping` covers two axes, not three.** `spat_unit` and `feat_type` get per-sample
reconciliation; the `values` name is passed **verbatim** to every child
(`.gm_assemble_expression`), and `.gm_discover_mapping()` only reads `names(g@cell_ID)` /
`names(g@feat_ID)`. So matrix names are *assumed identical across children*.

**Three implicit picks decide ground truth, all order-dependent:**

| site | code | picks |
|---|---|---|
| `.gm_assemble_expression` | `values <- common[[1L]]` | which matrix, from an intersection ordered by the first child |
| `.gm_resolve_axis(handle = NULL)` | `names(axis_map)[[1L]]` | which spat_unit / feat_type — this is fed §12 |
| `.gm_discover_mapping` | `unique(unlist(per_child))` | sets the order the two above read |

**Three silent narrowings:**

- `Filter(Negate(is.null), per_child)` — a child lacking the requested `values` is **dropped**; only *all* children failing raises. A joint matrix built from 2 of 5 samples is indistinguishable from one built from 5
- `feats_common <- Reduce(intersect, ...)` — features silently intersected
- cmeta assembly intersects columns "to avoid NA inflation"

**One mislabel**, admitted in a source comment: the joint `exprObj` inherits the *first
child's* spat_unit / feat_type tags even when `@mapping` reconciled several.

#### Decisions

**Give `@mapping` a third axis for `values`.** All three keying axes then reconcile the
same way, and there is no axis left that assembly infers rather than reads.

- discovery **seeds every participating sample to `"raw"`** — the ingest convention (`create.R:458`, `:849`; `setExpression` default)
- populating the mapping **fails loudly, naming the objects** that have no `"raw"`
- two remedies, both explicit and both the user's call: rename the child's matrix to `"raw"`, or edit that sample's mapping entry to point at whatever it *is* called — `c(A = "raw", B = "counts")`

This is strictly better than hardcoding `"raw"` in the assembly path: renaming data is no
longer the only escape, and federating a deliberately non-raw matrix stays available by
declaring it. `"raw"` is a *default in the manifest*, not a rule in the code.

**Keep the `cbind`-validity caveat as declaration-time friction, not a ban.** A matrix
name does not say whether concatenation is statistically valid: per-cell derivations
(library-size lognorm) concatenate fine, since each column used its own library size;
per-gene or per-batch derivations (scaled / z-scored, HVG-subset, batch-corrected) do not,
because each child was standardized to its own gene statistics, making the joint matrix
silently incomparable across samples. So declaring `values = c(A = "scaled", B = "scaled")`
should warn — the user is explicitly asking for it, which is the right place for the
friction. The default seed of `"raw"` means nobody arrives there by accident.

**Open sub-question — is the `values` axis flat or per-universe?** `spat_unit` and
`feat_type` are independent axes intersected by `.gm_resolve_participation`, but
expression names are nested *under* a pair: `@expression[[su]][[ft]][[name]]`. Either

- **flat** — one `values` map applied within whichever universe resolves. Simpler, matches the common case where every universe uses the same matrix names. *Recommended.*
- **per-universe** — `mapping$values[[su]][[ft]][[handle]]`. Exact, but a third nesting level in a structure users hand-edit.

Start flat; per-universe is an additive change if a real case needs it.

**Remaining changes:** replace the two `[[1L]]` picks with mapping lookups or a loud
error; give assembly an `on_missing = c("error", "drop", "fill")` defaulting to error;
stamp the participation set and per-sample resolved names onto a joint slot when it is
written; make `materialize()` the only writer, with getters assembling and returning
without caching back (accidentally already true — make it the contract).

#### Q5b — `@mapping` and `giottoSpace` are the same construct

`@mapping` should be a **recipe slot**, not a lookup table: when joint content does not
exist, consult the declaration for the handle and build it on the spot. The default entry
(`"raw"` everywhere) is just a seeded recipe, so a failing default and a failing user-set
entry go down the **same code path and produce the same message**. That unified failure is
the point — a privileged hardcoded default would fail differently from a user's.

Once framed that way, `@mapping` and `giottoSpace@samples` are the same thing: a
**per-sample keyed recipe** answering "how do I get this sample's contribution to a joint
thing." Mapping says how to *find* it (sample → child-level name); space says how to
*place* it (sample → ordered step list).

**Both have the same defect today, with opposite visible effects.**

| | non-participating child |
|---|---|
| expression federation | silently **dropped** (`Filter(Negate(is.null), per_child)`) |
| space resolution | silently **included at untransformed native coordinates** (`methods-resolver.R:294-301`) |

Space appears better only because `GiottoVisuals/R/gmulti.R:203` errors on samples outside
the space's key set — that check lives in **one consumer**, not in the engine.
`.space_sample_key_for` likewise returns `NULL` on ambiguity and
`.apply_space_to_subobj` then returns the subobject untouched. Called directly rather than
through the plot dispatcher, a non-participating child comes back silently mispositioned —
which is harder to notice than a missing one.

**Three rules both should follow:**

1. **Participation is the key set** — declared, never inferred from what happens to exist
2. **Keyed but unsatisfiable is a loud error naming the sample** — child lacks `"raw"`; step references a missing slot. Applies to the seeded default exactly as to a user entry
3. **Not-keyed has declared semantics** — mapping: does not contribute; space: *excluded*, not silently identity. Deliberate identity already has vocabulary (`giottoSpace("a")` with an empty step list), so it should require the empty key

**Default seeds legitimately differ, and this should be stated rather than discovered:**
mapping seeds **every child** (non-spatial federation is what a gmulti is for); space seeds
**nothing** (cross-sample spatial is opt-in). Same mechanism, opposite default.

**Entry shape — keep named character.** "Recipe" means assembly is driven by the
declaration, not that entries carry arbitrary steps. Policy (`on_missing`, later `combine`)
belongs on the handle, which is where the uniform failure lives. Entries are already the
same shape as space's keys, so upgrading to step lists later is additive — whereas going
there now invites per-sample transforms on expression and reopens the `cbind`-validity
problem this closes.

#### Q5c — parent handles vs child names

`@mapping` is parent-handle → per-sample child-level name, on both axes:
`names(mapping$spat_unit)` is the gmulti's own vocabulary; values are
`c(<sample> = "<child name>")`. `.gm_validate_mapping` constrains the **child** side —
samples must exist in `@objects`, and each child slot name must exist on that child
(`cell_ID` / `feat_ID`). The handle side is unconstrained, which is right for a parent
namespace. Joint slots and `@cell_ID` / `@feat_ID` are keyed by parent handles
(`.gm_narrowing_keys` treats `names(@mapping[[axis]])` as the authoritative universe).

The two-namespace design is sound. It leaks in three places, all the same
infer-rather-than-declare pattern:

1. **Discovery seeds handle == child name**, so by default the namespaces are indistinguishable — nothing marks a handle as a deliberate parent name versus an auto-discovered passthrough
2. **`.gm_resolve_axis` silently escapes into the child namespace** — an undeclared handle falls back to scanning children for a slot of that name. A handle that was never declared still "works" if some child happens to carry it
3. **Joint objects carry child-side tags.** Admitted in-source: the joint matrix inherits its `spat_unit` / `feat_type` tags from the first child's template "even though the cells across children may have come from different per-child spat_units." So `mg@expression[["nucleus"]][["rna"]]` can hold an `exprObj` whose own `@spat_unit` is `"nucleus_v1"` — one sample's private name, chosen by child order. `.gm_assemble_cell_metadata` uses the same first-child-as-template pattern

(3) is a live risk rather than an inconsistency: anything reading `spatUnit()` / `featType()`
off an assembled joint object gets a child's private name instead of the federated handle.

**It is an oversight, and it is fixable.** The template itself is legitimate — assembly
needs a well-formed `exprObj` shell and cloning the first child's is the cheap way to get
one. Only the identity tags should not ride along with the structure. Two changes:

1. **`.gm_resolve_axis` / `.gm_resolve_participation` must return the resolved parent handle.** Today they return only per-sample `list(su = <child_su>, ft = <child_ft>)`. When the caller passes `spat_unit` / `feat_type` explicitly the handle is already in scope; when they are `NULL`, `.gm_resolve_axis` picks `names(axis_map)[[1L]]` internally and **discards it**. That is the only reason this isn't a one-liner
2. **Assembly stamps the handle** — `spatUnit(template) <- su_handle`, `featType(template) <- ft_handle`. The setters exist and dispatch correctly (`exprObj` → `spatFeatData` → `spatData` + `featData`), so the `ANY` no-op method is not in play

**All four identity tags follow the same rule** — they should describe the federation, not
whichever child sorted first:

| tag | today | should be |
|---|---|---|
| `@spat_unit` | first child's child-level name | parent handle |
| `@feat_type` | first child's child-level name | parent handle |
| `@name` | first child's expression name | parent handle for the `values` axis |
| `@provenance` | first child's provenance (via `spatData` → `provData`) | **the parent `spat_unit` handle** |

`@name` is only accidentally correct today because `values` is assumed identical across
children; Q5a removes that assumption. `.gm_assemble_cell_metadata` uses the same
first-child-as-template pattern and needs the same fix.

**Provenance follows the existing convention** — `prov(res) <- spatUnit(x)` is already what
`aggregate.R:616`, `:718` and `classes-binpoints.R:152` do. Two reasons it is also right
here: the slot is typed `"ANY"` so the plural form stays available if a federation ever
spans several handles, and the per-child origins (`"nucleus_v1"`, `"nuc"`) are **already
recorded in `@mapping`** — copying them into `@provenance` would create a second copy that
can drift from the declaration. Parent handle keeps the artifact self-consistent and leaves
per-sample origin with the declaration that owns it.

Auditing downstream readers of these tags belongs in stage 3.

**Precedent — GiottoDisk ADR 0006**, *view state is not chain state; window-dependent ops
bake at push time*. `libraryNormParam` runs its aggregate in the producer and freezes the
factors into the record, so afterwards the record is a pure function of a value and an
axis id. Same move: if joint content depends on which children participated, bake the
participation set in at materialization so a later read cannot silently mean something
else. Related shape in other systems: manifest-not-inference (Iceberg, Hive), build
provenance stamped on the artifact (Iceberg snapshots, Nix/Bazel input hashes), explicit
union modes rather than silent intersection (SQL `INNER` vs `FULL OUTER`), and an explicit
compute boundary (`dbplyr::compute()`).

---

### Q7 — list recipes vs S4 step classes

Proposal: `viewFilter` / `viewCrop` / `viewSampleSelect` / `spaceTransform` become plain
tagged lists (`list(type = "crop", region = ..., relation = ...)`). `giottoView` and
`giottoSpace` stay S4.

**For:**

- **The step classes carry no dispatch.** `setMethod`/`signature` uses: `viewFilter` 0, `viewCrop` 0, `viewSampleSelect` 0, `spaceTransform` 1 (`show`), `viewStep` 1 (`show`). The resolver already filters them as type tags via `inherits()` at six sites — `s$type == "crop"` is a mechanical substitution.
- **Converges with GiottoDisk.** `@ops` there is already `list(type = ..., ...)` folded by `switch()` arms, chosen because records must survive `saveRDS` and reach parallel workers. The recipe layer solved the same problem differently in the same ecosystem.
- **Readable and editable** — a recipe becomes inspectable and hand-editable without S4 accessors, and JSON export becomes possible rather than blocked.

**Keep containers S4** — `giottoView` / `giottoSpace` carry ~20 real methods (`+`, `show`, `materialize`, `subset`, `crop`, `selectSamples`, `spin`, `affine`, `flip`, `rescale`, `shear`, `zoom`), `giottoSpace` validity, and slot typing on `gobject@view` / `@spaces`.

**Prerequisites — three non-serializable payloads, each already worth fixing:**

| blocker | fix |
|---|---|
| `viewFilter@env` is an environment | `.eager_substitute_env()` already inlines env-resident scalars/vectors to make the predicate self-contained; `env` is a residual fallback for functions and missing names. Deparse the predicate to a string and drop the slot |
| `viewCrop@region` is `"ANY"`, may hold `SpatExtent` / `SpatVector` — terra pointer-backed, not RDS-safe without `wrap()` | **Accept typed geometry at the call site, convert to WKT at record time.** Adopt GiottoDisk's existing cascade (below) rather than defining a second policy |
| `spaceTransform@args` is an open `...` passthrough | Per-op normalization or whitelist |

**Geometry recording — adopt GiottoDisk's cascade.** `GiottoDisk@dev/R/methods-spatRelate.R`
already solves this for the op chain, and `viewCrop` should mirror it rather than invent a
second policy:

- the **WKT `character` method is the canonical entry**; the record carries a plain string (`y_wkt`)
- typed inputs coerce and recurse — `SpatVector` does `terra::geom(y_use, wkt = TRUE)` then calls the WKT method; `sf` / `sfc` likewise
- multi-feature inputs are unioned (`terra::aggregate` / `sf::st_union`) into one geometry before serializing
- verbosity is bounded: features capped at `getOption("giottodisk.spatrelate_inline_max", 1000L)`, with an error directing large query sets to the store/store path
- WKT is geometry-only — attributes are not carried through

Two payoffs beyond serialization: the disk path receives WKT with **no conversion at resolve
time**, and the terra `(xmin, xmax, ymin, ymax)` convention is applied exactly once, at
`numeric(4)` → WKT, instead of being re-derived per substrate. `.materialize_crop_region`'s
pass-the-numeric-through branch — the source of the duplication — disappears.

**CRS — settled, reuse what exists.** The record carries **WKT only, no CRS field**.
GiottoDisk already resolves SRID on the *store* side via `.spatrelate_store_srid()`
(`methods-spatRelate.R:52`): `4326L` when the store has no CRS — the biology-data case —
`EPSG:NNNN` parsed when it does, `NA` otherwise so a mismatch surfaces as a sedonadb error
rather than a silent wrong answer. Writes set `edges = "planar"` explicitly
(`utils-spatial.R:254`) so no engine infers spherical. Per-engine differences are handled:
sedona gets the SRID literal, duckdb deliberately omits it (`ST_GeomFromText` takes no SRID
arg), terra is indifferent.

This is also the *correct* side for it. A recipe is re-resolved against current state by
design, so a CRS baked into a step record could assert something the store it resolves
against disagrees with. Keep CRS authoritative where it is.

Still unverified: whether the emitted WKT precision round-trips without shifting a crop
boundary. One check during stage 4.

**Correct while porting:** the current docs claim steps are "pure data appended in order —
no closures — so a view survives serialization and travels to parallel workers." That is
**false today**: an environment reference is as unserializable as a closure. Q7 makes the
claim true; until then the claim should not be repeated.

**Cost / timing:** touches the ~2,125-line view/space surface and changes the serialized
form of every slotted recipe. Nearly free during a fresh port; a breaking change to saved
gobjects afterwards. Lost: slot type checking at construction — recover with a constructor
plus one validator per step type, as GiottoDisk does.

## 7. Suggested stages

Each stage should build, test, and be independently reviewable.

| stage | contents | repo(s) |
|---|---|---|
| **0** | this plan; decide Q4 + Q5 | GiottoClass |
| **1** | fed 1, 2, 3 — class, registry, source | GiottoClass |
| **2** | fed 17, 18 — narrowing contract + additive composition, incl. the points/feature gap | GiottoClass |
| **3** | fed 5, 6, 7 — `@mapping` and the access layer | GiottoClass |
| **4** | vs 1, 2, 3, 5, 8 — recipe classes, resolver, steps, frames, `materialize()`, at the scope Q4 sets | GiottoClass |
| **5** | vs 4, 6 — centroid-routing refactor + broadcast-rule docs | GiottoClass |
| **6** | fed 4 — round-trip, incl. the missing `snapshotSave` method | GiottoDisk + GiottoClass |
| **7** | fed 11, 12 — `@groups`, combined defaults | GiottoClass |
| **8** | fed 9 — carry-keys, on its own | GiottoClass + Giotto |
| **9** | fed 19, 20 — dispatcher, panel sizing | GiottoVisuals |

Stages 1–5 are the substance and are GiottoClass-only. Giotto is untouched until stage
8, which is convenient given that's where the drift is.

## 8. Risks

- **Nothing is backed up.** The three checkpoint branches have no remote; ~106 commits exist only on local disk. Push before relying on them as the port source.
- **Two divergent GiottoVisuals variants** — `wip/gmulti-visuals-checkpoint` and `GiottoVisuals-federation-design` differ. Reconcile before stage 9, and note the former calls `GiottoClass:::.gm_inject_joint_metadata`, which federation §7 claims to have removed the need for.
- **`test-gmulti-structural-ops.R` (419 lines) was never tracked** and has no counterpart on the federation branch. Read it before stage 2 — it may cover cases `test-gmulti.R` doesn't.
- **`design.Rmd` has no gmulti coverage.** It documents the object model, schema, `initialize()`, and accessors — all of which gmulti extends. It needs a section before any of this lands on `gsource`.
- **Dispositions here are from the docs plus targeted verification, not a full read of 4,393 lines.** The Port items are judged on design rationale; drift and code quality inside them is unverified. Treat per-item review as part of each stage, not as done.

---

*Companion to the [implementation plan](IMPLEMENTATION_gmulti.md). Created 2026-08-12.*

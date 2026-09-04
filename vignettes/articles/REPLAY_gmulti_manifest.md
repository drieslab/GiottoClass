# Replay manifest — gmulti + view/space onto current upstream

Decides, per stage, **which of the two in-progress tracks each file comes from** and
**which pending edits fold in there**. Companion to [PLAN_gmulti2_port.md](PLAN_gmulti2_port.md),
which owns the dispositions and the design decisions (Q1–Q7); this page owns provenance
and sequencing only. Where they disagree, the PLAN wins on design and this page wins on
"which tree do I copy from".

Written after a merge attempt was aborted in favour of a replay. Every claim below was
checked against the branches on 2026-08-19, not inferred from the earlier docs.

---

## 1. Verified inputs

| ref | SHA | note |
|---|---|---|
| GiottoClass `upstream/gsource` | `284bf129` | **replay base.** Fully contains `upstream/dev` (`688ba880`) — verified ancestor, so "onto upstream dev" resolves here |
| GiottoClass `feature/gmulti2` | `4e43cf52` | staged track. Ahead 20 / behind 4 |
| GiottoClass `merge/federation-into-gsource` | `0574697b` | checkpoint track (merge of `feature/gmulti-federation-design` @ `4f58d861`). Ahead 61 / behind 4 |
| shared merge-base of both tracks | `b351ed2b` | local `gsource` merge of `feature/gany-dispatch`; the `5e50c38f` recorded earlier (PR #389) is its ancestor. **Diff from here, never from the upstream tip** — see §3 trap 1 and §9 step 0.5 |
| GiottoClass `feature/gany-dispatch` | `781998a8` | gAny extraction cut for independent upstreaming — 17 files, +72/−44, no gmulti code, no-op while `giotto` is the only concrete `gAny`. Ships first, see §9 |
| GiottoDisk `upstream/dev` | `a13f8e9` | |
| GiottoDisk `merge/federation-into-dev` | `28beb2d` | already merged to current upstream; 0 behind |
| GiottoDisk `feature/gmulti2` | `58253ba` | 0 ahead / 54 behind — **empty, nothing to take** |
| GiottoVisuals `upstream/gsource` | `eebbfc9` | |
| GiottoVisuals `feature/gmulti-federation-design` | `5969cb6` | 3 commits; the more developed variant |
| GiottoVisuals `wip/gmulti-visuals-checkpoint` | `54f19e9` | 1 commit; **diverged**, not an ancestor |

Track sizes from the shared base (`R/` + `tests/` only):

- checkpoint: **8,797 insertions / 417 deletions across 39 files** — all 8 implementation files
- staged: **1,490 / 71 across 14 files** — `R/gmulti.R` (579 lines) + gAny; no view/space files at all

---

## 2. Source-selection rule

**For any file both tracks touch, the staged track (`feature/gmulti2`) is the source.**
Four pieces of evidence, all verified:

1. **The class definitions are slot-identical** — the staged `giottoMulti` already declares
   the full slot set including `@mapping`, `@view`, `@spaces`. Stage 3+ content layers on
   with no class change.
2. **The staged roxygen is already correct where the checkpoint's is stale.** The
   checkpoint's `subset-giottoMulti` still describes the retired contract ("Eager: `@id_map`
   is narrowed"); the staged version reads "the surviving set is recorded on `@cell_ID` /
   `@feat_ID`" and "`@id_map` ... records identity, not selection". This is A5 already fixed.
3. **The staged tests are better scoped** — 38 `test_that` blocks / 395 lines with a header
   stating what is deliberately deferred, versus the checkpoint's 103 blocks / 1,516 lines
   covering everything at once. The staged file also documents *why* `@mapping` discovery is
   tested alongside the narrowing contract (`.gm_narrowing_keys()` depends on it).
4. **It is native to the #381 accessor contract.** The checkpoint needed a dedicated
   adaptation commit (`8ecd45a8`) to get there; the staged branch was cut after #381 landed.

Everything the staged track does not contain — the whole federation API, all of view/space,
the spatial-domain fan-out — comes from the checkpoint.

---

## 3. Replay mechanics and three traps

Take content diffs **from `b351ed2b`** (the true shared base, which already contains the
gAny extraction), apply onto a fresh branch cut from `upstream/gsource` **after the gAny
PR lands** (§9 step 0.5), and split into stage commits. Do not re-derive the work by hand.
Diffing from `5e50c38f` instead would re-include the gAny content and double-apply it once
the gAny PR is upstream. `b351ed2b` also predates PRs #390/#391, so trap 1 stays avoided.

**Trap 1 — diffing from the upstream tip deletes 1,033 lines of just-landed upstream work.**
`upstream/gsource..0574697b` shows the checkpoint "removing" `R/tif.R` (846 lines, PR #391
JPEG-2000/VRT) and `R/nn_search_hnsw.R` (187 lines, PR #390), plus `test-tif.R` (240) and
`test-nn-search-hnsw.R` (115). Those are files the branch predates, not deletions. Both must
survive the replay untouched.

**Trap 2 — do not replay either track's hnswKNN work.** Upstream re-implemented it
independently (PR #390). Verified separable: the staged `R/NN_network.R` diff contains **zero**
gmulti/gAny references — it is purely the engine work (adding an `"auto"` default). Drop from
the replay: `R/NN_hnsw.R` (151), `tests/testthat/test-NN_hnsw.R` (93), and all
`R/NN_network.R` changes on both tracks. This removes 6 of the 7 conflicts the merge hit.
See D4 for the one intent question it leaves open.

**Trap 3 — docs and code authority are split.** `PLAN_gmulti2_port.md` and the Q1/Q4/Q5/Q7
resolutions exist **only** on `feature/gmulti2`; the checkpoint carries the older
IMPLEMENTATION/DESIGN/STATUS set and no PLAN. Docs come from the staged track; the
IMPLEMENTATION pages need the §6 corrections applied as they land.

---

## 4. Per-stage manifest

Stage numbering follows PLAN §7. "A*" items are from the plan-vs-merge difference list.

### Stage 1 — class, registry, source (fed 1, 2, 3)

| | |
|---|---|
| **source** | `feature/gmulti2` **as-is** — already reviewed and tested |
| **files** | `R/gmulti.R` §CLASS/INITIALIZE/CONSTRUCTOR/INTROSPECTION + `.gm_resolve_source` / `.gm_compute_sig` / `.gm_build_{cell,feat}_idmap` / `.gm_resolve_objects`, DESCRIPTION, NAMESPACE. The gAny files (`R/classes-virtuals.R`, `R/classes.R` +1) landed upstream via PR #392. *Correction:* `R/methods-wrap.R` (+10) was listed here in error — those lines are adr/0005 deprecation roxygen from `ad66a280` (§5 do-not-replay), not gmulti content |
| **tests** | `test-gmulti.R` blocks: class + gAny dispatch, constructor validation, id_map registry, id_sig caching, `@source` |
| **folds in** | nothing — this stage is done |

### Stage 2 — narrowing contract + additive composition (fed 17, 18)

| | |
|---|---|
| **source** | `feature/gmulti2` for the contract (`1cde445c`) and `.narrow_subobject()`; **checkpoint** for the per-child spatial narrowing (`.gm_narrow_child_outputs`, `.gm_subobj_filter_by_local_ids`) |
| **files** | `R/gmulti.R` §SUBSET + `.gm_narrowing_keys` / `.gm_apply_view`, `R/subset.R` (staged +87 supersedes checkpoint +92 — verify equivalence), `R/auxilliary.R` `.narrow_subobject`, `R/slot_accessors.R` |
| **folds in** | **A6** — close the feature-axis gap here |

**Synthesis point.** The two tracks built complementary halves of subobject narrowing and
neither covers `giottoPoints`:

- staged `.narrow_subobject(x, cells, feats)` — the **joint-slot** classes: `exprObj`,
  `cellMetaObj`, `featMetaObj`, `spatEnrObj`, `dimObj`, `nnNetObj`. Its header calls itself
  the "single source of truth for which axis of each subobject class is cell-keyed and which
  is feature-keyed"
- checkpoint `.gm_subobj_filter_by_local_ids` — the **spatial** classes: `spatLocsObj`,
  `spatialNetworkObj`, `giottoPolygon`

Close A6 by extending `.narrow_subobject` with the spatial classes **including
`giottoPoints`** (feature axis) and having `.gm_subobj_filter_by_local_ids` delegate to it,
rather than replaying two class-dispatch tables that must stay in sync. Also wire
`getFeatureInfo` into `.gm_narrow_child_outputs` — currently only 3 of 4 call sites are wired.

### Stage 3 — `@mapping` and the access layer (fed 5, 6, 7)

| | |
|---|---|
| **source** | **checkpoint only** — `R/gmulti.R` §MULTI-SPECIFIC ACCESSORS (`idMap`, `gmultiMapping` + 3 setter forms, `.gm_validate_mapping`, `.gm_invalidate_joint_for_mapping_change`, `.gm_axis_changed_keys`), §SHARED-DOMAIN OVERRIDES (`getExpression` / `getCellMetadata` / `getFeatureMetadata`), the `.gm_assemble_*` family, `.parse_sample_qualified_name`, `.gm_slice_to_samples`, `.gm_resolve_axis`, `.gm_resolve_participation`, §SPATIAL-DOMAIN per-child dispatch, plus `R/combine_metadata.R` |
| **tests** | extract the `@mapping` / access-layer blocks from the checkpoint's 1,516-line `test-gmulti.R` |
| **folds in** | **A2 (Q5, all three axes at once)** and **A3 (Q1)** — this is the heaviest stage |

A2/A3 concretely, in the code as replayed:

- add the `values` axis to `@mapping`; auto-seed `"raw"` per participating sample (flat, not per-universe)
- replace `values <- common[[1L]]` (checkpoint `R/gmulti.R:1086`) with a mapping lookup or loud error
- make `.gm_resolve_axis` / `.gm_resolve_participation` **return the resolved parent handle** instead of discarding it
- `on_missing = c("error", "drop", "fill")`, default `error`, replacing the silent `Filter(Negate(is.null), per_child)` drops (checkpoint `:1101`, `:1158`) and the silent feature/cmeta intersects
- stamp participation set + per-sample resolved names on a joint slot at write
- **Q5c identity tags** — stamp the parent handle on `@spat_unit` / `@feat_type`, parent `values` handle on `@name`, parent spat_unit handle on `@provenance`; applies to `.gm_assemble_expression` and `.gm_assemble_cell_metadata`, both of which use first-child-as-template
- `NA_character_` deliberate-skip sentinel, plus the `is.na()` skip in `.gm_validate_mapping`
- block-on-expansion for materialized universes; auto-seed on child-add

**Discovery straddles stages.** `.gm_discover_mapping` ships in stage 1 (it is in the staged
`R/gmulti.R`), but the `values`-axis seeding and the NA sentinel are stage-3 edits to it.
Expect to touch a stage-1 file during stage 3, or land the discovery half of A2/A3 early —
decide which and be consistent, because the stage-1 tests assert the seeded shape
("empty giottoMulti seeds `@mapping` with both axes" becomes three axes).

### Stage 4 — recipe classes, resolver, steps, frames, `materialize()` (vs 1, 2, 3, 5, 8)

| | |
|---|---|
| **source** | **checkpoint only** — `R/classes-view.R` (194), `R/classes-space.R` (147), `R/classes-resolver.R` (95), `R/methods-view.R` (706), `R/methods-space.R` (354), `R/methods-resolver.R` (629); `test-view-space.R` (755) |
| **scope** | full `view =` / `space =` surface per the resolved Q4 (57 + 43 formals) |
| **folds in** | **A1 (Q7)**, **A4 (drops)**, **A5 (stale roxygen)** |

Q7 is the reason this stage is worth replaying rather than merging: converting the five step
`setClass`es to tagged lists changes the serialized form of every slotted recipe, which is
near-free now and breaking later. Its three prerequisites land here too — deparse the
predicate and drop `viewFilter@env`; convert `viewCrop@region` to WKT at record time via
GiottoDisk's existing cascade (WKT `character` canonical, typed inputs coerce and recurse,
multi-feature union, inline cap, **no CRS field** — store-side SRID stays authoritative);
normalize or whitelist `spaceTransform@args`.

Do not replay: `R/classes-federatedRead.R` (`federatedReadHandle` — zero consumers) or the
`attach_derived` generic + `stop()` stub. Fix on the way in: the `materialize()`
"`\strong{Status:} stub`" roxygen (2 sites) and the `# Mutation escape hatches (stubs) ####`
banner, both false — `materialize()` has three real methods and 24 test uses.

### Stage 5 — centroid-routing refactor + broadcast docs (vs 4, 6)

| | |
|---|---|
| **source** | checkpoint, reworked |
| **folds in** | **A7** — decide routing on `(predicate relation, polygon source availability)` and demote the cache to pure memoization, dropping the force-cache / force-`NULL`-cache patches. **A8** — document the `.space_record` broadcast rule and its order-sensitivity in `vignettes/view_and_space.Rmd` |

### Stage 6 — save/load round-trip (fed 4)

| | |
|---|---|
| **source** | GiottoDisk `merge/federation-into-dev` @ `28beb2d` — **no replay needed**, but see the stage-5 progress note: A7's two force-cache patches live here and must be dropped in favour of GiottoClass's `crop_relation_needs_geom()`, which needs exporting at that point |
| **state** | already merged to current `upstream/dev`, 0 behind, all R parses. Unique payload is 2,978 lines / 20 files, all gmulti-relevant: `parquetCoordinator` + `methods-resolveSubobject.R` (1,148), `snapshotSave(gDirSource, giottoMulti)` (105), `snapshotDelete` child cascade, spatRelate widening, `class-viewCoordinator.R`, `test-snapshot-gmulti.R` (166), `test-view-resolver.R` (926) |
| **note** | `parquetExprBase` + union streaming PCA are **already upstream** (PRs #44, `514cd30`); the merge deduplicated them and PCA now runs through upstream's `.pe_windows()` seam. Nothing to port |
| **verify** | `test-snapshot-gmulti.R` skips unless `@source` is a `giottoMulti` slot, so it is inert until the replayed GiottoClass is installed — re-run it after stage 1 lands |

### Stage 7 — `@groups`, combined defaults (fed 11, 12)

Build fresh; nothing exists on either track. Gated on **D2/D3** below.

### Stage 8 — carry-keys (fed 9)

| | |
|---|---|
| **source** | checkpoint for the GiottoClass sites (`R/auxilliary.R` key auto-detect + giottoMulti hard error, `create_average_DT` / `_detection_DT` reorders, `R/interoperability.R` reorders) |
| **blocked** | **the Giotto-repo producer guards are in neither merge branch** — `adjustGiottoMatrix`, `runDWLSDeconv`, `runGiottoHarmony`, `findScranMarkers`, `giottoToAnnDataZarr`, `cal_cell_niche_cluster_bin`. Locate them on the Giotto side before this stage; if they were never committed, this stage includes writing them |
| **why separate** | the giottoMulti hard error makes every positional producer fail loudly. Landing GiottoClass without the Giotto side means those verbs error on a gmulti. See **D1** |

### Stage 9 — GiottoVisuals dispatcher, per-panel sizing (fed 19, 20)

| | |
|---|---|
| **source** | `feature/gmulti-federation-design` @ `5969cb6` as the base — 3 commits, `+261/-166` over the other variant, and the `GiottoClass:::` reach is gone (verified: no references) |
| **selective** | `wip/gmulti-visuals-checkpoint` @ `54f19e9` has **1 unique commit** bundling two things: "joint metadata injection" (superseded — see §6) and "composite plot_output_handler" (independently useful). Cherry-pick the handler only; do not take the injection |
| **note** | both variants are 7 behind `upstream/gsource` (`eebbfc9`) |

---

## 5. Do not replay

| item | why |
|---|---|
| `R/NN_hnsw.R`, `test-NN_hnsw.R`, all `R/NN_network.R` changes (both tracks) | upstream PR #390 owns this; verified zero gmulti content |
| `R/classes-federatedRead.R` | zero consumers; re-add with sedonadb lowering |
| `attach_derived` generic + stub | body is `stop()` |
| checkpoint `subset-giottoMulti` roxygen | describes the retired contract; staged version is correct |
| local `gsource` commits `ad66a280`, `68d7b8c3`, `7ce3fe13` | design article + hnswKNN; not gmulti, and hnsw is superseded |
| **anything that touches** `R/tif.R`, `R/nn_search_hnsw.R` | new upstream work; must survive intact |

---

## 6. Doc corrections owed (new findings, not in PLAN)

1. **`.gm_inject_joint_metadata` is not gone.** Federation §7 states "`.gm_inject_joint_metadata`
   and the `GiottoClass:::` call are gone." Only the cross-package `:::` reach was removed —
   the helper is alive at checkpoint `R/gmulti.R:2082` with **two live callers** at
   `R/combine_metadata.R:44` and `:275`. Either keep it as an acknowledged in-package
   implementation detail (and fix the doc), or finish the job by routing `combine_metadata`
   through the access layer. Decide during stage 3; this is also what
   `wip/gmulti-visuals-checkpoint`'s unique commit re-introduces cross-package, hence §4 stage 9.
2. **`snapshotSave(gDirSource, giottoMulti)` now exists** — federation §4 "Partial", hub §6,
   and PLAN §1's "confirmed missing" row are all stale.
3. **PLAN §8's "GiottoDisk carries a large non-gmulti payload" is wrong** — `parquetExprBase`
   and union PCA were already upstream; the branch's unique content is gmulti-relevant.
4. **The unpushed giotto-view WIP is committed** (`f092ac8`), so hub §6's stale-`gmulti_libs`
   prerequisite is partly resolved.
5. **A5's stale-roxygen finding applies to the checkpoint only** — the staged track fixed it.

---

## 7. Decisions that gate stages

| D | question | gates |
|---|---|---|
| **D1** | Carry-keys: add a Giotto merge branch, soften the giottoMulti hard error until Giotto lands, or accept loud-fail with release notes? **Decided — loud-fail + release notes; see §9** | stage 8, and the release note for stage 3 |
| **D2** | `@groups` name-collision handling — reject at registration, resolution, or both? (PLAN Q2) | stage 7 |
| **D3** | `giottoSpace(group=)` — expand at construction or stay symbolic? (PLAN Q3) | stage 7, stage 4's `.resolve_samples` |
| **D4** | Does upstream's restored hnswKNN preserve the local intent — "engine defaults from network space" / `"auto"` default? If not, that is a **separate** upstream-facing change, not part of this replay | nothing here; do not let it re-enter the gmulti branch |
| **D5** | `getSpatialLocations(mg)` output shape — per-child list or one `sample::id` table? (PLAN Q6) | deferred; keep the checkpoint's list shape for now |
| **D6** | `.gm_inject_joint_metadata` — keep or finish removing? (§6 item 1) **Decided — finish removing, in stage 3; see §9** | stage 3, stage 9 |

---

## 8. Verification per stage

Each stage builds, tests, and is independently reviewable (PLAN §7). Additionally:

- **stage 1–2**: the staged `test-gmulti.R` must stay green after the `@mapping` axis count changes in stage 3 — expect to update the "seeds `@mapping` with both axes" assertion
- **stage 3**: A2's whole point is that silent-wrong becomes loud, so assert on the **error paths** (child missing `"raw"`, non-participating sample, NA sentinel) and on the stamped identity tags — a Pearson-style parity check cannot see a mislabelled handle
- **stage 4**: after Q7, assert a recipe survives `saveRDS` → `readRDS` → resolve, and that WKT round-trips without shifting a crop boundary (PLAN Q7 flags this as unverified)
- **stage 6**: needs an installed replayed GiottoClass before `test-snapshot-gmulti.R` does anything — its `skip_if` is on the `@source` slot
- **shared library**: rebuild `gmulti_libs` deliberately before trusting cross-package test results

---

## 9. Final path (decided 2026-09-04)

Branch states re-verified 2026-09-04: **no upstream in any of the three repos has moved**
since §1 was recorded — `284bf129` / `a13f8e9` / `eebbfc9` are still the tips, and every
track SHA and ahead/behind count above still holds. The GiottoDisk main worktree is dirty,
but on `refactor/points-drop-wkb` (points/WKB + stat-accumulator ADR) — unrelated, not
hidden gmulti work.

**Step 0 — push everything, unconditionally.** No source branch in any repo has a remote
copy; PLAN §8's "~106 commits exist only on local disk" now also covers stages 1–2 and
both plan documents. Back up to `origin` (jiajic): GiottoClass `feature/gmulti2`,
`merge/federation-into-gsource`, `feature/gmulti-federation-design`,
`feature/gany-dispatch`; GiottoDisk `merge/federation-into-dev`, `feature/giotto-view`;
GiottoVisuals `feature/gmulti-federation-design`, `wip/gmulti-visuals-checkpoint`.

**Step 0.5 — land gAny upstream first.** Push `feature/gany-dispatch` to
`giotto-suite/GiottoClass` and PR to `gsource`. It was cut for exactly this, is
behaviourally a no-op while `giotto` is the only concrete `gAny`, and removes 17 files of
accessor-signature lifts from the gmulti PR's review surface. Consequence: the replay diff
base is **`b351ed2b`** (§3), and the fresh branch is cut from the post-merge `gsource` tip.

**Then stages 1–9 exactly as §4**, with these resolutions:

- **Discovery straddle (§4 stage 3):** stage 1 lands `feature/gmulti2` as-is (two-axis
  seed); the `values`-axis seeding and NA sentinel are stage-3 edits to
  `.gm_discover_mapping`, and the "seeds `@mapping` with both axes" assertion is updated in
  the same stage-3 commit. Stage 1's provenance claim ("as-is, already reviewed") stays true.
- **D1 — loud-fail + release notes.** No existing user code passes a `giottoMulti` to
  positional producers; loud-beats-silent is the same principle A2 enforces. Stage 8
  includes *writing* the Giotto-side producer guards (in neither merge branch), as its own
  Giotto PR to `gsource`.
- **D6 — finish the removal in stage 3.** `combine_metadata.R` is replayed there anyway and
  the access layer it should route through lands in the same stage. Federation §7's claim
  becomes true instead of amended, and stage 9's injection commit stays dropped.
- **D2 / D3 — stay open.** They gate only stage 7 (build-fresh) and stage 4's
  `.resolve_samples` seam; decide when stage 7 starts, block nothing before it.
- **D4 confirmed** — upstream hnsw intent is checked separately, never on this branch.
  **D5 confirmed** — keep the checkpoint's per-child list shape.
- **Stage 2 commit checklist:** the "staged `subset.R` +87 supersedes checkpoint +92 —
  verify equivalence" check goes in the stage-2 commit message explicitly, not assumed.
- **Stage 6** stays no-replay: re-run `test-snapshot-gmulti.R` after stage 1 installs, then
  PR `merge/federation-into-dev` to GiottoDisk `dev`.

### Progress

- **2026-09-04 — steps 0 / 0.5 done.** All branches backed up to `origin`; PR #392 merged
  (true merge commit, `781998a8` in ancestry, `gsource` tip now `82fb8c3f`).
- **2026-09-04 — stages 1–2 replayed.** `feature/gmulti-replay` cut from `82fb8c3f`;
  13 commits cherry-picked from `feature/gmulti2` (9 docs + `1df4b938` stage 1 +
  `1cde445c` stage 2 + 2 plan-doc commits), skipping `bc03289c` (gAny, upstream via #392)
  and the two merge commits. One conflict: NEWS.md, union-resolved (upstream hnsw bullets +
  gmulti bullets). Verified `feature/gmulti2...feature/gmulti-replay` differs by exactly
  upstream PRs #390/#391 plus the three §5 do-not-replay local commits — traps 1 and 2
  held, nothing gmulti-relevant dropped (the `methods-wrap.R` +10 turned out to be
  `ad66a280` doc content, see the stage-1 correction). Full suite on the replay:
  **1208 pass / 0 fail / 0 skip**.
- Non-gmulti salvage from `ad66a280` (design.Rmd article, AGENTS.md conversion, adr/0005,
  wrap deprecation notes) is deliberately not on this branch — re-land it as its own
  upstream docs PR if still wanted.
- **2026-09-04 — stage 3 landed** (`0120cf69`). Checkpoint-sourced, with A2/A3, Q5c and D6
  folded in as §4 requires. Full suite on the merged state: **1485 pass / 0 fail / 0 skip**.
  Decisions taken while implementing, beyond what §4 specified:
    - The values axis resolves a *default* handle rather than only an explicit one: `"raw"`
      when declared, a lone declared handle otherwise, and a loud error when several exist.
      §4 said "mapping lookup or loud error" and left the no-argument case open.
    - `.gm_seed_new_samples()` is the child-add seeder. It needed a guard §4 did not
      anticipate: seeding only re-adds *handles* when a genuinely new sample carries them,
      or a bare `initialize()` resurrects a handle the user deliberately deleted (the
      stage-1 test "a user-set mapping survives bare re-init" catches this).
    - `on_missing` covers two failure kinds, not one: a keyed-but-unsatisfiable child, and
      mismatched feature panels / metadata columns. `"fill"` unions with 0-fill on the
      expression side (an absent measurement reads as 0) and NA-fill on metadata; a
      fully-missing child is dropped under both non-error modes, with a warning.
    - `@mapping` needed sample-key maintenance on the container ops that §4 does not list:
      `[` prunes dropped samples from every entry and `names<-` renames the keys.
      Without the rename, every entry silently stops matching and resolution falls back
      to the legacy child-scan.
    - Two support files needed multi awareness for the setters to resolve at all:
      `defaults.R` (`set_default_{spat_unit,feat_type}`) and `slot_list.R`
      (`list_cell_id_names`). Both prefer the mapping's declared handles, then fall back
      to the first child. Verified warning-neutral for plain `giotto` against a clean
      `upstream/gsource` worktree.
    - **Not ported, deliberately:** `idMap()` (the accessor `spatIDs`/`featIDs` already
      subsume — stage 1 dropped it on purpose and stage 3 does not revive it),
      `.gm_walk_apply_view()` (dead on the checkpoint too — its only reference is its own
      recursive call), plus the §5 items.
- **2026-09-04 — stage 5 landed** (`d12221ed`). Full suite: **1700 pass / 0 fail / 0 skip**.

  **Scoping correction — A7's two patches are not in GiottoClass.** §4 stage 5 reads as a
  GiottoClass-only rework, but `force cache for polygons` and `force NULL cache for
  points` live in **GiottoDisk** `R/methods-resolveSubobject.R` (the `parquetCoordinator`
  methods), which is stage 6's payload. GiottoClass's own cache was already pure
  memoization, so there was nothing to demote here. What stage 5 could do — and did — is
  put the *semantic decision* those patches should be replaced by into GiottoClass as the
  shared contract, so stage 6 drops them by calling it:
    - **`crop_relation_needs_geom()` is internal, not exported.** It is the rule GiottoDisk
      should share rather than duplicate, but nothing calls it across the package boundary
      yet — exporting now would be a public commitment made ahead of its consumer, for a
      predicate with no analyst utility. Export it in the change that makes GiottoDisk call
      it. (Verified new in this stage: no equivalent exists on
      `feature/gmulti-federation-design`, `merge/federation-into-gsource`, `feature/gmulti2`,
      or GiottoDisk's merge branch — all four forwarded `relation` straight into
      `terra::is.related()` against centroids, so the relation was passed but never routed.)
    - Routing is per crop step on `(relation, polygon source availability)`. Only
      `intersects` / `disjoint` are meaningful on a centroid; `within`, `covered_by`,
      `contains`, `covers`, `overlaps`, `touches`, `crosses` are area- or boundary-defined
      and degenerate against a point (`contains` returns nothing at all). Target storage
      kind is not a discriminator — the geom evaluation runs on the gobject's polygon
      source either way, and the resulting cell_ID set narrows the target downstream.
    - **This was silent-wrong, not merely approximate.** Every relation used to take the
      centroid path. Measured on the visium mini with a 3000-unit box: `intersects` keeps
      549 cells, `within` keeps 524, and the within-set is a strict subset — so `within`
      previously returned 549, over-inclusive by the 25 boundary-straddling cells. A
      geometry relation with no polygon source is now a loud error naming the remedy.
    - `disjoint` deliberately skips the AABB pre-filter: its survivors are the points
      *outside* the region, so pre-narrowing to bbox candidates would drop exactly the
      cells that survive. Guarded by the `intersects + disjoint == total` test.
    - `.region_is_rect()` (stage 4) survives as pure optimization *under* the centroid
      path, which is where §4 wanted the cache to sit — same principle, different lever.
    - A8: `vignettes/view_and_space.Rmd` ported (447 lines, no Q7-stale content — it is
      written at the API level, which Q7 did not change) plus two new sections: the
      broadcast rule with worked examples showing `+` and a pipe do not commute, and the
      relation-routing table. Both broadcast claims were run before being written down,
      and are asserted in a test.

  **Still owed to stage 6:** delete the two GiottoDisk patches and route those methods
  through `crop_relation_needs_geom()`. Until then the disk path still decides by storage
  kind, so a `within` crop over a backed store answers the centroid question — the exact
  divergence A7 closes in memory.
- **2026-09-04 — stage 4 landed** (`56ac5fa7`). Checkpoint-sourced, with Q7, A4 and A5
  folded in. Full suite: **1671 pass / 0 fail / 0 skip**; `test-view-space.R` is 186 of
  those. Q7 is done and verified end to end — steps are tagged lists, recipes survive
  `saveRDS` → `readRDS` → resolve, and the "no closures, travels to workers" claim the
  docs already made is finally true. All three prerequisites landed with it: the filter
  predicate is deparsed with env values substituted at record time (so `@env` is gone and
  a later reassignment of a captured variable cannot change the recipe); crop regions
  normalize to a single WKT string through GiottoDisk's cascade (WKT canonical, typed
  inputs coerce and recurse, multi-feature union, inline cap, no CRS field); transform
  args are whitelisted to atomic vectors / numeric matrices / `affine2d`.

  **Two latent bugs in the checkpoint, found by the replay:**
    1. `updateGiottoObject()`'s new-slot migration did `x@view <- NULL` unconditionally,
       so it **destroyed slotted recipes on every `loadGiotto()`**. The checkpoint's gate
       (`< "0.7.0"`) sat above its own version so fresh objects skipped it; at 0.6.0 every
       object hits the migration. Fixed to initialize only a genuinely absent slot. Worth
       noting the general shape: a migration helper that is not idempotent is a landmine
       even behind a correct gate.
    2. `pDataDT` / `fDataDT` were never lifted from `"giotto"` to `"gAny"` — a stage-3
       access-layer item missed there, surfaced here by a gmulti test. Now lifted.

  Decisions taken beyond what §4 specified:
    - **Numeric extents normalize to WKT too**, so WKT is the only stored form and
       `.materialize_crop_region()`'s numeric pass-through branch is gone. The rectangle
       fast path is recovered from the geometry at resolve time (`.region_is_rect()`:
       a single-part polygon with two distinct x and two distinct y values is its own
       bbox) rather than from the stored type, and applies only to
       `relation = "intersects"`. Stage 5's A7 owns the full routing decision and may
       subsume this.
    - **`sf` / `sfc` accepted** as crop inputs, matching the cascade's typed-input rule;
       the checkpoint accepted only numeric / SpatExtent / SpatVector.
    - Q7 removed the `viewStep` / `spaceTransform` `show` methods along with the classes;
       the label helpers survive and the container `show` methods call them. WKT is
       summarised as `<POLYGON: n vertices>` rather than printed literally.
    - Verified `objManifest(giotto())` fails on clean `upstream/gsource` too — an
      upstream bug in the just-landed manifest feature, not a consequence of the new
      `@view` / `@spaces` slots. It surfaces as a non-fatal `saveGiotto()` warning; the
      round-trip test is deliberately left on an empty gobject so it stays visible.
- **2026-09-04 — merged `upstream/gsource` @ `63be3a9a`** (`339c2507`, clean, 0 behind).
  Brought PR #393 (instructions deprecation cascade moved to `.instr_read` /
  `.instr_replace` / `.instr_change` internals) and the `objManifest()` / `manifestDiff()`
  subsystem. No source-file collision with stage 3 — only NAMESPACE and NEWS.md, both
  auto-merged. Worth knowing: #393 is why the suite's warning count fell from 2,882 to a
  handful, and `instructions()` still dispatches on `signature("giotto")`, so the
  giottoMulti fallback path in `defaults.R` is unaffected.

---

*Created 2026-08-19; §9 and the `b351ed2b` base correction added 2026-09-04. Companion to
[PLAN_gmulti2_port.md](PLAN_gmulti2_port.md) and the IMPLEMENTATION pages.*

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
| **source** | GiottoDisk `merge/federation-into-dev` @ `28beb2d`. **"No replay needed" was true when written and is now false** — that branch predates Q7 and Q8, so its resolver reads `view@steps` / `space@samples` and tests steps with `inherits(s, "viewFilter")`. It does not load against GiottoClass 0.7.0 at all (`importClassesFrom(GiottoClass, giottoView, giottoSpace)` fails at build). A7's two force-cache patches also live here. See the progress note below for what the port actually was |
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
- **2026-09-04 — stage 5 landed** (`d12221ed`), then **amended 2026-09-08** after review.

  **Scoping correction — A7's two patches are not in GiottoClass.** §4 stage 5 reads as a
  GiottoClass-only rework, but `force cache for polygons` and `force NULL cache for
  points` live in **GiottoDisk** `R/methods-resolveSubobject.R` (the `parquetCoordinator`
  methods), which is stage 6's payload. GiottoClass's own cache was already pure
  memoization, so there was nothing to demote here.

  **The crop geometry choice is DECLARED on the step, not inferred.** A crop step carries
  `geom = "centroid" | "poly"` (`crop(..., geom =)`, default `"centroid"`), and the
  resolver reads it. This is what §11 was actually asking for — "a distinct step type is
  how a view declares that it means the geometric predicate" — at the cost of a parameter
  rather than a step type.
    - **The first attempt inferred it from the relation name** via an internal
      `crop_relation_needs_geom()` predicate, which was then going to be exported so
      GiottoDisk could share the inference. Both halves were wrong: an inferred choice
      cannot be stated by a serialized recipe, and the export was a public commitment made
      ahead of its consumer. Declaration removes the shared-contract problem outright —
      GiottoDisk reads the field, so there is nothing to export. Verified the predicate was
      new in stage 5: no equivalent on `feature/gmulti-federation-design`,
      `merge/federation-into-gsource`, `feature/gmulti2`, or GiottoDisk's merge branch. All
      four forwarded `relation` straight into `terra::is.related()` against centroids, so
      the relation was passed but never routed.
    - **The relation classification in the first attempt was wrong**, measured against
      terra rather than reasoned about: `within` (strict interior) and `touches`
      (boundary-only) are perfectly well defined on a centroid, so restricting to
      `intersects`/`disjoint` would have refused two working relations. Only `contains`,
      `covers`, `overlaps`, `crosses` are always `FALSE` against a point — those promote
      `geom` to `"poly"` with a warning. `covered_by` is not a terra predicate at all and
      is rejected.
    - Re-measured honestly, one relation at a time (visium mini, 3000-unit centre box, 624
      cells): `intersects` gives 549 on both representations; `within` gives 549 on
      centroid and **524** on poly. The old "549 vs 524" line compared
      `intersects`/centroid against `within`/poly — varying relation *and* representation
      at once, so it conflated two questions.
    - **The geom arm routes through `spatRelate()`**, not a hand-rolled
      `terra::is.related()` call, so one call site covers terra in memory and
      sedona/duckdb on a store. That needed §11's stated prerequisite, which is now partly
      closed: GiottoClass's `spatRelate` gains y-methods for `character` (WKT),
      `SpatVector` and `sf`. The cascade runs opposite to GiottoDisk's — `SpatVector` is
      canonical in memory because terra consumes it, WKT is canonical on disk because it
      goes into SQL. Each side canonicalizes to what its engine eats; that is not drift.
      `engine` is accepted and validated in memory (`NULL`/`"auto"`/`"terra"` pass —
      `"auto"` means best-available and in memory that *is* terra; naming `"sedona"` or
      `"duckdb"` errors rather than silently returning a terra answer).
    - **Fixed a live bug found on the way**: `relate()` errored for any `spatLocsObj` x.
      `R/methods-relate.R:53` coerced with `as.points()` and line 55 immediately
      overwrote it with `x[]` (a data.table for a spatLocsObj), and line 56 guarded on `x`
      while assigning `y_use`. `spatLocsObj` is one of three `giottoSpatial` members, so a
      third of the input space was broken. Now one `.as_relate_geom()` helper, one place.
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

  **Still owed to stage 6:** delete the two GiottoDisk patches by replacing the
  `is.null(.cache)` test in `.push_view_to_pstore` with
  `identical(step$geom, "centroid")` — eager `id_filter` for the centroid arm, the
  existing lazy `spat_relate` op for the poly arm. Nothing to export; the field is on the
  step. The patches are currently *class*-keyed (polygon → forced cache → centroid;
  points → forced NULL cache → geom), so until this lands a backed `giottoPolygon` crop
  cannot honour `geom = "poly"` and silently answers the centroid question instead.
  **Done 2026-09-08, see the stage-6 entry below** — the routing turned out to need one
  more axis than "read `step$geom`", because a store's ability to answer a crop depends on
  whether one of its rows IS a cell.
- **2026-09-08 — stage 6 landed** on GiottoDisk `feature/gmulti-replay` (cut from
  `merge/federation-into-dev`, `upstream/dev` merged in). GiottoDisk suite: **1242 pass /
  0 fail / 0 skip** across 33 files, against a measured `upstream/dev` baseline of
  **1086 / 0 / 0** across 31 — so +156 and no regressions. `test-view-resolver.R` is 125
  of the new ones, and `test-snapshot-gmulti.R` executes for the first time (§8 predicted
  it would stay inert until a replayed GiottoClass was installed) and is green.

  **§4's "no replay needed" was wrong by the time it was reached.** The merge branch was
  written against the S4 recipe containers, so it needed the same Q7/Q8 conversion the
  GiottoClass side got: `view@steps` → `view$steps`, `space@samples` → `space$samples`,
  `inherits(s, "viewFilter")` → `.view_steps_of(view, "filter")`, and `step@predicate`
  → `str2lang(step$predicate)` (Q7 records the predicate deparsed). Without it the
  package does not even build, because `pkg_imports.R` imports `giottoView` /
  `giottoSpace` as classes.

  **A7 needed a second axis, not just `step$geom`.** The above prescription is right about
  crops but incomplete: whether a store can evaluate a crop on its own geometry depends on
  whether one store row IS one cell. Three cases, and `.cache` decides none of them:
    - cell-keyed geom store (a cell-polygon store) + `geom = "poly"` → lazy
      `spat_relate` on its own geom column. **This is the case the forced cache made
      unreachable.**
    - cell-keyed store + `geom = "centroid"` → eager cell_ID set. The store's geom column
      is the polygon, not the centroid, so pushing the predicate down there would answer a
      different question. This is why the centroid arm is eager *even on a geom store*.
    - non-cell-keyed store (transcript points) → always lazy on its own geometry; a crop
      there means "clip these points", matching the in-memory path. Expressed as
      `cell_keyed = FALSE` rather than by forcing `.cache = NULL`.

  `.cache` is now memoization plus an eager/lazy choice for FILTER steps only — with a
  cache they fold into one `id_filter`, without one each narrows the store on its own and
  keeps the lazy cross-store `[`-join for atlas-scale owners. It holds three
  target-independent slots (`filter_ids`, `crop_ids:centroid`, `crop_ids:poly`) rather than
  one, which is what lets a polygon store take the filter arm eagerly while pushing its own
  poly crops down. One shared cache per `materialize()` is still safe because no slot
  depends on the target.

  **Deleted a divergent duplicate.** GiottoDisk had its own `.cells_in_region_dt` /
  `.cells_in_region_for_view`, and they had silently drifted: no `disjoint` fix, no `geom`
  arm. Crop semantics now come from `GiottoClass:::.cells_in_crop_step()` for both arms;
  GiottoDisk keeps only the *fetch* (`.projected_spatlocs_dt`), which it has to own because
  `spatLocsObj@coordinates` may be a store and GiottoClass's fetch cannot `storeRead`.
  `.scope_space_to_sample_local` went with them. `.apply_space_to_subobj` stays local and
  deliberately differs — it dispatches transforms on the inner `parquetBase` rather than on
  the wrapper, which is the whole point on a backed geometry.

  **Two latent bugs the port surfaced, both fixed:**
    - `.space_composite_affine()` probed the space's transforms against a bare
      `SpatVector`. GiottoClass has no `spatShift(SpatVector)` method, so it died on the
      most common step there is. Probe is now a `spatLocsObj`, which implements all seven
      transform generics and is the carrier the centroid path already uses.
    - two `test-view-resolver.R` multi-sample crop assertions compared a `<sample>::`
      prefixed answer against unprefixed per-child IDs. They had never run: `.mk_multi()`
      needs a `createGiottoMulti()` that was unreleased when they were written. The joint
      cell vocabulary is prefixed, so the expectations were wrong, not the code.

  **Blocked on the GiottoClass PR.** `DESCRIPTION` now gates `GiottoClass (>= 0.7.0)`
  while `Remotes` still points at `@gsource`, which is 0.6.0. Deliberate: the gate states
  the real requirement, and pointing a Remote at a feature branch is the kind of pin that
  gets forgotten. This branch cannot merge until `feature/gmulti-replay` lands on
  GiottoClass `gsource` and a release carries 0.7.0.
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

- **2026-09-08 — version bumped to 0.7.0 and the substrate stack rebuilt.** `classes.R`
  already gated the `@view` / `@spaces` migration at `< "0.7.0"` while DESCRIPTION still
  said 0.6.0, which is gsource's own unreleased version — so nothing downstream could gate
  on the view/space API. NEWS gained a 0.7.0 section for the replay's entries; gsource's
  unreleased 0.6.0 content stayed in its own section. Note no 0.6.0 was ever released, so
  the `updateGiottoObject()` recipe-wipe fixed in stage 4 was never a shipped bug and is
  not in the release notes.

  The rebuild was not optional: **an installed GiottoDisk snapshots GiottoClass's whole
  method table for any generic it defines a method on** (`exportMethods(subset)` for
  `subset(parquetBase)`), and re-registers it on load. The August build therefore
  reinstated a pre-`view` `subset(giotto)` over the current one, which is what made
  `subset(g, pred, view = "x")` look like it had never worked and what blocked
  `build_vignettes`. Order matters: GiottoClass first, then Giotto, then GiottoDisk.
  GiottoDisk@dev needs `Giotto (>= 4.2.4)`, which only `upstream/gsource` has —
  `upstream/suite_dev` and `origin/gsource` are both still 4.2.3.

  Clearing it exposed a real bug the stale build had been masking: `view_and_space.Rmd`
  taught `view = <giottoView object>` in four places while `view` is `assert_string` at all
  three sites and `materialize` dispatches on `view = "character"`. Fixed to slotted names.

- **2026-09-08 — Q8: the containers collapse to plain lists.** Q7 kept `giottoView` /
  `giottoSpace` S4 on three grounds; measuring the gobject-side surface against the
  container-side surface after stage 5 falsified two of them and reframed the third. There
  is no slot typing (`@view` / `@spaces` are `nullOrList`), the containers are never a
  dispatch target outside their own builder verbs, and the "~20 methods" are largely a
  second copy of the gobject path — `subset` / `crop` on `giottoView` duplicate the capture
  and step construction verbatim, `+` on `giottoView` is a `stop()` stub, and the 7
  `giottoSpace` verbs already delegate through `.space_record()`.

  The one genuine gap was per-sample keying: a space is a sample-keyed map of step chains, a
  view is one flat chain, and `space = "name"` on a gobject verb had no way to scope a
  transform to one child. `samples =` on the transform verbs closes it and is strictly
  better than `+`, which inherited scope from construction history — see Q8 in
  [PLAN_gmulti2_port.md](PLAN_gmulti2_port.md) for the full argument, the target
  representation, and the `@groups` resolution guards owed to stage 7.

---

*Created 2026-08-19; §9 and the `b351ed2b` base correction added 2026-09-04. Companion to
[PLAN_gmulti2_port.md](PLAN_gmulti2_port.md) and the IMPLEMENTATION pages.*

# Network construction: a coherence audit

*GiottoClass 0.6.0, audited September 2026.*

This is not an ADR. ADRs record decisions; this records the state of the code
that a decision would act on. If it leads to reversing the polymorphic
`@network` slot, that reversal earns its own ADR superseding
[0004](../adr/0004-networks-store-igraph-one-constructor.md).

Every claim below was checked by running the code, not by reading it. Findings
are numbered so they can be cited from issues.

## The question, answered first

**Are spatial and expression networks coherently integrated? Do they share an
underlying class with two subclasses?**

Yes at the layer that matters, and that is by design rather than by accident.
ADR 0003 places `createNetwork()` and `networkParam` in a `create<Noun>` family
outside the five analysis verbs; ADR 0004 unified both network containers to
store a graph in `@network` behind that one constructor. The class tree is
keyed by *algorithm*:

```
networkParam (VIRTUAL)
├── NNNetworkParam (VIRTUAL)
│   ├── kNNNetworkParam
│   ├── sNNNetworkParam
│   └── radiusNetworkParam
└── delaunayNetworkParam
```

and the container methods are keyed on the **virtual root**:

```r
setMethod("createNetwork", signature("spatLocsObj", "networkParam"), ...)
setMethod("createNetwork", signature("dimObj",      "networkParam"), ...)
```

Both extract a coordinate matrix and hand it to the same `("matrix", <param>)`
method. So every algorithm works against tissue coordinates and against an
embedding, through one implementation. The algorithm layer is genuinely
space-agnostic, and that is the shared-hierarchy-with-subclasses the question
asks about.

**What is not coherent is everything above it.** The tree is keyed by
algorithm; the `giotto` methods are keyed by data domain; the user-facing
wrappers expose disjoint subsets of the same parameters. Three consequences
follow, and the rest of this document is those consequences.

## Findings

### N1 — `NNNetworkParam` is an empty tag

```r
setClass("NNNetworkParam", contains = c("networkParam", "VIRTUAL"))
```

Zero slots ([`R/NN_network.R`](../R/NN_network.R)). It exists so that one
`giotto` method can catch kNN and sNN together — that is its whole function.

Meanwhile four slots are declared independently on all four concrete classes:
`weight_fun`, `include_weight`, `include_distance`, `output`. And
`.finalize_network()` reads exactly those four off *whatever* param it is
given. The shared post-processor already depends on a contract the class system
does not express; a fifth param class that omitted one of them would fail at
run time rather than at `setClass`. `engine`, `ef` and `n_threads_build` are
duplicated verbatim on kNN and sNN, along with the roxygen describing them.

The fix is to move the shared slots up. It is deferred here because it changes
four exported class definitions and every `new()` call against them.

### N2 — the domain/algorithm mismatch

Exactly three methods take a `giotto` object:

| signature | resolves |
|---|---|
| `("giotto", "NNNetworkParam")` | a **dimension reduction** (`space = "expression"` default) |
| `("giotto", "radiusNetworkParam")` | **spatial locations** (added Sept 2026, see N9) |
| `("giotto", "delaunayNetworkParam")` | **spatial locations** |

So `kNNNetworkParam` means "expression network" or "spatial network" depending
on which method catches it, and a spatial kNN cannot go through the `giotto`
method at all with the default — it needs `space = "spatial"`, or it routes via
`createSpatialKNNnetwork()`. The `space` argument exists on the NN method and
on the radius method, and on **neither wrapper family**.

The tree cannot express this because the distinction is not a property of the
algorithm. A cleaner arrangement keys the *param* by algorithm and the *method*
by an explicit domain argument on every `giotto` entry point, rather than
letting the default vary by subclass. N9 is what happens when it does.

### N3 — two live bugs from the 0.6.0 igraph migration

Both reproduce on a 300-cell Delaunay network with 855 edges.

**`spatIDs(spatialNetworkObj)` returns `character(0)`.**
[`R/methods-IDs.R:141`](../R/methods-IDs.R#L141):

```r
if (inherits(net, "dataStore")) return(spatIDs(net, ...))
as.character(unique(c(x[]$from, x[]$to)))     # <- x[] is an igraph
```

`x[]` returns the `@network` slot, which since 0.6.0 is an igraph. `$from` on
an igraph is `NULL`, so the union of two `NULL`s is `character(0)`. Note that
the disk-backed branch is correct — only the *canonical in-memory* path is
broken. The `nnNetObj` sibling twenty lines below does it right with
`names(igraph::V(net))`; the same line would fix this one.

**`spat_net_to_igraph()` errors**, same cause, with
`please supply names for attributes`. It is exported and its `@examples` block
is runnable, so this is an `R CMD check` failure waiting for the next full
check run.

### N4 — `maximum_distance` is silently ignored on `kNNNetworkParam`

`filter` defaults to `FALSE`, and `maximum_distance` is only consulted when
`filter = TRUE`. Measured on 300 uniform points with `k = 6`,
`maximum_distance = 20`:

| | edges | longest edge |
|---|---|---|
| `filter = FALSE` (default) | 1800 | 86.7 |
| `filter = TRUE` | 456 | ≤ 20 |

A caller who sets a cutoff and nothing else gets edges four times longer than
the cutoff, with no warning. `createSpatialKNNnetwork()` hardcodes
`filter = TRUE`, so the wrapper is correct and only direct `createNetwork()`
callers are exposed.

`delaunayNetworkParam` has no `filter` slot at all and always applies its
cutoff. The same concept therefore has opposite defaults and two spellings
across sibling classes. Either `filter` should default to
`!is.null(maximum_distance)`, or setting a cutoff without `filter` should warn.

### N5 — dead slots

- `spatialNetworkObj@outputObj` and `@unfiltered` are `NULL` after every
  constructor in the package. Two of the five `getSpatialNetwork(output = )`
  choices can therefore only ever return nothing.
- `networkParam@param` (a `list`) is never written or read. Unlike the four
  sibling param families there are no `$`, `$<-` or `show` methods for
  `networkParam`, despite [`R/classes-utils.R:139`](../R/classes-utils.R#L139)
  documenting that params are "accessed and updated via the `$` operator".
  `kNNNetworkParam(k = 4)@param` is `list()`.

Removing them is a breaking change to two exported classes; documenting them as
defunct in the meantime costs nothing.

### N6 — the storage classes share no network parent

`nnData` and `spatNetData` are parallel virtuals whose common ancestry stops at
`nameData`/`miscData`. The duplication this causes is mechanical and visible:
the `[` and `[<-` methods for the two are byte-identical, as are their
`initialize` guards and their `spatIDs` `dataStore` branches. A `networkData`
virtual holding `network = "ANY"` would collapse four method pairs into one —
and would have made N3 a single-site bug rather than a divergence between two
implementations of the same idea.

`nnNetObj` also records neither `method` nor `parameters`, both of which
`spatialNetworkObj` has. `createNearestNetwork()` constructs a param object and
then discards it, so an NN network cannot say how it was built.

### N7 — wrapper asymmetries

Not all of these are defects, and listing them flatly would misrepresent one of
them.

| asymmetry | verdict |
|---|---|
| spatial kNN hardcodes `dbscan`; `createNearestNetwork()` also offers `hnsw` | **Principled, not a gap.** On 2D coordinates HNSW is approximate *and slower* than the exact kd-tree — 1.90 s vs 0.25 s at 200,000 points. It earns its place at PCA dimensionality. Worth a code comment, not a change. |
| Delaunay is spatial-only | **Inherent.** A triangulation of a 10-dimensional embedding is not a thing anyone wants. |
| no spatial sNN wrapper, though `createNetwork(spatLocsObj, sNNNetworkParam())` works | gap |
| `createNearestNetwork()` has no `output = "data.table"`, no `maximum_distance`, no `minimum_k` | gap |
| `weight_fun`, `include_weight`, `include_distance`, `output = "parquet"`, `backend`, and sNN's `nn_network` search-reuse are exposed by **zero** wrappers | gap — note in particular that disk-backed network *creation* is unreachable from the user API, though reading one now works |
| `feat_type` is accepted and silently discarded by both spatial wrappers | cosmetic defect |
| `createSpatialFeaturesKNNnetwork()` is a third parallel implementation that bypasses `createNetwork()` entirely | structural defect |

The gaps share one cause: each wrapper was written against the arguments its
own algorithm needed, so the union of wrapper arguments is smaller than the
union of param slots. A wrapper that took a param object directly would not
have this problem.

### N8 — the polymorphic-slot debt, quantified

ADR 0004 set its own revisit trigger:

> Revisit the polymorphic slot if the branch count grows past what a
> `nodeIDs()` generic plus the existing delegations can absorb.

As of this audit:

| | `inherits(x, "dataStore")` guards | of which guard `@network` |
|---|---|---|
| GiottoClass | 10 | 8 |
| Giotto | 2 | 2 |
| GiottoDisk | 9 | 2 |
| **total** | **21** | **12** |

`nodeIDs()` **does not exist** — no definition anywhere in the suite. So twelve
sites each carry their own answer to "is this an igraph or a store", and N3 is
what one of them looks like when it gets the *non*-store half wrong.

Two of those twelve were added in September 2026, which is the trigger firing
rather than approaching.

**This audit does not choose between the two options ADR 0004 anticipated.**
They are:

1. **Implement `nodeIDs()`** and the companion accessors, then delete the
   branches that exist only to get node identity or an edge table. Cheap, and
   it does not change any class definition. It does not remove the polymorphism
   — it caps the cost of it, and a thirteenth consumer still owes a branch for
   anything the accessors do not cover.
2. **Reverse the polymorphism**: `@network` always holds an igraph, and a
   backed project keeps its store beside it rather than inside it. Removes the
   whole class of bug, at the price of an ADR superseding 0004 and a migration
   for every saved backed project.

Option 1 is smaller and option 2 is more final; which is right depends on
whether the suite expects networks large enough that materializing an igraph is
itself the problem. That is a decision, and it belongs in an ADR.

### N9 — fixed on this branch

Three defects found by this audit originated in the September 2026 network
work, so they were fixed rather than filed
(`4855c9b4`, `59ff5633`, `88458ccc`):

- `getNearestNetwork()` had no `dataStore` branch though `getSpatialNetwork()`
  had just gained one; both non-object outputs failed on a backed project. The
  two accessors now share one reader.
- `radiusNetworkParam` inherited the `("giotto", "NNNetworkParam")` method and
  with it `space = "expression"`, so a radius given in microns was applied to
  PCA coordinates — a wrong graph with no error. A more specific method now
  defaults it to `"spatial"`. This is N2 producing a real bug.
- `radiusNetworkParam` had no path from a `giotto` object at all;
  `createSpatialNetwork()` gained `method = "radius"`. This is N7's gap column
  in its most extreme form: a fully implemented algorithm no user could call.

## Priority

Ordered by harm rather than by effort.

| | finding | why here |
|---|---|---|
| 1 | **N3** | Silent wrong answers today (`character(0)` instead of node IDs), and one exported function that fails its own example. Two-line fix each. |
| 2 | **N4** | Silent wrong answers today. Needs a default change or a warning, and a decision about which. |
| 3 | **N8** | Not a bug in itself; it is the mechanism that produced N3 and N9's first item, and will produce the next one. Needs an ADR before it needs code. |
| 4 | **N7** gaps | Capability the package has but does not offer. Additive, no risk. |
| 5 | **N1**, **N6** | Structural duplication. Real cost, but it manifests as maintenance rather than as wrong results. Breaking changes; batch them with the next major. |
| 6 | **N5** | Cosmetic until someone calls `output = "unfiltered"` and gets `NULL`. |

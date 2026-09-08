# =============================================================================
# giottoMulti — federated container for multiple giotto objects
# =============================================================================
#
# A data coordination harness: N child `giotto` objects presented as one
# analysable unit. Non-spatial content (expression, metadata, dim reductions)
# is joint across samples; spatial content stays per-child, because each child
# has its own coordinate space.
#
# `giottoMulti` contains the `gAny` virtual base rather than `giotto` — see
# `classes-virtuals.R` for why. The practical consequence is that a spatial
# method with no `giottoMulti` signature fails loudly instead of reading an
# empty slot.
#
# STAGED PORT — stages 1-3 of the sequence in
# `vignettes/articles/PLAN_gmulti2_port.md` (§7). This file carries the class,
# its construction path, the identity registry, the `@cell_ID` / `@feat_ID`
# narrowing contract, and (from stage 3) the `@mapping` federation layer:
#
#   * `gmultiMapping()` / `gmultiMapping<-` — the three-axis federation
#     declaration (`spat_unit`, `feat_type`, `values`), validated, with
#     universe-scoped invalidation and block-on-expansion (plan Q1/Q5)
#   * shared-domain overrides (`getExpression`, `getCellMetadata`,
#     `getFeatureMetadata`) — joint slot when populated, mapping-driven
#     assembly from children when not, `samples =` addressing
#   * spatial-domain per-child dispatch (get/set SpatialLocations,
#     SpatialNetwork, PolygonInfo, FeatureInfo, GiottoImage)
#   * container surface: `as(g, "giottoMulti")`, `[`, `[[<-`, `names<-`, show
#
# Deferred, with the stage that brings it:
#
#   * `@view` / `@spaces` recipe subsystem — stage 4 (plan vs 1-8). Those
#     slots hold plain tagged lists, not S4 step objects (plan Q7)
#   * `@groups` + combined defaults (the `[[1L]]` default-handle pick in
#     `.gm_resolve_axis`) — stage 7 (plan fed 11, 12)
#
# =============================================================================


# CLASS ####

#' @title S4 giottoMulti
#' @name giottoMulti-class
#' @description
#' Container for multiple `giotto` objects whose spatial information is kept
#' separate (one space per child) but whose expression-space analysis is
#' shared across all cells.
#'
#' @slot objects named `list` of `giotto` objects (children)
#' @slot id_map `list` with elements `cells` and `feats`, each a `data.table`
#'   mapping `(object, local_id) -> global_id`. This is an identity
#'   **registry** covering every child ID, and is never narrowed.
#' @slot id_sig `list` of per-child ID length-signatures. Drives id_map cache
#'   invalidation in `initialize()`.
#' @slot mapping `list` declaring how child-level content federates up to
#'   gmulti-level handles. Three named entries — `spat_unit`, `feat_type`,
#'   and `values` (expression matrix names; flat, not per-universe) — each a
#'   list of named character vectors mapping `sample -> child-level name`.
#'   Every entry keys every sample; `NA_character_` is the deliberate-skip
#'   sentinel ("this sample does not contribute to this handle").
#'   Auto-discovered at construction; edited via [gmultiMapping()].
#'
#' @slot expression shared expression matrices (rows = union of features,
#'   cols = global cell IDs)
#' @slot expression_feat available feature types
#' @slot cell_metadata shared cell metadata (one row per global cell ID)
#' @slot feat_metadata shared feature metadata (one row per global feature)
#' @slot cell_ID shared cell ID lists (global IDs). Records active narrowing
#'   from stage 2; `NULL` means unfiltered.
#' @slot feat_ID shared feature ID lists (global IDs), same contract.
#' @slot dimension_reduction shared joint dim-reductions (PCA, UMAP, harmony)
#' @slot nn_network shared joint NN graphs
#' @slot spatial_enrichment shared spatial enrichment results
#' @slot multiomics shared multi-omics info
#'
#' @slot instructions giotto-style instructions
#' @slot parameters analysis parameters (mirrors `giotto@parameters`)
#' @slot versions package versions
#' @slot misc miscellaneous
#' @slot source on-disk source / project manager (e.g.
#'   [GiottoDisk::gDirSource]) where multi-level shared-domain artifacts
#'   live. `NULL` for in-memory multis. Children may carry their own
#'   per-sample sources; multi-level slots (shared `@expression`,
#'   `@dimension_reduction`, `@nn_network`) write to this one.
#' @slot view named `list` of view recipes. Reserved for stage 4.
#' @slot spaces named `list` of coordinate-frame recipes. Reserved for stage 4.
#'
#' @returns giottoMulti object
#' @exportClass giottoMulti
giottoMulti <- setClass(
    "giottoMulti",
    contains = "gAny",
    slots = c(
        # multi-specific
        objects             = "list",
        id_map              = "list",
        id_sig              = "list",
        mapping             = "list",

        # shared-domain (names aligned with giotto)
        expression          = "nullOrList",
        expression_feat     = "nullOrChar",
        cell_metadata       = "nullOrList",
        feat_metadata       = "nullOrList",
        cell_ID             = "nullOrList",
        feat_ID             = "nullOrList",
        spatial_enrichment  = "nullOrList",
        dimension_reduction = "nullOrList",
        nn_network          = "nullOrList",
        multiomics          = "ANY",

        # infrastructure
        instructions        = "nullOrInstructions",
        parameters          = "ANY",
        versions            = "list",
        misc                = "list",
        source              = "ANY",
        view                = "nullOrList",
        spaces              = "nullOrList"
    ),
    prototype = list(
        objects             = list(),
        id_map              = list(cells = NULL, feats = NULL),
        id_sig              = list(),
        mapping             = list(spat_unit = list(), feat_type = list(),
            values = list()),

        expression          = NULL,
        expression_feat     = NULL,
        cell_metadata       = NULL,
        feat_metadata       = NULL,
        cell_ID             = NULL,
        feat_ID             = NULL,
        spatial_enrichment  = NULL,
        dimension_reduction = NULL,
        nn_network          = NULL,
        multiomics          = NULL,

        instructions        = NULL,
        parameters          = list(),
        versions            = .versions_info(),
        misc                = list(),
        source              = NULL,
        view                = NULL,
        spaces              = NULL
    )
)


# INITIALIZE ####

#' @noRd
setMethod("initialize", signature("giottoMulti"), function(.Object, objects = NULL, ...) {
    .Object <- callNextMethod(.Object, ...)

    # Construction path: ingest the children. Skipped on bare re-init calls
    # (no `objects` arg) so an already-constructed giottoMulti can be
    # re-initialized without re-supplying its children.
    if (!is.null(objects) && length(objects) > 0L) {
        checkmate::assert_list(objects, types = "giotto", names = "unique",
            .var.name = "objects")

        .Object@objects <- objects
    }

    # Default instructions, same lifecycle as on a single giotto: created
    # on first initialize if not user-supplied. The multi is the object the
    # user views and operates on, so a populated @instructions is needed
    # for downstream tools (python path, plotting prefs, etc.).
    if (is.null(.Object@instructions)) {
        .Object@instructions <- createGiottoInstructions()
    }

    if (length(.Object@objects) == 0L) return(.Object)

    # Source resolution: federated multi keeps per-child sources intact but
    # also carries one multi-level source for cross-sample shared-domain
    # artifacts (joint PCA, joint NN networks). Enforce backend-type
    # homogeneity across any sourced children: mixing parquet- and
    # bpcells-backed children breaks union/cbind dispatch downstream.
    .Object@source <- .gm_resolve_source(.Object@source, .Object@objects)

    # id_map: cache rebuilt only when child length-signatures differ from the
    # cached signature. Bare re-init when nothing changed is a no-op modulo a
    # signature comparison over N children — microseconds even at atlas scale.
    #
    # When the population changes (child added / removed / replaced),
    # @cell_ID / @feat_ID narrowing is reset to NULL: those slots record the
    # *surviving set from a prior filter on a specific population*, which is
    # no longer well-defined after a structural change. User-facing contract:
    # re-run the filter on the expanded multi to recompute. The alternative —
    # letting a new child silently inherit whatever narrowing the parent
    # carries — would report a filter that child never went through, so
    # narrowing is treated as eager state tied to a specific population.
    cur_sig <- .gm_compute_sig(.Object@objects)
    if (!identical(cur_sig, .Object@id_sig)) {
        .Object@id_map$cells <- .gm_build_cell_idmap(.Object@objects)
        .Object@id_map$feats <- .gm_build_feat_idmap(.Object@objects)
        .Object@id_sig <- cur_sig
        .Object@cell_ID <- NULL
        .Object@feat_ID <- NULL
    }

    # mapping: auto-discover only when empty (first construction). This is
    # what makes @mapping the authoritative universe of spat_unit / feat_type
    # handles, which `.gm_narrowing_keys()` depends on — deriving those keys
    # from the lazily-populated joint slots alone means a default-scoped
    # subset() records no narrowing at all and silently no-ops.
    #
    # When the mapping is already populated, seed entries for any sample the
    # entries do not key yet — the supported add paths (`[[<-`, direct
    # `@objects` edits caught by the signature check) run through here, so a
    # newly added child is auto-declared rather than left dangling (plan Q1).
    mapping_empty <- length(.Object@mapping$spat_unit) == 0L &&
        length(.Object@mapping$feat_type) == 0L &&
        length(.Object@mapping$values) == 0L
    if (mapping_empty) {
        .Object@mapping <- .gm_discover_mapping(.Object@objects)
    } else {
        .Object@mapping <- .gm_seed_new_samples(.Object)
    }

    .Object
})


# CONSTRUCTOR ####

#' @title Create a giottoMulti object
#' @name createGiottoMulti
#' @description Container for multiple `giotto` objects analyzed in a shared
#' expression space. Each child keeps its own spatial information; shared
#' analyses (joint dim reduction, NN graphs, clustering) live on the parent.
#'
#' @param objects named `list` of `giotto` objects
#' @param instructions a `giottoInstructions` object (optional)
#' @param source on-disk source / project manager (e.g.
#'   [GiottoDisk::gDirSource]) for cross-sample shared-domain artifacts.
#'   If `NULL` (default), auto-acquired from the first sourced child; if
#'   no child carries a source, the multi is in-memory. When supplied,
#'   must be the same backend class as any source the children carry.
#'
#' @returns `giottoMulti`
#' @examples
#' \dontrun{
#' g1 <- GiottoData::loadGiottoMini("visium")
#' g2 <- GiottoData::loadGiottoMini("viz")
#' mg <- createGiottoMulti(list(visium = g1, viz = g2))
#' }
#' @export
createGiottoMulti <- function(objects, instructions = NULL, source = NULL) {
    checkmate::assert_list(objects, types = "giotto", names = "unique")
    args <- list(objects = objects)
    if (!is.null(instructions)) args$instructions <- instructions
    if (!is.null(source)) args$source <- source
    do.call(new, c("giottoMulti", args))
}


# COERCION ####

#' Wrap a single `giotto` as a one-child `giottoMulti`.
#'
#' Useful as a quick path for code that wants to operate uniformly on a multi
#' (no class branches), and to gain the lazy view layer (`subset()` /
#' view-filter on read) on top of a single giotto without committing to a
#' destructive in-place subset.
#'
#' The child is named `"sample1"` by default. To pick a different name use
#' `createGiottoMulti(list(my_name = g))` directly.
#' @name as-giottoMulti
#' @aliases as,giotto,giottoMulti-method
setAs("giotto", "giottoMulti", function(from) {
    createGiottoMulti(list(sample1 = from))
})


# INTROSPECTION ####

#' @noRd
setMethod("names", "giottoMulti", function(x) names(x@objects))

#' @noRd
setMethod("length", "giottoMulti", function(x) length(x@objects))

# `[[` on a giottoMulti selects a CHILD, not a slot — unlike `[[` on a
# `giotto`, which returns slot contents. The two are not interchangeable, and
# code that walks joint slots must read them off the object directly.
#' @noRd
setMethod("[[", signature(x = "giottoMulti", i = "ANY", j = "missing"),
    function(x, i, j, ...) x@objects[[i]])

#' @noRd
setReplaceMethod("[[",
    signature(x = "giottoMulti", i = "ANY", j = "missing", value = "giotto"),
    function(x, i, j, ..., initialize = TRUE, value) {
        # Detect populated joint slots BEFORE mutation so we can warn
        # if the add leaves them incomplete for the new sample's cells.
        is_new <- !(i %in% names(x@objects))
        populated_before <- .gm_populated_joint_slots(x)

        x@objects[[i]] <- value
        # Default: re-initialize so id_map rebuilds, any prior narrowing
        # (@cell_ID / @feat_ID) clears (structural change invalidates the
        # surviving-set record), and @mapping auto-seeds the new sample
        # (plan Q1). Cost is a length-vector signature check on N children
        # (microseconds). Bulk-add callers can pass `initialize = FALSE`
        # and run `initialize(mg)` once at the end to amortize.
        if (isTRUE(initialize)) x <- initialize(x)

        # When adding a NEW sample to a gmulti with already-populated
        # joint slots, those slots cover only the original samples.
        # Surface this so the user knows to extend, recompute, or rebuild.
        if (is_new && length(populated_before) > 0L) {
            warning(.gm_add_joint_nudge(i, populated_before),
                call. = FALSE)
        }
        x
    }
)

#' @noRd
setMethod("[", signature(x = "giottoMulti", i = "ANY"),
    function(x, i, j, ..., drop = TRUE) {
        # Select children by name or integer index; return a new giottoMulti
        # with the chosen subset. Joint shared slots are auto-pruned to the
        # surviving sample::id globals so the result is self-consistent.
        sel <- if (is.character(i)) {
            bad <- setdiff(i, names(x))
            if (length(bad) > 0L) {
                stop("[giottoMulti] unknown child(ren): ",
                    paste(bad, collapse = ", "), call. = FALSE)
            }
            i
        } else {
            names(x)[i]
        }
        out <- x
        out@objects <- x@objects[sel]

        # Compute the surviving global cell-id set from kept samples'
        # current id_map rows, then prune every joint shared slot to it.
        # Features are not sample-namespaced so feat_metadata stays as-is.
        cm <- x@id_map$cells
        if (!is.null(cm) && nrow(cm) > 0L) {
            keep_globals <- cm[cm$object %in% sel, ]$global_id
            out@expression          <- .gm_prune_expression(out@expression, keep_globals)
            out@cell_metadata       <- .gm_prune_cell_metadata(out@cell_metadata, keep_globals)
            out@dimension_reduction <- .gm_prune_dim_reduction(out@dimension_reduction, keep_globals)
            out@nn_network          <- .gm_prune_nn_network(out@nn_network, keep_globals)
            out@spatial_enrichment  <- .gm_prune_spatial_enrichment(out@spatial_enrichment, keep_globals)
        }

        # @mapping entries key every sample — prune dropped samples so the
        # every-entry-keys-every-sample invariant holds for the survivors.
        out@mapping <- .gm_prune_mapping_samples(out@mapping, sel)

        # rebuild id_map for the new child set
        out@id_sig <- list()
        initialize(out)
    }
)

#' @noRd
setReplaceMethod("names", signature(x = "giottoMulti", value = "character"),
    function(x, value) {
        old_names <- names(x@objects)
        if (length(value) != length(old_names)) {
            stop(sprintf(
                "[giottoMulti] names() <- requires length %d (got %d)",
                length(old_names), length(value)
            ), call. = FALSE)
        }
        if (anyDuplicated(value)) {
            stop("[giottoMulti] child names must be unique", call. = FALSE)
        }

        # Rewrite sample::id prefix across every joint shared slot that
        # carries cell-axis keys. Features are never sample-namespaced
        # (id_map$feats$global_id == local_id) so feat_metadata stays as-is.
        # Disk-backed networks (parquetEdgeStore) get a separate alias-layer
        # treatment — this in-memory walk doesn't touch them.
        old_to_new <- stats::setNames(value, old_names)
        x@expression          <- .gm_rewrite_expression(x@expression, old_to_new)
        x@cell_metadata       <- .gm_rewrite_cell_metadata(x@cell_metadata, old_to_new)
        x@dimension_reduction <- .gm_rewrite_dim_reduction(x@dimension_reduction, old_to_new)
        x@nn_network          <- .gm_rewrite_nn_network(x@nn_network, old_to_new)
        x@spatial_enrichment  <- .gm_rewrite_spatial_enrichment(x@spatial_enrichment, old_to_new)
        x@cell_ID             <- .gm_rewrite_narrowing(x@cell_ID, old_to_new)

        # @mapping entries are keyed by sample name — rename the keys too,
        # or every entry silently stops matching and resolution falls back
        # to the legacy child-scan path.
        x@mapping <- .gm_rename_mapping_samples(x@mapping, old_to_new)

        names(x@objects) <- value
        # id_map embeds the old names in object column AND in global_id;
        # full rebuild is the simplest correct path.
        x@id_sig <- list()
        initialize(x)
    }
)


# SHOW ####

#' @noRd
setMethod("show", "giottoMulti", function(object) {
    cat(sprintf("An object of class %s\n", class(object)))

    # children: cell + feature counts from each child's active default
    # (spatIDs / featIDs). Mirrors what id_map and the view counters use,
    # so the per-child totals sum to the global "total" below — avoiding
    # the double-counting that summing lengths(child@cell_ID) would do
    # when a child has cells in multiple spat_units.
    nms <- names(object)
    cat(sprintf("  %d child object(s):\n", length(object)))
    for (nm in nms) {
        g <- object@objects[[nm]]
        n_c <- length(tryCatch(spatIDs(g), error = function(e) character()))
        n_f <- length(tryCatch(featIDs(g), error = function(e) character()))
        cat(sprintf("    %s: %d cells, %d features\n", nm, n_c, n_f))
    }

    # view: subset filter state — visible / total reflects whether the user
    # has narrowed the view. `n_c_vis` / `n_f_vis` go through the gmulti's
    # spatIDs / featIDs methods, which intersect @id_map with the active
    # @cell_ID / @feat_ID narrowing. Totals come straight from children
    # (the unfiltered baseline) and are independent of any narrowing.
    if (!is.null(object@id_map$cells) || !is.null(object@id_map$feats)) {
        n_c_vis <- length(tryCatch(spatIDs(object),
            error = function(e) character()))
        n_c_total <- sum(vapply(object@objects, function(g) {
            length(tryCatch(spatIDs(g),
                error = function(e) character()))
        }, integer(1L)))

        per_child_feats <- lapply(object@objects, function(g) {
            tryCatch(featIDs(g), error = function(e) character())
        })
        n_f_vis <- length(tryCatch(featIDs(object),
            error = function(e) character()))
        n_f_total <- length(unique(unlist(per_child_feats, use.names = FALSE)))

        c_flag <- if (n_c_vis < n_c_total) " (filtered)" else ""
        f_flag <- if (n_f_vis < n_f_total) " (filtered)" else ""
        cat(sprintf("  view: %d / %d cells%s, %d / %d features%s\n",
            n_c_vis, n_c_total, c_flag, n_f_vis, n_f_total, f_flag))

        # shared: how many features are simultaneously present in all active
        # children. This is what getExpression(mg) would return as features
        # when assembling from children. When all children share a panel the
        # count matches the view total; with mismatched panels it's smaller.
        f_intersect <- Reduce(intersect, per_child_feats)
        if (!is.null(object@id_map$feats)) {
            f_intersect <- intersect(f_intersect,
                unique(object@id_map$feats$global_id))
        }
        if (length(f_intersect) != n_f_total) {
            cat(sprintf("  shared: %d feature(s) common to all children\n",
                length(f_intersect)))
        }
    }

    # populated joint shared slots
    populated <- .gm_populated_joint_slots(object)
    if (length(populated) > 0L) {
        cat(sprintf("  joint slots: %s\n",
            paste(populated, collapse = ", ")))
    }

    # mapping: which handles federate across children, per axis. One line
    # per axis listing the gmulti-level handles and the number of
    # participating (non-NA) samples.
    fmt_axis <- function(axis_list) {
        if (length(axis_list) == 0L) return(NULL)
        entries <- vapply(names(axis_list), function(h) {
            sprintf("%s (%d)", h, sum(!is.na(axis_list[[h]])))
        }, character(1L))
        paste(entries, collapse = ", ")
    }
    for (axis in c("spat_unit", "feat_type", "values")) {
        line <- fmt_axis(object@mapping[[axis]])
        if (!is.null(line)) {
            cat(sprintf("  %s: %s\n", axis, line))
        }
    }

    invisible(NULL)
})


# SUBSET ####

#' @title Subset a giottoMulti
#' @name subset-giottoMulti
#' @description
#' Narrow the joint analysis view of the multi to a subset of global cell IDs
#' and/or global feature IDs. Eager: the surviving set is recorded on
#' `@cell_ID` / `@feat_ID` and every populated joint shared slot is trimmed in
#' place.
#'
#' Children (`@objects`) are the spatial axis and are not touched. If you want
#' narrowed spatial content on a specific child, do that explicitly on the
#' child.
#'
#' Subset returns a new `giottoMulti`; R copy-on-modify means the original is
#' untouched and acts as the "widen back" handle.
#' @param x a `giottoMulti`
#' @param subset predicate expression, captured unevaluated. Only used with
#'   `view = `, where it becomes the recorded filter step; the eager path
#'   narrows by ID vector instead (`cells` / `features`).
#' @param cells `character` vector of global cell IDs to retain. `NULL` =
#'   no cell-level filter.
#' @param features `character` vector of global feature IDs to retain.
#'   `NULL` = no feature-level filter.
#' @param negate logical. Invert the predicate. Folded into the recorded
#'   predicate, matching `subset(<giotto>)`.
#' @param view `NULL` or `character(1)`. When supplied, records the
#'   predicate as a filter step on the named view (created if new) instead
#'   of narrowing eagerly.
#' @param ... additional scope args forwarded to `spatValues()` when a
#'   recorded filter step resolves
#' @returns a `giottoMulti` with `@cell_ID` / `@feat_ID` narrowed and
#'   populated joint slots trimmed accordingly. `@id_map` (the identity
#'   registry) is left untouched — it records identity, not selection.
#' @examples
#' \dontrun{
#' subset(mg, cells = c("a::c1", "a::c2"))
#' subset(mg, leiden_clus == 1, view = "cluster1")
#' }
#' @export
setMethod("subset", "giottoMulti",
    function(x, subset, cells = NULL, features = NULL, negate = FALSE,
             view = NULL, ...) {
        # Recording path, mirroring subset(<giotto>). Q8 removed the recipe
        # builders, so this is now the only way to put a filter step on a
        # gmulti view. `subset` sits ahead of `cells` / `features` because
        # the generic's second positional is the predicate everywhere else;
        # every existing caller passes those two by name.
        if (!is.null(view)) {
            # Capture BEFORE testing for absence. `missing(subset)` forces
            # the promise when the argument arrived through the generic's
            # `...` into S4's `.local` wrapper, which evaluates the
            # predicate in the caller's frame and fails on the first column
            # name. An absent `subset` deparses to the bare symbol.
            pred <- substitute(subset)
            if (identical(pred, quote(subset))) {
                stop("`view = ` records a predicate, so `subset` is ",
                    "required. To narrow by ID instead, drop `view` and ",
                    "pass `cells = ` / `features = `.", call. = FALSE)
            }
            if (negate) pred <- call("!", pred)
            pred <- .eager_substitute_env(pred,
                .find_predicate_env(pred, parent.frame()))
            return(.record_view_on_gobject(x, view,
                .view_step_filter(pred, scope_args = list(...))))
        }
        subsetGiotto(
            gobject = x,
            cell_ids = cells,
            feat_ids = features
        )
    }
)


# ID ACCESSORS ####

# `@id_map` is read through the existing `spatIDs` / `featIDs` generics rather
# than a dedicated accessor. The registry's third column is derivable — the
# `sample::id` prefix names the child, and `object =` filters to it — so the
# (object, local_id, global_id) table has no caller that the ID vectors do not
# serve. Internal code reads the slot directly.

#' @rdname spatIDs-generic
#' @export
setMethod(
    "spatIDs", signature(x = "giottoMulti"),
    function(x, object = NULL, local = FALSE, spat_unit = NULL, ...) {
        m <- x@id_map$cells
        if (is.null(m) || nrow(m) == 0L) return(character())
        if (!is.null(object)) {
            target <- .gm_resolve_objects(x, object)
            # pre-compute keep to avoid data.table column-name shadowing
            keep <- m$object %in% target
            m <- m[keep, ]
        }
        # @cell_ID narrowing: when set, intersect with global ids. The slot
        # is nested by spat_unit; restrict to one spat_unit if requested,
        # else union across spat_units. Empty @cell_ID is "no narrowing".
        # The registry itself is never narrowed, so this applies on read.
        if (length(x@cell_ID) > 0L) {
            surv <- if (is.null(spat_unit)) {
                unique(unlist(x@cell_ID, use.names = FALSE))
            } else {
                x@cell_ID[[spat_unit]]
            }
            if (!is.null(surv)) {
                m <- m[m$global_id %in% surv, ]
            }
        }
        if (isTRUE(local)) return(m$local_id)
        m$global_id
    }
)

#' @rdname spatIDs-generic
#' @export
setMethod(
    "featIDs", signature(x = "giottoMulti"),
    function(x, object = NULL, local = FALSE, uniques = TRUE,
             feat_type = NULL, ...) {
        m <- x@id_map$feats
        if (is.null(m) || nrow(m) == 0L) return(character())
        if (!is.null(object)) {
            target <- .gm_resolve_objects(x, object)
            keep <- m$object %in% target
            m <- m[keep, ]
        }
        # @feat_ID narrowing: same pattern as @cell_ID in spatIDs above,
        # nested by feat_type instead of spat_unit.
        if (length(x@feat_ID) > 0L) {
            surv <- if (is.null(feat_type)) {
                unique(unlist(x@feat_ID, use.names = FALSE))
            } else {
                x@feat_ID[[feat_type]]
            }
            if (!is.null(surv)) {
                m <- m[m$global_id %in% surv, ]
            }
        }
        ids <- if (isTRUE(local)) m$local_id else m$global_id
        if (isTRUE(uniques)) unique(ids) else ids
    }
)


# INTERNAL HELPERS ####

#' Resolve which children a per-object method should operate on.
#'
#' @param x giottoMulti
#' @param object NULL (all children), or character vector of object names
#' @returns character vector of object names
#' @noRd
.gm_resolve_objects <- function(x, object = NULL) {
    if (is.null(object)) return(names(x))
    checkmate::assert_character(object)
    bad <- setdiff(object, names(x))
    if (length(bad) > 0L) {
        stop("[giottoMulti] unknown object(s): ",
            paste(bad, collapse = ", "), call. = FALSE)
    }
    object
}

# Decide the multi's @source slot from a (possibly-NULL) explicit value and
# the children's per-child sources.
# - All sourced children must share the same source class. Error otherwise.
# - If `explicit` is provided, it must also match that class. If it doesn't
#   match because no children have sources, accept it as-is.
# - If `explicit` is NULL, adopt the first sourced child's source.
# - If no children have sources and no explicit source provided, leave NULL.
#' @noRd
.gm_resolve_source <- function(explicit, objects) {
    child_sources <- lapply(objects, function(g) g@source)
    have_source <- !vapply(child_sources, is.null, logical(1L))
    if (any(have_source)) {
        classes <- vapply(child_sources[have_source],
            function(s) class(s)[[1L]], character(1L))
        if (length(unique(classes)) > 1L) {
            stop("[giottoMulti] children carry sources of different classes (",
                paste(unique(classes), collapse = ", "),
                "). All sourced children must use the same backend.",
                call. = FALSE)
        }
        if (!is.null(explicit) && class(explicit)[[1L]] != classes[[1L]]) {
            stop("[giottoMulti] explicit source class '",
                class(explicit)[[1L]],
                "' does not match children's source class '",
                classes[[1L]], "'.", call. = FALSE)
        }
    }
    if (!is.null(explicit)) return(explicit)
    first_idx <- which(have_source)[1L]
    if (!is.na(first_idx)) return(child_sources[[first_idx]])
    NULL
}

#' Keys over which a `":all:"` narrowing is recorded on @cell_ID / @feat_ID.
#'
#' `@mapping` is the authoritative universe for both axes: it is populated at
#' construction by `.gm_discover_mapping()` and covers every spat_unit /
#' feat_type any child declares. The joint slots are only a lazily-populated
#' cache — they are empty on a freshly constructed multi and stay empty until
#' something explicitly writes one, so deriving keys from them alone means a
#' `":all:"` narrowing records nothing at all and `subset()` silently no-ops.
#'
#' The joint-slot and `@cell_ID` / `@feat_ID` names are still unioned in, to
#' cover multis whose `@mapping` was cleared.
#' @noRd
.gm_narrowing_keys <- function(gobject, axis = c("spat_unit", "feat_type")) {
    axis <- match.arg(axis)
    declared <- names(gobject@mapping[[axis]])
    cached <- switch(axis,
        "spat_unit" = c(
            names(gobject@expression),
            names(gobject@cell_metadata),
            names(gobject@cell_ID)
        ),
        "feat_type" = c(
            unlist(lapply(gobject@expression, names), use.names = FALSE),
            unlist(lapply(gobject@feat_metadata, names), use.names = FALSE),
            names(gobject@feat_ID)
        )
    )
    unique(c(declared, cached))
}

#' Names a child carries on one mapping axis.
#'
#' For `spat_unit` / `feat_type` these are the child's `@cell_ID` /
#' `@feat_ID` keys (the authoritative source of which spat_units /
#' feat_types exist in that child); for `values` it is the union of
#' expression names across the child's nestings — the values axis is flat,
#' one map applied within whichever universe resolves (plan Q5).
#' @noRd
.gm_child_axis_names <- function(g, axis) {
    out <- switch(axis,
        spat_unit = tryCatch(names(slot(g, "cell_ID")),
            error = function(e) character()),
        feat_type = tryCatch(names(slot(g, "feat_ID")),
            error = function(e) character()),
        values = tryCatch(
            unique(unlist(lapply(g@expression, function(by_su) {
                lapply(by_su, names)
            }), use.names = FALSE)),
            error = function(e) character()),
        stop("[.gm_child_axis_names] unknown axis: ", axis, call. = FALSE)
    )
    out %||% character()
}

#' Auto-discover the federation mapping from a list of child gobjects.
#'
#' Assembles the symmetric trivial mapping for `spat_unit` / `feat_type`:
#' for every (handle, sample) pair, the entry keys **every** sample — the
#' handle name where the child carries a slot of that name,
#' `NA_character_` (the deliberate-skip sentinel) where it doesn't. So
#' participation is always the declared key set, never inferred from what
#' happens to exist (plan Q1/Q5b).
#'
#' Children with non-matching names (e.g. "rna" in one sample vs
#' "transcripts" in another for the same modality) get separate entries by
#' name — discovery never silently equates differently-named slots.
#' Reconciling them is a declaration edit via `gmultiMapping<-`.
#'
#' The `values` axis is seeded to the ingest convention `"raw"`: every
#' sample that has *any* expression is keyed `"raw"` — even when its
#' matrices are named differently, so the first joint read fails loudly
#' naming the sample (plan Q5b rule 2) instead of silently dropping it.
#' The remedies are the user's call: rename the child's matrix, point the
#' entry at what it *is* called, or set it `NA_character_` to skip.
#' Samples with no expression at all are seeded `NA` (nothing to federate).
#' @noRd
.gm_discover_mapping <- function(objects) {
    if (length(objects) == 0L) {
        return(list(spat_unit = list(), feat_type = list(), values = list()))
    }
    samples <- names(objects)
    discover_axis <- function(axis) {
        per_child <- lapply(objects, .gm_child_axis_names, axis = axis)
        all_names <- unique(unlist(per_child, use.names = FALSE))
        if (length(all_names) == 0L) return(list())
        out <- lapply(all_names, function(nm) {
            vals <- vapply(samples, function(s) {
                if (nm %in% per_child[[s]]) nm else NA_character_
            }, character(1L))
            stats::setNames(vals, samples)
        })
        names(out) <- all_names
        out
    }

    per_child_v <- lapply(objects, .gm_child_axis_names, axis = "values")
    has_expr <- vapply(samples, function(s) {
        length(per_child_v[[s]]) > 0L
    }, logical(1L))
    values_axis <- if (any(has_expr)) {
        list(raw = stats::setNames(
            ifelse(has_expr, "raw", NA_character_), samples))
    } else {
        list()
    }

    list(
        spat_unit = discover_axis("spat_unit"),
        feat_type = discover_axis("feat_type"),
        values = values_axis
    )
}

#' Seed `@mapping` entries for samples added after construction (plan Q1).
#'
#' Idempotent: only samples missing from an entry are appended; existing
#' entries (including `NA` skips) are never overwritten. Seeding rule per
#' handle: universe already materialized -> `NA_character_` (expansion is
#' blocked — opting in means dropping the joint content, then
#' re-declaring); otherwise the handle name when the child carries it,
#' `NA` when it doesn't. Handles the new children introduce that no
#' existing entry covers are added fresh from discovery.
#' @noRd
.gm_seed_new_samples <- function(x) {
    mapping <- x@mapping
    # multis built before the values axis existed: give them the slot
    if (is.null(mapping$values)) mapping$values <- list()
    samples <- names(x@objects)

    # which samples are genuinely new (keyed by no entry on any axis)?
    # computed before seeding so the handle-addition pass below can tell
    # a new child from a handle the user deliberately removed
    already_keyed <- unique(unlist(lapply(mapping, function(ax) {
        lapply(ax, names)
    }), use.names = FALSE))
    new_samples <- setdiff(samples, already_keyed)

    for (axis in c("spat_unit", "feat_type", "values")) {
        for (handle in names(mapping[[axis]])) {
            entry <- mapping[[axis]][[handle]]
            missing_s <- setdiff(samples, names(entry))
            if (length(missing_s) == 0L) next
            materialized <- .gm_universe_materialized(x, axis, handle)
            seeded <- vapply(missing_s, function(s) {
                if (materialized) return(NA_character_)
                child_names <- .gm_child_axis_names(x@objects[[s]], axis)
                target <- if (axis == "values") "raw" else handle
                if (target %in% child_names) target else NA_character_
            }, character(1L))
            mapping[[axis]][[handle]] <-
                c(entry, stats::setNames(seeded, missing_s))
        }
    }

    # Brand-new handles introduced by the added children get added with
    # plain discovery semantics — but ONLY when carried by a genuinely new
    # sample. A handle the user deliberately removed from the mapping must
    # not be re-added by a bare re-init (a user-set mapping survives).
    if (length(new_samples) > 0L) {
        discovered <- .gm_discover_mapping(x@objects)
        for (axis in c("spat_unit", "feat_type", "values")) {
            new_handles <- setdiff(names(discovered[[axis]]),
                names(mapping[[axis]]))
            for (handle in new_handles) {
                entry <- discovered[[axis]][[handle]]
                if (any(!is.na(entry[new_samples]))) {
                    mapping[[axis]][[handle]] <- entry
                }
            }
        }
    }
    mapping
}

#' Does joint content exist for this handle's universe?
#'
#' Used by the block-on-expansion rule (plan Q1) and child-add seeding:
#' joint content was built from a specific participation set, so widening
#' the declaration afterwards would leave content that omits a sample the
#' declaration now claims. Narrowing state (`@cell_ID` / `@feat_ID`) is
#' selection, not content — it doesn't count.
#' @noRd
.gm_universe_materialized <- function(x, axis, handle) {
    lvl1 <- function(sl) handle %in% names(sl)
    lvl2 <- function(sl) {
        handle %in% unlist(lapply(sl, names), use.names = FALSE)
    }
    lvl3 <- function(sl) {
        handle %in% unlist(lapply(sl, function(by_su) {
            lapply(by_su, names)
        }), use.names = FALSE)
    }
    switch(axis,
        spat_unit = lvl1(x@expression) || lvl1(x@cell_metadata) ||
            lvl1(x@feat_metadata) || lvl1(x@spatial_enrichment) ||
            lvl1(x@nn_network) ||
            # dimension_reduction: approach -> spat_unit -> ...
            lvl2(x@dimension_reduction),
        feat_type = lvl2(x@expression) || lvl2(x@cell_metadata) ||
            lvl2(x@feat_metadata) || lvl2(x@spatial_enrichment) ||
            lvl2(x@nn_network) ||
            lvl3(x@dimension_reduction),
        values = lvl3(x@expression),
        stop("[.gm_universe_materialized] unknown axis: ", axis,
            call. = FALSE)
    )
}

#' @noRd
.gm_prune_mapping_samples <- function(mapping, keep) {
    lapply(mapping, function(axis_list) {
        lapply(axis_list, function(entry) entry[names(entry) %in% keep])
    })
}

#' @noRd
.gm_rename_mapping_samples <- function(mapping, old_to_new) {
    lapply(mapping, function(axis_list) {
        lapply(axis_list, function(entry) {
            hit <- names(entry) %in% names(old_to_new)
            names(entry)[hit] <- unname(old_to_new[names(entry)[hit]])
            entry
        })
    })
}

#' Apply the active giottoMulti narrowing to a joint-slot subobject.
#'
#' Shared-domain getter methods read the joint slot and pass the result here.
#' Resolves which globals are currently in scope for the subobject's own
#' spat_unit / feat_type, then defers the filtering itself to
#' `.narrow_subobject()` — the per-class axis knowledge is shared with the
#' recipe layer's `resolveSubobject()` rather than duplicated here.
#'
#' A no-op on a single giotto: there is nothing to narrow against, and the
#' subobject is already aligned with the gobject's cells.
#' @noRd
.gm_apply_view <- function(x, gobject) {
    if (!inherits(gobject, "giottoMulti")) return(x)
    # Active narrowing lives in @cell_ID / @feat_ID, indexed by spat_unit /
    # feat_type. @id_map is the full identity registry, not an active filter.
    # Subobjects expose their own spat_unit / feat_type via accessors.
    su <- tryCatch(spatUnit(x), error = function(e) NULL)
    ft <- tryCatch(featType(x), error = function(e) NULL)
    cells <- if (!is.null(su) && length(su) == 1L) {
        gobject@cell_ID[[su]]
    } else NULL
    feats <- if (!is.null(ft) && length(ft) == 1L) {
        gobject@feat_ID[[ft]]
    } else NULL

    .narrow_subobject(x, cells = cells, feats = feats)
}

#' Compute a cheap length-signature of each child's ID slots.
#'
#' Used by `initialize(giottoMulti)` as a fast-path: if signatures match the
#' cached `@id_sig`, the id_map is up-to-date and we skip the rebuild. Catches
#' the realistic mutation modes (cells/features added or removed; child added
#' or replaced). Misses same-length-different-content edits, which are
#' user-error territory at the multi level.
#' @noRd
.gm_compute_sig <- function(objects) {
    lapply(objects, function(g) {
        list(
            cell = lengths(slot(g, "cell_ID")),
            feat = lengths(slot(g, "feat_ID"))
        )
    })
}

#' @noRd
.gm_build_cell_idmap <- function(objects, sep = "::") {
    parts <- lapply(names(objects), function(nm) {
        ids <- tryCatch(spatIDs(objects[[nm]]), error = function(e) character())
        if (length(ids) == 0L) return(NULL)
        data.table::data.table(
            object = nm,
            local_id = ids,
            global_id = paste(nm, ids, sep = sep)
        )
    })
    parts <- Filter(Negate(is.null), parts)
    if (length(parts) == 0L) return(NULL)
    data.table::rbindlist(parts)
}

#' @noRd
.gm_build_feat_idmap <- function(objects) {
    parts <- lapply(names(objects), function(nm) {
        ids <- tryCatch(featIDs(objects[[nm]]), error = function(e) character())
        if (length(ids) == 0L) return(NULL)
        # default passthrough: feature names are the same global vocabulary
        data.table::data.table(
            object = nm,
            local_id = ids,
            global_id = ids
        )
    })
    parts <- Filter(Negate(is.null), parts)
    if (length(parts) == 0L) return(NULL)
    data.table::rbindlist(parts)
}


# ---- helpers for add-time joint-slot nudge -------------------------------
#
# Adding a new sample to a gmulti with populated joint slots leaves those
# slots covering only the original samples. The new sample's cells are
# absent from joint @expression / @cell_metadata / @dimension_reduction /
# @nn_network / @spatial_enrichment until the user either re-runs the
# corresponding compute, drops the slot, or rebuilds via
# createGiottoMulti(). Surface this loudly so it can't be missed.

#' @noRd
.gm_populated_joint_slots <- function(x) {
    candidates <- c("expression", "cell_metadata", "feat_metadata",
        "dimension_reduction", "nn_network", "spatial_enrichment")
    has <- vapply(candidates, function(s) {
        v <- slot(x, s)
        !is.null(v) && length(v) > 0L
    }, logical(1L))
    candidates[has]
}

#' @noRd
.gm_add_joint_nudge <- function(new_sample, slots) {
    slot_list <- paste(slots, collapse = ", ")
    paste0(
        "[gmulti] added sample '", new_sample, "' but joint shared slot(s) ",
        "do not cover its cells: ", slot_list, ".\n",
        "These slots now reflect only the original samples. Options:\n",
        "  - Recompute affected slots (preferred when the analysis ",
        "context matters)\n",
        "  - Drop a stale slot if not needed: ",
        "g@<slot_name> <- list()   (e.g. g@nn_network <- list())\n",
        "  - Rebuild fresh: createGiottoMulti(g@objects)"
    )
}


# ---- helpers for sample-prefix rewriting on rename ------------------------
#
# Joint shared slots key cell-axis content by `sample::local_id`. Renaming a
# child (sample) requires rewriting that prefix everywhere it appears so
# downstream getters / view filters keep matching. Each helper walks one
# slot's specific nesting and returns the rewritten value. Features are
# never sample-namespaced (id_map$feats$global_id == local_id) so the
# feat_metadata / @feat_ID slots stay untouched.

#' @noRd
.rewrite_sample_id <- function(ids, old_to_new) {
    # Split "sample::rest" -> ("sample", "::rest"); remap sample part.
    pos <- regexpr("::", ids, fixed = TRUE)
    has_sep <- pos > 0L
    if (!any(has_sep)) return(ids)
    samp <- substr(ids[has_sep], 1L, pos[has_sep] - 1L)
    rest <- substr(ids[has_sep], pos[has_sep],
        nchar(ids[has_sep]))   # keeps the "::"
    new_samp <- unname(old_to_new[samp])
    # Samples not in the rename map keep their original prefix (safety).
    new_samp[is.na(new_samp)] <- samp[is.na(new_samp)]
    ids[has_sep] <- paste0(new_samp, rest)
    ids
}

#' @noRd
.gm_rewrite_expression <- function(expr_slot, old_to_new) {
    if (is.null(expr_slot) || length(expr_slot) == 0L) return(expr_slot)
    for (su in names(expr_slot)) {
        for (ft in names(expr_slot[[su]])) {
            for (v in names(expr_slot[[su]][[ft]])) {
                e <- expr_slot[[su]][[ft]][[v]]
                m <- e[]
                cn <- colnames(m)
                if (!is.null(cn)) {
                    colnames(m) <- .rewrite_sample_id(cn, old_to_new)
                    e[] <- m
                }
                expr_slot[[su]][[ft]][[v]] <- e
            }
        }
    }
    expr_slot
}

#' @noRd
.gm_rewrite_cell_metadata <- function(cm_slot, old_to_new) {
    if (is.null(cm_slot) || length(cm_slot) == 0L) return(cm_slot)
    cell_ID <- NULL    # NSE
    for (su in names(cm_slot)) {
        for (ft in names(cm_slot[[su]])) {
            cm <- cm_slot[[su]][[ft]]
            dt <- data.table::copy(cm[])
            if ("cell_ID" %in% names(dt)) {
                dt[, cell_ID := .rewrite_sample_id(cell_ID, old_to_new)]
                cm[] <- dt
            }
            cm_slot[[su]][[ft]] <- cm
        }
    }
    cm_slot
}

#' @noRd
.gm_rewrite_dim_reduction <- function(dr_slot, old_to_new) {
    if (is.null(dr_slot) || length(dr_slot) == 0L) return(dr_slot)
    for (red in names(dr_slot)) {
        for (su in names(dr_slot[[red]])) {
            for (ft in names(dr_slot[[red]][[su]])) {
                for (method in names(dr_slot[[red]][[su]][[ft]])) {
                    for (nm in names(dr_slot[[red]][[su]][[ft]][[method]])) {
                        d <- dr_slot[[red]][[su]][[ft]][[method]][[nm]]
                        rn <- rownames(d@coordinates)
                        if (!is.null(rn)) {
                            rownames(d@coordinates) <-
                                .rewrite_sample_id(rn, old_to_new)
                        }
                        dr_slot[[red]][[su]][[ft]][[method]][[nm]] <- d
                    }
                }
            }
        }
    }
    dr_slot
}

#' @noRd
.gm_rewrite_nn_network <- function(nn_slot, old_to_new) {
    if (is.null(nn_slot) || length(nn_slot) == 0L) return(nn_slot)
    for (su in names(nn_slot)) {
        for (ft in names(nn_slot[[su]])) {
            for (nn_type in names(nn_slot[[su]][[ft]])) {
                for (nm in names(nn_slot[[su]][[ft]][[nn_type]])) {
                    nn <- nn_slot[[su]][[ft]][[nn_type]][[nm]]
                    g <- nn@network
                    if (inherits(g, "igraph")) {
                        vn <- names(igraph::V(g))
                        if (!is.null(vn)) {
                            new_vn <- .rewrite_sample_id(vn, old_to_new)
                            g <- igraph::set_vertex_attr(g,
                                "name", value = new_vn)
                            nn@network <- g
                        }
                    }
                    nn_slot[[su]][[ft]][[nn_type]][[nm]] <- nn
                }
            }
        }
    }
    nn_slot
}

#' @noRd
.gm_rewrite_spatial_enrichment <- function(se_slot, old_to_new) {
    if (is.null(se_slot) || length(se_slot) == 0L) return(se_slot)
    cell_ID <- NULL    # NSE
    for (su in names(se_slot)) {
        for (ft in names(se_slot[[su]])) {
            for (nm in names(se_slot[[su]][[ft]])) {
                se <- se_slot[[su]][[ft]][[nm]]
                dt <- data.table::copy(se[])
                if ("cell_ID" %in% names(dt)) {
                    dt[, cell_ID := .rewrite_sample_id(cell_ID, old_to_new)]
                    se[] <- dt
                }
                se_slot[[su]][[ft]][[nm]] <- se
            }
        }
    }
    se_slot
}

#' @noRd
.gm_rewrite_narrowing <- function(ids_slot, old_to_new) {
    if (is.null(ids_slot) || length(ids_slot) == 0L) return(ids_slot)
    lapply(ids_slot, .rewrite_sample_id, old_to_new = old_to_new)
}


# ---- helpers for joint slot pruning on child subset ------------------------
#
# After `g[keep_samples]` drops some samples from @objects, joint shared
# slots that key on `sample::local_id` still reference orphan globals
# (cells from dropped samples). These helpers prune each slot to the
# surviving global cell-id set so the result is self-consistent.

#' @noRd
.gm_prune_expression <- function(expr_slot, keep_globals) {
    if (is.null(expr_slot) || length(expr_slot) == 0L) return(expr_slot)
    for (su in names(expr_slot)) {
        for (ft in names(expr_slot[[su]])) {
            for (v in names(expr_slot[[su]][[ft]])) {
                e <- expr_slot[[su]][[ft]][[v]]
                m <- e[]
                cn <- colnames(m)
                if (!is.null(cn)) {
                    keep_idx <- cn %in% keep_globals
                    e[] <- m[, keep_idx, drop = FALSE]
                }
                expr_slot[[su]][[ft]][[v]] <- e
            }
        }
    }
    expr_slot
}

#' @noRd
.gm_prune_cell_metadata <- function(cm_slot, keep_globals) {
    if (is.null(cm_slot) || length(cm_slot) == 0L) return(cm_slot)
    cell_ID <- NULL    # NSE
    for (su in names(cm_slot)) {
        for (ft in names(cm_slot[[su]])) {
            cm <- cm_slot[[su]][[ft]]
            dt <- data.table::copy(cm[])
            if ("cell_ID" %in% names(dt)) {
                cm[] <- dt[cell_ID %in% keep_globals]
            }
            cm_slot[[su]][[ft]] <- cm
        }
    }
    cm_slot
}

#' @noRd
.gm_prune_dim_reduction <- function(dr_slot, keep_globals) {
    if (is.null(dr_slot) || length(dr_slot) == 0L) return(dr_slot)
    for (red in names(dr_slot)) {
        for (su in names(dr_slot[[red]])) {
            for (ft in names(dr_slot[[red]][[su]])) {
                for (method in names(dr_slot[[red]][[su]][[ft]])) {
                    for (nm in names(dr_slot[[red]][[su]][[ft]][[method]])) {
                        d <- dr_slot[[red]][[su]][[ft]][[method]][[nm]]
                        rn <- rownames(d@coordinates)
                        if (!is.null(rn)) {
                            keep_idx <- rn %in% keep_globals
                            d@coordinates <- d@coordinates[keep_idx, ,
                                drop = FALSE]
                        }
                        dr_slot[[red]][[su]][[ft]][[method]][[nm]] <- d
                    }
                }
            }
        }
    }
    dr_slot
}

#' @noRd
.gm_prune_nn_network <- function(nn_slot, keep_globals) {
    if (is.null(nn_slot) || length(nn_slot) == 0L) return(nn_slot)
    for (su in names(nn_slot)) {
        for (ft in names(nn_slot[[su]])) {
            for (nn_type in names(nn_slot[[su]][[ft]])) {
                for (nm in names(nn_slot[[su]][[ft]][[nn_type]])) {
                    nn <- nn_slot[[su]][[ft]][[nn_type]][[nm]]
                    g <- nn@network
                    if (inherits(g, "igraph")) {
                        vn <- names(igraph::V(g))
                        if (!is.null(vn)) {
                            keep_v <- which(vn %in% keep_globals)
                            nn@network <- igraph::induced_subgraph(
                                g, igraph::V(g)[keep_v])
                        }
                    }
                    nn_slot[[su]][[ft]][[nn_type]][[nm]] <- nn
                }
            }
        }
    }
    nn_slot
}

#' @noRd
.gm_prune_spatial_enrichment <- function(se_slot, keep_globals) {
    if (is.null(se_slot) || length(se_slot) == 0L) return(se_slot)
    cell_ID <- NULL    # NSE
    for (su in names(se_slot)) {
        for (ft in names(se_slot[[su]])) {
            for (nm in names(se_slot[[su]][[ft]])) {
                se <- se_slot[[su]][[ft]][[nm]]
                dt <- data.table::copy(se[])
                if ("cell_ID" %in% names(dt)) {
                    se[] <- dt[cell_ID %in% keep_globals]
                }
                se_slot[[su]][[ft]][[nm]] <- se
            }
        }
    }
    se_slot
}


# ---- federation resolution --------------------------------------------------

# Resolve participation + per-sample child-level name for a gmulti-level
# handle on a given axis. The primary path consults `@mapping`; a
# legacy-compatible fallback inspects child slot keys directly when the
# mapping has no entry for the handle (e.g. a mapping cleared by hand).
# That fallback is the acknowledged child-namespace escape (plan Q5c leak
# 2) — kept so mapping-less objects stay usable.
#
# @param gobject giottoMulti
# @param axis "spat_unit", "feat_type" or "values"
# @param handle gmulti-level name. When NULL: for spat_unit / feat_type the
#   first declared handle is picked (deterministic; the combined-defaults
#   story is plan fed 12 / stage 7); for values the ingest-convention
#   handle "raw" is preferred, a single declared handle is used as-is, and
#   anything else is a loud error — the `common[[1L]]` implicit pick this
#   replaces is exactly what plan A2 removes.
#
# @returns list(handle = <resolved gmulti-level handle or NULL>,
#   map = named character). Names of `map` = participating sample names
#   (NA-keyed samples are declared skips and excluded). Empty `map` means
#   "fall back to legacy per-child defaults."
#' @noRd
.gm_resolve_axis <- function(gobject, axis, handle = NULL) {
    axis_map <- gobject@mapping[[axis]] %||% list()

    if (is.null(handle)) {
        if (length(axis_map) == 0L) {
            return(list(handle = NULL, map = character()))
        }
        handle <- if (axis == "values") {
            if ("raw" %in% names(axis_map)) {
                "raw"
            } else if (length(axis_map) == 1L) {
                names(axis_map)[[1L]]
            } else {
                stop("[gmulti] no `name`/`values` requested, no 'raw' handle ",
                    "declared, and several values handles exist (",
                    paste(names(axis_map), collapse = ", "),
                    "). Specify one explicitly.", call. = FALSE)
            }
        } else {
            names(axis_map)[[1L]]
        }
    }

    if (handle %in% names(axis_map)) {
        entry <- axis_map[[handle]]
        # every entry keys every sample (validated on set; discovery seeds
        # NA). A sample missing entirely is hand-edit territory: loud.
        missing_s <- setdiff(names(gobject@objects), names(entry))
        if (length(missing_s) > 0L) {
            stop(sprintf(paste0(
                "[gmulti] mapping %s[['%s']] does not key sample(s): %s.\n",
                "Every entry keys every sample; use NA_character_ for a ",
                "deliberate skip."),
                axis, handle, paste(missing_s, collapse = ", ")),
                call. = FALSE)
        }
        entry <- entry[names(entry) %in% names(gobject@objects)]
        entry <- entry[!is.na(entry)]   # NA = declared non-participation
        return(list(handle = handle, map = entry))
    }

    # Fallback: handle not declared in @mapping. Treat gmulti-name ==
    # child-name and find samples whose child slot carries it.
    samples <- names(gobject@objects)
    have <- vapply(samples, function(s) {
        handle %in% .gm_child_axis_names(gobject@objects[[s]], axis)
    }, logical(1L))
    samples <- samples[have]
    if (length(samples) == 0L) {
        return(list(handle = handle, map = character()))
    }
    list(handle = handle,
        map = stats::setNames(rep(handle, length(samples)), samples))
}

# Resolve the federation: per-sample (spat_unit, feat_type) child-level
# names for a given pair of gmulti-level handles. Combines two
# [.gm_resolve_axis] calls and intersects the participating samples.
#
# When EITHER axis has no mapping resolution, the corresponding side
# iterates every child via the legacy default resolution
# (set_default_spat_unit / set_default_feat_type) — backwards compat with
# gmulti objects whose @mapping wasn't populated.
#
# @returns list(su_handle =, ft_handle =, samples =) where `samples` is a
#   named list of `list(su = "<child_su>", ft = "<child_ft>")` keyed by
#   sample name (empty when no sample participates). The handles are the
#   resolved gmulti-level names — previously discarded, now returned so
#   assembly can stamp them (plan Q5c).
#' @noRd
.gm_resolve_participation <- function(gobject, spat_unit = NULL,
    feat_type = NULL) {
    su_res <- .gm_resolve_axis(gobject, "spat_unit", spat_unit)
    ft_res <- .gm_resolve_axis(gobject, "feat_type", feat_type)
    su_map <- su_res$map
    ft_map <- ft_res$map

    # Legacy fallback when an axis has no resolution (empty @mapping +
    # no usable child slot keys). We synthesize the per-child default
    # resolution that the pre-@mapping helpers used.
    if (length(su_map) == 0L) {
        su_map <- vapply(names(gobject@objects), function(nm) {
            g <- gobject@objects[[nm]]
            if (!is.null(spat_unit)) return(spat_unit)
            tryCatch(set_default_spat_unit(g),
                error = function(e) NA_character_)
        }, character(1L))
        su_map <- su_map[!is.na(su_map)]
    }
    if (length(ft_map) == 0L) {
        # ft depends on the resolved su per-child; pull each child's su
        # from the (already resolved) su_map
        ft_map <- vapply(names(gobject@objects), function(nm) {
            g <- gobject@objects[[nm]]
            if (!is.null(feat_type)) return(feat_type)
            if (!nm %in% names(su_map)) return(NA_character_)
            tryCatch(set_default_feat_type(g, spat_unit = su_map[[nm]]),
                error = function(e) NA_character_)
        }, character(1L))
        ft_map <- ft_map[!is.na(ft_map)]
    }

    participants <- intersect(names(su_map), names(ft_map))
    samples <- lapply(participants, function(nm) {
        list(su = unname(su_map[nm]), ft = unname(ft_map[nm]))
    })
    names(samples) <- participants

    list(
        su_handle = su_res$handle %||% spat_unit,
        ft_handle = ft_res$handle %||% feat_type,
        samples = samples
    )
}

# Parse a name string for a "sample::name" prefix. Returns a list with
# `sample` (NULL when no prefix) and `name` (the un-prefixed remainder).
#
# Used by gmulti-aware getters to let the caller address a per-sample
# slice via the existing `name` / `values` arg without needing a separate
# `samples =` call:
#
#     getExpression(mg, values = "B191::raw")
#     # equivalent to:
#     getExpression(mg, samples = "B191", values = "raw")
#
# Convention matches the cell_ID namespacing (`sample::cell_id`) so the
# whole stack uses one separator. Multi-`::` strings are intentionally
# only split at the FIRST occurrence — `"B191::raw_x::y"` becomes
# `sample = "B191"`, `name = "raw_x::y"` — to permit `::` inside user
# names if anyone ever needs them.
#
# Validation deferred to callers: the parser doesn't know which samples
# exist, so an unknown sample name passes through. Caller errors if the
# resolved sample isn't in @objects.
#' @noRd
.parse_sample_qualified_name <- function(name) {
    if (is.null(name) || !nzchar(name)) return(list(sample = NULL, name = name))
    if (!grepl("::", name, fixed = TRUE)) {
        return(list(sample = NULL, name = name))
    }
    idx <- regexpr("::", name, fixed = TRUE)
    sample <- substr(name, 1L, idx - 1L)
    rest <- substr(name, idx + 2L, nchar(name))
    list(sample = sample, name = rest)
}

# Slice a federated joint subobject to one or more samples. The joint
# subobject's cell-axis IDs follow the `sample::cell_id` convention
# (assembled in .gm_assemble_*); slicing matches any of the sample
# prefixes (OR semantics across the vector). Cell ID prefixes are
# preserved on output for global addressability.
#
# When `samples` is NULL, returns the input unchanged.
#' @noRd
.gm_slice_to_samples <- function(x, samples, gobject) {
    if (is.null(samples)) return(x)
    checkmate::assert_character(samples, min.len = 1L, any.missing = FALSE)
    bad <- setdiff(samples, names(gobject@objects))
    if (length(bad) > 0L) {
        stop(sprintf(
            "[gmulti getter] sample(s) '%s' not in @objects (have: %s)",
            paste(bad, collapse = ", "),
            paste(names(gobject@objects), collapse = ", ")),
            call. = FALSE)
    }
    prefixes <- paste0(samples, "::")
    starts_any <- function(ids) {
        # OR across prefixes: TRUE for ids that start with any of them.
        out <- rep(FALSE, length(ids))
        for (p in prefixes) out <- out | startsWith(ids, p)
        out
    }

    if (inherits(x, "exprObj")) {
        keep <- starts_any(colnames(x[]))
        x[] <- x[][, keep, drop = FALSE]
        return(x)
    }
    if (inherits(x, "cellMetaObj") || inherits(x, "spatEnrObj")) {
        cell_ID <- NULL  # data.table NSE
        dt <- x[]
        x[] <- dt[starts_any(cell_ID)]
        return(x)
    }
    if (inherits(x, "featMetaObj")) {
        # Feature IDs aren't sample-namespaced (passthrough convention),
        # so featmeta is sample-uniform — slicing is a no-op here.
        return(x)
    }
    if (inherits(x, "dimObj")) {
        coords <- x@coordinates
        keep <- starts_any(rownames(coords))
        x@coordinates <- coords[keep, , drop = FALSE]
        return(x)
    }
    if (inherits(x, "nnNetObj")) {
        g <- x@network
        vnames <- names(igraph::V(g))
        keep <- starts_any(vnames)
        x@network <- igraph::induced_subgraph(g, igraph::V(g)[keep])
        return(x)
    }
    x
}


# ---- joint assembly ---------------------------------------------------------

# Pad a matrix to a row (feature) universe, 0-filling missing rows.
# Supports base matrices and Matrix-package sparse/dense classes. Used by
# the `on_missing = "fill"` assembly mode; the 0-fill convention (an
# absent measurement reads as 0, not NA) is documented on the getters.
#' @noRd
.gm_pad_matrix_rows <- function(mat, all_rows) {
    missing_rows <- setdiff(all_rows, rownames(mat))
    if (length(missing_rows) == 0L) {
        return(mat[all_rows, , drop = FALSE])
    }
    pad <- if (inherits(mat, "Matrix")) {
        Matrix::Matrix(0, nrow = length(missing_rows), ncol = ncol(mat),
            sparse = TRUE,
            dimnames = list(missing_rows, colnames(mat)))
    } else {
        matrix(0, nrow = length(missing_rows), ncol = ncol(mat),
            dimnames = list(missing_rows, colnames(mat)))
    }
    rbind(mat, pad)[all_rows, , drop = FALSE]
}

# Shared handling for children that are keyed in @mapping but cannot
# produce the requested content ("keyed but unsatisfiable" — plan Q5b
# rule 2). `failures` is a character vector of per-sample descriptions.
#' @noRd
.gm_on_missing_children <- function(failures, on_missing, site) {
    if (length(failures) == 0L) return(invisible(NULL))
    msg <- paste0(
        "child(ren) keyed in @mapping but unsatisfiable: ",
        paste(failures, collapse = "; "), ".\n",
        "Remedies: rename the child's content, point that sample's mapping ",
        "entry at the actual name (see ?gmultiMapping), or set it ",
        "NA_character_ to skip the sample deliberately.")
    if (on_missing == "error") {
        stop("[", site, "] ", msg, call. = FALSE)
    }
    # "drop" and "fill" both drop the failing child — there is nothing to
    # fill a fully-missing child with. Loud, so a partial federation can't
    # pass as a full one.
    warning("[", site, "] on_missing = '", on_missing, "': dropping ", msg,
        call. = FALSE)
    invisible(NULL)
}

#' Assemble a joint expression matrix from children.
#'
#' Federation is driven by `@mapping`: the (spat_unit, feat_type) handles
#' resolve to per-sample child-level slot names via
#' `.gm_resolve_participation()`, and the expression name resolves through
#' the mapping's `values` axis — never by intersecting whatever names the
#' children happen to share (plan A2). Samples with an `NA` entry on any
#' involved axis are declared skips and do not contribute.
#'
#' A child that is keyed but cannot produce the requested matrix is a loud
#' error by default (`on_missing = "error"`); so are mismatched feature
#' panels. `"drop"` opts into intersection semantics; `"fill"` unions the
#' panels with 0-fill.
#'
#' The assembled `exprObj`'s identity tags describe the federation, not
#' whichever child sorted first (plan Q5c): parent handles on `@spat_unit`
#' / `@feat_type`, the values handle on `@name`, and the parent spat_unit
#' handle on `@provenance` (the `prov(res) <- spatUnit(x)` convention, see
#' aggregate.R). The participation set and per-sample resolved names are
#' stamped on `@misc$gmulti` so a later read cannot silently mean a
#' different sample set.
#'
#' When `@mapping` is empty (legacy multi), falls back to per-child
#' `set_default_spat_unit` / `set_default_feat_type` resolution and the
#' first-child template tags — matches the pre-`@mapping` behavior so old
#' objects keep working.
#'
#' Called by `getExpression(giottoMulti, ...)` when `@expression` is empty.
#' This is the baseline view; integration tools (Harmony, scVI, etc.)
#' overwrite it via `setExpression(mg, joint)` once they've produced a
#' corrected matrix.
#' @noRd
.gm_assemble_expression <- function(gobject, spat_unit, feat_type, values,
    on_missing = c("error", "drop", "fill")) {
    on_missing <- match.arg(on_missing)
    if (length(gobject@objects) == 0L) {
        stop("[gmulti getExpression] giottoMulti has no children to ",
            "assemble expression from", call. = FALSE)
    }

    part <- .gm_resolve_participation(gobject, spat_unit, feat_type)
    resolved <- part$samples
    if (length(resolved) == 0L) {
        stop(wrap_txt("No children participate in the requested
            (spat_unit, feat_type) federation. Check `gmultiMapping(mg)`
            or pass `spat_unit` / `feat_type` arguments that map to
            declared handles."), call. = FALSE)
    }

    # values: mapping lookup (plan A2 — replaces the `common[[1L]]` pick).
    v_res <- .gm_resolve_axis(gobject, "values", values)
    v_handle <- v_res$handle
    v_map <- v_res$map
    if (length(v_map) == 0L) {
        if (is.null(values)) {
            stop("[gmulti getExpression] no `name`/`values` requested and ",
                "@mapping declares no values axis. Declare one (",
                "gmultiMapping(mg, 'values', 'raw') <- c(...)) or pass ",
                "name = explicitly.", call. = FALSE)
        }
        # requested name undeclared and carried by no child — assembly
        # below would produce nothing; name it now
        stop(sprintf(paste0(
            "[gmulti getExpression] no child carries an expression named ",
            "'%s' and @mapping declares no handle of that name."),
            values), call. = FALSE)
    }

    # participation on the values axis composes with the (su, ft) set;
    # NA-keyed samples are declared skips on either
    contributors <- intersect(names(resolved), names(v_map))
    if (length(contributors) == 0L) {
        stop(sprintf(paste0(
            "[gmulti getExpression] no sample participates in both the ",
            "(spat_unit, feat_type) federation (%s) and values handle ",
            "'%s' (%s)."),
            paste(names(resolved), collapse = ", "), v_handle,
            paste(names(v_map), collapse = ", ")), call. = FALSE)
    }

    failures <- character()
    per_child <- lapply(contributors, function(nm) {
        r <- resolved[[nm]]
        e <- tryCatch(getExpression(gobject@objects[[nm]],
            spat_unit = r$su, feat_type = r$ft,
            values = v_map[[nm]],
            output = "exprObj", set_defaults = FALSE),
            error = function(err) NULL)
        if (is.null(e)) {
            return(sprintf("%s (values = '%s', spat_unit = '%s', feat_type = '%s')",
                nm, v_map[[nm]], r$su, r$ft))
        }
        mat <- e[]
        colnames(mat) <- paste(nm, colnames(mat), sep = "::")
        list(mat = mat, exprObj = e, name = nm)
    })
    names(per_child) <- contributors
    is_failure <- vapply(per_child, is.character, logical(1L))
    .gm_on_missing_children(unlist(per_child[is_failure]), on_missing,
        site = "gmulti getExpression")
    per_child <- per_child[!is_failure]
    if (length(per_child) == 0L) {
        stop("[gmulti getExpression] no child could provide the requested ",
            "expression", call. = FALSE)
    }

    # feature panels: identical panels concatenate as-is; anything else is
    # governed by on_missing — error (default), drop (intersect), or fill
    # (union with 0-fill)
    per_feats <- lapply(per_child, function(x) rownames(x$mat))
    feats_common <- Reduce(intersect, per_feats)
    feats_union <- unique(unlist(per_feats, use.names = FALSE))
    if (length(feats_common) != length(feats_union)) {
        if (on_missing == "error") {
            stop(sprintf(paste0(
                "[gmulti getExpression] feature panels differ across ",
                "children: %d shared of %d union. Pass on_missing = 'drop' ",
                "to intersect, or 'fill' to union with 0-fill."),
                length(feats_common), length(feats_union)), call. = FALSE)
        }
        if (on_missing == "drop" && length(feats_common) == 0L) {
            stop("[gmulti getExpression] no features common to all ",
                "children for this expression set", call. = FALSE)
        }
    }
    mats <- if (on_missing == "fill") {
        lapply(per_child, function(x) .gm_pad_matrix_rows(x$mat, feats_union))
    } else {
        lapply(per_child, function(x) x$mat[feats_common, , drop = FALSE])
    }
    joint_mat <- do.call(cbind, mats)

    # First child's exprObj is the structural template only — the identity
    # tags are re-stamped with the federation handles below (plan Q5c).
    template <- per_child[[1L]]$exprObj
    template[] <- joint_mat
    if (!is.null(part$su_handle)) {
        spatUnit(template) <- part$su_handle
        # per-child origins are recorded in @mapping; provenance carries
        # the parent handle so the artifact stays self-consistent
        prov(template) <- part$su_handle
    }
    if (!is.null(part$ft_handle)) featType(template) <- part$ft_handle
    if (!is.null(v_handle)) objName(template) <- v_handle

    # participation stamp — bake which children built this artifact and
    # what each contributed (ADR 0006 shape: content that depends on the
    # participant set records that set at build time)
    misc <- template@misc %||% list()
    misc$gmulti <- list(
        participation = names(per_child),
        resolved = stats::setNames(lapply(names(per_child), function(nm) {
            list(su = resolved[[nm]]$su, ft = resolved[[nm]]$ft,
                values = unname(v_map[[nm]]))
        }), names(per_child))
    )
    template@misc <- misc
    template
}

#' Assemble joint cell metadata from children's per-child cell metadata.
#'
#' Pulls each child's cellMetaObj for the federated nesting, prefixes
#' cell_ID to globals (sample::id), and rbinds. Column-set mismatches are
#' governed by `on_missing`: error (default), drop (intersect columns), or
#' fill (union, NA-fill via rbindlist).
#'
#' Identity tags carry the federation handles (plan Q5c), same as
#' expression assembly. `cellMetaObj` has no `@misc` slot, so the
#' participation stamp lives on the `list_ID` column — every row carries
#' its sample of origin.
#' @noRd
.gm_assemble_cell_metadata <- function(gobject, spat_unit, feat_type,
    on_missing = c("error", "drop", "fill")) {
    on_missing <- match.arg(on_missing)
    cell_ID <- list_ID <- NULL    # NSE

    part <- .gm_resolve_participation(gobject, spat_unit, feat_type)
    resolved <- part$samples

    per_child <- lapply(names(resolved), function(nm) {
        r <- resolved[[nm]]
        cm <- tryCatch(getCellMetadata(gobject@objects[[nm]],
            spat_unit = r$su, feat_type = r$ft,
            output = "cellMetaObj", set_defaults = FALSE),
            error = function(e) NULL)
        if (is.null(cm)) {
            return(sprintf("%s (spat_unit = '%s', feat_type = '%s')",
                nm, r$su, r$ft))
        }
        dt <- data.table::copy(cm[])
        dt[, cell_ID := paste(nm, cell_ID, sep = "::")]
        # Sample-origin tag — matches joinGiottoObjects' convention so
        # downstream tools (e.g. runGiottoHarmony's vars_use = "list_ID"
        # default) work out of the box on the assembled multi metadata.
        dt[, list_ID := nm]
        list(cm = cm, dt = dt)
    })
    names(per_child) <- names(resolved)
    is_failure <- vapply(per_child, is.character, logical(1L))
    .gm_on_missing_children(unlist(per_child[is_failure]), on_missing,
        site = "gmulti getCellMetadata")
    per_child <- per_child[!is_failure]
    if (length(per_child) == 0L) {
        stop("[gmulti getCellMetadata] no child has cell metadata for the ",
            "requested nesting", call. = FALSE)
    }

    per_cols <- lapply(per_child, function(x) names(x$dt))
    cols_common <- Reduce(intersect, per_cols)
    cols_union <- unique(unlist(per_cols, use.names = FALSE))
    if (length(cols_common) != length(cols_union) && on_missing == "error") {
        stop(sprintf(paste0(
            "[gmulti getCellMetadata] metadata columns differ across ",
            "children (%d shared of %d union; differing: %s). Pass ",
            "on_missing = 'drop' to intersect, or 'fill' to union with NA."),
            length(cols_common), length(cols_union),
            paste(setdiff(cols_union, cols_common), collapse = ", ")),
            call. = FALSE)
    }
    joint_dt <- if (on_missing == "fill") {
        data.table::rbindlist(lapply(per_child, function(x) x$dt),
            use.names = TRUE, fill = TRUE)
    } else {
        data.table::rbindlist(lapply(per_child, function(x) {
            x$dt[, cols_common, with = FALSE]
        }), use.names = TRUE)
    }

    template <- per_child[[1L]]$cm
    template[] <- joint_dt
    if (!is.null(part$su_handle)) {
        spatUnit(template) <- part$su_handle
        prov(template) <- part$su_handle
    }
    if (!is.null(part$ft_handle)) featType(template) <- part$ft_handle
    template
}

#' Assemble joint feature metadata from children's per-child feat metadata.
#'
#' Parallel to .gm_assemble_cell_metadata. Feature IDs are passthrough
#' (no global namespacing), so the rbind happens directly; rows for the
#' same feature across children are deduplicated by `feat_ID` (first
#' child's row wins — typical assumption is shared panel).
#' @noRd
.gm_assemble_feat_metadata <- function(gobject, spat_unit, feat_type,
    on_missing = c("error", "drop", "fill")) {
    on_missing <- match.arg(on_missing)

    part <- .gm_resolve_participation(gobject, spat_unit, feat_type)
    resolved <- part$samples

    per_child <- lapply(names(resolved), function(nm) {
        r <- resolved[[nm]]
        fm <- tryCatch(getFeatureMetadata(gobject@objects[[nm]],
            spat_unit = r$su, feat_type = r$ft,
            output = "featMetaObj", set_defaults = FALSE),
            error = function(e) NULL)
        if (is.null(fm)) {
            return(sprintf("%s (spat_unit = '%s', feat_type = '%s')",
                nm, r$su, r$ft))
        }
        list(fm = fm, dt = data.table::copy(fm[]))
    })
    names(per_child) <- names(resolved)
    is_failure <- vapply(per_child, is.character, logical(1L))
    .gm_on_missing_children(unlist(per_child[is_failure]), on_missing,
        site = "gmulti getFeatureMetadata")
    per_child <- per_child[!is_failure]
    if (length(per_child) == 0L) {
        stop("[gmulti getFeatureMetadata] no child has feature metadata ",
            "for the requested nesting", call. = FALSE)
    }

    per_cols <- lapply(per_child, function(x) names(x$dt))
    cols_common <- Reduce(intersect, per_cols)
    cols_union <- unique(unlist(per_cols, use.names = FALSE))
    if (length(cols_common) != length(cols_union) && on_missing == "error") {
        stop(sprintf(paste0(
            "[gmulti getFeatureMetadata] metadata columns differ across ",
            "children (differing: %s). Pass on_missing = 'drop' to ",
            "intersect, or 'fill' to union with NA."),
            paste(setdiff(cols_union, cols_common), collapse = ", ")),
            call. = FALSE)
    }
    joint_dt <- if (on_missing == "fill") {
        unique(data.table::rbindlist(lapply(per_child, function(x) x$dt),
            use.names = TRUE, fill = TRUE), by = "feat_ID")
    } else {
        unique(data.table::rbindlist(lapply(per_child, function(x) {
            x$dt[, cols_common, with = FALSE]
        }), use.names = TRUE), by = "feat_ID")
    }

    template <- per_child[[1L]]$fm
    template[] <- joint_dt
    if (!is.null(part$su_handle)) {
        spatUnit(template) <- part$su_handle
        prov(template) <- part$su_handle
    }
    if (!is.null(part$ft_handle)) featType(template) <- part$ft_handle
    template
}

#' Fetch a child augmented with joint-only metadata columns, via the
#' access layer.
#'
#' Per-child consumers (combineMetadata / combineCellData, per-panel
#' plotting) need joint analysis columns (leiden, harmony projections ...)
#' that by the child-immutability invariant live only on the parent's
#' joint slot. This fetches the sample's slice through
#' `getCellMetadata(mg, samples = )` — the access-layer choke point, so
#' any active narrowing applies — strips the `sample::` prefix, and adds
#' the joint-only columns to a local copy of the child. The child in
#' `@objects` is never mutated.
#' @noRd
.gm_child_with_joint_meta <- function(mg, child_name) {
    cell_ID <- NULL # NSE
    child_g <- mg@objects[[child_name]]
    joint_cm <- tryCatch(
        getCellMetadata(mg, output = "data.table", samples = child_name),
        error = function(e) NULL
    )
    if (is.null(joint_cm) || nrow(joint_cm) == 0L) return(child_g)

    prefix <- paste0(child_name, "::")
    joint_cm <- joint_cm[startsWith(cell_ID, prefix)]
    if (nrow(joint_cm) == 0L) return(child_g)
    joint_cm <- data.table::copy(joint_cm)
    joint_cm[, cell_ID := substring(cell_ID, nchar(prefix) + 1L)]

    child_cm <- tryCatch(pDataDT(child_g), error = function(e) NULL)
    if (is.null(child_cm)) return(child_g)
    joint_only <- setdiff(names(joint_cm), c(names(child_cm), "list_ID"))
    if (length(joint_only) == 0L) return(child_g)

    addCellMetadata(child_g,
        new_metadata = joint_cm[, c("cell_ID", joint_only), with = FALSE],
        by_column = TRUE,
        column_cell_ID = "cell_ID")
}


# MULTI-SPECIFIC ACCESSORS ####

# mapping accessor — declares child-level federation per axis ####

#' @title gmulti federation mapping accessor
#' @name gmultiMapping
#' @description
#' Get or set the `@mapping` slot: declares which child-level spat_units,
#' feat_types and expression names (`values`) federate up to gmulti-level
#' handles, with per-child name reconciliation.
#'
#' Auto-discovered at construction: `spat_unit` / `feat_type` get the
#' symmetric trivial mapping (handle == child-level name), and `values` is
#' seeded to the ingest convention `"raw"`. Every entry keys **every**
#' sample; `NA_character_` is the deliberate-skip sentinel ("this sample
#' does not contribute to this handle"). A keyed sample whose child cannot
#' satisfy the entry is a loud error at read time, naming the sample.
#'
#' The setter has three forms:
#'
#' * Full replacement: `gmultiMapping(mg) <- list(spat_unit = ..., feat_type = ..., values = ...)`
#' * Whole-axis replacement: `gmultiMapping(mg, "spat_unit") <- list(cell = ..., nucleus = ...)`
#' * Single-entry replacement: `gmultiMapping(mg, "values", "raw") <- c(B191 = "raw", B215 = "counts")`
#'
#' Setting a single-entry vector to `NULL` removes that entry. Assigning
#' the top-level mapping to `NULL` triggers fresh auto-discovery from the
#' current child population.
#'
#' All setter forms validate the resulting mapping and invalidate joint
#' slot state per affected universe — joint state for unrelated universes
#' survives untouched. **Expansion is blocked once a universe is
#' materialized**: joint content was built from a specific participation
#' set, so widening the declaration afterwards would leave content that
#' omits a sample the declaration now claims. Opting a sample in is
#' explicit — drop the joint content for that universe, then re-declare.
#'
#' @param x a `giottoMulti`
#' @param which one of `"spat_unit"`, `"feat_type"` or `"values"` to
#'   narrow the return / target axis; default `NULL` returns or replaces
#'   the full mapping list
#' @param handle a single gmulti-level handle name (e.g. `"cell"`) under
#'   the chosen axis; used only by the single-entry setter
#' @param value depends on the setter form: full mapping list,
#'   axis-shaped list, single per-sample-named char vector, or `NULL`
#' @param ... see `which` and `handle`
#' @returns the requested mapping (full list or one axis)
#' @export
setGeneric("gmultiMapping",
    function(x, ...) standardGeneric("gmultiMapping"))

#' @rdname gmultiMapping
#' @export
setMethod("gmultiMapping", "giottoMulti",
    function(x, which = NULL, ...) {
        if (is.null(which)) return(x@mapping)
        which <- match.arg(which, c("spat_unit", "feat_type", "values"))
        x@mapping[[which]]
    }
)

#' @rdname gmultiMapping
#' @export
setGeneric("gmultiMapping<-",
    function(x, ..., value) standardGeneric("gmultiMapping<-"))

#' @rdname gmultiMapping
#' @export
setMethod("gmultiMapping<-", "giottoMulti",
    function(x, ..., value) {
        dots <- list(...)
        # Accept positional or named (which, handle). R's setter dispatch
        # reserves the trailing position for `value` — extra positional
        # args bind in order.
        which <- dots[["which"]] %||%
            (if (length(dots) >= 1L) dots[[1L]] else NULL)
        handle <- dots[["handle"]] %||%
            (if (length(dots) >= 2L) dots[[2L]] else NULL)

        old_mapping <- x@mapping

        if (is.null(which) && is.null(handle)) {
            # Full replacement.
            if (is.null(value)) {
                x@mapping <- .gm_discover_mapping(x@objects)
                return(.gm_invalidate_joint_for_mapping_change(x, old = NULL))
            }
            new_mapping <- .gm_validate_mapping(value, x@objects)
            .gm_assert_no_participation_expansion(x, old_mapping, new_mapping)
            x@mapping <- new_mapping
            return(.gm_invalidate_joint_for_mapping_change(x,
                old = old_mapping))
        }

        # Axis-scoped or entry-scoped replacement: build the candidate
        # full mapping then route through the same validate + block +
        # invalidate path so the universe-scoped logic is identical.
        which <- match.arg(which, c("spat_unit", "feat_type", "values"))
        candidate <- old_mapping
        if (is.null(candidate$values)) candidate$values <- list()

        if (is.null(handle)) {
            # Whole-axis replacement. NULL drops the axis entirely.
            if (is.null(value)) {
                candidate[[which]] <- list()
            } else {
                checkmate::assert_list(value,
                    .var.name = sprintf("mapping$%s", which))
                candidate[[which]] <- value
            }
        } else {
            # Single-entry replacement. NULL drops that entry.
            checkmate::assert_string(handle)
            if (is.null(value)) {
                candidate[[which]][[handle]] <- NULL
            } else {
                checkmate::assert_character(value, names = "unique",
                    .var.name = sprintf("mapping$%s$%s", which, handle))
                candidate[[which]][[handle]] <- value
            }
        }

        new_mapping <- .gm_validate_mapping(candidate, x@objects)
        .gm_assert_no_participation_expansion(x, old_mapping, new_mapping)
        x@mapping <- new_mapping
        .gm_invalidate_joint_for_mapping_change(x, old = old_mapping)
    }
)

# Validate a candidate @mapping against the current child population.
# - Must be a list with `spat_unit`, `feat_type` and `values` named
#   entries (each a list of named char vectors).
# - Every entry must key every sample in @objects; NA_character_ is the
#   deliberate-skip sentinel and passes.
# - Non-NA per-sample child names must exist on the named child for the
#   relevant axis (cell_ID keys / feat_ID keys / expression names).
# - Declaring a per-gene-standardized matrix (e.g. "scaled") for
#   federation warns: each child was standardized to its own gene
#   statistics, so the joint concat is not comparable across samples.
#   The user is explicitly asking for it, which is the right place for
#   the friction (plan Q5).
# Errors clearly on invalid input rather than silently passing through.
#' @noRd
.gm_validate_mapping <- function(value, objects) {
    checkmate::assert_list(value, names = "unique", .var.name = "mapping")
    axes <- c("spat_unit", "feat_type", "values")
    if (!all(axes %in% names(value))) {
        stop("[gmultiMapping<-] value must have `spat_unit`, `feat_type` ",
            "and `values` named entries (each a list of per-sample char ",
            "vectors)", call. = FALSE)
    }
    sample_names <- names(objects)
    validate_axis <- function(axis_list, axis) {
        checkmate::assert_list(axis_list,
            .var.name = sprintf("mapping$%s", axis))
        for (handle in names(axis_list)) {
            entry <- axis_list[[handle]]
            checkmate::assert_character(entry, names = "unique",
                .var.name = sprintf("mapping$%s$%s", axis, handle))
            bad_s <- setdiff(names(entry), sample_names)
            if (length(bad_s) > 0L) {
                stop(sprintf(
                    "[gmultiMapping<-] %s[['%s']] references unknown sample(s): %s",
                    axis, handle, paste(bad_s, collapse = ", ")),
                    call. = FALSE)
            }
            missing_s <- setdiff(sample_names, names(entry))
            if (length(missing_s) > 0L) {
                stop(sprintf(paste0(
                    "[gmultiMapping<-] %s[['%s']] does not key sample(s): ",
                    "%s. Every entry keys every sample; use NA_character_ ",
                    "for a deliberate skip."),
                    axis, handle, paste(missing_s, collapse = ", ")),
                    call. = FALSE)
            }
            for (s in names(entry)) {
                if (is.na(entry[[s]])) next   # deliberate-skip sentinel
                child_keys <- .gm_child_axis_names(objects[[s]], axis)
                if (!entry[[s]] %in% child_keys) {
                    stop(sprintf(paste0(
                        "[gmultiMapping<-] %s[['%s']][['%s']] = '%s' not ",
                        "present on that child (have: %s)"),
                        axis, handle, s, entry[[s]],
                        paste(child_keys, collapse = ", ")),
                        call. = FALSE)
                }
            }
            if (axis == "values" &&
                any(!is.na(entry) & entry == "scaled")) {
                warning(sprintf(paste0(
                    "[gmultiMapping<-] values[['%s']] federates 'scaled' ",
                    "matrices. Per-gene standardization used each child's ",
                    "own gene statistics, so the joint matrix is not ",
                    "comparable across samples. Make sure this is intended."),
                    handle), call. = FALSE)
            }
        }
    }
    validate_axis(value$spat_unit, "spat_unit")
    validate_axis(value$feat_type, "feat_type")
    validate_axis(value$values, "values")
    value
}

# Block-on-expansion (plan Q1): a handle's participation set cannot be
# expanded once that universe has been materialized at the parent —
# existing joint content was built from a specific participation set, and
# silently widening the declaration afterwards leaves content that omits a
# sample the declaration now claims. Opting a sample in is explicit: drop
# the joint content for that universe, then re-declare.
#' @noRd
.gm_assert_no_participation_expansion <- function(x, old, new) {
    for (axis in c("spat_unit", "feat_type", "values")) {
        old_axis <- old[[axis]] %||% list()
        new_axis <- new[[axis]] %||% list()
        for (handle in names(new_axis)) {
            old_entry <- old_axis[[handle]]
            new_entry <- new_axis[[handle]]
            old_p <- names(old_entry)[!is.na(old_entry)]
            new_p <- names(new_entry)[!is.na(new_entry)]
            added <- setdiff(new_p, old_p)
            if (length(added) == 0L) next
            if (.gm_universe_materialized(x, axis, handle)) {
                stop(sprintf(paste0(
                    "[gmultiMapping<-] cannot expand participation for %s ",
                    "handle '%s' (adding: %s) -- joint content for that ",
                    "universe is already materialized and does not cover ",
                    "the added sample(s).\nDrop the joint content for ",
                    "'%s' first, then re-declare."),
                    axis, handle, paste(added, collapse = ", "), handle),
                    call. = FALSE)
            }
        }
    }
    invisible(NULL)
}

# Invalidate joint-slot state for any universe whose mapping changed.
# Conservative: when `old` is NULL (NULL-assigned reset), drop all joint
# state; when `old` is provided, drop only the universes whose per-sample
# vector changed. Slot nestings differ and are handled per-slot:
# cell_metadata / feat_metadata / spatial_enrichment / nn_network are
# spat_unit -> feat_type; expression adds a values level;
# dimension_reduction is approach -> spat_unit -> feat_type -> ... .
#' @noRd
.gm_invalidate_joint_for_mapping_change <- function(x, old = NULL) {
    if (is.null(old)) {
        # full reset
        x@expression <- NULL
        x@cell_metadata <- NULL
        x@feat_metadata <- NULL
        x@dimension_reduction <- NULL
        x@nn_network <- NULL
        x@spatial_enrichment <- NULL
        return(x)
    }
    new <- x@mapping
    su_changed <- .gm_axis_changed_keys(old$spat_unit, new$spat_unit)
    ft_changed <- .gm_axis_changed_keys(old$feat_type, new$feat_type)
    v_changed <- .gm_axis_changed_keys(old$values, new$values)

    drop_keys <- function(sl, keys) {
        if (is.null(sl)) return(NULL)
        sl[!names(sl) %in% keys]
    }
    drop_lvl2 <- function(sl, keys) {
        if (is.null(sl)) return(NULL)
        lapply(sl, function(l) {
            if (!is.list(l)) return(l)
            l[!names(l) %in% keys]
        })
    }

    # su -> ft nested slots
    x@cell_metadata <- drop_lvl2(drop_keys(x@cell_metadata, su_changed),
        ft_changed)
    x@feat_metadata <- drop_lvl2(drop_keys(x@feat_metadata, su_changed),
        ft_changed)
    x@spatial_enrichment <- drop_lvl2(
        drop_keys(x@spatial_enrichment, su_changed), ft_changed)
    x@nn_network <- drop_lvl2(drop_keys(x@nn_network, su_changed),
        ft_changed)

    # expression: su -> ft -> values
    ex <- drop_lvl2(drop_keys(x@expression, su_changed), ft_changed)
    if (!is.null(ex) && length(v_changed) > 0L) {
        ex <- lapply(ex, function(by_su) {
            lapply(by_su, function(by_ft) {
                by_ft[!names(by_ft) %in% v_changed]
            })
        })
    }
    x@expression <- ex

    # dimension_reduction: approach -> su -> ft -> method -> name
    if (!is.null(x@dimension_reduction)) {
        x@dimension_reduction <- lapply(x@dimension_reduction,
            function(by_appr) {
                drop_lvl2(drop_keys(by_appr, su_changed), ft_changed)
            })
    }
    x
}

# Return the set of top-level keys whose per-sample vector differs (or
# entries that were added or removed).
#' @noRd
.gm_axis_changed_keys <- function(old_axis, new_axis) {
    old_axis <- old_axis %||% list()
    new_axis <- new_axis %||% list()
    keys <- union(names(old_axis), names(new_axis))
    changed <- vapply(keys, function(k) {
        !identical(old_axis[[k]], new_axis[[k]])
    }, logical(1L))
    keys[changed]
}


# SHARED-DOMAIN OVERRIDES ON giottoMulti ####

#' @rdname getExpression
#' @param samples (giottoMulti) character vector of sample names to slice
#'   the joint result to; `NULL` (default) returns all participants
#' @param on_missing (giottoMulti) how assembly treats a keyed child that
#'   cannot produce the requested content, and mismatched feature panels:
#'   `"error"` (default — a partial federation cannot pass as a full one),
#'   `"drop"` (drop the child / intersect panels, with a warning), or
#'   `"fill"` (union panels with 0-fill; a fully-missing child is still
#'   dropped, with a warning)
#' @export
setMethod("getExpression", "giottoMulti",
    function(gobject, spat_unit = NULL, feat_type = NULL, name = NULL,
             values = NULL, output = c("exprObj", "matrix"),
             set_defaults = TRUE, samples = NULL,
             on_missing = c("error", "drop", "fill")) {
        output <- match.arg(output, choices = c("exprObj", "matrix"))
        on_missing <- match.arg(on_missing)

        # `values` is the back-compat alias for `name` — same contract as
        # the gAny method.
        if (!is.null(values)) {
            if (!is.null(name) && !identical(name, values)) {
                stop("[gmulti getExpression] 'name' and 'values' both ",
                    "supplied but differ. Use one -- 'name' is preferred.",
                    call. = FALSE)
            }
            name <- values
        }

        # Parse "sample::name" prefix in name. Resolves to a single
        # sample; conflicts with `samples = ` arg if both are set and
        # disagree.
        if (!is.null(name) && length(name) == 1L) {
            parsed <- .parse_sample_qualified_name(name)
            if (!is.null(parsed$sample)) {
                if (!is.null(samples) &&
                    !identical(samples, parsed$sample)) {
                    stop(sprintf(paste(
                        "[gmulti getExpression] conflicting sample selection:",
                        "`samples = %s` vs name prefix '%s::'",
                        sep = " "),
                        paste(sprintf("'%s'", samples), collapse = ","),
                        parsed$sample), call. = FALSE)
                }
                samples <- parsed$sample
                name <- parsed$name
            }
        }

        # unknown samples error up front (participation is checked after
        # assembly, but a typo should not read as non-participation)
        if (!is.null(samples)) {
            bad <- setdiff(samples, names(gobject@objects))
            if (length(bad) > 0L) {
                stop(sprintf(
                    "[gmulti getter] sample(s) '%s' not in @objects (have: %s)",
                    paste(bad, collapse = ", "),
                    paste(names(gobject@objects), collapse = ", ")),
                    call. = FALSE)
            }
        }

        # Capture before default resolution so the assembly path can tell
        # user-supplied from defaulted (children may have different
        # layouts; assembly resolves per-child through @mapping).
        nospec_unit <- is.null(spat_unit)
        nospec_feat <- is.null(feat_type)

        # Defaults come from @mapping (the authoritative handle universe);
        # the giotto-style instruction defaults are the legacy fallback for
        # mapping-less objects.
        if (isTRUE(set_defaults)) {
            if (is.null(spat_unit)) {
                spat_unit <- .gm_resolve_axis(gobject, "spat_unit",
                    NULL)$handle
            }
            if (is.null(feat_type)) {
                feat_type <- .gm_resolve_axis(gobject, "feat_type",
                    NULL)$handle
            }
            if (is.null(spat_unit) || is.null(feat_type)) {
                .set_default_nesting(gobject, spat_unit, feat_type)
            }
        }

        # Joint slot populated for this universe? The slot is
        # authoritative — eager subset trims it in place. Defer to gAny,
        # which reads the slot directly. The default name comes from the
        # mapping's values axis, not from whatever the slot happens to
        # hold first (plan A2); the first-listed pick survives only for
        # mapping-less legacy objects.
        joint_avail <- if (!is.null(spat_unit) && !is.null(feat_type)) {
            list_expression_names(gobject,
                spat_unit = spat_unit, feat_type = feat_type)
        } else {
            character()
        }
        target_values <- name
        if (is.null(target_values)) {
            target_values <- tryCatch(
                .gm_resolve_axis(gobject, "values", NULL)$handle,
                error = function(e) NULL)
            if (is.null(target_values) && length(joint_avail) > 0L) {
                target_values <- joint_avail[[1L]]   # legacy pick
            }
        }
        if (!is.null(target_values) && target_values %in% joint_avail) {
            e <- callNextMethod(gobject, name = target_values,
                spat_unit = spat_unit, feat_type = feat_type,
                output = "exprObj", set_defaults = FALSE)
            # Safety filter — joint slot can drift relative to @cell_ID /
            # @feat_ID when filterGiotto / subsetGiotto narrow without
            # cascading through the joint cache. Cheap re-filter keeps
            # the slot honest.
            e <- .gm_apply_view(e, gobject)
            e <- .gm_slice_to_samples(e, samples, gobject)
            if (output == "matrix") return(e[])
            return(e)
        }

        # Joint slot empty — assemble from children via @mapping, prefix
        # to globals. Integration output overrides assembly via
        # setExpression(mg, ...) — that's the materialization entry point.
        e <- .gm_assemble_expression(gobject,
            spat_unit = if (nospec_unit) NULL else spat_unit,
            feat_type = if (nospec_feat) NULL else feat_type,
            values = name,
            on_missing = on_missing)

        # `samples =` must name participants — a declared skip (NA) or a
        # dropped child is not silently an empty column set
        if (!is.null(samples)) {
            participation <- e@misc$gmulti$participation
            nonpart <- setdiff(samples, participation)
            if (length(nonpart) > 0L) {
                stop(sprintf(paste0(
                    "[gmulti getExpression] sample(s) %s do not participate ",
                    "in this universe (participants: %s). Check ",
                    "gmultiMapping(mg)."),
                    paste(sprintf("'%s'", nonpart), collapse = ", "),
                    paste(participation, collapse = ", ")),
                    call. = FALSE)
            }
        }

        e <- .gm_apply_view(e, gobject)
        e <- .gm_slice_to_samples(e, samples, gobject)
        if (output == "matrix") return(e[])
        e
    }
)

#' @rdname getCellMetadata
#' @param samples (giottoMulti) character vector of sample names to slice
#'   the joint result to; `NULL` (default) returns all participants
#' @param on_missing (giottoMulti) how assembly treats a keyed child that
#'   cannot produce the requested content, and mismatched column sets:
#'   `"error"` (default), `"drop"` (intersect), or `"fill"` (union / NA)
#' @export
setMethod("getCellMetadata", "giottoMulti", function(gobject,
    spat_unit = NULL,
    feat_type = NULL,
    output = c("cellMetaObj", "data.table"),
    copy_obj = TRUE,
    set_defaults = TRUE,
    samples = NULL,
    on_missing = c("error", "drop", "fill")) {
    output <- match.arg(output, choices = c("cellMetaObj", "data.table"))
    on_missing <- match.arg(on_missing)
    nospec_unit <- is.null(spat_unit)
    nospec_feat <- is.null(feat_type)
    if (isTRUE(set_defaults)) {
        if (is.null(spat_unit)) {
            spat_unit <- .gm_resolve_axis(gobject, "spat_unit", NULL)$handle
        }
        if (is.null(feat_type)) {
            feat_type <- .gm_resolve_axis(gobject, "feat_type", NULL)$handle
        }
        if (is.null(spat_unit) || is.null(feat_type)) {
            .set_default_nesting(gobject, spat_unit, feat_type)
        }
    }

    # Joint slot populated? Defer to gAny, then safety-filter through
    # .gm_apply_view — the slot can drift relative to @cell_ID when
    # filterGiotto / subsetGiotto narrow without re-trimming the joint
    # cache.
    joint <- if (!is.null(spat_unit) && !is.null(feat_type)) {
        gobject@cell_metadata[[spat_unit]][[feat_type]]
    } else {
        NULL
    }
    if (inherits(joint, "cellMetaObj")) {
        cm <- callNextMethod(gobject,
            spat_unit = spat_unit, feat_type = feat_type,
            output = "cellMetaObj", copy_obj = copy_obj,
            set_defaults = FALSE)
        cm <- .gm_apply_view(cm, gobject)
        cm <- .gm_slice_to_samples(cm, samples, gobject)
        if (output == "data.table") return(cm[])
        return(cm)
    }

    # Empty — assemble from children via @mapping. setCellMetadata(mg, ...)
    # is the materialization entry point.
    cm <- .gm_assemble_cell_metadata(gobject,
        spat_unit = if (nospec_unit) NULL else spat_unit,
        feat_type = if (nospec_feat) NULL else feat_type,
        on_missing = on_missing)
    cm <- .gm_apply_view(cm, gobject)
    cm <- .gm_slice_to_samples(cm, samples, gobject)
    if (output == "data.table") return(cm[])
    cm
})

#' @rdname getFeatureMetadata
#' @param samples (giottoMulti) accepted for API symmetry; feature IDs are
#'   not sample-namespaced, so this only validates the names
#' @param on_missing (giottoMulti) how assembly treats a keyed child that
#'   cannot produce the requested content, and mismatched column sets:
#'   `"error"` (default), `"drop"` (intersect), or `"fill"` (union / NA)
#' @export
setMethod("getFeatureMetadata", "giottoMulti", function(gobject,
    spat_unit = NULL,
    feat_type = NULL,
    output = c("featMetaObj", "data.table"),
    copy_obj = TRUE,
    set_defaults = TRUE,
    samples = NULL,
    on_missing = c("error", "drop", "fill")) {
    output <- match.arg(output, choices = c("featMetaObj", "data.table"))
    on_missing <- match.arg(on_missing)
    nospec_unit <- is.null(spat_unit)
    nospec_feat <- is.null(feat_type)
    if (isTRUE(set_defaults)) {
        if (is.null(spat_unit)) {
            spat_unit <- .gm_resolve_axis(gobject, "spat_unit", NULL)$handle
        }
        if (is.null(feat_type)) {
            feat_type <- .gm_resolve_axis(gobject, "feat_type", NULL)$handle
        }
        if (is.null(spat_unit) || is.null(feat_type)) {
            .set_default_nesting(gobject, spat_unit, feat_type)
        }
    }

    # Feature IDs aren't sample-namespaced (passthrough) — `samples = ` is
    # accepted for API symmetry but is a no-op at the featmeta level. It
    # validates against @objects so a typo still errors loudly.
    if (!is.null(samples)) {
        checkmate::assert_character(samples,
            min.len = 1L, any.missing = FALSE)
        bad <- setdiff(samples, names(gobject@objects))
        if (length(bad) > 0L) {
            stop(sprintf(
                "[gmulti getFeatureMetadata] sample(s) '%s' not in @objects",
                paste(bad, collapse = ", ")), call. = FALSE)
        }
    }
    joint <- if (!is.null(spat_unit) && !is.null(feat_type)) {
        gobject@feat_metadata[[spat_unit]][[feat_type]]
    } else {
        NULL
    }
    if (inherits(joint, "featMetaObj")) {
        return(callNextMethod(gobject,
            spat_unit = spat_unit, feat_type = feat_type,
            output = output, copy_obj = copy_obj, set_defaults = FALSE))
    }

    # Empty — assemble via @mapping, then apply any active narrowing.
    fm <- .gm_assemble_feat_metadata(gobject,
        spat_unit = if (nospec_unit) NULL else spat_unit,
        feat_type = if (nospec_feat) NULL else feat_type,
        on_missing = on_missing)
    fm <- .gm_apply_view(fm, gobject)
    if (output == "data.table") return(fm[])
    fm
})


# SPATIAL-DOMAIN METHODS — per-child dispatch ####
#
# Getters: `object = NULL` (default) routes to all children and returns a
# named list of per-child results. Pass a character vector of names to scope
# to specific children. Children are returned as-is — subset() on the multi
# narrows the joint analysis view, not children's spatial state — except
# that an active @cell_ID / @feat_ID narrowing is applied to the outputs
# (output-level filter; the children themselves are never mutated).
#
# Setters: `object` must name exactly one child. Setting "broadcast"
# semantics across children would silently duplicate spatial data and
# is almost never what the caller means; require an explicit target.

# Resolve the per-child target arg accepting either the legacy `object`
# name or the canonical `samples` (preferred). Conflicting values error.
#' @noRd
.gm_resolve_per_child_arg <- function(gobject, object, samples) {
    if (!is.null(object) && !is.null(samples)) {
        if (!identical(unname(object), unname(samples))) {
            stop("[gmulti getter] conflicting `object = ` and `samples = ` ",
                "arguments; pass one or the other (samples is preferred).",
                call. = FALSE)
        }
    }
    if (!is.null(samples)) object <- samples
    .gm_resolve_objects(gobject, object)
}

#' @noRd
.gm_set_target <- function(gobject, object) {
    if (missing(object) || is.null(object)) {
        stop("[gmulti setter] `object` must name the child to write into",
            call. = FALSE)
    }
    if (length(object) != 1L) {
        stop("[gmulti setter] `object` must be length 1 for setters on a ",
            "giottoMulti", call. = FALSE)
    }
    .gm_resolve_objects(gobject, object)
}

# Narrow a list of per-child subobjects by the global @cell_ID[[su]] /
# @feat_ID[[ft]] allow-lists. Output-level filter — runs once after the
# federation getter has assembled its per-child list; per-class axis
# knowledge is delegated to .narrow_subobject() (shared with the eager
# channel — plan A6). Returns the list unchanged when no narrowing is in
# effect. Cell globals carry the `sample::` prefix and are localized per
# child; feature globals are passthrough.
#' @noRd
.gm_narrow_child_outputs <- function(out_list, gobject, spat_unit = NULL,
    feat_type = NULL) {
    allowed_global <- if (!is.null(spat_unit) && length(spat_unit) == 1L) {
        gobject@cell_ID[[spat_unit]]
    } else {
        NULL
    }
    allowed_feats <- if (!is.null(feat_type) && length(feat_type) == 1L) {
        gobject@feat_ID[[feat_type]]
    } else {
        NULL
    }
    if (is.null(allowed_global) && is.null(allowed_feats)) return(out_list)
    Map(function(child_obj, sample_name) {
        prefix <- paste0(sample_name, "::")
        local_cells <- if (!is.null(allowed_global)) {
            sub(paste0("^", prefix), "",
                allowed_global[startsWith(allowed_global, prefix)])
        } else {
            NULL
        }
        .narrow_subobject(child_obj, cells = local_cells,
            feats = allowed_feats)
    }, out_list, names(out_list))
}

#' @rdname getSpatialLocations
#' @param object,samples (giottoMulti) children to read from; `samples` is
#'   the canonical name, `object` the legacy alias. `NULL` = all children
#' @export
setMethod("getSpatialLocations", signature("giottoMulti"),
    function(gobject, spat_unit = NULL, name = NULL, ...,
        object = NULL, samples = NULL) {
        objs <- .gm_resolve_per_child_arg(gobject, object, samples)
        su <- spat_unit %||%
            .gm_resolve_axis(gobject, "spat_unit", NULL)$handle
        out <- lapply(objs, function(nm) {
            getSpatialLocations(gobject@objects[[nm]],
                spat_unit = spat_unit, name = name, ...)
        })
        names(out) <- objs
        .gm_narrow_child_outputs(out, gobject, spat_unit = su)
    }
)

#' @rdname setSpatialLocations
#' @param object (giottoMulti) name of the single child to write into
#' @export
setMethod("setSpatialLocations", signature("giottoMulti"),
    function(gobject, x, spat_unit = NULL, name = NULL, ..., object = NULL) {
        nm <- .gm_set_target(gobject, object)
        gobject@objects[[nm]] <- setSpatialLocations(
            gobject@objects[[nm]], x = x,
            spat_unit = spat_unit, name = name, ...)
        gobject
    }
)

#' @rdname getSpatialNetwork
#' @param object,samples (giottoMulti) children to read from; `samples` is
#'   the canonical name, `object` the legacy alias. `NULL` = all children
#' @export
setMethod("getSpatialNetwork", signature("giottoMulti"),
    function(gobject, spat_unit = NULL, name = NULL, ...,
        object = NULL, samples = NULL) {
        objs <- .gm_resolve_per_child_arg(gobject, object, samples)
        su <- spat_unit %||%
            .gm_resolve_axis(gobject, "spat_unit", NULL)$handle
        out <- lapply(objs, function(nm) {
            getSpatialNetwork(gobject@objects[[nm]],
                spat_unit = spat_unit, name = name, ...)
        })
        names(out) <- objs
        .gm_narrow_child_outputs(out, gobject, spat_unit = su)
    }
)

#' @rdname setSpatialNetwork
#' @param object (giottoMulti) name of the single child to write into
#' @export
setMethod("setSpatialNetwork", signature("giottoMulti"),
    function(gobject, x, spat_unit = NULL, name = NULL, ..., object = NULL) {
        nm <- .gm_set_target(gobject, object)
        gobject@objects[[nm]] <- setSpatialNetwork(
            gobject@objects[[nm]], x = x,
            spat_unit = spat_unit, name = name, ...)
        gobject
    }
)

#' @rdname getPolygonInfo
#' @param object,samples (giottoMulti) children to read from; `samples` is
#'   the canonical name, `object` the legacy alias. `NULL` = all children
#' @export
setMethod("getPolygonInfo", signature("giottoMulti"),
    function(gobject, name = NULL, ..., object = NULL, samples = NULL) {
        objs <- .gm_resolve_per_child_arg(gobject, object, samples)
        # `name` is poly_info's analogue of spat_unit for narrowing — falls
        # back to the default handle when absent. (`polygon_name` is the
        # deprecated alias, still honoured on the child method.)
        args <- list(...)
        su <- name %||% args$polygon_name %||% args$spat_unit %||%
            .gm_resolve_axis(gobject, "spat_unit", NULL)$handle
        out <- lapply(objs, function(nm) {
            getPolygonInfo(gobject@objects[[nm]], name = name, ...)
        })
        names(out) <- objs
        .gm_narrow_child_outputs(out, gobject, spat_unit = su)
    }
)

#' @rdname setPolygonInfo
#' @param object (giottoMulti) name of the single child to write into
#' @export
setMethod("setPolygonInfo", signature("giottoMulti"),
    function(gobject, x, name = NULL, ..., object = NULL) {
        nm <- .gm_set_target(gobject, object)
        gobject@objects[[nm]] <- setPolygonInfo(
            gobject@objects[[nm]], x = x, name = name, ...)
        gobject
    }
)

#' @rdname getFeatureInfo
#' @param object,samples (giottoMulti) children to read from; `samples` is
#'   the canonical name, `object` the legacy alias. `NULL` = all children
#' @export
setMethod("getFeatureInfo", signature("giottoMulti"),
    function(gobject, feat_type = NULL, ..., object = NULL, samples = NULL) {
        objs <- .gm_resolve_per_child_arg(gobject, object, samples)
        ft <- feat_type %||%
            .gm_resolve_axis(gobject, "feat_type", NULL)$handle
        out <- lapply(objs, function(nm) {
            getFeatureInfo(gobject@objects[[nm]],
                feat_type = feat_type, ...)
        })
        names(out) <- objs
        # feature-axis narrowing (@feat_ID) — the fourth call site of the
        # output-level filter (plan A6 closes the feature-axis gap)
        .gm_narrow_child_outputs(out, gobject, feat_type = ft)
    }
)

#' @rdname setFeatureInfo
#' @param object (giottoMulti) name of the single child to write into
#' @export
setMethod("setFeatureInfo", signature("giottoMulti"),
    function(gobject, x, feat_type = NULL, ..., object = NULL) {
        nm <- .gm_set_target(gobject, object)
        gobject@objects[[nm]] <- setFeatureInfo(
            gobject@objects[[nm]], x = x, feat_type = feat_type, ...)
        gobject
    }
)

#' @rdname getGiottoImage
#' @param object,samples (giottoMulti) children to read from; `samples` is
#'   the canonical name, `object` the legacy alias. `NULL` = all children
#' @export
setMethod("getGiottoImage", signature("giottoMulti"),
    function(gobject, name = NULL, ..., object = NULL, samples = NULL) {
        objs <- .gm_resolve_per_child_arg(gobject, object, samples)
        out <- lapply(objs, function(nm) {
            getGiottoImage(gobject@objects[[nm]], name = name, ...)
        })
        names(out) <- objs
        out
    }
)

#' @rdname setGiottoImage
#' @param object (giottoMulti) name of the single child to write into
#' @export
setMethod("setGiottoImage", signature("giottoMulti"),
    function(gobject, x, name = NULL, ..., object = NULL) {
        nm <- .gm_set_target(gobject, object)
        gobject@objects[[nm]] <- setGiottoImage(
            gobject@objects[[nm]], x = x, name = name, ...)
        gobject
    }
)

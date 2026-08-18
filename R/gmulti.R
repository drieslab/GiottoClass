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
# STAGED PORT — stage 1 of the sequence in
# `vignettes/articles/PLAN_gmulti2_port.md` (§7). This file currently carries
# only the class, its construction path, and the identity registry:
#
#   * the class definition, with the full slot set declared up front so later
#     stages add machinery rather than editing the class (a class change would
#     need a matching `updateGiottoObject()` migration; see AGENTS.md)
#   * `initialize()` — child ingest, instructions, source resolution, id_map
#   * `createGiottoMulti()`
#   * enough introspection to inspect `@objects`
#
# Deferred, with the stage that brings it:
#
#   * `@cell_ID` / `@feat_ID` narrowing contract, and the reset of those slots
#     when the child population changes — stage 2 (plan fed 17, 18)
#   * `@mapping` auto-discovery and the federated access layer — stage 3
#     (plan fed 5, 6, 7). Until then federation falls back to per-child
#     default spat_unit / feat_type resolution, which is the documented
#     behaviour for an unpopulated mapping
#   * `@view` / `@spaces` recipe subsystem — stage 4 (plan vs 1-8). Those
#     slots hold plain tagged lists, not S4 step objects (plan Q7)
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
#' @slot mapping `list` declaring how child-level spat_units and feat_types
#'   federate up to gmulti-level handles. Two named entries, `spat_unit` and
#'   `feat_type`, each a list of named character vectors mapping
#'   `sample -> child-level name`. Populated from stage 3 of the port; empty
#'   until then.
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
        mapping             = list(spat_unit = list(), feat_type = list()),

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
    # Stage 2 adds the other half of this branch: resetting @cell_ID /
    # @feat_ID to NULL on a population change, because those slots record the
    # surviving set from a prior filter on a specific population and stop
    # being well-defined once a child is added, removed, or replaced.
    cur_sig <- .gm_compute_sig(.Object@objects)
    if (!identical(cur_sig, .Object@id_sig)) {
        .Object@id_map$cells <- .gm_build_cell_idmap(.Object@objects)
        .Object@id_map$feats <- .gm_build_feat_idmap(.Object@objects)
        .Object@id_sig <- cur_sig
    }

    # Stage 3 adds @mapping auto-discovery here.

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


# INTROSPECTION ####

# Enough to inspect @objects. The rest of the introspection surface (show,
# compact, subset, the shared-domain overrides) arrives with stages 2-3.

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
        # Stage 2 intersects with @cell_ID here — the registry is never
        # narrowed, so the active surviving set is applied on read.
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
        # Stage 2 intersects with @feat_ID here, as spatIDs does with @cell_ID.
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

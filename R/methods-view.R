#' @include classes-view.R
#' @include classes.R
#' @include generics.R
NULL

# =============================================================================
# methods-view.R — public API for giottoView
#
# Views are subset/narrowing recipes only — no transforms. Transforms live on
# `giottoSpace` (see methods-space.R). A view may reference a slotted space
# via `@space`; crop regions are then meaningful in that frame.
#
# Methods added here (record-style on view receiver):
#   subset(view, predicate)        NSE
#   crop(view, region)             record a crop step
#   selectSamples(view, ...)       record a samples step (gmulti-only)
#
# `+` for `giottoView + giottoView` is reserved (errors for now).
# =============================================================================


# Constructor ####
# `giottoView()` and `giottoView(space = "x")` dispatch through the generic
# on `signature(gobject = "missing", name = "missing")` (defined below
# alongside the accessor methods). `...` carries the optional `space`
# argument through to the method.


# Internal helper ####
.view_record_step <- function(view, step) {
    view@steps <- c(view@steps, list(step))
    view
}


# Indirect-usage routing: lets generics like subset() / crop() on a
# `giotto` accept `view = <name>` and record the step rather than
# executing eagerly. Returns the gobject with the named view slotted /
# appended.
.record_view_on_gobject <- function(gobject, view, step, space = NULL) {
    # view contract: character(1) name of a slotted view, or NULL.
    # Inline giottoView objects were considered and rejected (see
    # vignettes/articles/DESIGN_gmulti_federation.md). If programmatic
    # composition is needed, build the view, slot it under a name, then
    # reference it:
    #   v <- giottoView() |> subset(...) |> crop(...)
    #   giottoView(g, "tmp") <- v
    #   subset(g, ..., view = "tmp")
    checkmate::assert_string(view, .var.name = "view")
    existing <- if (view %in% giottoViews(gobject)) {
        giottoView(gobject, view)
    } else {
        giottoView()
    }
    existing <- .view_bind_space(existing, space)
    new_view <- .view_record_step(existing, step)
    giottoView(gobject, view) <- new_view
    gobject
}

# Substitute env-resident scalar / vector values into `pred` so the
# predicate becomes self-contained. This is what lets the step store a
# deparsed string with no environment attached: after substitution the only
# free names left are data columns and functions, and functions resolve
# through the package chain at eval time.
#
# Functions and missing names are left alone.
.eager_substitute_env <- function(pred, env) {
    all_vars <- all.vars(pred)
    sub_list <- list()
    for (v in all_vars) {
        if (exists(v, envir = env, inherits = TRUE)) {
            val <- tryCatch(get(v, envir = env, inherits = TRUE),
                error = function(e) NULL)
            if (!is.null(val) && !is.function(val)) {
                sub_list[[v]] <- val
            }
        }
    }
    if (length(sub_list) == 0L) return(pred)
    do.call("substitute", list(pred, sub_list))
}

# Walk the call stack to find the user-level frame that holds the
# predicate's free variables. S4 dispatch, the pipe, and testthat wrappers
# each insert frames; `parent.frame()` alone lands on the dispatch frame,
# which usually has no user locals. Record-time only — the frame is used
# for substitution and then discarded.
.find_predicate_env <- function(pred, default) {
    vars <- all.vars(pred)
    if (length(vars) == 0L) return(default)
    for (i in seq_len(8L)) {
        f <- tryCatch(parent.frame(i), error = function(e) NULL)
        if (is.null(f)) break
        if (any(vapply(vars,
            function(v) exists(v, envir = f, inherits = FALSE),
            logical(1L)))) {
            return(f)
        }
    }
    default
}


# subset() — NSE predicate capture on views ####

#' @title Subset (filter) a giottoView
#' @name subset-view
#' @description
#' Record a predicate-style filter step on a [giottoView-class]. The
#' predicate expression is captured unevaluated (NSE) at call site; at
#' resolution time it is evaluated against `spatValues(g, feats = <names
#' in predicate>)`, with any additional arguments (`spat_unit`, `feat_type`,
#' `negate`, ...) forwarded through.
#'
#' Values from the calling frame are substituted into the predicate at
#' record time, so `subset(v, cluster == target)` records the *value* of
#' `target` rather than a reference to it — the recipe does not change if
#' `target` is later reassigned, and it survives serialization.
#'
#' Cell-keyed propagation to other slots is automatic via cell_ID relations
#' — no flag needed.
#'
#' @param x a `giottoView`
#' @param subset an unevaluated predicate expression (captured via NSE)
#' @param ... additional arguments forwarded to the underlying subset /
#'   spatValues call at resolution time (`spat_unit`, `feat_type`, `negate`,
#'   `feat_ids`, `cell_ids`, ...)
#' @returns the view, with the filter step recorded
#' @examples
#' v <- subset(giottoView(), cluster == "A")
#' v
NULL

#' @rdname subset-view
#' @export
setMethod("subset", signature(x = "giottoView"),
    function(x, subset, ...) {
        pred <- substitute(subset)
        env <- .find_predicate_env(pred, parent.frame())
        pred <- .eager_substitute_env(pred, env)
        .view_record_step(x, .view_step_filter(pred, scope_args = list(...)))
    }
)


# crop() — region narrowing on views ####
# Note: name kept as crop() for ergonomics (familiar verb), but the semantic
# is relate-based membership, not geometric clipping. The view records the
# region + relation; resolution narrows cells whose centroid satisfies the
# relation against the region. Geometry of surviving subobjects is NOT
# modified — only the cell set narrows.

#' @rdname crop
#' @param relation `character(1)`. Spatial relation evaluated against cell
#'   centroids. One of `"intersects"` (default), `"within"`, `"contains"`,
#'   `"covers"`, `"covered_by"`, `"overlaps"`, `"touches"`, `"crosses"`,
#'   `"disjoint"`. Passed through to [terra::is.related].
#' @export
setMethod("crop", signature(x = "giottoView", y = "ANY"),
    function(x, y, relation = "intersects", ..., space = NULL) {
        checkmate::assert_character(relation, len = 1L, any.missing = FALSE)
        x <- .view_bind_space(x, space)
        .view_record_step(x,
            .view_step_crop(.normalize_crop_region(y), relation))
    }
)


# Crop region recording — WKT is the canonical form ####
#
# A recorded region is always a single WKT string. This mirrors the cascade
# GiottoDisk's `methods-spatRelate.R` already uses for its op chain rather
# than defining a second policy, and it buys three things:
#
#   * serializable: terra objects are C++ pointer-backed and do not survive
#     `saveRDS` or a trip to a parallel worker.
#   * the disk path receives WKT with no conversion at resolve time.
#   * the terra `(xmin, xmax, ymin, ymax)` convention is applied exactly
#     once, here, instead of being re-derived per substrate.
#
# WKT is geometry only — attributes are not carried through. CRS is
# deliberately NOT recorded: a recipe is re-resolved against current state
# by design, so a baked-in CRS could assert something the store it resolves
# against disagrees with. SRID stays authoritative on the store side
# (GiottoDisk's `.spatrelate_store_srid()`).

# Maximum features accepted for inline recording. A recipe is meant to be
# a compact description; a large query set belongs in a store.
.view_crop_inline_max <- function() {
    getOption("giotto.view_crop_inline_max", 1000L)
}

#' Normalize a crop region to a single WKT string.
#'
#' The WKT `character` method is the canonical entry; every other accepted
#' type coerces and recurses into it.
#' @keywords internal
#' @noRd
.normalize_crop_region <- function(y) {
    # canonical entry: WKT character
    if (is.character(y)) {
        checkmate::assert_character(y, min.len = 1L, any.missing = FALSE,
            .var.name = "region")
        if (length(y) > .view_crop_inline_max()) {
            stop(sprintf(paste0(
                "[crop] region has %d geometries, above the inline cap of ",
                "%d. Pass a single unioned geometry, or resolve against a ",
                "store instead of recording the query set inline ",
                "(see `giotto.view_crop_inline_max`)."),
                length(y), .view_crop_inline_max()), call. = FALSE)
        }
        if (length(y) > 1L) {
            # multi-feature: union into one geometry so the record is one
            # region rather than a set the resolver would have to fold
            return(.normalize_crop_region(terra::vect(y)))
        }
        # must parse as geometry
        ok <- tryCatch({
            terra::vect(y)
            TRUE
        }, error = function(e) FALSE)
        if (!ok) {
            stop("[crop] region is not valid WKT: ", y, call. = FALSE)
        }
        return(y)
    }
    if (is.numeric(y)) {
        checkmate::assert_numeric(y, len = 4L, any.missing = FALSE,
            .var.name = "region")
        # terra extent convention, applied exactly once
        return(.normalize_crop_region(terra::as.polygons(terra::ext(y))))
    }
    if (inherits(y, "SpatExtent")) {
        return(.normalize_crop_region(terra::as.polygons(y)))
    }
    if (inherits(y, "SpatVector")) {
        y_use <- if (nrow(y) > 1L) terra::aggregate(y) else y
        return(.normalize_crop_region(terra::geom(y_use, wkt = TRUE)))
    }
    if (inherits(y, c("sf", "sfc"))) {
        package_check("sf", repository = "CRAN")
        geom <- if (inherits(y, "sf")) sf::st_geometry(y) else y
        if (length(geom) > 1L) geom <- sf::st_union(geom)
        return(.normalize_crop_region(sf::st_as_text(geom)))
    }
    stop("[crop] region must be WKT character, numeric(4), SpatExtent, ",
        "SpatVector, or sf/sfc (got '", class(y)[[1L]], "')", call. = FALSE)
}


#' Deserialize a recorded crop region for substrate consumers.
#'
#' Called at the boundary where the recipe hands off to terra-backed crop
#' machinery or the in-memory relate path.
#' @keywords internal
#' @noRd
.materialize_crop_region <- function(r) {
    if (is.null(r)) return(NULL)
    terra::vect(r)
}


# Bind a view to a named space. First call sets @space; later calls with
# the same name are a no-op; later calls with a different name error.
# `space = NULL` is a no-op (leave whatever's there).
.view_bind_space <- function(view, space) {
    if (is.null(space)) return(view)
    checkmate::assert_character(space, len = 1L, any.missing = FALSE)
    cur <- view@space
    if (is.na(cur)) {
        view@space <- space
        return(view)
    }
    if (!identical(cur, space)) {
        stop("view is already bound to space '", cur, "'; ",
            "cannot rebind to '", space, "'. ",
            "Build a fresh view if a different frame is needed.",
            call. = FALSE)
    }
    view
}


# selectSamples() — gmulti-only sample selector ####

#' @title Select samples within a gmulti-scoped view
#' @name selectSamples
#' @description
#' Record a sample-selection step on a [giottoView-class] that will be
#' consumed against a [giottoMulti-class]. Picks which children participate
#' in resolution. The step is resolved FIRST, before any other step. Warns
#' at resolution time if the parent is not a `giottoMulti`.
#'
#' @param x a `giottoView`
#' @param ... `character` child names (or a single `character` vector)
#' @returns the view, with the sample-select step recorded
#' @examples
#' v <- selectSamples(giottoView(), "sample1", "sample2")
#' v
#' @export
setGeneric("selectSamples",
    function(x, ...) standardGeneric("selectSamples"))

#' @rdname selectSamples
#' @export
setMethod("selectSamples", signature(x = "giottoView"),
    function(x, ...) {
        samples <- unlist(list(...), use.names = FALSE)
        .view_record_step(x, .view_step_samples(samples))
    }
)


# Composition (+) ####
# Cross-view composition is reserved for a follow-up.

#' @noRd
setMethod("+", signature(e1 = "giottoView", e2 = "giottoView"),
    function(e1, e2) {
        stop("giottoView + giottoView: cross-view composition is not yet ",
            "implemented.", call. = FALSE)
    }
)


# Accessors ####

#' @title Slotted views on a giotto object
#' @name giottoView
#' @description
#' List, retrieve, attach, or remove [giottoView-class] objects slotted into
#' a [giotto-class] object's `@view` slot.
#'
#' * `giottoView()` — construct an empty standalone view
#' * `giottoView(g, "name")` — retrieve a slotted view by name
#' * `giottoView(g, "name") <- v` — slot in (or replace) a view
#' * `giottoView(g, "name") <- NULL` — remove a view
#' * `giottoViews(g)` — list slotted view names
#'
#' Views are subset/narrowing recipes; for coordinate-frame recipes see
#' [giottoSpace-class].
#'
#' @param gobject a `giotto` object (or omitted for the constructor)
#' @param name `character(1)`. The slot key.
#' @param value a `giottoView`, or `NULL` to remove.
#' @param ... additional arguments. For the bare constructor, `space`.
#' @returns the view, an updated gobject, or a character vector of view names
#' @examples
#' g <- giotto()
#' giottoView(g, "demo") <- giottoView()
#' giottoViews(g)
NULL

#' @rdname giottoView
#' @export
setGeneric("giottoView",
    function(gobject, name, ...) standardGeneric("giottoView"))

#' @rdname giottoView
#' @export
setGeneric("giottoView<-",
    function(gobject, name, ..., value) standardGeneric("giottoView<-"))

#' @rdname giottoView
#' @export
setGeneric("giottoViews",
    function(gobject, ...) standardGeneric("giottoViews"))

#' @rdname giottoView-class
#' @export
setMethod("giottoView", signature(gobject = "missing", name = "missing"),
    function(gobject, name, space = NA_character_, ...) {
        if (!is.na(space)) {
            checkmate::assert_character(space, len = 1L, any.missing = FALSE)
        }
        new("giottoView", space = as.character(space))
    }
)

#' @rdname giottoView
#' @export
setMethod("giottoView", signature(gobject = "gAny", name = "character"),
    function(gobject, name, ...) {
        checkmate::assert_character(name, len = 1L)
        v <- gobject@view[[name]]
        if (is.null(v)) {
            stop("no slotted giottoView named '", name, "'. ",
                "Available: ", paste(giottoViews(gobject), collapse = ", "),
                call. = FALSE)
        }
        v
    }
)

#' @rdname giottoView
#' @export
setMethod("giottoView", signature(gobject = "gAny", name = "missing"),
    function(gobject, name, ...) {
        nm <- giottoViews(gobject)
        if (length(nm) == 0L) return(NULL)
        if (length(nm) == 1L) return(gobject@view[[nm]])
        stop("multiple views slotted; specify `name`. ",
            "Available: ", paste(nm, collapse = ", "), call. = FALSE)
    }
)

#' @rdname giottoView
#' @export
setMethod("giottoView<-",
    signature(gobject = "gAny", name = "character", value = "giottoView"),
    function(gobject, name, ..., value) {
        checkmate::assert_character(name, len = 1L)
        value@name <- name
        if (is.null(gobject@view)) gobject@view <- list()
        gobject@view[[name]] <- value
        gobject
    }
)

#' @rdname giottoView
#' @export
setMethod("giottoView<-",
    signature(gobject = "gAny", name = "character", value = "NULL"),
    function(gobject, name, ..., value) {
        if (is.null(gobject@view) || !name %in% names(gobject@view)) {
            return(gobject)
        }
        gobject@view[[name]] <- NULL
        gobject
    }
)

#' @rdname giottoView
#' @export
setMethod("giottoViews", signature(gobject = "gAny"),
    function(gobject, ...) {
        nm <- names(gobject@view)
        if (is.null(nm)) character() else nm
    }
)


# materialize ####

#' @title materialize a giottoView into a new gobject
#' @name materialize
#' @description
#' Resolve a [giottoView-class] (optionally with a slotted [giottoSpace-class]
#' frame) against a gobject and return a new gobject containing the projected
#' subobjects. Use this when downstream work needs to produce structured
#' outputs (spatial networks, dim reductions) on top of the projected data —
#' those outputs live in the materialised gobject, never in the parent.
#'
#' Read-only contract: the input gobject is not mutated.
#'
#' @param gobject a `giotto` object
#' @param view either a `giottoView` or a `character(1)` slot key
#' @param space `character(1)` optional — name of a slotted `giottoSpace` to
#'   resolve in. If `NULL`, uses the view's own `@space` reference (which
#'   may itself be `NA`).
#' @param coordinator a [viewCoordinator-class]-inheriting object brokering
#'   IDs and joins between storage backings. Defaults to the coordinator
#'   selected from `gobject@source` (in-memory for non-disk gobjects).
#' @param slots optional `character` vector of slot names to narrow.
#'   When `NULL` (default), all slot lists are walked (`cell_metadata`,
#'   `expression`, `dimension_reduction`, `spatial_enrichment`,
#'   `feat_metadata`, `spatial_locs`, `spatial_info`, `feat_info`,
#'   `images`). When supplied, only the listed slots are walked — the
#'   rest are left untouched on the returned object. Useful for
#'   internal helpers that only consume a subset of slots and want to
#'   share one resolver pass without paying for irrelevant slots.
#' @param ... reserved
#' @returns a new `giotto` object reflecting the resolved view
#' @export
setGeneric("materialize",
    function(gobject, view, ...) standardGeneric("materialize"))


# All slots `materialize()` knows how to walk, in the canonical order
# (tabular → spatial → images). Used as the default slot set when
# `slots = NULL` and to validate caller-supplied slot names.
.materialize_default_slots <- c(
    "cell_metadata", "expression", "dimension_reduction",
    "spatial_enrichment", "feat_metadata",
    "spatial_locs", "spatial_info", "feat_info", "images"
)


# Validate and order a caller-supplied slot vector against the
# canonical walk order. NULL → all default slots. Unknown slot names
# error.
#' @keywords internal
#' @noRd
.materialize_slot_filter <- function(slots) {
    if (is.null(slots)) return(.materialize_default_slots)
    bad <- setdiff(slots, .materialize_default_slots)
    if (length(bad) > 0L) {
        stop(sprintf(
            "[materialize] unknown slot(s): %s. Available: %s",
            paste(bad, collapse = ", "),
            paste(.materialize_default_slots, collapse = ", ")
        ), call. = FALSE)
    }
    intersect(.materialize_default_slots, slots)  # canonical order
}

# Internal implementation: materialize on a giotto with an already-resolved
# giottoView object. Called from the public character-signature method
# (after slot lookup) and from the giottoMulti per-child loop (where the
# view object is already in hand).
#' @keywords internal
#' @noRd
.materialize_giotto_resolved <- function(gobject, view,
                                          space = NULL,
                                          coordinator = NULL,
                                          slots = NULL,
                                          ...) {
    if (is.null(coordinator)) {
        coordinator <- .default_view_coordinator(gobject)
    }
    # Normalise output space to a giottoSpace (or NULL) once at the
    # entry point so per-subobject resolution doesn't re-look-up by
    # name. The predicate space (view@space) is consulted independently
    # by the crop step handlers — it is not conflated with output here.
    space_obj <- .resolve_space(gobject, space)
    # Per-call cache shared across all slot walks within this
    # materialize. surviving_cell_ids computed at most once per call.
    cache <- .new_resolver_cache()

    out <- gobject

    # Walk the (possibly filtered) slot list in canonical order:
    # tabular → spatial → images. Slot names not in `slots` are
    # left untouched on the returned gobject.
    for (slot_name in .materialize_slot_filter(slots)) {
        out <- .materialize_walk(out, slot_name,
            view, space_obj, coordinator, cache)
    }

    # Networks (spatial_network, nn_network) intentionally not walked:
    # they're built from a particular cell state and don't carry
    # spatial coords; view/space resolution would be misleading.

    out
}

#' @rdname materialize
#' @export
setMethod("materialize",
    signature(gobject = "giotto", view = "character"),
    function(gobject, view, space = NULL, coordinator = NULL,
             slots = NULL, ...) {
        v <- giottoView(gobject, view)
        .materialize_giotto_resolved(gobject, v,
            space = space, coordinator = coordinator,
            slots = slots, ...)
    }
)


# materialize on giottoMulti ####
# 1. Apply the samples step FIRST — narrow children before any per-child
#    work touches storage (matters at 4B-points-per-multi scale).
# 2. Per-surviving-child materialize with the child-scoped giottoSpace.
# 3. Narrow joint shared slots via the existing resolveSubobject dispatch —
#    spatValues works on a multi, so joint-level predicates resolve against
#    joint slots and the surviving global cell_IDs narrow each joint
#    subobject.

# Internal implementation: materialize on a giottoMulti with an
# already-resolved giottoView object.
#' @keywords internal
#' @noRd
.materialize_gmulti_resolved <- function(gobject, view,
                                          space = NULL,
                                          coordinator = NULL,
                                          slots = NULL,
                                          ...) {
    if (is.null(coordinator)) {
        coordinator <- .default_view_coordinator(gobject)
    }
    space_obj <- .resolve_space(gobject, space)
    cache <- .new_resolver_cache()

    # Resolve the samples step FIRST — narrow children before any
    # per-child work touches storage.
    selected <- .resolve_sample_select(gobject, view)
    if (length(selected) == 1L && is.na(selected)) {
        selected <- names(gobject@objects)
    } else {
        selected <- intersect(selected, names(gobject@objects))
    }

    out <- gobject
    out@objects <- gobject@objects[selected]

    # Per-surviving-child materialize with the child-scoped space.
    # `slots` is forwarded so per-child narrowing matches the joint-level
    # scope.
    out@objects <- stats::setNames(lapply(selected, function(samp) {
        child <- out@objects[[samp]]
        child_space <- .scope_space_to_sample(space_obj, samp)
        .materialize_giotto_resolved(child, view, space = child_space,
            coordinator = coordinator, slots = slots, ...)
    }), selected)

    # Narrow joint shared slots. Only multi-level cell_metadata /
    # expression / dim_reduction / spatial_enrichment / feat_metadata are
    # legitimately joint, so intersect the filter with that subset.
    joint_candidates <- c("cell_metadata", "expression",
        "dimension_reduction", "spatial_enrichment", "feat_metadata")
    joint_slots <- intersect(.materialize_slot_filter(slots),
        joint_candidates)
    for (slot_name in joint_slots) {
        out <- .materialize_walk(out, slot_name,
            view, space_obj, coordinator, cache)
    }

    out
}

#' @rdname materialize
#' @export
setMethod("materialize",
    signature(gobject = "giottoMulti", view = "character"),
    function(gobject, view, space = NULL, coordinator = NULL,
             slots = NULL, ...) {
        v <- giottoView(gobject, view)
        .materialize_gmulti_resolved(gobject, v,
            space = space, coordinator = coordinator,
            slots = slots, ...)
    }
)

# Walk one slot list (potentially nested by spat_unit / feat_type) calling
# resolveSubobject on each subobject. The slot is a `nullOrList`; structure
# is recursive — list of lists of subobjects. Apply the resolver leaf-wise.
# `cache` (optional env from .new_resolver_cache) memoises
# surviving_cell_ids across all subobjects walked within one materialize.
#' @keywords internal
#' @noRd
.materialize_walk <- function(gobject, slot_name, view, space, coordinator,
                              cache = NULL) {
    x <- methods::slot(gobject, slot_name)
    if (is.null(x) || length(x) == 0L) return(gobject)
    methods::slot(gobject, slot_name) <- .materialize_apply(
        x, gobject, view, space, coordinator, cache)
    gobject
}

.materialize_apply <- function(node, gobject, view, space, coordinator,
                               cache = NULL) {
    if (is.list(node) && !isS4(node)) {
        return(lapply(node, .materialize_apply, gobject = gobject,
            view = view, space = space, coordinator = coordinator,
            cache = cache))
    }
    if (isS4(node) && inherits(node, "giottoSubobject")) {
        return(resolveSubobject(node, gobject, view, space, coordinator,
            .cache = cache))
    }
    node
}


# Show methods ####

#' @noRd
setMethod("show", signature("giottoView"), function(object) {
    cat("<giottoView>\n")
    nm <- if (is.na(object@name)) "<ephemeral>" else object@name
    cat(sprintf("  name : %s\n", nm))
    sp <- if (is.na(object@space)) "<native>" else object@space
    cat(sprintf("  space: %s\n", sp))
    if (length(object@steps) == 0L) {
        cat("  (empty — pipe through `subset()`, `crop()`, `selectSamples()`)\n")
        return(invisible(NULL))
    }
    cat("  steps:\n")
    for (s in object@steps) {
        cat(sprintf("    - %s\n", .view_step_label(s)))
    }
})

# Compact one-line label for a view step — used by show methods.
.view_step_label <- function(step) {
    switch(step$type,
        filter = {
            scope <- if (length(step$scope_args) > 0L) {
                scope_str <- paste(names(step$scope_args), "=",
                    vapply(step$scope_args, function(a) {
                        tryCatch(paste(deparse(a), collapse = " "),
                            error = function(e) "?")
                    }, character(1L)), collapse = ", ")
                sprintf(" [%s]", scope_str)
            } else ""
            sprintf("filter:    %s%s", step$predicate, scope)
        },
        crop = {
            rel <- if (identical(step$relation, "intersects")) ""
                else sprintf(" [%s]", step$relation)
            sprintf("crop:      %s%s", .wkt_label(step$region), rel)
        },
        samples = sprintf("samples:   %s",
            paste(step$samples, collapse = ", ")),
        sprintf("<%s>", step$type)
    )
}

# WKT strings are long; show the geometry type and vertex count instead of
# the literal, which would wrap over several lines for any real polygon.
.wkt_label <- function(wkt) {
    type <- sub("^\\s*([A-Za-z]+).*$", "\\1", wkt)
    n <- lengths(regmatches(wkt, gregexpr(",", wkt, fixed = TRUE))) + 1L
    sprintf("<%s: %d vertices>", toupper(type), n)
}

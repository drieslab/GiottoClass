#' @include classes-resolver.R
#' @include classes-view.R
#' @include classes-space.R
#' @include classes.R
NULL

# =============================================================================
# methods-resolver.R — resolveSubobject generic + dataTableCoordinator methods
#
# resolveSubobject(subobj, gobject, view, space, coordinator, ...) takes one
# subobject, the parent gobject (for spatValues lookups), and the
# view+space+coordinator. Returns a new subobject projected through the
# recipe.
#
# The `coordinator` is a `viewCoordinator`-inheriting object that brokers
# IDs and joins between storage backings during resolution — see
# `R/classes-resolver.R` for the protocol notes.
#
# Cell-set narrowing is computed once via `.surviving_cell_ids()`; tabular
# subobjects narrow by that set, spatial subobjects narrow + apply
# transforms via existing eager GiottoClass dispatch.
# =============================================================================


# Generic ####

#' @title resolveSubobject
#' @name resolveSubobject
#' @description Apply a [giottoView-class] (and optional [giottoSpace-class])
#' recipe to a single subobject, returning the projected subobject. Dispatch
#' is on `(subobj, coordinator)` so different storage-bridging coordinators
#' register different methods.
#'
#' @param subobj a giotto subobject (e.g. `cellMetaObj`, `spatLocsObj`, ...)
#' @param gobject the parent `giotto` (needed for cross-slot lookups via
#'   `spatValues()`)
#' @param view a [giottoView-class] or `NULL`
#' @param space a [giottoSpace-class] or `NULL`
#' @param coordinator a [viewCoordinator-class]-inheriting object brokering
#'   IDs and joins between storage backings
#' @param ... reserved for backend-specific args
#' @returns the projected subobject (same class as `subobj`)
#' @export
setGeneric("resolveSubobject",
    function(subobj, gobject, view, space, coordinator, ...)
        standardGeneric("resolveSubobject"))


# Coordinator protocol ####

#' @title prepareIds
#' @name prepareIds
#' @description Coordinator-side protocol method: promote an R-memory
#' cell_ID character vector into the form the coordinator's preferred
#' backend uses for filtering. For [dataTableCoordinator-class] this is
#' the identity transform; downstream coordinators (e.g. duckDB / sedona
#' from GiottoDisk) register methods that perform ephemeral table
#' registration or similar.
#'
#' @param coordinator a [viewCoordinator-class]-inheriting object
#' @param ids character vector of cell_IDs
#' @param ... reserved
#' @returns the prepared IDs in the coordinator's preferred form
#' @export
setGeneric("prepareIds",
    function(coordinator, ids, ...) standardGeneric("prepareIds"))

#' @rdname prepareIds
#' @export
setMethod("prepareIds", signature(coordinator = "dataTableCoordinator"),
    function(coordinator, ids, ...) ids
)


# Helpers ####

#' @title defaultViewCoordinator
#' @name defaultViewCoordinator
#' @description Pick the default [viewCoordinator-class] for resolving views
#' on a `gobject` whose source is `source`. GiottoClass provides the
#' `ANY`-signature method returning [dataTableCoordinator-class] (in-memory
#' reference). Downstream packages register their own coordinators by
#' adding methods for their source class — e.g. GiottoDisk registers a
#' `gsource` method returning `parquetCoordinator()`. S4 inheritance picks
#' up subclasses automatically.
#'
#' @param source the `@source` slot of the gobject (`NULL` is handled
#'   upstream by `.default_view_coordinator()`)
#' @param ... reserved
#' @returns a `viewCoordinator`-inheriting object
#' @export
setGeneric("defaultViewCoordinator",
    function(source, ...) standardGeneric("defaultViewCoordinator"))

#' @rdname defaultViewCoordinator
#' @export
setMethod("defaultViewCoordinator", signature(source = "ANY"),
    function(source, ...) dataTableCoordinator()
)

# Resolve which `coordinator` to use when the caller doesn't pass one
# explicitly. In-memory by default for sourceless gobjects; otherwise
# dispatch on the source class via [defaultViewCoordinator()].
#' @keywords internal
#' @noRd
.default_view_coordinator <- function(gobject) {
    src <- gobject@source
    if (is.null(src)) return(dataTableCoordinator())
    defaultViewCoordinator(src)
}

# Resolve the samples step into the set of children participating. For a
# plain giotto, sample selection is a no-op (returns NA to signal "no
# multi-scope"); for giottoMulti, returns the intersection of selected names
# with available children.
#' @keywords internal
#' @noRd
.resolve_sample_select <- function(gobject, view) {
    ss_steps <- .view_steps_of(view, "samples")
    if (length(ss_steps) == 0L) return(NA)
    if (!inherits(gobject, "giottoMulti")) {
        warning(call. = FALSE,
            "selectSamples step ignored: parent is not a giottoMulti")
        return(NA)
    }
    sel <- unique(unlist(lapply(ss_steps, function(s) s$samples)))
    avail <- names(gobject@objects)
    bad <- setdiff(sel, avail)
    if (length(bad) > 0L) {
        warning(call. = FALSE, sprintf(
            "selectSamples: missing children ignored (%s)",
            paste(bad, collapse = ", ")))
    }
    intersect(sel, avail)
}

# Free-var names in `predicate` that name data columns — these are what
# need pulling via spatValues.
#
# The predicate arrives self-contained: `.eager_substitute_env()` inlined
# every env-resident value at record time, and `all.vars()` does not report
# function names in call position. So what is left is columns, plus (rarely)
# a function passed as a value, which is filtered out here.
#
# NSE caveat: a column whose name collides with a function's (`c`, `mean`)
# is treated as the function. Same ambiguity as dplyr's `mutate(df, x = x)`.
#' @keywords internal
#' @noRd
.predicate_column_refs <- function(predicate) {
    Filter(
        function(v) !exists(v, envir = globalenv(), mode = "function",
            inherits = TRUE),
        all.vars(predicate)
    )
}

# Evaluate one filter step against the parent gobject via spatValues and
# return the surviving cell_ID vector. Predicates that reference columns not
# co-existing in a single artifact raise via spatValues' own contract.
#
# The predicate is stored deparsed, so it is parsed here. `globalenv()` is
# the evaluation enclosure: the step carries no environment by design (Q7),
# and functions resolve from there through the attached-package chain.
#' @keywords internal
#' @noRd
.eval_view_filter <- function(step, gobject) {
    pred <- str2lang(step$predicate)
    cols <- .predicate_column_refs(pred)
    if (length(cols) == 0L) {
        # purely constant predicate; pull all cell_IDs and let eval decide
        cell_ids <- spatIDs(gobject)
        keep <- eval(pred, envir = list(), enclos = globalenv())
        return(if (isTRUE(keep)) cell_ids else character())
    }
    sv_args <- c(list(gobject = gobject, feats = cols),
        step$scope_args %||% list())
    sv <- do.call(spatValues, sv_args)
    keep <- eval(pred, envir = sv, enclos = globalenv())
    if (!is.logical(keep)) {
        stop("view filter predicate did not evaluate to a logical vector: ",
            step$predicate, call. = FALSE)
    }
    sv[["cell_ID"]][which(keep)]
}

# Normalise the explicit `space` argument to a giottoSpace (or NULL).
# Accepts: NULL (no space), a giottoSpace, or a character name to look up
# on the gobject.
#
# IMPORTANT: this only resolves the *output* space -- the frame that
# transforms get applied to on returned data. The *predicate* frame
# (how a recorded crop region is interpreted) lives on `view@space` and
# is consulted directly by the crop step handlers. Conflating the two was
# the original bug that caused `getSpatialLocations(g, view = "test")` to
# silently return rotated coords whenever `view@space` was set.
#' @keywords internal
#' @noRd
.resolve_space <- function(gobject, space = NULL) {
    if (is.null(space)) return(NULL)
    if (inherits(space, "giottoSpace")) return(space)
    if (is.character(space)) return(giottoSpace(gobject, space))
    stop("`space` must be NULL, a character(1), or a giottoSpace",
        call. = FALSE)
}

# Pick the right sample key from a giottoSpace for the current gobject.
# Single-giotto: prefer ":default:", else the only key, else NULL (no
# matching transform). Multi-giotto child resolution handled by caller.
#' @keywords internal
#' @noRd
.space_sample_key_for <- function(gobject, space) {
    if (is.null(space)) return(NULL)
    keys <- names(space@samples)
    if (length(keys) == 0L) return(NULL)
    if (.space_default_sample %in% keys) return(.space_default_sample)
    if (length(keys) == 1L) return(keys[[1L]])
    NULL
}

# Apply a giottoSpace's transforms to a subobject via existing eager
# GiottoClass dispatch. Each transform step becomes
# `do.call(op, c(list(x = subobj), args))`. No-op if space is NULL or no
# matching sample key.
#' @keywords internal
#' @noRd
.apply_space_to_subobj <- function(subobj, gobject, space, coordinator) {
    if (is.null(space)) return(subobj)
    key <- .space_sample_key_for(gobject, space)
    if (is.null(key)) return(subobj)
    steps <- space@samples[[key]]
    for (step in steps) {
        subobj <- do.call(step$op, c(list(x = subobj), step$args))
    }
    subobj
}

# Is this region an axis-aligned rectangle?
#
# A recorded region is always WKT (Q7), so the numeric-extent fast path can
# no longer be selected by the stored type. It is recovered from the
# geometry instead: a single-part polygon with exactly two distinct x and
# two distinct y values IS its own bounding box.
#
# Only sound for `relation = "intersects"`, where "centroid in bbox" and
# the relation agree; every other relation goes through terra::is.related.
#' @keywords internal
#' @noRd
.region_is_rect <- function(region) {
    if (!inherits(region, "SpatVector")) return(FALSE)
    if (nrow(region) != 1L) return(FALSE)
    if (!identical(terra::geomtype(region), "polygons")) return(FALSE)
    g <- tryCatch(terra::geom(region), error = function(e) NULL)
    if (is.null(g)) return(FALSE)
    if (length(unique(g[, "part"])) != 1L) return(FALSE)
    length(unique(g[, "x"])) == 2L && length(unique(g[, "y"])) == 2L
}

# Given a spatLocs data.table (with cell_ID, sdimx, sdimy), a region
# (SpatVector, materialized from the recorded WKT), and a relation
# ("intersects" by default), return cell_IDs whose centroid satisfies
# the relation against the region.
#
# Strategy:
#   * axis-aligned rectangle + "intersects" → AABB-only check (fast path)
#   * otherwise → AABB pre-filter narrows candidates, then
#     terra::is.related gives the precise survival set
#
# Routing here is refined by the follow-up centroid-routing pass, which
# decides on (relation, polygon source availability) rather than on
# geometry shape alone.
#' @keywords internal
#' @noRd
.cells_in_region <- function(sl_dt, region, relation = "intersects") {
    if (is.null(region)) return(sl_dt$cell_ID)
    sdimx <- sdimy <- NULL  # NSE

    bbox <- terra::ext(region)[]
    in_bbox <- sl_dt$sdimx >= bbox[[1L]] & sl_dt$sdimx <= bbox[[2L]] &
               sl_dt$sdimy >= bbox[[3L]] & sl_dt$sdimy <= bbox[[4L]]

    if (identical(relation, "intersects") && .region_is_rect(region)) {
        return(sl_dt$cell_ID[in_bbox])
    }

    candidates <- sl_dt[in_bbox]
    if (nrow(candidates) == 0L) return(character())
    pts <- terra::vect(
        as.matrix(candidates[, .(sdimx, sdimy)]), type = "points")
    surv <- terra::is.related(pts, region, relation)
    candidates$cell_ID[surv]
}

# Pull the gobject's spatLocs (active spat_unit) as a data.table, optionally
# applying the relevant space's transforms first. Used by .surviving_cell_ids
# for crop-step interpretation.
#
# giottoMulti: getSpatialLocations returns a per-child named list (spatial
# locations live per-child, no joint slot). Scope the space to each child,
# apply, and rbind the coordinate DTs with `<sample>::` prefixed cell_IDs
# so crop-step results match the joint cell_metadata vocabulary.
#' @keywords internal
#' @noRd
.get_projected_spatlocs <- function(gobject, space, coordinator) {
    cell_ID <- NULL  # NSE
    sl <- tryCatch(getSpatialLocations(gobject, output = "spatLocsObj"),
        error = function(e) NULL)
    if (is.null(sl)) return(NULL)
    if (is.list(sl) && !inherits(sl, "spatLocsObj")) {
        parts <- lapply(names(sl), function(nm) {
            child_sl <- sl[[nm]]
            if (!inherits(child_sl, "spatLocsObj")) return(NULL)
            child_space <- .scope_space_to_sample(space, nm)
            if (!is.null(child_space)) {
                child_sl <- .apply_space_to_subobj(child_sl, gobject,
                    child_space, coordinator)
            }
            dt <- data.table::copy(child_sl@coordinates)
            dt[, cell_ID := paste(nm, cell_ID, sep = "::")]
            dt
        })
        parts <- Filter(Negate(is.null), parts)
        if (length(parts) == 0L) return(NULL)
        return(data.table::rbindlist(parts, use.names = TRUE, fill = TRUE))
    }
    if (!is.null(space)) {
        sl <- .apply_space_to_subobj(sl, gobject, space, coordinator)
    }
    sl@coordinates
}

# JIT helper for getters: apply view/space projection to a single subobject
# fetched by an accessor. Returns the subobject unchanged if neither view
# nor space is supplied. Normalises character `view` / `space` lookups
# against the gobject; picks the default resolver from `gobject@source`.
#
# Use at the tail of a getter:
#   obj <- getterLogic(...)
#   obj <- .apply_view_space(obj, gobject, view, space)
#   return(obj)
#' @keywords internal
#' @noRd
.apply_view_space <- function(subobj, gobject, view = NULL, space = NULL,
                              coordinator = NULL) {
    if (is.null(view) && is.null(space)) return(subobj)
    # view contract: character(1) name of a slotted view, or NULL.
    # Inline giottoView objects were considered and rejected — views
    # are curated artifacts; build + slot via giottoView<-(g, name) <- v
    # if programmatic composition is needed. See
    # vignettes/articles/DESIGN_gmulti_federation.md for the reasoning.
    if (!is.null(view)) {
        checkmate::assert_string(view, .var.name = "view")
    }
    v <- if (is.null(view)) NULL else giottoView(gobject, view)
    # `space` here is the OUTPUT frame -- the predicate frame (view@space)
    # is consulted independently by the crop step handlers below.
    s <- .resolve_space(gobject, space)
    co <- if (is.null(coordinator)) .default_view_coordinator(gobject)
        else coordinator
    resolveSubobject(subobj, gobject, v, s, co)
}


# Per-call cache for expensive computations (currently just the surviving
# cell_ID set). Created by materialize() at entry; threaded through
# resolveSubobject via `.cache` in `...`. Each materialize call gets a
# fresh env; JIT getter calls that don't pass a cache just recompute.
#
# Scope: per-materialize-call. Persistent caching across calls needs a
# version-stamp invalidation scheme (deferred).
#' @keywords internal
#' @noRd
.new_resolver_cache <- function() new.env(parent = emptyenv())

# Memoising wrapper around .surviving_cell_ids. Reads/writes through `cache`
# if supplied; falls back to a direct call when cache is NULL.
#' @keywords internal
#' @noRd
.cached_surviving_cell_ids <- function(gobject, view, coordinator,
                                       cache = NULL) {
    if (is.null(cache)) {
        return(.surviving_cell_ids(gobject, view, coordinator))
    }
    if (exists("surviving_ids", envir = cache, inherits = FALSE)) {
        return(get("surviving_ids", envir = cache))
    }
    ids <- .surviving_cell_ids(gobject, view, coordinator)
    assign("surviving_ids", ids, envir = cache)
    ids
}

# Compute the cell_ID set that survives a view's filter + crop steps.
# Returns a character vector of cell_IDs; NULL means "no narrowing" (all
# cells survive).
#
# The PREDICATE frame for crop steps is `view@space` -- the frame the
# crop region was drawn in. This is independent of any output space the
# caller may have requested via the explicit `space=` arg, which is why
# this helper does not take a `space` argument.
#' @keywords internal
#' @noRd
.surviving_cell_ids <- function(gobject, view, coordinator) {
    if (is.null(view)) return(NULL)

    filter_steps <- .view_steps_of(view, "filter")
    crop_steps   <- .view_steps_of(view, "crop")

    if (length(filter_steps) == 0L && length(crop_steps) == 0L) return(NULL)

    surviving <- spatIDs(gobject)
    for (step in filter_steps) {
        keep <- .eval_view_filter(step, gobject)
        surviving <- intersect(surviving, keep)
    }
    if (length(crop_steps) > 0L) {
        pred_space <- if (!is.na(view@space)) {
            .resolve_space(gobject, view@space)
        } else NULL
        sl_dt <- .get_projected_spatlocs(gobject, pred_space, coordinator)
        if (is.null(sl_dt)) {
            warning("crop step skipped: no spatial locations available",
                call. = FALSE)
        } else {
            for (step in crop_steps) {
                region <- .materialize_crop_region(step$region)
                keep <- .cells_in_region(sl_dt, region, step$relation)
                surviving <- intersect(surviving, keep)
            }
        }
    }
    surviving
}

# Apply every recorded crop step geometrically to a non-cell-keyed
# subobject (points, images), in the post-transform frame.
#' @keywords internal
#' @noRd
.apply_crops_geometrically <- function(subobj, view) {
    for (step in .view_steps_of(view, "crop")) {
        subobj <- crop(subobj, .materialize_crop_region(step$region))
    }
    subobj
}


# Tabular subobject methods (dataTableCoordinator) ####
# Tabular subobjects (cellMetaObj, exprObj, dimObj, spatEnrObj) narrow by
# the surviving cell_ID set and are otherwise untouched by space transforms
# (which are no-ops on non-spatial data).
#
# Note: for dataTableCoordinator, `prepareIds()` is the identity transform,
# so these methods consume `keep` directly via `%in%`. Backed coordinators
# (duckDB / sedona) register their own resolveSubobject methods that route
# through `prepareIds()` to promote the ID set into a JOIN-able table
# reference before applying it.

#' @rdname resolveSubobject
#' @export
setMethod("resolveSubobject",
    signature(subobj = "cellMetaObj", coordinator = "dataTableCoordinator"),
    function(subobj, gobject, view, space, coordinator, ...) {
        cache <- list(...)$.cache
        keep <- .cached_surviving_cell_ids(gobject, view, coordinator, cache)
        if (is.null(keep)) return(subobj)
        .narrow_subobject(subobj, cells = keep)
    }
)

#' @rdname resolveSubobject
#' @export
setMethod("resolveSubobject",
    signature(subobj = "exprObj", coordinator = "dataTableCoordinator"),
    function(subobj, gobject, view, space, coordinator, ...) {
        cache <- list(...)$.cache
        keep <- .cached_surviving_cell_ids(gobject, view, coordinator, cache)
        if (is.null(keep)) return(subobj)
        .narrow_subobject(subobj, cells = keep)
    }
)

#' @rdname resolveSubobject
#' @export
setMethod("resolveSubobject",
    signature(subobj = "dimObj", coordinator = "dataTableCoordinator"),
    function(subobj, gobject, view, space, coordinator, ...) {
        cache <- list(...)$.cache
        keep <- .cached_surviving_cell_ids(gobject, view, coordinator, cache)
        if (is.null(keep)) return(subobj)
        .narrow_subobject(subobj, cells = keep)
    }
)

#' @rdname resolveSubobject
#' @export
setMethod("resolveSubobject",
    signature(subobj = "spatEnrObj", coordinator = "dataTableCoordinator"),
    function(subobj, gobject, view, space, coordinator, ...) {
        cache <- list(...)$.cache
        keep <- .cached_surviving_cell_ids(gobject, view, coordinator, cache)
        if (is.null(keep)) return(subobj)
        .narrow_subobject(subobj, cells = keep)
    }
)

#' @rdname resolveSubobject
#' @export
setMethod("resolveSubobject",
    signature(subobj = "featMetaObj", coordinator = "dataTableCoordinator"),
    function(subobj, gobject, view, space, coordinator, ...) {
        # Feature metadata is feat-keyed, not cell-keyed. View filters that
        # carry `feat_ids` in their scope_args could narrow it; otherwise
        # featMetaObj passes through untouched.
        subobj
    }
)


# Spatial subobject methods (dataTableCoordinator) ####
# Spatial subobjects narrow by surviving cell_IDs (where cell-keyed) AND
# apply the space's transforms via existing eager GiottoClass dispatch.
# Crop is interpreted via the surviving cell_ID set (centroid-in-region
# semantics) for cell-keyed spatial subobjects; for non-cell-keyed
# (points, images), crop is applied geometrically at the subobject level.

#' @rdname resolveSubobject
#' @export
setMethod("resolveSubobject",
    signature(subobj = "spatLocsObj", coordinator = "dataTableCoordinator"),
    function(subobj, gobject, view, space, coordinator, ...) {
        cache <- list(...)$.cache
        keep <- .cached_surviving_cell_ids(gobject, view, coordinator, cache)
        if (!is.null(keep)) {
            subobj <- .narrow_subobject(subobj, cells = keep)
        }
        .apply_space_to_subobj(subobj, gobject, space, coordinator)
    }
)

#' @rdname resolveSubobject
#' @export
setMethod("resolveSubobject",
    signature(subobj = "giottoPolygon", coordinator = "dataTableCoordinator"),
    function(subobj, gobject, view, space, coordinator, ...) {
        cache <- list(...)$.cache
        keep <- .cached_surviving_cell_ids(gobject, view, coordinator, cache)
        if (!is.null(keep)) {
            # Polygon's poly_ID is conventionally aligned with cell_ID for
            # the cells spat_unit. (Unlinked-poly cascade is out of scope.)
            subobj <- .narrow_subobject(subobj, cells = keep)
            # Cached ID list, if present
            if (length(subobj@unique_ID_cache) > 0L) {
                subobj@unique_ID_cache <- intersect(
                    subobj@unique_ID_cache, keep)
            }
        }
        .apply_space_to_subobj(subobj, gobject, space, coordinator)
    }
)

#' @rdname resolveSubobject
#' @export
setMethod("resolveSubobject",
    signature(subobj = "giottoPoints", coordinator = "dataTableCoordinator"),
    function(subobj, gobject, view, space, coordinator, ...) {
        # Points are not cell-keyed; cell narrowing doesn't apply directly.
        # A crop applies geometrically at the subobject level, in the
        # post-transform frame: transform first, then crop in that frame.
        subobj <- .apply_space_to_subobj(subobj, gobject, space, coordinator)
        .apply_crops_geometrically(subobj, view)
    }
)

#' @rdname resolveSubobject
#' @export
setMethod("resolveSubobject",
    signature(subobj = "giottoLargeImage",
        coordinator = "dataTableCoordinator"),
    function(subobj, gobject, view, space, coordinator, ...) {
        subobj <- .apply_space_to_subobj(subobj, gobject, space, coordinator)
        .apply_crops_geometrically(subobj, view)
    }
)

#' @rdname resolveSubobject
#' @export
setMethod("resolveSubobject",
    signature(subobj = "giottoAffineImage",
        coordinator = "dataTableCoordinator"),
    function(subobj, gobject, view, space, coordinator, ...) {
        subobj <- .apply_space_to_subobj(subobj, gobject, space, coordinator)
        .apply_crops_geometrically(subobj, view)
    }
)

#' @rdname resolveSubobject
#' @export
setMethod("resolveSubobject",
    signature(subobj = "giottoImage", coordinator = "dataTableCoordinator"),
    function(subobj, gobject, view, space, coordinator, ...) {
        subobj <- .apply_space_to_subobj(subobj, gobject, space, coordinator)
        .apply_crops_geometrically(subobj, view)
    }
)

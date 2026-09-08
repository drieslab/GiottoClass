# =============================================================================
# giottoView — read-only subset / narrowing recipe
# =============================================================================
#
# A view describes a deferred, read-only NARROWING of a `giotto` (or
# `giottoMulti`) object. It is a *recipe*, not a snapshot: each time it is
# consumed it is re-resolved against the current state of the gobject.
#
# Spatial transforms (positioning) live in [giottoSpace], NOT here.
# Views handle subsets, crops, and sample-selection only. The two compose at
# the consumer-function API: `plot(g, space = "atlas", view = "tumor")`.
#
# A view may optionally reference a named space via `space`. This is what
# defines the coordinate frame in which extent-based crops are meaningful:
# a recorded crop region is interpreted in that frame, so the resolver
# positions the data before evaluating the region against it.
#
# Shape — views are built through the gobject, by name:
#   g <- subset(g, cluster == "A", view = "tumor_focus")
#   g <- crop(g, c(0, 100, 0, 100), view = "tumor_focus", space = "atlas")
#   g <- selectSamples(mg, "a", "b", view = "tumor_focus")   # gmulti only
#
# `.record_view_on_gobject()` creates the named view on first use and
# appends on later calls, so there is no separate construction step.
#
# RECIPES ARE PLAIN LISTS, NOT S4 (decisions Q7 + Q8)
# ---------------------------------------------------
# A view is
#
#   list(steps = list(<step>, ...), space = NA_character_, misc = list())
#
# and a step is `list(type = "<tag>", ...)`. Q7 made the steps lists; Q8
# made the container follow, because the S4 class was not load-bearing:
# `@view` is `nullOrList` so it enforced nothing, the class was never a
# dispatch target outside its own builder verbs, and those verbs were a
# second copy of the gobject-side surface.
#
# Validation moved with the shape rather than disappearing:
# `.validate_view()` and `.validate_view_step()` run in the recorder and in
# the `giottoView<-` setter, so the check still happens at the boundary
# where the user's call site is in scope for the error message.
#
# The reason the recorded form is normalized: a recipe must survive
# `saveRDS` and reach a parallel worker. Three payloads made that false, and
# each is normalized at record time rather than at resolve time:
#
#   * the filter predicate is stored **deparsed to a character string**, so
#     no environment reference rides along. Env-resident scalars/vectors are
#     substituted into the expression first (`.eager_substitute_env()`), so
#     the string is self-contained; functions resolve through the package
#     chain at eval time.
#   * a crop region is stored as **WKT**, geometry only. terra objects are
#     C++ pointer-backed and do not survive a round-trip.
#   * transform arguments are whitelisted to serializable types.
#
# GiottoDisk's `@ops` chain is the same shape for the same reason.
#
# Step taxonomy (subset-flavor only — transforms live on giottoSpace):
#   type = "filter"    predicate-style row filter (deparsed NSE)
#   type = "crop"      region-based crop (region meaningful in `space`)
#   type = "samples"   gmulti-only child filter
#
# Cell-keyed propagation:
#   A `subset()` predicate is evaluated against `spatValues(g)` for the
#   columns it references. Surviving cell_IDs propagate to cell-keyed slots
#   (expression, spatial_locs, dim_reduction, polys with a `cell_ID` column)
#   automatically via the existing relational structure — no flag needed.
#   Polygons without a `cell_ID` linkage are out of scope for views; link
#   them first or handle them in a separate step.
#
# Read-only contract:
#   * RESOLVING a view never mutates the gobject and never touches the data
#     it narrows.
#   * RECORDING a step is the one write: `subset(g, ..., view = "v")`
#     returns the gobject with the recipe updated, data untouched.
#   * `materialize()` — explicit escape hatch from a view to a new
#     standalone gobject.
#
# See `R/classes-space.R` for the spatial-transform recipe.
# See `R/methods-view.R` for the recorder and the accessors.
# =============================================================================


# view step constructors + validators ####
#
# One constructor and one validator per step type. This is what replaces
# `setClass`'s slot type checking after Q7 — the check happens once, at
# record time, which is also where the user's call site is still in scope
# for a good error message.

# Recognized step tags, in the order the resolver considers them.
.view_step_types <- c("samples", "filter", "crop")

# What a cell is represented by when a crop predicate is evaluated. A crop
# narrows the CELL SET, so the cell has to be reduced to a geometry: either
# its centroid (its `spatial_locs` row) or its polygon. The choice is
# DECLARED on the step rather than inferred from the relation, so a
# serialized recipe says which question it asks, and so the backed
# resolvers in {GiottoDisk} can read it instead of re-deriving it.
.view_crop_geoms <- c("centroid", "poly")

# Predicates terra accepts for point/polygon pairs. Note "covered_by" is
# NOT among them despite appearing in some terra docs — it errors.
.view_crop_relations <- c("intersects", "disjoint", "within", "touches",
    "contains", "covers", "overlaps", "crosses")

# Always FALSE when one side is reduced to a point: a point cannot contain
# or cover a polygon, cannot overlap it (that needs equal dimensions), and
# cannot cross it. Measured against terra, not assumed. Asking for one of
# these on a centroid is not an approximation, it is an empty result, so
# the constructor promotes `geom` to "poly" for them.
.view_crop_poly_only_relations <- c("contains", "covers", "overlaps",
    "crosses")

# Scope arguments a filter step may carry alongside its predicate. They are
# forwarded verbatim to `spatValues()` when the step resolves, so this list
# is exactly its scoping formals.
#
# Validated at record time because `subset()` funnels its `...` in here.
# Before this was checked, `subset(g, pred, view = "v")` recorded
# `negate = FALSE` and the step failed at RESOLVE time with
# `unused argument (negate = FALSE)` from inside `spatValues()` -- far from
# the call that caused it. `spatValues()` pulls named values out of slots
# and has no notion of negation, so `negate` never belonged here: the
# recorder folds it into the predicate as `!(pred)` instead, matching what
# the eager path does, so the step carries the effective predicate.
.view_filter_scope_args <- c("spat_unit", "feat_type", "expression_values",
    "spat_loc_name", "spat_enr_name", "poly_info", "dim_reduction_to_use",
    "dim_reduction_name")

#' Construct a filter step.
#'
#' `predicate` arrives as a language object and is stored deparsed, so the
#' step carries no environment. `scope_args` carries any additional
#' arguments passed alongside the predicate at the `subset()` call site
#' (e.g. `spat_unit`, `feat_type`, `negate`) so they reach the underlying
#' `spatValues()` call at resolution time.
#' @noRd
.view_step_filter <- function(predicate, scope_args = list()) {
    if (is.character(predicate)) {
        checkmate::assert_string(predicate, .var.name = "predicate")
        pred_txt <- predicate
    } else if (is.language(predicate)) {
        # deparse(nlines=) would silently truncate a long predicate; join
        # the full multi-element deparse instead
        pred_txt <- paste(deparse(predicate), collapse = " ")
    } else {
        stop("[view step] `predicate` must be an expression or a string ",
            "(got '", class(predicate)[[1L]], "')", call. = FALSE)
    }
    checkmate::assert_list(scope_args, names = "unique",
        .var.name = "scope_args")
    # `subset()` passes its scoping formals through unconditionally, so most
    # arrive NULL. Drop them: a recorded recipe should read as what the user
    # actually asked for.
    if (length(scope_args) > 0L) {
        scope_args <- scope_args[
            !vapply(scope_args, is.null, logical(1L))]
    }
    unknown <- setdiff(names(scope_args), .view_filter_scope_args)
    if (length(unknown) > 0L) {
        stop("[view step] filter cannot record ",
            paste(sprintf("`%s`", unknown), collapse = ", "),
            ". A filter step scopes the `spatValues()` lookup its predicate ",
            "resolves against, so it accepts: ",
            paste(.view_filter_scope_args, collapse = ", "),
            call. = FALSE)
    }
    .validate_view_step(list(
        type = "filter",
        predicate = pred_txt,
        scope_args = scope_args
    ))
}

#' Construct a crop step.
#'
#' `region` is normalized to WKT by `.normalize_crop_region()` before it
#' reaches here.
#'
#' `geom` declares what represents a cell — see `.view_crop_geoms`. For the
#' relations that are always FALSE against a point it is promoted to
#' `"poly"` with a warning, and the step records the **effective** value, so
#' a recorded recipe never claims to do something other than what it will
#' do.
#' @noRd
.view_step_crop <- function(region, relation = "intersects",
    geom = "centroid") {
    checkmate::assert_string(relation, .var.name = "relation")
    checkmate::assert_string(geom, .var.name = "geom")
    if (!relation %in% .view_crop_relations) {
        stop("[view step] crop relation '", relation, "' is not available. ",
            "One of: ", paste(.view_crop_relations, collapse = ", "),
            call. = FALSE)
    }
    if (!geom %in% .view_crop_geoms) {
        stop("[view step] crop geom '", geom, "' is not available. One of: ",
            paste(.view_crop_geoms, collapse = ", "), call. = FALSE)
    }
    if (relation %in% .view_crop_poly_only_relations &&
        identical(geom, "centroid")) {
        warning(sprintf(paste0(
            "[crop] relation '%s' is always FALSE against a cell centroid, ",
            "so it needs the cell polygon; recording geom = \"poly\". Pass ",
            "geom = \"poly\" explicitly to silence this."),
            relation), call. = FALSE)
        geom <- "poly"
    }
    .validate_view_step(list(
        type = "crop",
        region = region,
        relation = relation,
        geom = geom
    ))
}

#' Construct a sample-select step (giottoMulti only).
#' @noRd
.view_step_samples <- function(samples) {
    checkmate::assert_character(samples, min.len = 1L, any.missing = FALSE,
        .var.name = "samples")
    .validate_view_step(list(type = "samples", samples = samples))
}

#' Validate one view step, whatever produced it.
#'
#' Also the guard for a hand-built or hand-edited step — recipes are plain
#' lists now, so a user can write one directly and should get told when it
#' is malformed rather than at resolve time.
#' @noRd
.validate_view_step <- function(step) {
    if (!is.list(step) || is.null(step$type)) {
        stop("[view step] a step must be a list with a `type` element",
            call. = FALSE)
    }
    if (!step$type %in% .view_step_types) {
        stop("[view step] unknown type '", step$type, "'. Known: ",
            paste(.view_step_types, collapse = ", "), call. = FALSE)
    }
    switch(step$type,
        filter = {
            checkmate::assert_string(step$predicate,
                .var.name = "step$predicate")
            # must parse — catches a hand-written predicate string early
            ok <- tryCatch({
                str2lang(step$predicate)
                TRUE
            }, error = function(e) FALSE)
            if (!ok) {
                stop("[view step] filter predicate does not parse: ",
                    step$predicate, call. = FALSE)
            }
            checkmate::assert_list(step$scope_args, null.ok = TRUE,
                .var.name = "step$scope_args")
        },
        crop = {
            checkmate::assert_string(step$region, .var.name = "step$region")
            checkmate::assert_choice(step$relation, .view_crop_relations,
                .var.name = "step$relation")
            checkmate::assert_choice(step$geom, .view_crop_geoms,
                .var.name = "step$geom")
            # Unreachable via .view_step_crop(), which promotes instead.
            # Catches a hand-poked step whose declaration cannot answer
            # its own relation.
            if (step$relation %in% .view_crop_poly_only_relations &&
                identical(step$geom, "centroid")) {
                stop("[view step] crop relation '", step$relation,
                    "' cannot be evaluated with geom = \"centroid\" ",
                    "(always FALSE against a point)", call. = FALSE)
            }
        },
        samples = {
            checkmate::assert_character(step$samples, min.len = 1L,
                any.missing = FALSE, .var.name = "step$samples")
        }
    )
    step
}

#' Steps of one type, in recorded order.
#'
#' Replaces the `Filter(function(s) inherits(s, "<class>"), steps)` idiom
#' the S4 steps needed. Tolerates a `NULL` view so callers can stay
#' branch-free.
#' @noRd
.view_steps_of <- function(view, type) {
    if (is.null(view)) return(list())
    Filter(function(s) identical(s$type, type), view$steps)
}


# view recipe ####

# The fields a view carries. `misc` holds provenance / cache keys; nothing
# reads it internally. The dropped S4 slots were `name` (redundant -- the
# name is the key under `gobject@view`) and `source` (documented as
# reserved, never read).
.view_fields <- c("steps", "space", "misc")

#' Construct a view recipe.
#'
#' The single place a view's shape is written down, so a field added here
#' reaches every producer.
#' @noRd
.new_view <- function(steps = list(), space = NA_character_,
    misc = list()) {
    list(
        steps = steps,
        space = as.character(space),
        misc = misc
    )
}

#' Validate a whole view, whatever produced it.
#'
#' Runs in the recorder and in `giottoView<-`. Rejecting unknown fields is
#' deliberate: a recipe is hand-editable now, and a typo'd field would
#' otherwise be carried silently and ignored at resolve time.
#' @noRd
.validate_view <- function(view, .var.name = "view") {
    if (!is.list(view)) {
        stop("[view] `", .var.name, "` must be a list (got '",
            class(view)[[1L]], "')", call. = FALSE)
    }
    unknown <- setdiff(names(view), .view_fields)
    if (length(unknown) > 0L) {
        stop("[view] unknown field(s): ",
            paste(sprintf("`%s`", unknown), collapse = ", "),
            ". A view holds: ", paste(.view_fields, collapse = ", "),
            call. = FALSE)
    }
    checkmate::assert_list(view$steps, .var.name = paste0(.var.name, "$steps"))
    checkmate::assert_character(view$space, len = 1L,
        .var.name = paste0(.var.name, "$space"))
    checkmate::assert_list(view$misc, null.ok = TRUE,
        .var.name = paste0(.var.name, "$misc"))
    lapply(view$steps, .validate_view_step)
    view
}

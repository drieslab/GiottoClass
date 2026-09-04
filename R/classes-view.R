# =============================================================================
# giottoView — read-only subset / narrowing recipe
# =============================================================================
#
# `giottoView` is a composable standalone S4 class describing a deferred,
# read-only NARROWING of a `giotto` (or `giottoMulti`) object. A view is a
# *recipe*, not a snapshot: each time it is consumed it is re-resolved
# against the current state of the underlying gobject.
#
# Spatial transforms (positioning) live in [giottoSpace-class], NOT here.
# Views handle subsets, crops, and sample-selection only. The two compose at
# the consumer-function API: `plot(g, space = "atlas", view = "tumor")`.
#
# A view may optionally reference a named space via `@space`. This is what
# defines the coordinate frame in which extent-based crops are meaningful:
# `crop(c(0, 100, 0, 100))` records as a crop step against the view's
# `@space` reference, so the resolver knows to position the data first.
#
# Shape:
#   v <- giottoView(space = "atlas") |>
#       subset(cluster == "A") |>             # cell-keyed predicate
#       crop(c(0, 100, 0, 100))               # crop in atlas frame
#
#   v_multi <- giottoView() |>
#       selectSamples("a", "b") |>            # gmulti-only child filter
#       subset(cluster == "A")
#
#   giottoView(g, "tumor_focus") <- v
#
# STEPS ARE PLAIN TAGGED LISTS, NOT S4 (design decision Q7)
# ---------------------------------------------------------
# A step is `list(type = "<tag>", ...)`. The container classes stay S4 —
# they carry real methods (`+`, `show`, `materialize`, `subset`, `crop`,
# `selectSamples`, the transform generics) and slot typing on
# `gobject@view` / `@spaces`. The steps carried no dispatch at all, so S4
# bought nothing and cost serializability.
#
# The reason this matters: a recipe must survive `saveRDS` and reach a
# parallel worker. Three payloads made that false, and each is normalized
# at record time rather than at resolve time:
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
#   type = "crop"      region-based crop (region meaningful in @space)
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
# Read-only contract (signature-as-contract):
#   * Views never mutate the underlying gobject.
#   * Functions that accept a `view =` parameter return their result.
#   * Functions that mutate the gobject do not accept a `view =` parameter.
#   * `materialize()` — explicit escape hatch from a view to a new
#     standalone gobject.
#
# See `R/classes-space.R` for the spatial-transform recipe (`giottoSpace`).
# See `R/methods-view.R` and `R/methods-space.R` for the constructors,
# composition, accessors, and show methods.
# =============================================================================


# view step constructors + validators ####
#
# One constructor and one validator per step type. This is what replaces
# `setClass`'s slot type checking after Q7 — the check happens once, at
# record time, which is also where the user's call site is still in scope
# for a good error message.

# Recognized step tags, in the order the resolver considers them.
.view_step_types <- c("samples", "filter", "crop")

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
#' @noRd
.view_step_crop <- function(region, relation = "intersects") {
    checkmate::assert_string(relation, .var.name = "relation")
    .validate_view_step(list(
        type = "crop",
        region = region,
        relation = relation
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
            checkmate::assert_string(step$relation,
                .var.name = "step$relation")
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
    Filter(function(s) identical(s$type, type), view@steps)
}


# giottoView ####

#' @title S4 giottoView class
#' @name giottoView-class
#' @description A `giottoView` is a composable, read-only, lazy subset recipe
#' over a `giotto` (or `giottoMulti`) object. It records predicate filters,
#' region crops, and sample selectors. Spatial transforms (positioning) live
#' on [giottoSpace-class], not here.
#'
#' At resolution time, the recorded steps are applied against the current
#' gobject state without mutating it. Slotted views (added via
#' `giottoView(g, "name") <- v`) travel with the gobject through save/load.
#'
#' Compose by piping through `subset()`, `crop()`, `selectSamples()`:
#'
#' ```r
#' v <- giottoView(space = "atlas") |>
#'     subset(cluster == "A") |>
#'     crop(c(0, 100, 0, 100))
#'
#' giottoView(g, "tumor_focus") <- v
#' ```
#'
#' @slot steps `list` of steps. Each step is a plain tagged list
#'   (`list(type = "filter" | "crop" | "samples", ...)`) so a recipe is
#'   inspectable, hand-editable, and survives serialization — see the notes
#'   at the top of `R/classes-view.R`.
#' @slot space `character(1)`. Optional reference to a slotted space name
#'   (see [giottoSpace-class]). The coordinate frame in which crop regions
#'   are interpreted. `NA_character_` means the gobject's native frame.
#' @slot name `character(1)`. `NA_character_` until the view is slotted into
#'   a gobject; thereafter holds the slot key.
#' @slot source `ANY`. Reserved pointer / fingerprint. `NULL` for standalone.
#' @slot misc `list`. Provenance, cache keys, version stamps.
#' @returns `giottoView`
#' @examples
#' giottoView()
#' @export
#' @exportClass giottoView
setClass(
    "giottoView",
    slots = list(
        steps  = "list",
        space  = "character",
        name   = "character",
        source = "ANY",
        misc   = "list"
    ),
    prototype = list(
        steps  = list(),
        space  = NA_character_,
        name   = NA_character_,
        source = NULL,
        misc   = list()
    ),
    validity = function(object) {
        if (length(object@steps) > 0L) {
            ok <- tryCatch({
                lapply(object@steps, .validate_view_step)
                TRUE
            }, error = function(e) conditionMessage(e))
            if (!isTRUE(ok)) return(ok)
        }
        TRUE
    }
)

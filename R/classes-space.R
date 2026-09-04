# =============================================================================
# giottoSpace — coordinate-frame recipe (parallel space opt-in)
# =============================================================================
#
# `giottoSpace` is a composable standalone S4 class describing a coordinate
# frame for a `giotto` (single-sample) or `giottoMulti` (multi-sample) object.
# Slotted spaces are named alternate coordinate frames that consumer functions
# opt into via `space = "name"`.
#
# Unlike views (which are read-only narrowings), spaces are NOT subject to
# the read-only contract — analyses run in a non-native space are fine; the
# coordinate frame just differs. Mutations still target the underlying data
# in its native frame.
#
# Sample scope: transforms in a `giottoSpace` are SAMPLE-UNIFORM. Within a
# single sample, all spatial elements (cells, polys, points, image,
# spatlocs) move together. Per-element overrides are deliberately not
# supported here — sample-level is the granularity that matches the typical
# spatialomics alignment workflow.
#
# Shape:
#   # single-sample (giotto)
#   s <- giottoSpace() |> affine(M)
#   giottoSpace(g, "tilted") <- s
#
#   # multi-sample (giottoMulti)
#   sa <- giottoSpace("sample_a") |> affine(M_a)
#   sb <- giottoSpace("sample_b") |> affine(M_b)
#   atlas <- sa + sb                       # combine sample recipes
#   giottoSpace(mg, "atlas") <- atlas
#
# `+` composition:
#   * same-sample (`sample_a` + `sample_a`) → steps concatenated in order
#   * different-sample → samples merged into one giottoSpace keyed by name
#
# Step taxonomy — one type, `"transform"`: a deferred call to one of the
# GiottoClass spatial transform generics (`affine`, `spin`, `spatShift`,
# `flip`, `rescale`, `shear`, `zoom`). At resolution time the receiving
# gobject (or child) is spliced as the first argument and `do.call()`
# dispatches to the existing transform method.
#
# Steps are plain tagged lists rather than S4 for the reasons given at the
# top of `R/classes-view.R` (decision Q7). `args` is whitelisted to
# serializable types at record time so the recipe survives `saveRDS` and
# reaches a parallel worker.
#
# Storage on gobject:
#   `gobject@spaces` — named list of `giottoSpace`. Sentinel sample name
#   `:default:` is used for single-giotto entries (no explicit sample).
#
# See `R/classes-view.R` for the subset/narrowing recipe.
# See `R/methods-space.R` for the constructors, `+` composition, record
# methods on the existing transform generics, accessor, show.
# =============================================================================


# Sentinel for sample-anonymous (single-giotto) space construction.
.space_default_sample <- ":default:"

# Transform generics a space step may defer to. A space step is executed
# by `do.call(op, ...)`, so this list is also the guard against an
# arbitrary function name reaching a recorded recipe.
.space_ops <- c("spin", "spatShift", "affine", "flip", "rescale", "shear",
    "zoom")


# space step constructor + validator ####

#' Construct a transform step.
#'
#' `args` are whitelisted to serializable types — see
#' `.assert_space_args_serializable()`. `spin` / `affine` get their
#' rotation origin defaulted here so the recorded step is a complete
#' description of the operation rather than one that depends on the
#' receiving object's extent at resolve time.
#' @noRd
.space_step_transform <- function(op, args = list()) {
    checkmate::assert_string(op, .var.name = "op")
    if (!op %in% .space_ops) {
        stop("[space step] unknown transform '", op, "'. Known: ",
            paste(.space_ops, collapse = ", "), call. = FALSE)
    }
    checkmate::assert_list(args, .var.name = "args")
    if (op %in% c("spin", "affine") && length(args) > 0L) {
        if (is.null(args[["x0"]])) args$x0 <- 0
        if (is.null(args[["y0"]])) args$y0 <- 0
    }
    .assert_space_args_serializable(op, args)
    .validate_space_step(list(type = "transform", op = op, args = args))
}

#' Whitelist transform arguments to types that survive serialization.
#'
#' The transform generics take numeric / character / logical scalars and
#' vectors, matrices (an affine matrix), and `affine2d` objects. Anything
#' else — most importantly a terra object, which is a C++ pointer — would
#' make the recipe unserializable, which is the whole thing Q7 fixes. Fail
#' at record time, where the call site is still in scope.
#' @noRd
.assert_space_args_serializable <- function(op, args) {
    ok_one <- function(a) {
        if (is.null(a)) return(TRUE)
        if (inherits(a, "affine2d")) return(TRUE)
        if (is.matrix(a) && is.numeric(a)) return(TRUE)
        is.atomic(a) && (is.numeric(a) || is.character(a) || is.logical(a))
    }
    bad <- !vapply(args, ok_one, logical(1L))
    if (any(bad)) {
        nms <- names(args)[bad]
        nms[!nzchar(nms) | is.na(nms)] <- "<unnamed>"
        stop(sprintf(paste0(
            "[space step] %s() argument(s) %s cannot be recorded: got %s. ",
            "A recipe must survive serialization, so transform arguments ",
            "are limited to atomic vectors, numeric matrices, and affine2d ",
            "objects. Convert a terra object to a matrix or affine2d ",
            "first."),
            op, paste(sprintf("`%s`", nms), collapse = ", "),
            paste(vapply(args[bad], function(a) class(a)[[1L]],
                character(1L)), collapse = ", ")), call. = FALSE)
    }
    invisible(TRUE)
}

#' Validate one space step, whatever produced it.
#' @noRd
.validate_space_step <- function(step) {
    if (!is.list(step) || is.null(step$type)) {
        stop("[space step] a step must be a list with a `type` element",
            call. = FALSE)
    }
    if (!identical(step$type, "transform")) {
        stop("[space step] unknown type '", step$type,
            "'. Known: transform", call. = FALSE)
    }
    checkmate::assert_string(step$op, .var.name = "step$op")
    if (!step$op %in% .space_ops) {
        stop("[space step] unknown transform '", step$op, "'. Known: ",
            paste(.space_ops, collapse = ", "), call. = FALSE)
    }
    checkmate::assert_list(step$args, .var.name = "step$args")
    step
}


# giottoSpace ####

#' @title S4 giottoSpace class
#' @name giottoSpace-class
#' @description A `giottoSpace` is a composable, opt-in coordinate-frame
#' recipe over a `giotto` (single-sample) or `giottoMulti` (multi-sample)
#' object. It records a sequence of spatial transform steps keyed by sample
#' name; consumer functions opt into a named space via `space = "name"` and
#' the recorded transforms are applied to position the data in that frame.
#'
#' Transforms in a space are SAMPLE-UNIFORM — within a single sample, all
#' spatial elements move together. Per-element overrides are not supported;
#' use [materialize()] and per-element transforms post-hoc for that.
#'
#' Compose with `+` to combine per-sample recipes into a multi-sample space:
#'
#' ```r
#' sa <- giottoSpace("sample_a") |> affine(M_a)
#' sb <- giottoSpace("sample_b") |> affine(M_b)
#' atlas <- sa + sb
#' giottoSpace(mg, "atlas") <- atlas
#' ```
#'
#' Same-sample composition concatenates steps in order:
#'
#' ```r
#' s <- (giottoSpace("sample_a") |> spin(30)) +
#'      (giottoSpace("sample_a") |> spatShift(dx = 10))
#' ```
#'
#' @slot samples named `list`. Keys are sample names (`:default:` for
#'   single-giotto context); values are lists of transform steps to apply in
#'   order. Each step is a plain tagged list
#'   (`list(type = "transform", op = , args = )`) — see the notes at the top
#'   of `R/classes-view.R` for why steps are not S4.
#' @slot name `character(1)`. `NA_character_` until slotted into a gobject.
#' @slot source `ANY`. Reserved pointer / fingerprint. `NULL` for standalone.
#' @slot misc `list`. Provenance, cache keys.
#' @returns `giottoSpace`
#' @examples
#' giottoSpace()
#' giottoSpace("sample_a")
#' @export
#' @exportClass giottoSpace
setClass(
    "giottoSpace",
    slots = list(
        samples = "list",
        name    = "character",
        source  = "ANY",
        misc    = "list"
    ),
    prototype = list(
        samples = list(),
        name    = NA_character_,
        source  = NULL,
        misc    = list()
    ),
    validity = function(object) {
        if (length(object@samples) > 0L) {
            nms <- names(object@samples)
            if (is.null(nms) || any(is.na(nms)) || any(nms == "")) {
                return("@samples must be a named list (sample name -> step list)")
            }
            ok <- tryCatch({
                for (steps in object@samples) {
                    lapply(steps, .validate_space_step)
                }
                TRUE
            }, error = function(e) conditionMessage(e))
            if (!isTRUE(ok)) return(ok)
        }
        TRUE
    }
)

# =============================================================================
# giottoSpace — coordinate-frame recipe (parallel space opt-in)
# =============================================================================
#
# A space describes a coordinate frame for a `giotto` (single-sample) or
# `giottoMulti` (multi-sample) object. Slotted spaces are named alternate
# coordinate frames that consumer functions opt into via `space = "name"`.
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
# Shape — spaces are built through the gobject, by name:
#   # single-sample (giotto)
#   g <- affine(g, M, space = "tilted")
#
#   # multi-sample (giottoMulti). `samples =` scopes the transform to named
#   # children, which is what a cross-sample layout needs.
#   mg <- affine(mg, M_a, space = "atlas", samples = "sample_a")
#   mg <- spatShift(mg, dx = 8000, space = "atlas", samples = "sample_b")
#
# Recording twice against the same sample concatenates steps in order.
# `samples = NULL` records against every sample already keyed in the space,
# or against the `:default:` sentinel when the space is new.
#
# Q8 replaced `+` and the sample-keyed constructor `giottoSpace("sample_a")`
# with `samples =`. The old form inherited scope from construction history:
# `.space_record()` appended to every sample keyed so far, so
# `(a + b) |> spin(30)` differed from `(a |> spin(30)) + b` with nothing at
# the call site to say which had happened.
#
# Step taxonomy — one type, `"transform"`: a deferred call to one of the
# GiottoClass spatial transform generics (`affine`, `spin`, `spatShift`,
# `flip`, `rescale`, `shear`, `zoom`). At resolution time the receiving
# gobject (or child) is spliced as the first argument and `do.call()`
# dispatches to the existing transform method.
#
# Steps are recorded individually and never folded at record time, which is
# what keeps a recipe hand-editable. Application is currently stepwise too
# -- one pass per step per subobject -- and should not be: all the ops
# except `zoom` are affine, and `affine2d` already composes them. Planned,
# with the shape and the exactness argument, in
# vignettes/articles/IMPLEMENTATION_viewspace.md section 5.
#
# A space is
#
#   list(samples = list("<sample>" = list(<step>, ...)), misc = list())
#
# Both the steps and the container are plain lists, for the reasons given at
# the top of `R/classes-view.R` (decisions Q7 and Q8). `args` is whitelisted
# to serializable types at record time so the recipe survives `saveRDS` and
# reaches a parallel worker.
#
# Storage on gobject:
#   `gobject@spaces` — named list of space recipes. Sentinel sample name
#   `:default:` is used for single-giotto entries (no explicit sample).
#
# See `R/classes-view.R` for the subset/narrowing recipe.
# See `R/methods-space.R` for the recorder and the accessors.
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


# space recipe ####

# The fields a space carries. Dropped with the S4 class: `name` (redundant
# -- the name is the key under `gobject@spaces`) and `source` (documented
# as reserved, never read).
.space_fields <- c("samples", "misc")

#' Construct a space recipe.
#'
#' The single place a space's shape is written down.
#'
#' Starts with NO sample keys. The `:default:` sentinel is added by
#' `.space_record()` only when a transform is recorded without a `samples`
#' scope -- seeding it here instead would leave an empty sentinel chain
#' beside the real keys on every per-sample space, which then reads as a
#' third participating sample and makes `.scope_space_to_sample()` fall
#' back to it for children that should have matched nothing.
#' @noRd
.new_space <- function(samples = list(), misc = list()) {
    list(samples = samples, misc = misc)
}

#' Validate a whole space, whatever produced it.
#'
#' Runs in the recorder and in `giottoSpace<-`. Unknown fields are rejected
#' for the same reason as in `.validate_view()`: recipes are hand-editable,
#' and a typo'd field would otherwise be ignored at resolve time.
#' @noRd
.validate_space <- function(space, .var.name = "space") {
    if (!is.list(space)) {
        stop("[space] `", .var.name, "` must be a list (got '",
            class(space)[[1L]], "')", call. = FALSE)
    }
    unknown <- setdiff(names(space), .space_fields)
    if (length(unknown) > 0L) {
        stop("[space] unknown field(s): ",
            paste(sprintf("`%s`", unknown), collapse = ", "),
            ". A space holds: ", paste(.space_fields, collapse = ", "),
            call. = FALSE)
    }
    checkmate::assert_list(space$samples,
        .var.name = paste0(.var.name, "$samples"))
    checkmate::assert_list(space$misc, null.ok = TRUE,
        .var.name = paste0(.var.name, "$misc"))
    if (length(space$samples) > 0L) {
        nms <- names(space$samples)
        if (is.null(nms) || any(is.na(nms)) || any(!nzchar(nms))) {
            stop("[space] `", .var.name, "$samples` must be a named list ",
                "(sample name -> step list)", call. = FALSE)
        }
        for (steps in space$samples) {
            checkmate::assert_list(steps,
                .var.name = paste0(.var.name, "$samples[[i]]"))
            lapply(steps, .validate_space_step)
        }
    }
    space
}

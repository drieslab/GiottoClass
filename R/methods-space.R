#' @include classes-space.R
#' @include classes.R
#' @include generics.R
NULL

# =============================================================================
# methods-space.R — public API for giottoSpace
#
# Strategy: the existing GiottoClass spatial transform generics (`spin`,
# `spatShift`, `affine`, `flip`, `rescale`, `shear`, `zoom`) gain a
# `giottoSpace` signature that RECORDS the call as a transform step on
# whichever sample the space is currently scoped to. Eager dispatch on
# `giotto` is unchanged.
#
# `+` combines spaces: same-sample → step concat; different-sample → keyed
# merge into a multi-sample space.
# =============================================================================


# Constructor ####

#' @title Construct an empty giottoSpace
#' @name giottoSpace-construct
#' @description Construct an empty [giottoSpace-class] for one sample. With no
#' argument, the space is sample-anonymous (single-giotto context); pass a
#' character to bind the space to a named sample for `giottoMulti` use.
#'
#' @param sample `character(1)` or missing. Sample name. Missing / NA defaults
#'   to the single-giotto sentinel `":default:"`.
#' @returns `giottoSpace`
#' @examples
#' giottoSpace()
#' giottoSpace("sample_a")
NULL

#' @rdname giottoSpace
#' @export
setGeneric("giottoSpace",
    function(gobject, name, ...) standardGeneric("giottoSpace"))

#' @rdname giottoSpace
#' @export
setGeneric("giottoSpace<-",
    function(gobject, name, ..., value) standardGeneric("giottoSpace<-"))

#' @rdname giottoSpace
#' @export
setGeneric("giottoSpaces",
    function(gobject, ...) standardGeneric("giottoSpaces"))

# Empty-constructor: dispatch through the giottoSpace generic on missing,
# missing so it coexists with the slotted accessor.
#' @rdname giottoSpace-construct
#' @export
setMethod("giottoSpace", signature(gobject = "missing", name = "missing"),
    function(gobject, name, ...) {
        new("giottoSpace",
            samples = stats::setNames(list(list()), .space_default_sample))
    }
)

# Construct a space bound to a specific sample name. `giottoSpace("sample_a")`
# positionally binds the sample name to the `gobject` slot of the generic;
# this method handles that. Also handles the equivalent named form
# `giottoSpace(name = "sample_a")` via the second method below.
#' @rdname giottoSpace-construct
#' @export
setMethod("giottoSpace", signature(gobject = "character", name = "missing"),
    function(gobject, name, ...) {
        sample <- gobject
        checkmate::assert_character(sample, len = 1L, any.missing = FALSE)
        new("giottoSpace",
            samples = stats::setNames(list(list()), sample))
    }
)

#' @rdname giottoSpace-construct
#' @export
setMethod("giottoSpace", signature(gobject = "missing", name = "character"),
    function(gobject, name, ...) {
        checkmate::assert_character(name, len = 1L, any.missing = FALSE)
        new("giottoSpace", samples = stats::setNames(list(list()), name))
    }
)


# Internal helpers ####

# Append a transform step to a giottoSpace. The step is appended to every
# sample currently keyed in the space — one sample for a freshly-constructed
# space, several if the user merged samples via `+` before piping further
# transforms.
#
# ORDER-SENSITIVE, and deliberately so: `(a + b) |> spin(30)` records the
# spin on both a and b, whereas `(a |> spin(30)) + b` records it on a only.
# Documented in vignettes/articles/view_and_space.Rmd.
.space_record <- function(space, op, args) {
    step <- .space_step_transform(op, args)
    space@samples <- lapply(space@samples, function(steps) c(steps, list(step)))
    space
}


# Indirect-usage routing: lets transform generics (spin / spatShift /
# affine / flip / rescale / shear / zoom) on a `giotto` accept
# `space = <name|giottoSpace>` and record the transform as a step.
# Returns:
#   - the gobject (with the named space slotted / appended) when `space`
#     is a character name
#   - the modified giottoSpace object when `space` is a recipe (caller
#     continues building before slotting later)
.record_space_on_gobject <- function(gobject, space, op, args) {
    if (is.character(space)) {
        checkmate::assert_character(space, len = 1L, any.missing = FALSE)
        existing <- if (space %in% giottoSpaces(gobject)) {
            giottoSpace(gobject, space)
        } else {
            giottoSpace()
        }
        new_space <- .space_record(existing, op, args)
        giottoSpace(gobject, space) <- new_space
        return(gobject)
    }
    if (inherits(space, "giottoSpace")) {
        return(.space_record(space, op, args))
    }
    stop("`space` must be NULL (eager), a character name, ",
        "or a giottoSpace object", call. = FALSE)
}


# Record methods on spatial transform generics ####

#' @rdname spin
#' @export
setMethod("spin", signature(x = "giottoSpace"), function(x, angle = 0, ...) {
    .space_record(x, "spin", list(angle = angle, ...))
})

#' @rdname spatShift
#' @export
setMethod("spatShift", signature(x = "giottoSpace"), function(x, ...) {
    .space_record(x, "spatShift", list(...))
})

#' @rdname affine
#' @export
setMethod("affine", signature(x = "giottoSpace", y = "ANY"),
    function(x, y, ...) {
        .space_record(x, "affine", c(list(y = y), list(...)))
    }
)

#' @rdname flip
#' @export
setMethod("flip", signature(x = "giottoSpace"), function(x, ...) {
    .space_record(x, "flip", list(...))
})

#' @rdname rescale
#' @export
setMethod("rescale", signature(x = "giottoSpace"), function(x, ...) {
    .space_record(x, "rescale", list(...))
})

#' @rdname shear
#' @export
setMethod("shear", signature(x = "giottoSpace"), function(x, ...) {
    .space_record(x, "shear", list(...))
})

#' @rdname zoom
#' @export
setMethod("zoom", signature(x = "giottoSpace"), function(x, ...) {
    .space_record(x, "zoom", list(...))
})


# Composition (+) ####
# Combine two giottoSpace recipes:
#   same-sample → concatenate step lists in order
#   different-sample → merge sample keys into a multi-sample space

#' @noRd
setMethod("+", signature(e1 = "giottoSpace", e2 = "giottoSpace"),
    function(e1, e2) {
        out <- e1
        for (samp in names(e2@samples)) {
            if (is.null(out@samples[[samp]])) {
                out@samples[[samp]] <- e2@samples[[samp]]
            } else {
                out@samples[[samp]] <- c(out@samples[[samp]],
                    e2@samples[[samp]])
            }
        }
        # validate to catch any accidental NA / empty key
        validObject(out)
        out
    }
)


# Accessors ####

#' @title Slotted spaces on a giotto object
#' @name giottoSpace
#' @description
#' List, retrieve, attach, or remove [giottoSpace-class] objects slotted into
#' a [giotto-class] object's `@spaces` slot.
#'
#' * `giottoSpace()` — construct an empty standalone space (sample-anonymous)
#' * `giottoSpace("sample_a")` — construct an empty space bound to a named
#'   sample (for `giottoMulti` use)
#' * `giottoSpace(g, "name")` — retrieve a slotted space by name
#' * `giottoSpace(g, "name") <- s` — slot in (or replace) a space
#' * `giottoSpace(g, "name") <- NULL` — remove a space
#' * `giottoSpaces(g)` — list slotted space names
#'
#' Multiple slotted spaces serve as named alternate coordinate frames of the
#' same gobject. Consumer functions opt into a frame via the `space =`
#' parameter.
#'
#' @param gobject a `giotto` object (or omitted for the constructor)
#' @param name `character(1)`. The slot key (or sample name for the bare
#'   constructor).
#' @param value a `giottoSpace`, or `NULL` to remove.
#' @param ... additional arguments (none currently used)
#' @returns the space, an updated gobject, or a character vector of space names
#' @examples
#' g <- giotto()
#' s <- giottoSpace() # empty
#' giottoSpace(g, "demo") <- s
#' giottoSpaces(g)
NULL

#' @rdname giottoSpace
#' @export
setMethod("giottoSpace", signature(gobject = "gAny", name = "character"),
    function(gobject, name, ...) {
        checkmate::assert_character(name, len = 1L)
        s <- gobject@spaces[[name]]
        if (is.null(s)) {
            stop("no slotted giottoSpace named '", name, "'. ",
                "Available: ", paste(giottoSpaces(gobject), collapse = ", "),
                call. = FALSE)
        }
        s
    }
)

#' @rdname giottoSpace
#' @export
setMethod("giottoSpace", signature(gobject = "gAny", name = "missing"),
    function(gobject, name, ...) {
        nm <- giottoSpaces(gobject)
        if (length(nm) == 0L) return(NULL)
        if (length(nm) == 1L) return(gobject@spaces[[nm]])
        stop("multiple spaces slotted; specify `name`. ",
            "Available: ", paste(nm, collapse = ", "), call. = FALSE)
    }
)

#' @rdname giottoSpace
#' @export
setMethod("giottoSpace<-",
    signature(gobject = "gAny", name = "character", value = "giottoSpace"),
    function(gobject, name, ..., value) {
        checkmate::assert_character(name, len = 1L)
        value@name <- name
        if (is.null(gobject@spaces)) gobject@spaces <- list()
        gobject@spaces[[name]] <- value
        gobject
    }
)

#' @rdname giottoSpace
#' @export
setMethod("giottoSpace<-",
    signature(gobject = "gAny", name = "character", value = "NULL"),
    function(gobject, name, ..., value) {
        if (is.null(gobject@spaces) ||
            !name %in% names(gobject@spaces)) return(gobject)
        gobject@spaces[[name]] <- NULL
        gobject
    }
)

#' @rdname giottoSpace
#' @export
setMethod("giottoSpaces", signature(gobject = "gAny"),
    function(gobject, ...) {
        nm <- names(gobject@spaces)
        if (is.null(nm)) character() else nm
    }
)


# Show methods ####

#' @noRd
setMethod("show", signature("giottoSpace"), function(object) {
    cat("<giottoSpace>\n")
    nm <- if (is.na(object@name)) "<ephemeral>" else object@name
    cat(sprintf("  name: %s\n", nm))
    if (length(object@samples) == 0L) {
        cat("  (empty — pipe through spatial generics like `spin()`, `affine()`)\n")
        return(invisible(NULL))
    }
    cat("  samples:\n")
    for (samp in names(object@samples)) {
        steps <- object@samples[[samp]]
        label <- if (samp == .space_default_sample) "<single>" else samp
        cat(sprintf("    %s : %d step(s)\n", label, length(steps)))
        for (s in steps) {
            cat(sprintf("      - %s\n", .space_step_label(s)))
        }
    }
})

# Scope a multi-sample space down to one child. Returns a giottoSpace whose
# @samples carries only the transforms for `sample_name`, re-keyed under
# the single-sample sentinel so the child's per-sample resolver picks them
# up naturally. Falls back to NULL if the child has no matching key — the
# resolver treats that as "no space transform for this child."
#' @keywords internal
#' @noRd
.scope_space_to_sample <- function(space, sample_name) {
    if (is.null(space)) return(NULL)
    keys <- names(space@samples)
    pick <- if (sample_name %in% keys) sample_name
        else if (.space_default_sample %in% keys) .space_default_sample
        else NULL
    if (is.null(pick)) return(NULL)
    out <- space
    out@samples <- stats::setNames(list(space@samples[[pick]]),
        .space_default_sample)
    out
}


# Compact one-line label for a transform step — used by show methods.
.space_step_label <- function(step) {
    args_str <- paste(vapply(seq_along(step$args), function(i) {
        nm <- names(step$args)[i]
        val <- tryCatch(deparse(step$args[[i]], nlines = 1L)[[1L]],
            error = function(e) "?")
        if (is.null(nm) || nm == "") val else paste0(nm, " = ", val)
    }, character(1L)), collapse = ", ")
    sprintf("%s(%s)", step$op, args_str)
}

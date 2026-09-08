# Giotto instructions ####


#' @rdname giotto_instructions
#' @param python_path path to python binary to use or directory one level
#' up from the `env` directory (similar to output of
#' `reticulate::miniconda_path()`)
#' @param show_plot print plot to console, default = TRUE
#' @param return_plot return plot as object, default = TRUE
#' @param save_plot automatically save plot, dafault = FALSE
#' @param save_dir path to directory where to save plots
#' @param plot_format format of plots (defaults to png)
#' @param dpi resolution for raster images
#' @param units units of format (defaults to in)
#' @param height height of plots
#' @param width width of  plots
#' @param is_docker using docker implementation of Giotto (defaults to FALSE)
#' @param plot_count (global option) start count for creating automatic unique
#' plots
#' @param fiji_path path to fiji executable
#' @param no_python_warn turn off warning that no compatible python env has
#' been detected
#' @export
createGiottoInstructions <- function(python_path = getOption("giotto.py_path"),
    show_plot = NULL,
    return_plot = NULL,
    save_plot = NULL,
    save_dir = NULL,
    plot_format = NULL,
    dpi = NULL,
    units = NULL,
    height = NULL,
    width = NULL,
    is_docker = FALSE,
    plot_count = 0,
    fiji_path = NULL,
    no_python_warn = FALSE) {
    # python path to use
    # try used here to allow instructions to be made in the absence of a
    # compatible python env
    python_path <- try(
        if (is_docker) {
            set_giotto_python_path(python_path = "/usr/bin/python3")
            # fixed path in docker version
        } else {
            set_giotto_python_path(python_path = python_path)
        },
        silent = TRUE
    )

    if ((is.null(python_path) || inherits(python_path, "try-error")) &&
        (!no_python_warn && !getOption("giotto.no_python_warn", FALSE))) {
        warning(wrap_txt(
            "Python is required for full Giotto functionality.
            Turn off this message by setting option
            \"giotto.no_python_warn\" = TRUE"),
            call. = FALSE
        )
        options("giotto.has_conda" = FALSE)
        options("giotto.no_python_warn" = TRUE)
    }

    # print plot to console
    if (is.null(show_plot)) {
        show_plot <- TRUE
    }

    # print plot to console
    if (is.null(return_plot)) {
        return_plot <- TRUE
    }

    # print plot to console
    if (is.null(save_plot)) {
        save_plot <- FALSE
    }

    # directory to save results to
    if (is.null(save_dir)) {
        save_dir <- getwd()
    }
    save_dir <- as.character(save_dir)

    # plot format
    if (is.null(plot_format)) {
        plot_format <- "png"
    }
    plot_format <- as.character(plot_format)

    # dpi of raster images
    if (is.null(dpi)) {
        dpi <- 300
    }
    dpi <- as.numeric(dpi)

    # units for height and width
    if (is.null(units)) {
        units <- "in"
    }
    units <- as.character(units)

    # height of plot
    if (is.null(height)) {
        height <- 9
    }
    height <- as.numeric(height)

    # width of plot
    if (is.null(width)) {
        width <- 9
    }
    width <- as.numeric(width)


    ## global options ##
    # ---------------- #

    # plot count
    options("giotto.plot_count" = plot_count)

    # fiji path
    options("giotto.fiji" = fiji_path)


    # return instructions list

    instructions_list <- create_giotto_instructions(
        python_path = python_path,
        show_plot = show_plot,
        return_plot = return_plot,
        save_plot = save_plot,
        save_dir = save_dir,
        plot_format = plot_format,
        dpi = dpi,
        units = units,
        height = height,
        width = width,
        is_docker = is_docker
    )

    return(instructions_list)
}


#' @keywords internal
create_giotto_instructions <- function(python_path = NULL,
    show_plot = NULL,
    return_plot = NULL,
    save_plot = NULL,
    save_dir = NULL,
    plot_format = NULL,
    dpi = NULL,
    units = NULL,
    height = NULL,
    width = NULL,
    is_docker = NULL) {
    instructions_list <- list(
        python_path = python_path,
        show_plot = show_plot,
        return_plot = return_plot,
        save_plot = save_plot,
        save_dir = save_dir,
        plot_format = plot_format,
        dpi = dpi,
        units = units,
        height = height,
        width = width,
        is_docker = is_docker
    )
    class(instructions_list) <- c("giottoInstructions", "list")
    return(instructions_list)
}



# instructions internals ####

# Implementation for `instructions()` / `instructions<-()`. The accessors call
# these directly so that internal access never routes through the deprecated
# exports (and so never emits their deprecation warning).

# Mirrors the shape of `instructions()` itself: accepts either a `giotto`
# object or a `giottoInstructions` list, and returns the whole set of
# instructions when no `param` is named, one value when one is.
#' @keywords internal
#' @noRd
.instr_read <- function(giotto_instructions,
    param = NULL,
    default) {
    # get instructions if provided the giotto object
    if (inherits(giotto_instructions, "giotto")) {
        giotto_instructions <- giotto_instructions@instructions
    }

    if (is.null(param)) {
        return(giotto_instructions)
    }

    # stop if parameter is not found
    if (!param %in% names(giotto_instructions)) {
        if (!missing(default)) {
            return(default)
        }
        stop("\t parameter ", param, " is not in Giotto instructions \t")
    }
    giotto_instructions[[param]]
}


#' @keywords internal
#' @noRd
.instr_change <- function(gobject,
    params = NULL,
    new_values = NULL,
    return_gobject = TRUE,
    init_gobject = TRUE) {
    instrs <- gobject@instructions

    if (is.null(params) | is.null(new_values)) {
        stop("\t params and new_values can not be NULL \t")
    }

    if (length(params) != length(new_values)) {
        stop("\t length of params need to be the same as new values \t")
    }

    ## swap with new values
    instrs[params] <- new_values

    ## make sure that classes remain consistent
    new_instrs <- lapply(seq_len(length(instrs)), function(x) {
        if (names(instrs[x]) %in% c("dpi", "height", "width")) {
            instrs[[x]] <- as.numeric(instrs[[x]])
        } else if (names(instrs[x]) %in%
            c("show_plot", "return_plot", "save_plot", "is_docker")) {
            instrs[[x]] <- as.logical(instrs[[x]])
        } else if (names(instrs[x]) %in%
            c(
                "active_spat_unit", "active_feat_type", "plot_format",
                "units"
            )) {
            instrs[[x]] <- as.character(instrs[[x]])
        } else {
            instrs[[x]] <- instrs[[x]]
        }
    })

    names(new_instrs) <- names(instrs)
    class(new_instrs) <- "giottoInstructions"


    if (isTRUE(return_gobject)) {
        gobject@instructions <- new_instrs
        if (isTRUE(init_gobject)) gobject <- initialize(gobject)
        return(gobject)
    } else {
        return(new_instrs)
    }
}



#' @keywords internal
#' @noRd
.instr_replace <- function(gobject,
    instructions = NULL,
    init_gobject = TRUE) {
    instrs_needed <- names(create_giotto_instructions())

    # validate new instructions
    if (!all(instrs_needed %in% names(instructions)) | is.null(instructions)) {
        stop(wrap_txt("You need to provide a named list for all instructions,",
            "like the outcome of createGiottoInstructions",
            errWidth = TRUE
        ))
    } else {
        gobject@instructions <- instructions
        if (isTRUE(init_gobject)) gobject <- initialize(gobject)
        return(gobject)
    }
}


# deprecated ####

# Thin aliases over the internals above, kept only so that direct callers get
# a warning pointing at `instructions()` / `instructions<-()`. They stay
# exported because downstream suite packages still call them; de-exporting is
# a separate breaking change. Nothing inside GiottoClass calls these — the
# accessors use the `.instr_*` internals, so the warning fires for user code
# only and cannot accumulate across internal access.

#' @title Read a giotto instruction
#' @description Deprecated. Use [instructions()] instead.
#' @param giotto_instructions giotto object or a `giottoInstructions` list
#' @param param parameter to retrieve
#' @param default value to return when `param` is absent. When missing, an
#' absent `param` is an error.
#' @returns the value of the requested instruction param
#' @keywords internal
#' @export
readGiottoInstructions <- function(giotto_instructions,
    param = NULL,
    default) {
    deprecate_soft(
        when = "0.3.5",
        what = "readGiottoInstructions()",
        with = "instructions()"
    )

    # unlike `instructions()`, this one has always required a param
    if (is.null(param)) {
        stop("\t readGiottoInstructions needs a parameter to work \t")
    }

    if (missing(default)) {
        .instr_read(giotto_instructions = giotto_instructions, param = param)
    } else {
        .instr_read(
            giotto_instructions = giotto_instructions,
            param = param,
            default = default
        )
    }
}


#' @title Show giotto instructions
#' @description Deprecated. Use [instructions()] instead.
#' @param gobject giotto object
#' @returns named list of giotto instructions
#' @keywords internal
#' @export
showGiottoInstructions <- function(gobject) {
    deprecate_soft(
        when = "0.3.5",
        what = "showGiottoInstructions()",
        with = "instructions()"
    )

    .instr_read(gobject)
}


#' @title Change giotto instructions
#' @description Deprecated. Use `instructions(gobject, param) <- value`
#' instead.
#' @param gobject giotto object
#' @param params parameter(s) to change
#' @param new_values new value(s) for `params`
#' @param return_gobject logical. Return the giotto object (default `TRUE`)
#' rather than the instructions list alone.
#' @param init_gobject logical. Re-initialize the object when returning it
#' (default `TRUE`)
#' @returns giotto object with changed instructions, or the instructions
#' list when `return_gobject = FALSE`
#' @keywords internal
#' @export
changeGiottoInstructions <- function(gobject,
    params = NULL,
    new_values = NULL,
    return_gobject = TRUE,
    init_gobject = TRUE) {
    deprecate_soft(
        when = "0.3.5",
        what = "changeGiottoInstructions()",
        with = "`instructions<-`()"
    )

    .instr_change(
        gobject = gobject,
        params = params,
        new_values = new_values,
        return_gobject = return_gobject,
        init_gobject = init_gobject
    )
}


#' @title Replace giotto instructions
#' @description Deprecated. Use `instructions(gobject) <- value` instead.
#' @param gobject giotto object
#' @param instructions named list of all instructions, as produced by
#' [createGiottoInstructions()]
#' @param init_gobject logical. Re-initialize the object before returning it
#' (default `TRUE`)
#' @returns giotto object with replaced instructions
#' @keywords internal
#' @export
replaceGiottoInstructions <- function(gobject,
    instructions = NULL,
    init_gobject = TRUE) {
    deprecate_soft(
        when = "0.3.5",
        what = "replaceGiottoInstructions()",
        with = "`instructions<-`()"
    )

    .instr_replace(
        gobject = gobject,
        instructions = instructions,
        init_gobject = init_gobject
    )
}


# internals ####

#' @export
print.giottoInstructions <- function(x, ...) {
    cat(sprintf("<%s>\n", class(x)[1]))
    print_list(x)
}

# Giotto instructions ####


#' @title Create instructions for giotto functions
#' @name createGiottoInstructions
#' @description Function to set global instructions for giotto functions
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
#' @returns named vector with giotto instructions
#' @seealso More online information can be found
#' here \url{http://giottosuite.com}
#' @examples
#' createGiottoInstructions()
#' @export
createGiottoInstructions <- function(
    python_path = getOption("giotto.py_path"),
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

    if ((is.null(python_path) || inherits(python_path, "try-error")) &
        !no_python_warn) {
        warning(wrap_txt("Python is required for full Giotto functionality."),
                call. = FALSE)
        options("giotto.has_conda" = FALSE)
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


#' @title Read giotto instructions associated with giotto object
#' @name readGiottoInstructions
#' @description Retrieves the instruction associated with the provided parameter
#' @param giotto_instructions giotto object or result from
#' createGiottoInstructions()
#' @param param parameter to retrieve
#' @param default default object to return if parameter to retrieve does not
#' exist
#' @returns specific parameter
#' @examples
#' readGiottoInstructions(
#'     giotto_instructions = createGiottoInstructions(),
#'     param = "show_plot"
#' )
#' @export
readGiottoInstructions <- function(giotto_instructions,
    param = NULL,
    default) {
    # get instructions if provided the giotto object
    if (inherits(giotto_instructions, "giotto")) {
        giotto_instructions <- giotto_instructions@instructions
    }

    # stop if parameter is not found
    if (is.null(param)) {
        stop("\t readGiottoInstructions needs a parameter to work \t")
    } else if (!param %in% names(giotto_instructions)) {
        if (!missing(default)) {
            return(default)
        }
        stop("\t parameter ", param, " is not in Giotto instructions \t")
    } else {
        specific_instruction <- giotto_instructions[[param]]
    }
    return(specific_instruction)
}


#' @title Show giotto instructions associated with giotto object
#' @name showGiottoInstructions
#' @description Function to display all instructions from giotto object
#' @param gobject giotto object
#' @returns named vector with giotto instructions
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' showGiottoInstructions(g)
#' @export
showGiottoInstructions <- function(gobject) {
    instrs <- gobject@instructions
    return(instrs)
}


#' @title Change giotto instruction(s) associated with giotto object
#' @name changeGiottoInstructions
#' @description Function to change one or more instructions from giotto object.
#' If more than one item is supplied to \code{params} and \code{new_values}, use
#' a vector of values. Does not call \code{initialize} on the giotto object
#' @param gobject giotto object
#' @param params parameter(s) to change
#' @param new_values new value(s) for parameter(s)
#' @param return_gobject (boolean, default = TRUE) return giotto object
#' @param init_gobject (boolean, default = TRUE) initialize gobject if returning
#' @returns giotto object with one or more changed instructions
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' changeGiottoInstructions(
#'     gobject = g, params = "save_plot",
#'     new_values = TRUE
#' )
#' @export
changeGiottoInstructions <- function(gobject,
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

    # if(!all(params %in% names(instrs))) {
    #   stop('\t all params need to be part of Giotto instructions \t')
    # }

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



    if (isTRUE(return_gobject)) {
        gobject@instructions <- new_instrs
        if (isTRUE(init_gobject)) gobject <- initialize(gobject)
        return(gobject)
    } else {
        return(new_instrs)
    }
}



#' @title Replace all giotto instructions in giotto object
#' @name replaceGiottoInstructions
#' @description Function to replace all instructions from giotto object. Does
#' not call \code{initialize} on the giotto object
#' @param gobject giotto object
#' @param instructions new
#' instructions (e.g. result from createGiottoInstructions)
#' @param init_gobject (boolean, default = TRUE) initialize gobject when
#' returning
#' @returns giotto object with replaces instructions
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' replaceGiottoInstructions(
#'     gobject = g,
#'     instructions = createGiottoInstructions()
#' )
#' @export
replaceGiottoInstructions <- function(gobject,
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

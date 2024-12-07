#### logging of giotto functions ####


# Functions to find the previous function name or args have been moved to
# GiottoUtils prev_call.R


#' @title Update giotto parameters
#' @name update_giotto_params
#' @description
#' For developer use. Adds an entry to the `giotto` object object history.
#' Care currently needs to be taken when a function that contains a call to
#' this function is called from within yet another function. In such cases,
#' a `toplevel < 0` or setting a temporary `"giotto.update_param" = FALSE` with
#' `GiottoUtils::gwith_option()` may be the best option to avoid either
#' evaluation errors or strange history entries. A new `update_giotto_params()`
#' call can then be added that describes the function of the topmost function
#' if desired.
#' @param gobject giotto object
#' @param description description of function run
#' @param return_gobject logical. Whether the giotto object should be returned
#' @param toplevel expected relative stackframe where call that is being
#' recorded was made. If negative, param recording is skipped
#' @returns giotto object or list of parameters
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' update_giotto_params(g, toplevel = 1)
#' @export
update_giotto_params <- function(gobject,
    description = "_test",
    return_gobject = TRUE,
    toplevel = 2) {
    if (toplevel < 0 || !getOption("giotto.update_param", TRUE)) {
        return(gobject)
    } # skip if toplevel negative

    parameters_list <- gobject@parameters
    number_of_rounds <- length(parameters_list)
    update_name <- paste0(number_of_rounds, description)

    parameters_list[[update_name]] <- get_args(toplevel = toplevel)

    if (return_gobject == TRUE) {
        gobject@parameters <- parameters_list
        return(gobject)
    } else {
        return(list(plist = parameters_list, newname = update_name))
    }
}



#' @title Giotto object history
#' @name objHistory
#' @description Print and return giotto object history
#' @param object giotto object
#' @param summarized logical. whether print should be summarized
#' @returns list
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' objHistory(g)
#' objHistory(g, summarized = TRUE)
#' @export
objHistory <- function(object, summarized = FALSE) {
    p <- object@parameters

    if (summarized) {
        message("Processing steps:")
        for (step in names(p)) {
            message(step)
            sub_step <- p[[step]]
            if (any(grepl("name", names(sub_step)) == TRUE)) {
                selected_names <- grep("name", names(sub_step), value = TRUE)
                wrap_msg("\t name info: ", sub_step[selected_names])
            }
        }
    } else {
        message("Steps and parameters used:")
        for (i in seq_along(p)) {
            cat(GiottoUtils::color_blue(sprintf("<%s>\n", names(p)[[i]])))
            GiottoUtils::print_list(p[[i]], pre = "  ")
        }
    }
    invisible(x = object@parameters)
}



#' @title showProcessingSteps
#' @name showProcessingSteps
#' @description shows the sequential processing steps that were performed
#' on a Giotto object in a summarized format
#' @param gobject giotto object
#' @returns list of processing steps and names
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' showProcessingSteps(g)
#' @export
showProcessingSteps <- function(gobject) {
    deprecate_warn(
        when = "0.4.0",
        what = "showProcessingSteps()",
        with = "objHistory()",
        details = "objHistory with arg `summarized = TRUE` replaces this functionality"
    )

    parameters <- gobject@parameters

    message("Processing steps:")

    for (step in names(parameters)) {
        message(step)

        sub_step <- parameters[[step]]

        if (any(grepl("name", names(sub_step)) == TRUE)) {
            selected_names <- grep("name", names(sub_step), value = TRUE)
            wrap_msg("\t name info: ", sub_step[selected_names])
        }
    }
}

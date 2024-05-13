#### logging of giotto functions ####


# Functions to find the previous function name or args have been moved to
# GiottoUtils prev_call.R


#' @title Update giotto parameters
#' @name update_giotto_params
#' @param gobject giotto object
#' @param description description of function run
#' @param return_gobject logical. Whether the giotto object should be returned
#' @param toplevel expected relative stackframe where call that is being
#' recorded was made
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
#' @returns list
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' objHistory(g)
#' @export
objHistory <- function(object) {
    message("Steps and parameters used:")
    message(object@parameters)
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

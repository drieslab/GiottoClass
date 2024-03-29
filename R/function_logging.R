#### logging of giotto functions ####


# Functions to find the previous function name or args have been moved to
# GiottoUtils prev_call.R


#' @title Update giotto parameters
#' @name update_giotto_params
#' @param gobject giotto object
#' @param description description of function run
#' @param return_gobject logical. Whether the giotto object should be returned
#' @param toplevel expected relative stackframe where call that is being recorded
#' was made
#' @export
update_giotto_params <- function(
        gobject,
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
#' @export
objHistory <- function(object) {
    cat("Steps and parameters used: \n \n")
    print(object@parameters)
    cat("\n\n")
    invisible(x = object@parameters)
}



#' @title showProcessingSteps
#' @name showProcessingSteps
#' @description shows the sequential processing steps that were performed
#' on a Giotto object in a summarized format
#' @param gobject giotto object
#' @return list of processing steps and names
#' @export
showProcessingSteps <- function(gobject) {
    parameters <- gobject@parameters

    cat("Processing steps: \n \n")

    for (step in names(parameters)) {
        cat("\n", step, "\n")

        sub_step <- parameters[[step]]

        if (any(grepl("name", names(sub_step)) == TRUE)) {
            selected_names <- grep("name", names(sub_step), value = T)
            cat("\t name info: ", sub_step[selected_names], "\n")
        }
    }
}

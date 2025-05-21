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
#' @param attachments named list. Items to attach. These are intended for lightweight
#' param classes containing settings. No large items should be added here.
#' @returns giotto object or list of parameters
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' update_giotto_params(g, toplevel = 1)
#' @export
update_giotto_params <- function(
        gobject,
        description = "_test",
        return_gobject = TRUE,
        toplevel = 2,
        attachments = NULL) {
    checkmate::assert_list(attachments, null.ok = TRUE)
    parameters_list <- gobject@parameters
    number_of_rounds <- length(parameters_list)
    update_name <- paste0(number_of_rounds, description)

    # return before updating if toplevel negative or global setting is FALSE
    if (skip_update <- toplevel < 0 ||
        !getOption("giotto.update_param", TRUE)) {
        if (isTRUE(return_gobject)) {
            return(gobject)
        } else {
            return(list(
                plist = parameters_list,
                newname = names(tail(parameters_list, 1L))
            ))
        }
    }

    # `get_args()` can be problematic. Allow skip right before this step.

    # update parameters list
    new_entry <- get_args(toplevel = toplevel)
    class(new_entry) <- c("ghistory_item", "character")
    if (!is.null(attachments)) {
        attr(new_entry, "attachments") <- attachments
    }

    parameters_list[[update_name]] <- new_entry
    class(parameters_list) <- "ghistory"

    if (isTRUE(return_gobject)) {
        gobject@parameters <- parameters_list
        return(gobject)
    } else {
        return(list(plist = parameters_list, newname = update_name))
    }
}

#' @export
#' @keywords internal
print.ghistory <- function(x) {
    message("Steps and parameters used:")
    for (i in seq_along(x)) {
        cat(GiottoUtils::color_blue(sprintf("<%s>\n", names(x)[[i]])))
        print(x[[i]])
    }
}

#' @export
#' @keywords internal
print.ghistory_item <- function(x) {
    GiottoUtils::print_list(x, pre = "  ")
    atts <- attr(x, "attachments")
    if (!is.null(atts)) {
        for (a in names(atts)) {
            cat(" ", GiottoUtils::color_yellow(sprintf("<%s> :\n", a)))
            cat(str_reformat(atts[[a]], indent = 4))
        }
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
    }
    object@parameters
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

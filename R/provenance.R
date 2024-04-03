#### provenance ####

#' @title Check provenance info matches across list of S4 subobjects
#' @name .prov_match
#' @param ... list of s4 subobjects to match provenance
#' @param verbose print discovered provenance info
#' @returns logical or NULL
#' @keywords internal
.prov_match <- function(..., verbose = FALSE) {
    if (length(match.call()) == 2) {
        if (isS4(...)) stop("more than one object must be provided")
        s4_list <- as.list(...)
    } else if (length(match.call()) > 2) {
        s4_list <- list(...)
    }

    if (!all(unlist(lapply(s4_list, inherits, "provData")))) {
        stop("Not all items to test contain provenance information\n")
    }

    prov_list <- lapply(s4_list, prov)

    if (isTRUE(verbose)) print(unlist(prov_list))

    length(unique(prov_list)) == 1
}

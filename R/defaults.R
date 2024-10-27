# functions for setting default behavior


#' @title set_default_spat_unit
#' @name set_default_spat_unit
#' @description
#' Function to guess a default spatial unit. Also see [activeSpatUnit()] in
#' methods-instructions.R for a way to manually assign this default
#' @inheritParams data_access_params
#' @returns character
#' @keywords internal
#' @examples
#' g <- createGiottoObject()
#'
#' set_default_spat_unit(gobject = g, spat_unit = "cell")
#' @export
set_default_spat_unit <- function(gobject,
    spat_unit = NULL) {
    # If a spatial unit is provided, use it directly
    if (!is.null(spat_unit)) {
        if (!inherits(spat_unit, "character")) {
            stop("spat_unit input must be character")
        }
        return(spat_unit)
    }

    # set a default
    spat_unit <- try(instructions(gobject, "active_spat_unit"), silent = TRUE)

    if (inherits(spat_unit, "try-error")) {
        if (!is.null(gobject@expression) & length(gobject@expression) > 0L) {
            spat_unit <- names(gobject@expression)[[1L]]
        } else if (!is.null(gobject@spatial_info)) {
            spat_unit <- names(gobject@spatial_info)[[1L]]
        } else {
            warning("No default for spat_unit could be set \n")
            spat_unit <- NULL
        }
    }

    return(spat_unit)
}



#' @title set_default_feat_type
#' @name set_default_feat_type
#' @description
#' Function to guess a default feature type. Also see [activeFeatType()] in
#' methods-instructions.R for a way to manually assign this default
#' @inheritParams data_access_params
#' @returns character
#' @keywords internal
#' @examples
#' g <- createGiottoObject()
#'
#' set_default_feat_type(gobject = g, spat_unit = "cell", feat_type = "rna")
#' @export
set_default_feat_type <- function(gobject,
    feat_type = NULL,
    spat_unit) {
    # if a feature type is provided, use it directly
    if (!is.null(feat_type)) {
        if (!inherits(feat_type, "character")) {
            stop("feat_type input must be character")
        }
        return(feat_type)
    }

    # set a default
    feat_type <- try(instructions(gobject, "active_feat_type"), silent = TRUE)

    if (inherits(feat_type, "try-error")) {
        if (!is.null(gobject@expression) & length(gobject@expression) > 0L) {
            feat_type <- names(gobject@expression[[spat_unit]])[[1L]]
            if (is.null(feat_type)) {
                warning(wrap_txt("No existing feat_types to default to in given
                                spat_unit"))
            }
        } else if (!is.null(gobject@feat_info)) {
            feat_type <- names(gobject@feat_info)[[1L]]
        } else {
            warning("No default for feat_type could be set \n")
            feat_type <- NULL
        }
    }

    return(feat_type)
}

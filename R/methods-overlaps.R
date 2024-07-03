#' @include generics.R
NULL

# docs ----------------------------------------------------------- #
#' @title overlaps-generic
#' @name overlaps-generic
#' @description Access list of overlaps information from object
#' @param x object
#' @aliases overlaps
#' @returns list of overlaps from object
#' @examples
#' g <- GiottoData::loadSubObjectMini("giottoPolygon")
#'
#' overlaps(g)
NULL
# ---------------------------------------------------------------- #


#' @describeIn overlaps-generic Get overlaps information from giottoPolygon
#' @param name (optional) name of overlaps information to retrieve
#' @export
setMethod(
    "overlaps", signature(x = "giottoPolygon"),
    function(x, name = NULL) {
        if (is.null(name)) {
            # return entire list
            return(x@overlaps)
        } else {
            # return named entry
            return(x@overlaps[[name]])
        }
    }
)

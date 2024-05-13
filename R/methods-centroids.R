#' @importMethodsFrom terra centroids
NULL

# docs ----------------------------------------------------------- #
#' @title centroids-generic
#' @name centroids-generic
#' @description Access centroids information from polygon objects
#' @param x object
#' @aliases centroids
#' @details For giottoPolygon, if centroids already exist, pulls from
#' \code{spatVectorCentroids} slot. Otherwise, generates from
#' \code{spatVector} slot de novo
#' @returns spatVectorCentroids or spatVector
#' @examples
#' g <- GiottoData::loadSubObjectMini("giottoPolygon")
#'
#' centroids(g)
NULL
# ---------------------------------------------------------------- #

#' @rdname centroids-generic
#' @param append_gpolygon whether to append the centroids results to the
#' `giottoPolygon` instead of returning bare `SpatVector`. Defaults to FALSE
#' @export
setMethod(
    "centroids", signature(x = "giottoPolygon"),
    function(x, append_gpolygon = FALSE) {
        if (!is.null(x@spatVectorCentroids)) { # centroids exist in gpoly
            if (isTRUE(append_gpolygon)) {
                return(x)
            } else {
                return(x@spatVectorCentroids)
            }
        } else { # centroids do not exist in gpoly
            ctrds <- terra::centroids(x@spatVector)
            if (isTRUE(append_gpolygon)) {
                x@spatVectorCentroids <- ctrds
                return(x)
            } else {
                return(ctrds)
            }
        }
    }
)

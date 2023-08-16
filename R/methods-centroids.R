
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
NULL
# ---------------------------------------------------------------- #

#' @rdname centroids-generic
#' @export
setMethod('centroids', signature(x = 'giottoPolygon'),
          function(x) {
            if(!is.null(x@spatVectorCentroids)) {
              return(x@spatVectorCentroids)
            } else {
              return(terra::centroids(x@spatVector))
            }
          })

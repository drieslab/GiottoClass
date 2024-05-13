#' @include generics.R
NULL

# docs ----------------------------------------------------------- #
#' @title Dimensions of giotto objects
#' @name dims-generic
#' @description Find the dimensions of an object
#' @param x object to check dimensions of
#' @returns numeric
#' @examples
#' g <- GiottoData::loadSubObjectMini("giottoPoints")
#'
#' nrow(g)
NULL
# ---------------------------------------------------------------- #




# nrow ####

#' @describeIn dims-generic Find rows of giottoPoints object
#' @export
setMethod(
    "nrow", signature("giottoPoints"),
    function(x) terra::nrow(x@spatVector)
)

#' @describeIn dims-generic Find rows of giottoPolygon object
#' @export
setMethod(
    "nrow", signature("giottoPolygon"),
    function(x) terra::nrow(x@spatVector)
)

#' @describeIn dims-generic Find rows of giotto S4s with data.table
#' based \code{coordinates} slots
#' @export
setMethod("nrow", signature("spatLocsObj"), function(x) nrow(x@coordinates))


# TODO
# setMethod('dims', signature('coordDataMT'), function(x) nrow(x@coordinates))

#' @describeIn dims-generic Find rows of giotto S4s with data.table
#' based \code{coordinates} slots
#' @export
setMethod("nrow", signature("exprData"), function(x) nrow(x@exprMat))

#' @describeIn dims-generic Find rows of giotto S4s with data.table
#' based \code{coordinates} slots
#' @export
setMethod("nrow", signature("metaData"), function(x) nrow(x@metaDT))

#' @describeIn dims-generic Find rows of spatialNetworkObj
#' @export
setMethod(
    "nrow", signature("spatialNetworkObj"),
    function(x) nrow(x@networkDT)
)

#' @rdname dims-generic
#' @export
setMethod("nrow", signature("enrData"), function(x) nrow(x@enrichDT))

#' @rdname dims-generic
#' @export
setMethod("nrow", signature("dimObj"), function(x) nrow(x@coordinates))



# ncol ####

# setMethod('nrow', signature = 'giotto', function(x) {
#   avail_exp = list_expression(x)
#   get_expression_values(x,
#                         spat_unit = avail_exp$spat_unit[1],
#                         feat_type = avail_exp$feat_type[1],
#                         values = avail_exp$name[1]) %>%
#     nrow()
# })


#' @describeIn dims-generic Find cols of giotto S4s with Matrix
#' based \code{exprMat} slots
#' @export
setMethod("ncol", signature("exprData"), function(x) ncol(x@exprMat))

#' @describeIn dims-generic Find cols of giotto S4s with data.table
#' based \code{metaDT} slots
#' @export
setMethod("ncol", signature("metaData"), function(x) ncol(x@metaDT))

#' @describeIn dims-generic Find cols of giotto S4s with enrData
#' @export
setMethod("ncol", signature("enrData"), function(x) ncol(x@enrichDT))

#' @rdname dims-generic
#' @export
setMethod("ncol", signature("dimObj"), function(x) ncol(x@coordinates))





## dim() generic ####

#' @describeIn dims-generic Find dimensions of giotto S4s with Matrix
#' based \code{exprMat} slots
#' @export
setMethod("dim", signature("exprData"), function(x) dim(x@exprMat))

#' @describeIn dims-generic Find dimensions of giotto S4s with data.table
#' based \code{metaDT} slots
#' @export
setMethod("dim", signature("metaData"), function(x) dim(x@metaDT))

#' @rdname dims-generic
#' @export
setMethod("dim", signature("enrData"), function(x) dim(x@enrichDT))

#' @rdname dims-generic
#' @export
setMethod(
    "dim", signature("giottoLargeImage"),
    function(x) dim(x@raster_object)
)

#' @rdname dims-generic
#' @export
setMethod("dim", signature("giottoPolygon"), function(x) dim(x[]))

#' @rdname dims-generic
#' @export
setMethod("dim", signature("giottoPoints"), function(x) dim(x[]))

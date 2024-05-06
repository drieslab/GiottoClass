#' @include generics.R
NULL

#' @title Copy an entire object
#' @name copy
#' @description S4 generic for Giotto's S4 subobjects to return with full
#' copies of certain subobjects that usually return referenced information.
#' @param x a Giotto S4 class subobject
#' @seealso \code{\link[data.table]{copy}} \code{\link[terra]{deepcopy}}
#' @aliases copy
#' @returns giotto subobjects
#' @examples
#' g <- GiottoData::loadSubObjectMini("exprObj")
#'
#' copy(g)
NULL

#' @describeIn copy Copy \emph{data.table}-based spatial locations object.
#' @export
setMethod("copy", signature(x = "coordDataDT"), function(x) {
    x@coordinates <- data.table::copy(x@coordinates)
    x
})

#' @include generics.R
NULL

# NOTE:
# dimnames MUST be provided for rownames and colnames methods to be well
# behaved

#' @title Row and column names
#' @name row-plus-colnames-generic
#' @aliases colnames rownames
#' @description Retrieve or set the row or column names of an object
#' @param x object
#' @return A character vector of row or col names
#' @keywords internal
#' @examples
#' g <- GiottoData::loadSubObjectMini("exprObj")
#'
#' colnames(g)
NULL

#' @title Dimnames of an object
#' @name dimnames
#' @description
#' Retrieve or set the dimnames of an object
#' @param x object
#' @returns character
#' @keywords internal
#' @examples
#' g <- GiottoData::loadSubObjectMini("exprObj")
#'
#' dimnames(g)
NULL

#' @title Names of objects
#' @name names
#' @description
#' Get or set the names of the layers of a `giottoLargeImage`
#' @param x object
#' @param value character. Name(s) to set in the object
#' @returns character or the object being renamed if the replacement function
#' is used.
#' @keywords internal
#' @examples
#' img <- GiottoData::loadSubObjectMini("giottoLargeImage")
#' names(img)
#' names(img) <- "dapi"
NULL

#' @rdname row-plus-colnames-generic
#' @export
setMethod("colnames", signature("giotto"), function(x) x$cell_ID)

#' @rdname row-plus-colnames-generic
#' @export
setMethod("colnames", signature(x = "exprObj"), function(x) colnames(x[]))

#' @rdname row-plus-colnames-generic
#' @export
setMethod("colnames", signature(x = "cellMetaObj"), function(x) colnames(x[]))

#' @rdname row-plus-colnames-generic
#' @export
setMethod("colnames", signature(x = "featMetaObj"), function(x) colnames(x[]))

#' @rdname row-plus-colnames-generic
#' @export
setMethod("colnames", signature(x = "spatEnrObj"), function(x) colnames(x[]))

#' @rdname row-plus-colnames-generic
#' @export
setMethod("colnames", signature(x = "spatLocsObj"), function(x) colnames(x[]))

#' @rdname row-plus-colnames-generic
#' @export
setMethod("colnames", signature(x = "dimObj"), function(x) colnames(x[]))



#' @rdname row-plus-colnames-generic
#' @export
setMethod("rownames", signature("giotto"), function(x) {
    fDataDT(x)$feat_ID
})

#' @rdname row-plus-colnames-generic
#' @export
setMethod("rownames", signature(x = "exprObj"), function(x) rownames(x[]))

#' @rdname row-plus-colnames-generic
#' @export
setMethod("rownames", signature(x = "dimObj"), function(x) rownames(x[]))

#' @rdname row-plus-colnames-generic
#' @export
setMethod("rownames", signature(x = "metaData"), function(x) rownames(x[]))



#' @rdname dimnames
#' @export
setMethod("dimnames", signature("giotto"), function(x) {
    list(rownames(x), colnames(x))
})

#' @rdname dimnames
#' @export
setMethod("dimnames", signature(x = "exprObj"), function(x) dimnames(x[]))

#' @rdname dimnames
#' @export
setMethod("dimnames", signature(x = "dimObj"), function(x) dimnames(x[]))

#' @rdname dimnames
#' @export
setMethod("dimnames", signature(x = "spatLocsObj"), function(x) dimnames(x[]))

#' @rdname dimnames
#' @export
setMethod("dimnames", signature(x = "metaData"), function(x) dimnames(x[]))

#' @rdname dimnames
#' @export
setMethod("dimnames", signature(x = "enrData"), function(x) dimnames(x[]))

#' @rdname dimnames
#' @export
setMethod("dimnames", signature(x = "dimObj"), function(x) dimnames(x[]))


#' @rdname names
setMethod("names", signature(x = "giottoLargeImage"), function(x) names(x[]))
#' @rdname names
setMethod("names<-", signature(x = "giottoLargeImage"), function(x, value) {
    names(x[]) <- value
    x
})
#' @rdname names
setMethod("names", signature("processParam"), function(x) names(x@param))
#' @rdname names
setMethod("names<-", signature("processParam"), function(x, value) {
    names(x@param) <- value
    x
})
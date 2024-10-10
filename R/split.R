
#' @name splitGiotto
#' @title Split a Giotto Object
#' @description
#' Split a Giotto object based on a cell metadata column into a list of multiple 
#' Giotto objects.
#' @param gobject giotto object to split
#' @param by cell metadata column by which to split the object
#' @param spat_unit character. Controls which spatial unit to pull splitting 
#' information from. However, all spatial units will always be affected by the 
#' split.
#' @param feat_type character. Split affects these feature type(s). Default is 
#' "rna"
#' @export
#' @returns `list` of `giotto` objects
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' splitGiotto(g, "leiden_clus")
#' 
splitGiotto <- function(gobject, by, spat_unit = NULL, feat_type = NULL) {
    split_factor <- unique(spatValues(gobject, feats = by, verbose = FALSE)[[by]])
    
    # define names for numeric factors
    split_factor_names <- if (is.numeric(split_factor)) {
        paste(by, split_factor, sep = "_")
    } else {
        split_factor_names <- split_factor
    }
    
    names(split_factor) <- split_factor_names
    lapply(split_factor, function(clus) {
        split_call <- call("==", as.name(by), clus)
        subset(gobject, subset = split_call, quote = FALSE)
    })
}

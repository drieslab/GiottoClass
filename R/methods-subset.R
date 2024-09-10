
setMethod(
    "[", signature(x = "giotto", i = "gIndex", j = "missing", drop = "missing"),
    function(x, i, ..., drop) {
        x[i, j = NULL]
    }
)

setMethod(
    "[", signature(x = "giotto", i = "missing", j = "gIndex", drop = "missing"),
    function(x, j, ..., drop) {
        x[i = NULL, j]
    })

setMethod(
    "[", signature(x = "giotto", i = "gIndex", j = "gIndex", drop = "missing"),
    function(x, i, j, spat_unit = NULL, feat_type = NULL, ..., drop) {
        
        x <- .slice_giotto(x, spat_unit = spat_unit, feat_type = feat_type)

        if (is.numeric(i)) {
            fx <- fDataDT(x,
                spat_unit = spat_unit,
                feat_type = feat_type
            )
            i <- fx[i, feat_ID]
        }

        if (is.numeric(j)) {
            cx <- pDataDT(x,
                spat_unit = spat_unit,
                feat_type = feat_type
            )
            j <- cx[j, cell_ID]
        }

        subsetGiotto(x,
            spat_unit = spat_unit,
            feat_type = feat_type,
            feat_ids = i,
            cell_ids = j
        )
    })


setMethod("subset", signature("giotto"), function(
        x, features, cells, spat_unit = NULL, feat_type = NULL, ...
) {
    
    sub_f <- substitute(features)
    sub_c <- substitute(cells)
    x <- .slice_giotto(x, spat_unit = spat_unit, feat_type = feat_type)
    
    # features
    fids <- NULL
    if (!missing(sub_f)) {
        fx <- fDataDT(x,
            spat_unit = spat_unit,
            feat_type = feat_type
        )
        fids <- subset(fx, subset = eval(sub_f))$feat_ID
    }

    # cells
    sids <- NULL
    if (!missing(sub_c)) {
        cx <- pDataDT(x,
            spat_unit = spat_unit,
            feat_type = feat_type
        )
        sids <- subset(cx, subset = eval(sub_c))$cell_ID
    }
    
    subsetGiotto(x,
        spat_unit = spat_unit,
        feat_type = feat_type,
        feat_ids = fids,
        cell_ids = sids
    )
})





.slice_giotto <- function(
        x, spat_unit = ":all:", feat_type = ":all:", verbose = FALSE
) {
    
    spat_unit <- spat_unit %null% ":all:"
    feat_type <- feat_type %null% ":all:"
    
    if (identical(spat_unit, ":all:") && identical(feat_type, ":all:")) {
        return(x) # return early if no slicing needed
    }
    
    # data lists
    spat_only <- .giotto_datalist(x, 
        c("spatial_info", "spatial_locs", "spatial_network")
    )
    
    feat_only <- .giotto_datalist(x, c("feat_info"))
    
    spat_feat <- .giotto_datalist(x,
        c(
            "expression", "cell_metadata", "feat_metadata",
            "spatial_enrichment", 
            "nn_network", "dimension_reduction",
            "multiomics"
        )
    )
    
    # select data
    if (!identical(spat_unit, ":all:")) { # select if not all
        spat_only <- spat_only[spatUnit(spat_only) %in% spat_unit]
        spat_feat <- spat_feat[spatUnit(spat_feat) %in% spat_unit]
    }
    
    if (!identical(feat_type, ":all:")) {
        feat_only <- feat_only[featType(feat_only) %in% feat_type]
        spat_feat <- spat_feat[featType(spat_feat) %in% feat_type]
    }
    
    # combine selected data
    datalist <- c(spat_only, feat_only, spat_feat)
        
    g <- giotto(
        images = x@images,
        parameters = x@parameters,
        instructions = x@instructions,
        offset_file = x@offset_file,
        versions = x@versions,
        join_info = x@join_info,
        h5_file = x@h5_file,
        initialize = FALSE
    )
    g <- setGiotto(g,
        datalist,
        initialize = FALSE,
        check_preqs = FALSE,
        verbose = FALSE
    )
    
    return(initialize(g))
}





.giotto_datalist <- function(x, slots = c(
    "spatial_info", "spatial_locs", "spatial_network", 
    "feat_info",
    "expression", "cell_metadata", "feat_metadata",
    "spatial_enrichment", 
    "nn_network", "dimension_reduction",
    "multiomics"
)) {
    lapply(slots, function(gslot) methods::slot(x, gslot)) |>
        unlist(recursive = TRUE, use.names = FALSE)
}


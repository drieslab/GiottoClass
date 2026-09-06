
## Slot Depth Information ####
# Function to provide correct slot nesting depth definitions for easy testing
# Values provided are for when the slots are populated
giotto_slot_depths <- function() {
    data.table::data.table(
        slot = c(
            "expression",
            "expression_feat",
            "spatial_locs",
            "spatial_info",
            "feat_info",
            "cell_metadata",
            "feat_metadata",
            "cell_ID",
            "feat_ID",
            "spatial_network",
            "spatial_enrichment",
            "dimension_reduction",
            "nn_network",
            "images"
            # 'spatial_grid',
            # 'parameters',
            # 'instructions',
            # 'offset_file',
            # 'versions',
            # 'join_info'
            # 'multiomics'
        ),
        depth = c(
            3L, 0L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 3L, 5L, 4L, 1L
        )
    )
}



## Read S4 Nesting Tags ####
#' @noRd
#' @description SHOULD ONLY BE CALLED FROM ACCESSORS. RELIES ON SPECIFIC NAMES
#' FOR NESTING ELEMENTS IN PARENT FRAME. \cr
#' Reads the nesting information attached to giotto S4 subobjects and
#' compares against the already-existing default values in parent frame. Final
#' nesting values will be sent to the parent frame.
#' @param x a giotto S4 subobject
#' @param nest_elements named character vector of nesting elements for how the
#' information should be nested. (see details)
#' @param specified named logical vector for whether user specified input for a
#' nesting element
#' @returns modified S4 object
#' @details Nesting elements define the nesting structure within giotto slots.
#' Common examples are 'spat_unit', 'feat_type', and 'name' \cr
#' This function compares the nest_elements that are currently available vs
#' those that are suggested by the S4 subobject's appended information on the
#' basis of whether the existing nest_elements were directly specified by the
#' user or if they were simply default values \cr
#' If values were directly specified, then the external nest_elements values
#' will be used downstream and assigned into the relevant S4 subobject slots.
#' If the values were NOT specified then the subobject values will be used
#' downstream. Values will be directly pulled from and set to the parent frame,
#' with the exception of the S4 object itself.
#' @keywords internal
read_s4_nesting <- function(x) {
    p <- parent.frame()

    s_names <- methods::slotNames(x)


    # Determine nesting element to use. If parent frame variables are edited, it
    # will happen within the if statements.

    # if S4 objects will also be edited within if statements, but values will
    # only be sent back to parent frame at end of function

    if ("spat_unit" %in% s_names) {
        if (isTRUE(p$nospec_unit)) {
            if (!is.na(spatUnit(x))) p$spat_unit <- spatUnit(x)
        } else {
            spatUnit(x) <- p$spat_unit
        }
    }

    if ("feat_type" %in% s_names) {
        if (isTRUE(p$nospec_feat)) {
            if (!is.na(featType(x))) p$feat_type <- featType(x)
        } else {
            featType(x) <- p$feat_type
        }
    }

    if ("name" %in% s_names) {
        if (isTRUE(p$nospec_name)) {
            if (!is.na(objName(x))) p$name <- objName(x)
        } else {
            objName(x) <- p$name
        }
    }

    if ("provenance" %in% s_names) {
        if (is.null(p$provenance)) {
            if (!is.null(prov(x))) p$provenance <- prov(x)
        } else {
            prov(x) <- p$provenance
        }
    }

    if ("reduction" %in% s_names) {
        if (isTRUE(p$nospec_red)) {
            if (!is.na(slot(x, "reduction"))) {
                p$reduction <- slot(x, "reduction")
            }
        } else {
            slot(x, "reduction") <- p$reduction
        }
    }

    if ("reduction_method" %in% s_names) {
        if (isTRUE(p$nospec_red_method)) {
            if (!is.na(slot(x, "reduction_method"))) {
                p$reduction_method <- slot(x, "reduction_method")
            }
        } else {
            slot(x, "reduction_method") <- p$reduction_method
        }
    }

    if ("nn_type" %in% s_names) {
        if (isTRUE(p$nospec_net)) {
            if (!is.na(slot(x, "nn_type"))) p$nn_type <- slot(x, "nn_type")
        } else {
            slot(x, "nn_type") <- p$nn_type
        }
    }




    return(x)
}









## mechanical helpers ####

#' @title Resolve spat_unit + feat_type defaults in caller's frame
#' @description
#' Mechanical helper used by getters and setters that take both
#' `spat_unit` and `feat_type` args. When the caller passes either as
#' `NULL`, the gobject's currently-active defaults are filled in.
#' \cr
#' Resolved values are written **directly back into the caller's frame**
#' (assigning to the same-named local variables). This collapses the
#' nine-line paired `set_default_*` block into a single line at each
#' callsite without resorting to multi-value return + unpacking.
#' @param gobject giotto object
#' @param spat_unit caller-supplied value (NULL → resolve from gobject)
#' @param feat_type caller-supplied value (NULL → resolve from gobject)
#' @returns invisibly returns NULL. Side effect: writes `spat_unit` and
#' `feat_type` into `parent.frame()`.
#' @keywords internal
#' @noRd
.set_default_nesting <- function(gobject, spat_unit = NULL, feat_type = NULL) {
    p <- parent.frame()
    p$spat_unit <- set_default_spat_unit(
        gobject = gobject, spat_unit = spat_unit
    )
    p$feat_type <- set_default_feat_type(
        gobject = gobject, spat_unit = p$spat_unit, feat_type = feat_type
    )
    invisible()
}

# prune empty slot nesting levels
.prune_nesting <- function(x, ns) {
    if (is.null(x)) return(x)
    if (length(ns) > 0L) {
        x[[ns[1L]]] <- .prune_nesting(x[[ns[1L]]], ns[-1L])
    }
    if (length(x) == 0L) return(NULL)
    x
}

# Standard message for setters where x = NULL means "remove the entry".
# `what` is the noun phrase to interpolate, e.g. "expression",
# "spatial locations", "nearest neighbor network".
.vmsg_null_remove <- function(verbose, what) {
    vmsg(.v = verbose, sprintf(
        "NULL passed to x. Removing specified %s.", what
    ))
}


## cell_ID slot ####

#' @title Get cell IDs for a given spatial unit
#' @name get_cell_id
#' @inheritParams data_access_params
#' @returns character vector of cell_IDs
#' @description Data for each spatial unit is expected to agree on a single
#' set of cell_IDs that are shared across any feature types. These cell_IDs
#' are stored within the giotto object's \code{cell_ID} slot. Getters and
#' setters for this slot directly retrieve (get) or replace (set) this slot.
#' @seealso set_cell_id
#' @keywords internal
#' @noRd
get_cell_id <- function(gobject,
    spat_unit = NULL,
    set_defaults = TRUE) {
    checkmate::assert_true(inherits(gobject, "giotto"))
    if (isTRUE(set_defaults)) {
        spat_unit <- set_default_spat_unit(
            gobject = gobject,
            spat_unit = spat_unit
        )
    }

    cell_IDs <- slot(gobject, "cell_ID")[[spat_unit]]
    cell_IDs <- as.character(cell_IDs)

    return(cell_IDs)
}





#' @title Set cell IDs for a given spatial unit
#' @name set_cell_id
#' @inheritParams data_access_params
#' @param cell_IDs character vector of cell IDs to set. (See details)
#' @param verbose be verbose
#' @description Setter function for the cell_ID slot. Directly replaces (sets)
#' this slot
#' @returns giotto object with set cell_ID slot
#' @details
#' Data for each spatial unit is expected to agree on a single set of cell_IDs
#' that are shared across any feature types. These cell_IDs are stored within
#' the giotto object's \code{cell_ID} slot. \cr
#'
#' Pass \code{NULL} to \code{cell_IDs} param in order to delete the entry. \cr
#' Pass \code{'initialize'} to \code{cell_IDs} param in order to initialize the
#' specified entry. \cr
#'
#' \strong{NOTE:} The main purpose of the setter is to initialize, as cell_ID
#' values are AUTOMATICALLY updated every time \code{initialize()} is called
#' on the giotto object.
#' @seealso get_cell_id
#' @keywords internal
#' @noRd
set_cell_id <- function(gobject,
    spat_unit = NULL,
    cell_IDs,
    set_defaults = TRUE,
    verbose = TRUE) {
    checkmate::assert_true(inherits(gobject, "giotto"))

    # set default spat_unit
    if (isTRUE(set_defaults)) {
        spat_unit <- set_default_spat_unit(
            gobject = gobject,
            spat_unit = spat_unit
        )
    }

    if (!is.null(cell_IDs)) {
        if (!inherits(cell_IDs, "character")) {
            stop("cell_IDs must be a character vector.")
        }
    }

    # if input is 'initialize', RESET/reinitialize object
    if (identical(cell_IDs, "initialize")) {
        vmsg(.v = verbose, "Initializing", spat_unit, "cell_IDs.")
        expr_avail <- list_expression(gobject = gobject, spat_unit = spat_unit)
        si_avail <- list_spatial_info(gobject = gobject)


        # get cell ID values
        if (spat_unit %in% expr_avail$spat_unit) { # preferred from expression

            cell_IDs <- spatIDs(getExpression(
                gobject = gobject,
                spat_unit = spat_unit,
                feat_type = expr_avail$feat_type[[1L]],
                values = expr_avail$name[[1L]],
                output = "exprObj",
                set_defaults = TRUE
            ))
        } else if (spat_unit %in% si_avail$spat_info) { # fallback to spat_info

            cell_IDs <- spatIDs(getPolygonInfo(
                gobject = gobject,
                name = spat_unit,
                return_giottoPolygon = TRUE
            ))
        } else {
            # catch
            stop(wrap_txt("No data found to initialize cell_ID with",
                errWidth = TRUE
            ))
        }
    }


    # set values
    cell_IDs <- as.character(cell_IDs)
    slot(gobject, "cell_ID")[[spat_unit]] <- cell_IDs

    return(gobject)
}






## feat_ID slot ####

#' @title Get feat IDs for a given feature type
#' @name get_feat_id
#' @inheritParams data_access_params
#' @returns character
#' @description Across a single modality/feature type, all feature information
#' is expected to share a single set of feat_IDs. These feat_IDs are stored
#' within the giotto object's \code{feat_ID} slot. Getters and setters for this
#' slot directly (get) or replace (set) this slot.
#' @seealso set_feat_id
#' @family functions to set data in giotto object
#' @keywords internal
#' @noRd
get_feat_id <- function(gobject,
    feat_type = NULL,
    set_defaults = TRUE) {
    assert_giotto(gobject)
    if (isTRUE(set_defaults)) {
        .set_default_nesting(gobject, spat_unit = NULL, feat_type = feat_type)
    }

    feat_IDs <- slot(gobject, "feat_ID")[[feat_type]]
    feat_IDs <- as.character(feat_IDs)
    return(feat_IDs)
}





#' @title Set feat IDs for a given feature type
#' @name set_feat_id
#' @inheritParams data_access_params
#' @param feat_IDs character vector of feature IDs to set.
#' @param verbose be verbose
#' @description Setter function for the feat_ID slot. Directly replaces (sets)
#' this slot
#' @returns giotto object with set cell_ID slot
#' @details
#' Across a single modality/feature type, and within a spatial unit, all feature
#' information is expected to share a single set of feat_IDs. These feat_IDs
#' are stored within the giotto object's \code{feat_ID} slot separated by
#' feat_type. \cr
#'
#' Pass \code{NULL} to \code{feat_IDs} param in order to delete the entry. \cr
#' Pass \code{'initialize'} to \code{feat_IDs} param in order to initialize the
#' specified entry. \cr
#'
#' \strong{NOTE:} The main purpose of the setter is to initialize, as feat_ID
#' values are AUTOMATICALLY updated every time \code{initialize()} is called on
#' the giotto object.
#' @seealso get_feat_id
#' @family functions to set data in giotto object
#' @keywords internal
#' @noRd
set_feat_id <- function(gobject,
    feat_type = NULL,
    feat_IDs,
    set_defaults = TRUE,
    verbose = TRUE) {
    assert_giotto(gobject)

    if (isTRUE(set_defaults)) {
        if (identical(feat_IDs, "initialize")) {
            spat_unit <- handle_warnings(
                # expected to be missing sometimes with init
                set_default_spat_unit(
                    gobject = gobject,
                    spat_unit = NULL
                )
            )$result
        } else {
            spat_unit <- set_default_spat_unit(
                gobject = gobject,
                spat_unit = NULL
            )
        }
        feat_type <- set_default_feat_type(
            gobject = gobject,
            spat_unit = spat_unit,
            feat_type = feat_type
        )
    }

    if (!is.null(feat_IDs)) {
        if (!inherits(feat_IDs, "character")) {
            stop("feat_IDs must be a character vector.\n")
        }
    }

    # initialize feat_ID
    if (identical(feat_IDs, "initialize")) {
        expr_avail <- list_expression(gobject = gobject, feat_type = feat_type)
        fi_avail <- list_feature_info(gobject = gobject)

        if (feat_type %in% expr_avail$feat_type) { # preferred from expression

            feat_IDs <- featIDs(getExpression(
                gobject = gobject,
                spat_unit = expr_avail$spat_unit[[1L]],
                feat_type = feat_type,
                values = expr_avail$name[[1L]],
                set_defaults = FALSE,
                output = "exprObj"
            ))
        } else if (feat_type %in% fi_avail$feat_info) {
            # fallback to feature info

            feat_IDs <- unique(featIDs(getFeatureInfo(
                gobject = gobject,
                feat_type = feat_type,
                return_giottoPoints = TRUE,
                set_defaults = FALSE
            )))
        } else {
            # catch
            stop(wrap_txt("No data found to intitialize feat_ID with",
                errWidth = TRUE
            ))
        }
    }


    feat_IDs <- as.character(feat_IDs)
    slot(gobject, "feat_ID")[[feat_type]] <- feat_IDs

    return(gobject)
}


## cell metadata slot ####

#' @title getCellMetadata
#' @name getCellMetadata
#' @inheritParams data_access_params
#' @param output return as either 'data.table' or 'cellMetaObj'
#' @returns a data.table or cellMetaObj
#' @description Get cell metadata from giotto object
#' @seealso pDataDT
#' @family functions to get data from giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' getCellMetadata(g)
#' @export
setGeneric("getCellMetadata",
    function(gobject, spat_unit = NULL, feat_type = NULL, ...)
        standardGeneric("getCellMetadata"))

#' @rdname getCellMetadata
#' @export
setMethod("getCellMetadata", signature("gAny"), function(gobject,
    spat_unit = NULL,
    feat_type = NULL,
    output = c("cellMetaObj", "data.table"),
    copy_obj = TRUE,
    set_defaults = TRUE) {
    output <- match.arg(output, choices = c("cellMetaObj", "data.table"))

    if (isTRUE(set_defaults)) {
        .set_default_nesting(gobject, spat_unit, feat_type)
    }

    cellMeta <- gobject@cell_metadata[[spat_unit]][[feat_type]]

    if (inherits(cellMeta, "list") | is.null(cellMeta)) {
        stop("metadata referenced does not exist.")
    }
    if (!inherits(cellMeta, "cellMetaObj")) {
        stop("metadata referenced is not cellMetaObj")
    }

    if (isTRUE(copy_obj)) cellMeta[] <- data.table::copy(cellMeta[])

    if (output == "cellMetaObj") return(cellMeta)
    if (output == "data.table") return(slot(cellMeta, "metaDT"))
})






#' @title Set cell metadata
#' @name setCellMetadata
#' @description Function to set cell metadata into giotto object
#' @inheritParams data_access_params
#' @param x cellMetaObj or list of cellMetaObj to set. Passing NULL will
#' reset a specified set of cell metadata in the giotto object.
#' @param provenance provenance information (optional)
#' @param verbose be verbose
#' @returns giotto object
#' @family functions to set data in giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' m1 <- getCellMetadata(g, output = "data.table")
#' m2 <- data.frame(
#'     cell_ID = m1$cell_ID,
#'     new_column = sample(letters, 624, replace = TRUE)
#' )
#'
#' setCellMetadata(gobject = g, x = createCellMetaObj(m2))
#' @export
setGeneric("setCellMetadata",
    function(gobject, x, spat_unit = NULL, feat_type = NULL, ...)
        standardGeneric("setCellMetadata"))

#' @rdname setCellMetadata
#' @export
setMethod("setCellMetadata", signature("gAny"), function(gobject,
    x,
    spat_unit = NULL,
    feat_type = NULL,
    provenance = NULL,
    verbose = NULL,
    initialize = TRUE,
    ...) {
    if (!methods::hasArg(x)) {
        stop(wrap_txt("x param (data to set) must be given",
            errWidth = TRUE
        ))
    }

    # check hierarchical slots
    if (getOption("giotto.check_valid", TRUE)) {
        used_su <- list_cell_id_names(gobject)
        if (is.null(used_su)) {
            stop(wrap_txt(
                "Add expression or spatial (polygon) information first"
            ))
        }
    }

    # Validate input type
    if (!inherits(x, c("cellMetaObj", "NULL", "list"))) {
        stop(wrap_txt("only cellMetaObj or lists of cellMetaObj accepted.
            For raw or external data, please first use readCellMetadata()"))
    }

    # List input: validate items and iterate via self-recursion. Each item is
    # set with initialize = FALSE; a single initialize() runs after the loop to
    # avoid quadratic init and to prevent re-entrancy issues under testthat.
    if (inherits(x, "list")) {
        if (!all(vapply(x, inherits, "cellMetaObj", FUN.VALUE = logical(1L)))) {
            stop(wrap_txt("only cellMetaObj or lists of cellMetaObj accepted.
                For raw or external data, please first use readCellMetadata()"))
        }
        for (obj_i in seq_along(x)) {
            gobject <- setCellMetadata(
                gobject = gobject,
                x = x[[obj_i]],
                spat_unit = spat_unit,
                feat_type = feat_type,
                provenance = provenance,
                verbose = verbose,
                initialize = FALSE
            )
        }
        if (isTRUE(initialize)) return(initialize(gobject))
        return(gobject)
    }

    # `nospec_*` are read from this frame by read_s4_nesting() / direct slot
    # writes below, to decide whether to overwrite the subobject's nesting
    # values with caller-supplied ones.
    nospec_unit <- is.null(spat_unit)
    nospec_feat <- is.null(feat_type)

    # NULL: remove specified entry
    if (is.null(x)) {
        .vmsg_null_remove(verbose, "metadata")
        gobject@cell_metadata[[spat_unit]][[feat_type]] <- NULL
        if (isTRUE(initialize)) return(initialize(gobject))
        return(gobject)
    }

    # Single cellMetaObj — resolve nesting against subobject's own slots
    if (isTRUE(nospec_unit)) {
        if (!is.na(slot(x, "spat_unit"))) spat_unit <- slot(x, "spat_unit")
    } else {
        slot(x, "spat_unit") <- spat_unit
    }
    if (isTRUE(nospec_feat)) {
        if (!is.na(slot(x, "feat_type"))) feat_type <- slot(x, "feat_type")
    } else {
        slot(x, "feat_type") <- feat_type
    }
    if (!is.null(provenance)) {
        slot(x, "provenance") <- provenance
    }

    # Notify on replacement
    potential_names <- list_cell_metadata(
        gobject,
        spat_unit = spat_unit
    )[, feat_type]
    if (feat_type %in% potential_names) {
        vmsg(.v = verbose,
            '> Cell metadata for spat_unit "',
            spat_unit, '" and feat_type "', feat_type,
            '" already exists and will be replaced with new metadata.'
        )
    }

    gobject@cell_metadata[[spat_unit]][[feat_type]] <- x

    vmsg(.v = verbose,
        "Setting cell metadata [",
        spatUnit(x), "][", featType(x), "] ",
        sep = ""
    )

    if (isTRUE(initialize)) return(initialize(gobject))
    gobject
})


#' Build an empty cellMetaObj seeded with the gobject's cell_IDs.
#' Used by initialize() and init_cell_metadata() to create placeholder
#' metadata for a (spat_unit, feat_type) pair without touching slot
#' contents directly.
#' @noRd
.create_init_cell_meta <- function(gobject, spat_unit, feat_type,
        provenance = NULL) {
    create_cell_meta_obj(
        metaDT = data.table::data.table(
            cell_ID = get_cell_id(gobject, spat_unit = spat_unit)
        ),
        col_desc = c(cell_ID = "cell-specific unique ID value"),
        spat_unit = spat_unit,
        feat_type = feat_type,
        provenance = if (is.null(provenance)) spat_unit else provenance
    )
}








## feature metadata slot ####

#' @title getFeatureMetadata
#' @name getFeatureMetadata
#' @inheritParams data_access_params
#' @param output return as either 'data.table' or 'featMetaObj'
#' @param copy_obj whether to perform a deepcopy of the data.table information
#' @returns a data.table or featMetaObj
#' @description Get feature metadata from giotto object
#' @family functions to get data from giotto object
#' @seealso fDataDT
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' getFeatureMetadata(g)
#' @export
setGeneric("getFeatureMetadata",
    function(gobject, spat_unit = NULL, feat_type = NULL, ...)
        standardGeneric("getFeatureMetadata"))

#' @rdname getFeatureMetadata
#' @export
setMethod("getFeatureMetadata", signature("gAny"), function(gobject,
    spat_unit = NULL,
    feat_type = NULL,
    output = c("featMetaObj", "data.table"),
    copy_obj = TRUE,
    set_defaults = TRUE) {
    output <- match.arg(output, choices = c("featMetaObj", "data.table"))

    if (isTRUE(set_defaults)) {
        .set_default_nesting(gobject, spat_unit, feat_type)
    }

    # metadata objects do not have names
    featMeta <- gobject@feat_metadata[[spat_unit]][[feat_type]]
    if (is.null(featMeta)) stop("metadata referenced does not exist")
    if (!inherits(featMeta, "featMetaObj")) {
        stop("metadata referenced is not featMetaObj")
    }

    if (isTRUE(copy_obj)) featMeta[] <- data.table::copy(featMeta[])

    if (output == "featMetaObj") return(featMeta)
    if (output == "data.table") return(featMeta[])
})






#' @title Set feature metadata
#' @name setFeatureMetadata
#' @description Function to set feature metadata into giotto object
#' @inheritParams data_access_params
#' @param x featMetaObj or list of featMetaObj to set. Passing NULL will
#' reset a specified set of feature metadata in the giotto object.
#' @param provenance provenance information (optional)
#' @param verbose be verbose
#' @returns giotto object
#' @family functions to set data in giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#' m1 <- getFeatureMetadata(g, output = "data.table")
#' m2 <- data.frame(
#'     feat_ID = m1$feat_ID,
#'     new_column = paste0("gene_", m1$feat_ID)
#' )
#'
#' setFeatureMetadata(gobject = g, x = createFeatMetaObj(m2))
#' @export
setGeneric("setFeatureMetadata",
    function(gobject, x, spat_unit = NULL, feat_type = NULL, ...)
        standardGeneric("setFeatureMetadata"))

#' @rdname setFeatureMetadata
#' @export
setMethod("setFeatureMetadata", signature("gAny"), function(gobject,
    x,
    spat_unit = NULL,
    feat_type = NULL,
    provenance = NULL,
    verbose = NULL,
    initialize = TRUE,
    ...) {
    if (!methods::hasArg(x)) {
        stop(wrap_txt("x param (data to set) must be given",
            errWidth = TRUE
        ))
    }

    # Validate input type
    if (!inherits(x, c("featMetaObj", "NULL", "list"))) {
        stop(wrap_txt("only featMetaObj or lists of featMetaObj accepted.
            For raw or external data, please first use readFeatMetadata()"))
    }

    # List input: validate items and iterate via self-recursion. Each item is
    # set with initialize = FALSE; a single initialize() runs after the loop.
    if (inherits(x, "list")) {
        if (!all(vapply(x, inherits, "featMetaObj", FUN.VALUE = logical(1L)))) {
            stop(wrap_txt("only featMetaObj or lists of featMetaObj accepted.
                For raw or external data, please first use readFeatMetadata()"))
        }
        for (obj_i in seq_along(x)) {
            gobject <- setFeatureMetadata(
                gobject = gobject,
                x = x[[obj_i]],
                spat_unit = spat_unit,
                feat_type = feat_type,
                provenance = provenance,
                verbose = verbose,
                initialize = FALSE
            )
        }
        if (isTRUE(initialize)) return(initialize(gobject))
        return(gobject)
    }

    nospec_unit <- is.null(spat_unit)
    nospec_feat <- is.null(feat_type)

    # NULL: remove specified entry
    if (is.null(x)) {
        .vmsg_null_remove(verbose, "metadata")
        gobject@feat_metadata[[spat_unit]][[feat_type]] <- NULL
        if (isTRUE(initialize)) return(initialize(gobject))
        return(gobject)
    }

    # Single featMetaObj — resolve nesting against subobject's own slots
    if (isTRUE(nospec_unit)) {
        if (!is.na(slot(x, "spat_unit"))) spat_unit <- slot(x, "spat_unit")
    } else {
        slot(x, "spat_unit") <- spat_unit
    }
    if (isTRUE(nospec_feat)) {
        if (!is.na(slot(x, "feat_type"))) feat_type <- slot(x, "feat_type")
    } else {
        slot(x, "feat_type") <- feat_type
    }
    if (!is.null(provenance)) {
        slot(x, "provenance") <- provenance
    }

    # Notify on replacement
    potential_names <- list_feat_metadata(
        gobject,
        spat_unit = spat_unit
    )[, feat_type]
    if (feat_type %in% potential_names) {
        vmsg(.v = verbose,
            '> Feat metadata for spat_unit "', spat_unit,
            '" and feat_type "', feat_type,
            '" already exists and will be replaced with new metadata.'
        )
    }

    gobject@feat_metadata[[spat_unit]][[feat_type]] <- x

    vmsg(.v = verbose,
        "Setting feature metadata [",
        spatUnit(x), "][", featType(x), "] ",
        sep = ""
    )

    if (isTRUE(initialize)) return(initialize(gobject))
    gobject
})


#' Build an empty featMetaObj seeded with the gobject's feat_IDs.
#' Used by initialize() and init_feat_metadata() to create placeholder
#' metadata for a (spat_unit, feat_type) pair without touching slot
#' contents directly.
#' @noRd
.create_init_feat_meta <- function(gobject, spat_unit, feat_type,
        provenance = NULL) {
    create_feat_meta_obj(
        metaDT = data.table::data.table(
            feat_ID = get_feat_id(gobject, feat_type = feat_type)
        ),
        col_desc = c(feat_ID = "feature-specific unique ID value"),
        spat_unit = spat_unit,
        feat_type = feat_type,
        provenance = if (is.null(provenance)) spat_unit else provenance
    )
}





## expression values slot ####




#' @title Get expression values
#' @name getExpression
#' @aliases getExpressionValues
#' @description Function to get expression values from giotto object
#' @inheritParams data_access_params
#' @param name name of expression values to extract (e.g. `"raw"`,
#' `"normalized"`, `"scaled"`). Canonical form, consistent with the rest
#' of the slot accessors.
#' @param values back-compat alias for `name`. If both are supplied they
#' must agree.
#' @param output what object type to retrieve the expression as. Currently
#' either matrix' for the matrix object contained in the exprObj or
#' 'exprObj' (default) for the exprObj itself are allowed.
#' @returns exprObj or matrix depending on output param
#' @family expression accessor functions
#' @family functions to get data from giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' getExpression(g)
#' @export
setGeneric("getExpression",
    function(gobject, spat_unit = NULL, feat_type = NULL, name = NULL, ...)
        standardGeneric("getExpression"))

#' @rdname getExpression
#' @export
setMethod("getExpression", signature("gAny"), function(
        gobject,
        spat_unit = NULL,
        feat_type = NULL,
        name = NULL,
        values = NULL,
        output = c("exprObj", "matrix"),
        set_defaults = TRUE) {
    # `values` is an alias for `name` (the canonical form, consistent
    # with the rest of the slot accessors). When both are supplied they
    # must agree.
    if (!is.null(values)) {
        if (!is.null(name) && !identical(name, values)) {
            stop("getExpression: 'name' and 'values' both supplied but ",
                "differ. Use one — 'name' is preferred.", call. = FALSE)
        }
        name <- values
    }
    output <- match.arg(output, choices = c("exprObj", "matrix"))

    if (isTRUE(set_defaults)) {
        .set_default_nesting(gobject, spat_unit, feat_type)
    }

    potential_names <- list_expression_names(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type
    )

    if (is.null(name)) name <- potential_names[[1]]
    if (is.null(name)) {
        stop(wrap_txt(
            "No expression values discovered by getter:",
            "\nspat_unit:", spat_unit,
            "\nfeat_type:", feat_type
        ))
    }

    # Targeted error messages for the standard giotto pipeline names
    if (name == "scaled" & !"scaled" %in% potential_names) {
        stop(wrap_txt("Scaled expression not found.
                First run scaling (& normalization) step(s)", errWidth = TRUE))
    } else if (name == "normalized" & !"normalized" %in% potential_names) {
        stop(wrap_txt("Normalized expression not found.
                First run normalization step", errWidth = TRUE))
    } else if (name == "custom" & !"custom" %in% potential_names) {
        stop(wrap_txt("Custom expression not found.
                First add custom expression matrix", errWidth = TRUE))
    }

    if (!name %in% potential_names) {
        stop(wrap_txt("Requested expression info not found [spat_unit:",
            spat_unit, "] [feat_type:",
            feat_type, "] [name:", name, "]",
            sep = "",
            errWidth = TRUE
        ))
    }

    expr_vals <- gobject@expression[[spat_unit]][[feat_type]][[name]]

    # Output
    if (output == "exprObj") {
        return(expr_vals)
    } else if (output == "matrix") {
        return(expr_vals[])
    }
})




#' @title Set expression data
#' @name setExpression
#' @aliases setExpressionValues
#' @description Function to set expression values for `giotto` object.
#' @inheritParams data_access_params
#' @param x `exprObj` or `list` of `exprObj` to set. Passing `NULL` will remove
#' a specified set of expression data from the `giotto` object
#' @param name `character`. Name for the expression information. `NULL`
#'   (default) takes the name from `x`, falling back to `"raw"` when `x` is
#'   unnamed.
#' @param provenance provenance information (optional)
#' @param write `logical` (default = `FALSE`). For `gsource` managed projects,
#'   whether to force a write of `x`.
#' @param verbose verbosity
#' @param ... additional params to pass (none implemented)
#' @returns giotto object
#' @family expression accessor functions
#' @family functions to set data in giotto object
#' @examples
#' g <- createGiottoObject()
#' m <- matrix(rnorm(100), nrow = 10)
#' colnames(m) <- paste0("cell_", seq_len(10))
#' rownames(m) <- paste0("feat_", seq_len(10))
#'
#' g <- setExpression(gobject = g, x = createExprObj(m, name = "raw"))
#' @export
setGeneric("setExpression",
    function(gobject, x, spat_unit = NULL, feat_type = NULL, name = NULL, ...)
        standardGeneric("setExpression"))

#' @rdname setExpression
#' @export
setMethod("setExpression", signature("gAny"), function(gobject, x,
    spat_unit = NULL,
    feat_type = NULL,
    name = NULL,
    provenance = NULL,
    write = FALSE,
    verbose = NULL,
    initialize = TRUE,
    ...) {
    if (!methods::hasArg(x)) {
        stop("[setExpression] `x` must be provided", call. = FALSE)
    }

    # Validate input type
    .validate_throw_err <- function() {
        stop(wrap_txt(
            "[setExpression] Only exprObj or lists of exprObj accepted.
            For raw or external data, please first use readExprData()"),
        call. = FALSE)
    }
    if (!inherits(x, c("exprObj", "NULL", "list"))) {
        .validate_throw_err()
    }

    # `nospec_*` are read from this frame by read_s4_nesting() to decide
    # whether to overwrite the subobject's nesting values with caller-supplied
    # ones, or vice versa.
    nospec_unit <- is.null(spat_unit)
    nospec_feat <- is.null(feat_type)
    nospec_name <- is.null(name)

    # List input: validate items and iterate. Only forward nesting args that
    # the caller actually supplied; otherwise the recursive call's match.call()
    # would see name = "raw" (default) and clobber each subobj's own @name.
    if (inherits(x, "list")) {
        if (!all(vapply(x, inherits, "exprObj", FUN.VALUE = logical(1L)))) {
            .validate_throw_err()
        }
        base_args <- list(
            write = write,
            verbose = verbose,
            initialize = FALSE
        )
        if (!nospec_unit) base_args$spat_unit <- spat_unit
        if (!nospec_feat) base_args$feat_type <- feat_type
        if (!nospec_name) base_args$name <- name
        if (!is.null(provenance)) base_args$provenance <- provenance
        for (obj_i in seq_along(x)) {
            gobject <- do.call(setExpression, c(
                list(gobject = gobject, x = x[[obj_i]]),
                base_args
            ))
        }
        if (isTRUE(initialize)) return(initialize(gobject))
        return(gobject)
    }

    # NULL: remove specified entry
    if (is.null(x)) {
        vmsg(.v = verbose,
            "NULL passed to x param. Removing specified expression"
        )
        gobject@expression[[spat_unit]][[feat_type]][[name]] <- NULL
        # prune if empty
        gobject@expression <- .prune_nesting(gobject@expression, 
            c(spat_unit, feat_type)
        )

        if (isTRUE(initialize)) return(initialize(gobject))
        return(gobject)
    }

    # Resolve defaults only when the subobject doesn't already carry them.
    # If the exprObj has spat_unit/feat_type and the caller didn't override,
    # read_s4_nesting() below will pick them up from the subobj — no need to
    # consult the gobject's defaults.
    if (!(!is.na(spatUnit(x)) & !is.na(featType(x)) &
        isTRUE(nospec_unit) & isTRUE(nospec_feat))) {
        .set_default_nesting(gobject, spat_unit, feat_type)
    }

    # NOTE: read_s4_nesting modifies spat_unit / feat_type / name / provenance
    # in this frame based on the nospec_* flags.
    x <- read_s4_nesting(x)
    # Fallback for the rare case where neither the caller nor the
    # subobject supplied a name (objName(x) is NA).
    if (is.null(name)) name <- "raw"

    # Notify on replacement
    potential_names <- list_expression_names(gobject,
        spat_unit = spat_unit,
        feat_type = feat_type
    )
    if (name %in% potential_names) {
        vmsg(.v = verbose, "> ", name, 
            " already exists and will be replaced with new values \n"
        )
    }

    vmsg(.v = verbose, sprintf("Setting expression [%s][%s] %s", 
        spatUnit(x), featType(x), objName(x)
    ))


    ## 7. Write matrix to disk if needed
    # Gated on `giotto` rather than `@source` alone: this method dispatches on
    # gAny, and the single-object write-through path is not the right one for a
    # federated parent, whose shared-domain artifacts go to its multi-level
    # source instead.
    memory_matrix <- c("matrix", "Matrix")
    if (inherits(gobject, "giotto") && !is.null(gobject@source)) {
        gsrc <- .gsource(gobject)
        mat <- x[]
        if (inherits(mat, memory_matrix) || isTRUE(write)) {
            store <- GiottoDisk::sourceWrite(gsrc, mat)
            x@misc$uid <- store@uid
            # Keep the store itself, as setFeatureInfo() does. storeRead()
            # returns the lazy Arrow query over it, which reports the
            # triplet file's shape (nnz x 4) rather than features x cells
            # and is not a `parquetExprStore`, so every downstream verb that
            # dispatches on the store class silently misses.
            x[] <- store
        }
    }

    gobject@expression[[spat_unit]][[feat_type]][[name]] <- x
    if (isTRUE(initialize)) return(initialize(gobject))
    gobject
})





## multiomics slot ####



#' @title Set multiomics integration results
#' @name set_multiomics
#' @description Set a multiomics integration result in a Giotto object
#'
#' @param gobject A Giotto object
#' @param spat_unit spatial unit (e.g. 'cell')
#' @param feat_type (e.g. 'rna_protein')
#' @param result A matrix or result from multiomics
#' integration (e.g. theta weighted values from runWNN)
#' @param integration_method multiomics integration method used. Default = 'WNN'
#' @param result_name Default = 'theta_weighted_matrix'
#' @param verbose be verbose
#'
#' @returns A giotto object
#' @family multiomics accessor functions
#' @family functions to set data in giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' set_multiomics(
#'     gobject = g, x = matrix(rnorm(100), nrow = 10),
#'     spat_unit = "cell", feat_type = "rna_protein"
#' )
#' @export
set_multiomics <- function(gobject, ...) setMultiomics(gobject, ...)

#' @title Set multiomics integration results
#' @name setMultiomics
#' @description Set a multiomics integration result in a Giotto object
#'
#' @param gobject A Giotto object
#' @param spat_unit spatial unit (e.g. 'cell')
#' @param feat_type (e.g. 'rna_protein')
#' @param x A matrix or result from multiomics
#' integration (e.g. theta weighted values from runWNN)
#' @param integration_method multiomics integration method used. Default = 'WNN'
#' @param result_name Default = 'theta_weighted_matrix'
#' @param verbose be verbose
#' @param ... additional params to pass
#' @param result deprecated. Use \code{x}
#'
#' @returns A giotto object
#' @family multiomics accessor functions
#' @family functions to set data in giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' setMultiomics(
#'     gobject = g, x = matrix(rnorm(100), nrow = 10),
#'     spat_unit = "cell", feat_type = "rna_protein"
#' )
#' @export
setGeneric("setMultiomics",
    function(gobject, x, spat_unit = NULL, feat_type = NULL, ...)
        standardGeneric("setMultiomics"))

#' @rdname setMultiomics
#' @export
setMethod("setMultiomics", signature("gAny"), function(gobject,
    x,
    spat_unit = NULL,
    feat_type = NULL,
    integration_method = "WNN",
    result_name = "theta_weighted_matrix",
    verbose = NULL,
    ...,
    result = deprecated()) {
    x <- GiottoUtils::deprecate_param(result, x,
        fun = "setMultiomics", when = "0.5.2"
    )
    .set_default_nesting(gobject, spat_unit, feat_type)

    # If input is null, remove object
    if (is.null(x)) {
        vmsg(.v = verbose,
            "NULL passed to x. Removing specified result"
        )
        gobject@multiomics[[spat_unit]][[feat_type]][[
            integration_method
        ]][[result_name]] <- x
        return(gobject)
    }

    # Notify on replacement
    potential_names <- names(
        slot(gobject, "multiomics")[[spat_unit]][[integration_method]][[feat_type]]
    )
    if (result_name %in% potential_names) {
        vmsg(.v = verbose,
            '> "', result_name,
            '" already exists and will be replaced with new result'
        )
    }

    gobject@multiomics[[spat_unit]][[feat_type]][[
        integration_method
    ]][[result_name]] <- x
    gobject
})

#' @title Get multiomics integration results
#' @name get_multiomics
#' @description Get a multiomics integration result from a Giotto object
#'
#' @param gobject A Giotto object
#' @param spat_unit spatial unit (e.g. 'cell')
#' @param feat_type integrated feature type (e.g. 'rna_protein')
#' @param integration_method multiomics integration method used. Default = 'WNN'
#' @param result_name Default = 'theta_weighted_matrix'
#'
#' @returns A multiomics integration result (e.g. theta_weighted_matrix from WNN)
#' @family multiomics accessor functions
#' @family functions to get data from giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' g <- setMultiomics(
#'     gobject = g, x = matrix(rnorm(100), nrow = 10),
#'     spat_unit = "cell", feat_type = "rna_protein"
#' )
#'
#' get_multiomics(gobject = g, spat_unit = "cell", feat_type = "rna_protein")
#' @export
get_multiomics <- function(gobject, ...) getMultiomics(gobject, ...)

#' @title Get multiomics integration results
#' @name getMultiomics
#' @description Get a multiomics integration result from a Giotto object
#'
#' @param gobject A Giotto object
#' @param spat_unit spatial unit (e.g. 'cell')
#' @param feat_type integrated feature type (e.g. 'rna_protein')
#' @param integration_method multiomics integration method used. Default = 'WNN'
#' @param result_name Default = 'theta_weighted_matrix'
#'
#' @returns A multiomics integration result (e.g. theta_weighted_matrix from WNN)
#' @family multiomics accessor functions
#' @family functions to get data from giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' g <- setMultiomics(
#'     gobject = g, x = matrix(rnorm(100), nrow = 10),
#'     spat_unit = "cell", feat_type = "rna_protein"
#' )
#'
#' getMultiomics(gobject = g, spat_unit = "cell", feat_type = "rna_protein")
#' @export
setGeneric("getMultiomics",
    function(gobject, spat_unit = NULL, feat_type = NULL, ...)
        standardGeneric("getMultiomics"))

#' @rdname getMultiomics
#' @export
setMethod("getMultiomics", signature("gAny"), function(gobject,
    spat_unit = NULL,
    feat_type = NULL,
    integration_method = "WNN",
    result_name = "theta_weighted_matrix") {
    .set_default_nesting(gobject, spat_unit, feat_type)

    # automatic result selection
    if (is.null(result_name)) {
        result_to_use <- names(
            gobject@multiomics[[spat_unit]][[integration_method]][[feat_type]]
        )[[1]]
        if (is.null(result_to_use)) {
            stop('There is currently no multiomics integration created for
            spatial unit: "', spat_unit, '" and feature type "', feat_type, '".
            First run runWNN() or other multiomics integration method\n')
        } else {
            message('The result name was not specified, default to the
                    first: "', result_to_use, '"')
        }
    }

    result <- gobject@multiomics[[spat_unit]][[feat_type]][[
        integration_method
    ]][[result_name]]
    if (is.null(result)) {
        stop(
            'result: "', result_to_use,
            '" does not exist. Create a multiomics integration first'
        )
    }

    result
})




## spatial locations slot ####





#' @title Get spatial locations
#' @name getSpatialLocations
#' @description Function to get a spatial location data.table
#' @inheritParams data_access_params
#' @param name name of spatial
#' locations (defaults to first name in spatial_locs slot, e.g. "raw")
#' @param output what object type to get the spatial locations as. Default is as
#' a 'spatLocsObj'. Returning as 'data.table' is also possible.
#' @param copy_obj whether to copy/duplicate when getting the
#' object (default = TRUE)
#' @param verbose be verbose
#' @param simplify logical. Whether or not to take object out of a list when
#' there is a length of 1.
#' @returns data.table with coordinates or spatLocsObj depending on \code{output}
#' @family spatial location data accessor functions
#' @family functions to get data from giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' getSpatialLocations(g)
#' @export
setGeneric("getSpatialLocations",
    function(gobject, spat_unit = NULL, name = NULL, ...)
        standardGeneric("getSpatialLocations"))

#' @rdname getSpatialLocations
#' @export
setMethod("getSpatialLocations", signature("giotto"), function(gobject,
    spat_unit = NULL,
    name = NULL,
    output = c("spatLocsObj", "data.table"),
    copy_obj = TRUE,
    verbose = TRUE,
    set_defaults = TRUE,
    simplify = TRUE) {
    output <- match.arg(output, choices = c("spatLocsObj", "data.table"))
    all_su <- identical(spat_unit, ":all:")

    if (isTRUE(set_defaults)) {
        spat_unit <- set_default_spat_unit(
            gobject = gobject, spat_unit = spat_unit
        )
    }

    data_type <- "spatial locations/centroids"
    slotdata <- slot(gobject, "spatial_locs")
    if (is.null(slotdata) || length(slotdata) == 0L) {
        stop(wrap_txt(sprintf(
            "No %s in giotto object", data_type
        ), errWidth = TRUE), call. = FALSE)
    }
    # filter out length 0 spat_units
    slotdata <- slotdata[lengths(slotdata) > 0L]

    avail_su <- names(slotdata)
    missing_su <- spat_unit[!spat_unit %in% avail_su]
    if (length(missing_su) > 0L && !all_su) {
        stop(wrap_txt(sprintf(
            "No %s for spat_unit(s): '%s'", data_type,
            paste(missing_su, collapse = "', ")
        ), errWidth = TRUE), call. = FALSE)
    }

    if (!all_su) slotdata <- slotdata[spat_unit]
    # list depth should be 2: 1. spat_unit(s), 2. spatLocsObj
    su_names <- names(slotdata)

    out_list <- lapply(su_names, function(su) {
        su_data <- slotdata[[su]]
        if (is.null(name)) return(su_data[[1]])
        if (identical(name, ":all:")) return(su_data)

        missing_sln <- name[!name %in% objName(su_data)]
        if (length(missing_sln) > 0L) {
            stop(wrap_txt(sprintf(
                "No %s with name '%s' and spat_unit '%s'\n", data_type,
                paste(missing_sln, collapse = "', '"), su
            ), errWidth = TRUE), call. = FALSE)
        }

        su_data[name]
    })
    out <- Reduce("c", out_list)
    if (!inherits(out, "list")) out <- list(out)
    names(out) <- NULL

    out <- lapply(out, function(x) {
        if (isTRUE(copy_obj)) x[] <- data.table::copy(x[])
        switch(output, "spatLocsObj" = x, "data.table" = x[])
    })
    if (isTRUE(simplify)) out <- .simplify_list(out)
    out
})






















#' @title Set spatial locations
#' @name setSpatialLocations
#' @description Function to set a spatial location slot
#' @inheritParams data_access_params
#' @param x spatLocsObj or list of spatLocsObj. Passing NULL will remove a
#' specified set of spatial locations data.
#' @param name name of spatial locations. NULL (default) takes the name from
#' \code{x}, falling back to "raw" when \code{x} is unnamed
#' @param provenance provenance information (optional)
#' @param verbose be verbose
#' @details Spatial information will be set to the nested location described
#' by their tagged spat_unit and name information. An alternative location can
#' also be specified through the respective params in this function.
#' @returns giotto object
#' @family spatial location data accessor functions
#' @family functions to set data in giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' x <- getSpatialLocations(g, output = "data.table")
#' sl <- data.frame(cell_ID = x$cell_ID, sdimx = rnorm(624), sdimy = rnorm(624))
#'
#' setSpatialLocations(gobject = g, x = createSpatLocsObj(sl, name = "raw"))
#' @export
setGeneric("setSpatialLocations",
    function(gobject, x, spat_unit = NULL, name = NULL, ...)
        standardGeneric("setSpatialLocations"))

#' @rdname setSpatialLocations
#' @export
setMethod("setSpatialLocations", signature("giotto"), function(gobject,
    x,
    spat_unit = NULL,
    name = NULL,
    provenance = NULL,
    verbose = NULL,
    initialize = TRUE,
    ...) {
    if (!methods::hasArg(x)) {
        stop(wrap_txt("x (data to set) param must be given"))
    }

    # check hierarchical slots
    if (getOption("giotto.check_valid", TRUE)) {
        avail_ex <- list_expression(gobject)
        avail_si <- list_spatial_info(gobject)
        if (is.null(avail_ex) && is.null(avail_si)) {
            stop(wrap_txt(
                "Add expression or spatial (polygon) information first"
            ))
        }
    }

    # Validate input type
    if (!inherits(x, c("spatLocsObj", "NULL", "list"))) {
        stop(wrap_txt("Only spatLocsObj or lists of spatLocsObj accepted.
            For raw or external data, please first use readSpatLocsData()"))
    }

    # `nospec_*` are read from this frame by read_s4_nesting() to decide
    # whether to overwrite the subobject's nesting values with caller-supplied
    # ones, or vice versa.
    nospec_unit <- is.null(spat_unit)
    nospec_name <- is.null(name)

    # List input: validate items and iterate. Only forward nesting args that
    # the caller actually supplied; otherwise the recursive call would
    # clobber each subobj's own @name with our top-level default.
    if (inherits(x, "list")) {
        if (!all(vapply(x, inherits, "spatLocsObj", FUN.VALUE = logical(1L)))) {
            stop(wrap_txt("Only spatLocsObj or lists of spatLocsObj accepted.
                For raw or external data, please first use readSpatLocsData()"))
        }
        base_args <- list(
            verbose = verbose,
            initialize = FALSE
        )
        if (!nospec_unit) base_args$spat_unit <- spat_unit
        if (!nospec_name) base_args$name <- name
        if (!is.null(provenance)) base_args$provenance <- provenance
        for (obj_i in seq_along(x)) {
            gobject <- do.call(setSpatialLocations, c(
                list(gobject = gobject, x = x[[obj_i]]),
                base_args
            ))
        }
        if (isTRUE(initialize)) return(initialize(gobject))
        return(gobject)
    }

    # NULL: remove specified entry
    if (is.null(x)) {
        .vmsg_null_remove(verbose, "spatial locations")
        gobject@spatial_locs[[spat_unit]][[name]] <- NULL
        gobject@spatial_locs <- .prune_nesting(
            gobject@spatial_locs, spat_unit
        )

        if (isTRUE(initialize)) return(initialize(gobject))
        return(gobject)
    }

    # NOTE: read_s4_nesting modifies spat_unit / name / provenance in this
    # frame based on the nospec_* flags.
    x <- read_s4_nesting(x)
    # Fallback for the rare case where neither the caller nor the
    # subobject supplied a name (objName(x) is NA).
    if (is.null(name)) name <- "raw"

    # Notify on replacement
    potential_names <- list_spatial_locations_names(gobject,
        spat_unit = spat_unit
    )
    if (name %in% potential_names) {
        vmsg(.v = verbose,
            "> ", name,
            " already exists and will be replaced with new spatial
            locations \n"
        )
    }

    vmsg(.v = verbose,
        "Setting spatial locations [", spatUnit(x), "] ",
        objName(x),
        sep = ""
    )

    gobject@spatial_locs[[spat_unit]][[name]] <- x
    if (isTRUE(initialize)) return(initialize(gobject))
    gobject
})




## dimension reduction slot ####

#' @title Get dimension reduction
#' @name getDimReduction
#' @inheritParams data_access_params
#' @param reduction reduction on cells or features (e.g. "cells", "feats")
#' @param reduction_method reduction method (e.g. "pca", "umap", "tsne")
#' @param name name of reduction results
#' @param output object type to return as. Either 'dimObj' (default) or 'matrix'
#' of the embedding coordinates.
#' @description Function to get a dimension reduction object
#' @returns dim reduction object (default) or dim reduction coordinates
#' @family dimensional reduction data accessor functions
#' @family functions to get data from giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' getDimReduction(g)
#' @export
setGeneric("getDimReduction",
    function(gobject, spat_unit = NULL, feat_type = NULL, name = NULL, ...)
        standardGeneric("getDimReduction"))

#' @rdname getDimReduction
#' @export
setMethod("getDimReduction", signature("gAny"), function(gobject,
    spat_unit = NULL,
    feat_type = NULL,
    name = NULL,
    reduction = c("cells", "feats"),
    reduction_method = NULL,
    output = c("dimObj", "matrix"),
    set_defaults = TRUE) {
    # back-compat: "data.table" used to be accepted for matrix output
    if (!identical(output, c("dimObj", "matrix"))) {
        if (output == "data.table") output <- "matrix"
    }
    output <- match.arg(output, choices = c("dimObj", "matrix"))
    reduction <- match.arg(arg = reduction, choices = c("cells", "feats"))
    reduction_method <- match.arg(
        arg = reduction_method,
        choices = unique(c("pca", "umap", "tsne", reduction_method))
    )

    if (isTRUE(set_defaults)) {
        .set_default_nesting(gobject, spat_unit, feat_type)
    }

    potential_drs <- list_dim_reductions_names(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type,
        data_type = reduction,
        dim_type = reduction_method
    )

    if (is.null(name)) name <- potential_drs[[1L]]
    if (is.null(name)) {
        stop(wrap_txt(sprintf(
            "No dimension reduction for \"%s\" has been applied\n", reduction
        )), call. = FALSE)
    }

    if (!name %in% potential_drs) {
        stop(wrap_txt(
            errWidth = TRUE,
            "Requested dimension reduction not found",
            sprintf(
                "[spat_unit:\"%s\"] [feat_type:\"%s\"] [name: \"%s\"]",
                spat_unit, feat_type, name
            )
        ))
    }

    reduction_res <- gobject@dimension_reduction[[reduction]][[
        spat_unit]][[feat_type]][[reduction_method]][[name]]

    if (output == "dimObj") return(reduction_res)
    if (output == "matrix") return(slot(reduction_res, "coordinates"))
})



#' @title Set dimension reduction data
#' @name setDimReduction
#' @description Function to dimension reduction information into the Giotto
#' object.
#' @inheritParams data_access_params
#' @param x dimObj or list of dimObj to set. Passing NULL will remove a
#' specified set of dimension reduction information from the gobject
#' @param name name of reduction results. NULL (default) takes the name from
#' \code{x}, falling back to "pca" when \code{x} is unnamed
#' @param reduction reduction on cells or features
#' @param reduction_method reduction method (e.g. "pca")
#' @param provenance provenance information (optional)
#' @param verbose be verbose
#' @returns giotto object
#' @keywords autocomplete
#' @family dimensional reduction data accessor functions
#' @family functions to set data in giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' dimred <- getDimReduction(g)
#'
#' setDimReduction(gobject = g, x = dimred)
#' @export
setGeneric("setDimReduction",
    function(gobject, x, spat_unit = NULL, feat_type = NULL, name = NULL, ...)
        standardGeneric("setDimReduction"))

#' @rdname setDimReduction
#' @export
setMethod("setDimReduction", signature("gAny"), function(gobject,
    x,
    spat_unit = NULL,
    feat_type = NULL,
    name = NULL,
    reduction = c("cells", "feats"),
    reduction_method = c("pca", "umap", "tsne"),
    provenance = NULL,
    verbose = NULL,
    initialize = TRUE,
    ...) {
    if (!methods::hasArg(x)) {
        stop(wrap_txt("x (data to set) param must be given"))
    }

    # check hierarchical slots
    if (getOption("giotto.check_valid", TRUE)) {
        avail_ex <- list_expression(gobject)
        if (is.null(avail_ex)) stop(wrap_txt("Add expression information first"))
    }

    # Validate input type
    if (!inherits(x, c("dimObj", "NULL", "list"))) {
        stop(wrap_txt("Only dimObj or lists of dimObj accepted.
            For raw or external data, please first use readDimReducData()"))
    }

    # `nospec_*` are read from this frame by read_s4_nesting() to decide
    # whether to overwrite the subobject's nesting values with caller-supplied
    # ones, or vice versa. Compute before match.arg() resolves the vectors.
    nospec_unit <- is.null(spat_unit)
    nospec_feat <- is.null(feat_type)
    nospec_name <- is.null(name)
    nospec_red <- is.null(match.call()$reduction)
    nospec_red_method <- is.null(match.call()$reduction_method)

    reduction <- match.arg(reduction, choices = c("cells", "feats"))

    # List input: validate items and iterate. Only forward nesting args that
    # the caller supplied; otherwise recursive call's match.call() would see
    # the defaults and clobber each subobj's own slots.
    if (inherits(x, "list")) {
        if (!all(vapply(x, inherits, "dimObj", FUN.VALUE = logical(1L)))) {
            stop(wrap_txt("Only dimObj or lists of dimObj accepted.
                For raw or external data, please first use readDimReducData()"))
        }
        base_args <- list(
            verbose = verbose,
            initialize = FALSE
        )
        if (!nospec_unit) base_args$spat_unit <- spat_unit
        if (!nospec_feat) base_args$feat_type <- feat_type
        if (!nospec_name) base_args$name <- name
        if (!nospec_red) base_args$reduction <- reduction
        if (!nospec_red_method) base_args$reduction_method <- reduction_method
        if (!is.null(provenance)) base_args$provenance <- provenance
        for (obj_i in seq_along(x)) {
            gobject <- do.call(setDimReduction, c(
                list(gobject = gobject, x = x[[obj_i]]),
                base_args
            ))
        }
        if (isTRUE(initialize)) return(initialize(gobject))
        return(gobject)
    }

    # NULL: remove specified entry
    if (is.null(x)) {
        .vmsg_null_remove(verbose, "dimension reduction")
        gobject@dimension_reduction[[reduction]][[spat_unit]][[
            feat_type]][[reduction_method]][[name]] <- NULL
        gobject@dimension_reduction <- .prune_nesting(
            gobject@dimension_reduction,
            c(reduction, spat_unit, feat_type, reduction_method)
        )

        if (isTRUE(initialize)) return(initialize(gobject))
        return(gobject)
    }

    # NOTE: read_s4_nesting modifies spat_unit / feat_type / name /
    # reduction / reduction_method / provenance in this frame based on
    # nospec_* flags.
    x <- read_s4_nesting(x)
    # Fallback for the rare case where neither the caller nor the
    # subobject supplied a name (objName(x) is NA).
    if (is.null(name)) name <- "pca"

    # Notify on replacement
    potential_names <- list_dim_reductions_names(gobject,
        spat_unit = spat_unit,
        feat_type = feat_type,
        data_type = reduction,
        dim_type = reduction_method
    )
    if (name %in% potential_names) {
        vmsg(.v = verbose, "> ", name,
            " already exists and will be replaced with
            new dimension reduction object \n")
    }

    vmsg(.v = verbose,
        "Setting dimension reduction [", spatUnit(x), "][",
        featType(x), "] ",
        objName(x),
        sep = ""
    )

    slot(gobject, "dimension_reduction")[[reduction]][[spat_unit]][[
        feat_type]][[reduction_method]][[name]] <- x
    if (isTRUE(initialize)) return(initialize(gobject))
    gobject
})




## nearest neighbor network slot ####

#' @title Get nearest neighbor network
#' @name getNearestNetwork
#' @description Get a NN-network from a Giotto object
#' @inheritParams data_access_params
#' @param nn_type "kNN" or "sNN"
#' @param name name of NN network to be used
#' @param output return a giotto `nnNetObj`, `igraph`, `data.table` object.
#' Default 'nnNetObj'
#' @returns Giotto `nnNetObj`, `igraph` or `data.table` object
#' @family expression space nearest network accessor functions
#' @family functions to get data from giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' getNearestNetwork(gobject = g)
#' @export
setGeneric("getNearestNetwork",
    function(gobject, spat_unit = NULL, feat_type = NULL, name = NULL, ...)
        standardGeneric("getNearestNetwork"))

#' @rdname getNearestNetwork
#' @export
setMethod("getNearestNetwork", signature("gAny"), function(gobject,
    spat_unit = NULL,
    feat_type = NULL,
    name = NULL,
    nn_type = NULL,
    output = c("nnNetObj", "igraph", "data.table"),
    set_defaults = TRUE) {
    output <- match.arg(
        arg = output,
        choices = c("nnNetObj", "igraph", "data.table")
    )

    if (isTRUE(set_defaults)) {
        .set_default_nesting(gobject, spat_unit, feat_type)
    }

    # Fall back to first available nn_type if not supplied
    if (is.null(nn_type)) {
        nn_type <- names(
            slot(gobject, "nn_network")[[spat_unit]][[feat_type]]
        )[[1]]
        if (is.null(nn_type)) {
            stop(wrap_txt('There is currently no nearest-neighbor network
                        created for spatial unit: "', spat_unit,
                '" and feature type "', feat_type,
                '". First run createNearestNetwork()\n',
                sep = ""
            ))
        }
        wrap_msg('The NN network type was not specified, default to the
                first: "', nn_type, '"', sep = "")
    }

    # Fall back to first available name if not supplied
    if (is.null(name)) {
        name <- names(
            slot(gobject, "nn_network")[[spat_unit]][[feat_type]][[nn_type]]
        )[[1]]
        if (is.null(name)) {
            stop(wrap_txt('There is currently no nearest-neighbor network
                        built for spatial unit: "', spat_unit,
                '" feature type: "', feat_type,
                '" and network type: "', nn_type, '"\n',
                sep = ""
            ))
        }
        wrap_msg('The NN network name was not specified, default to the
                first: "', name, '"', sep = "")
    }

    nnNet <- slot(gobject, "nn_network")[[spat_unit]][[
        feat_type]][[nn_type]][[name]]
    if (is.null(nnNet)) {
        stop(wrap_txt('nn_type: "', nn_type,
            '" or name: "', name, '" does not exist.
                Create a nearest-neighbor network first',
            sep = ""
        ))
    }

    if (output == "nnNetObj") return(nnNet)
    if (output == "igraph") return(slot(nnNet, "network"))
    if (output == "data.table") {
        return(data.table::setDT(
            igraph::as_data_frame(x = slot(nnNet, "network"), what = "edges")
        ))
    }
})




#' @title Set nearest neighbor network
#' @name setNearestNetwork
#' @description Set a NN-network for a Giotto object
#' @inheritParams data_access_params
#' @param x nnNetObj or list of nnNetObj. Passing NULL will remove a specified
#' set of nearest neighbor network information from the gobject
#' @param nn_type "kNN" or "sNN"
#' @param name name of NN network to be used. NULL (default) takes the name
#' from \code{x}, falling back to "sNN.pca" when \code{x} is unnamed
#' @param provenance provenance information (optional)
#' @param verbose be verbose
#' @returns giotto object
#' @family expression space nearest network accessor functions
#' @family functions to set data in giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' dimred <- getNearestNetwork(gobject = g)
#'
#' setNearestNetwork(gobject = g, x = dimred)
#' @export
setGeneric("setNearestNetwork",
    function(gobject, x, spat_unit = NULL, feat_type = NULL, name = NULL, ...)
        standardGeneric("setNearestNetwork"))

#' @rdname setNearestNetwork
#' @export
setMethod("setNearestNetwork", signature("gAny"), function(gobject,
    x,
    spat_unit = NULL,
    feat_type = NULL,
    name = NULL,
    nn_type = "sNN",
    provenance = NULL,
    verbose = NULL,
    initialize = TRUE,
    ...) {
    if (!methods::hasArg(x)) {
        stop(wrap_txt("x (data to set) param must be given"))
    }

    # check hierarchical slots
    if (getOption("giotto.check_valid", TRUE)) {
        avail_dr <- list_dim_reductions(gobject)
        if (is.null(avail_dr)) {
            stop(wrap_txt("Add dimension reduction information first"))
        }
    }

    # Validate input type
    if (!inherits(x, c("nnNetObj", "NULL", "list"))) {
        stop(wrap_txt("Only nnNetObj or lists of nnNetObj accepted.
            For raw or external data, please first use readNearestNetData()"))
    }

    # `nospec_*` are read from this frame by read_s4_nesting() to decide
    # whether to overwrite the subobject's nesting values with caller-supplied
    # ones, or vice versa.
    nospec_unit <- is.null(spat_unit)
    nospec_feat <- is.null(feat_type)
    nospec_net <- is.null(match.call()$nn_type)
    nospec_name <- is.null(name)

    # List input: validate items and iterate. Only forward nesting args that
    # the caller supplied; otherwise the recursive call's match.call() would
    # see the defaults and clobber each subobj's own slots.
    if (inherits(x, "list")) {
        if (!all(vapply(x, inherits, "nnNetObj", FUN.VALUE = logical(1L)))) {
            stop(wrap_txt("Only nnNetObj or lists of nnNetObj accepted.
                For raw or external data, please first use readNearestNetData()"))
        }
        base_args <- list(
            verbose = verbose,
            initialize = FALSE
        )
        if (!nospec_unit) base_args$spat_unit <- spat_unit
        if (!nospec_feat) base_args$feat_type <- feat_type
        if (!nospec_net) base_args$nn_type <- nn_type
        if (!nospec_name) base_args$name <- name
        if (!is.null(provenance)) base_args$provenance <- provenance
        for (obj_i in seq_along(x)) {
            gobject <- do.call(setNearestNetwork, c(
                list(gobject = gobject, x = x[[obj_i]]),
                base_args
            ))
        }
        if (isTRUE(initialize)) return(initialize(gobject))
        return(gobject)
    }

    # NULL: remove specified entry
    if (is.null(x)) {
        .vmsg_null_remove(verbose, "nearest neighbor network")
        gobject@nn_network[[spat_unit]][[feat_type]][[nn_type]][[name]] <- NULL
        gobject@nn_network <- .prune_nesting(
            gobject@nn_network, c(spat_unit, feat_type, nn_type)
        )

        if (isTRUE(initialize)) return(initialize(gobject))
        return(gobject)
    }

    # NOTE: read_s4_nesting modifies spat_unit / feat_type / name / nn_type /
    # provenance in this frame based on nospec_* flags.
    x <- read_s4_nesting(x)
    # Fallback for the rare case where neither the caller nor the
    # subobject supplied a name (objName(x) is NA).
    if (is.null(name)) name <- "sNN.pca"

    # Notify on replacement
    potential_names <- list_nearest_networks_names(gobject,
        spat_unit = spat_unit,
        feat_type = feat_type,
        nn_type = nn_type
    )
    if (name %in% potential_names) {
        vmsg(.v = verbose, sprintf(
            "> '%s' already exists and will be replaced with
            new nearest neighbor network", name
        ))
    }

    vmsg(.v = verbose, sprintf(
        "Setting nearest neighbor network [%s][%s] %s",
        spatUnit(x), featType(x), objName(x)
    ))

    # ensure legacy pre-0.6.0 schema migrates before storing
    x <- methods::initialize(x)

    # Write to disk if needed. Mirrors the pattern in setExpression /
    # setPolygonInfo / setFeatureInfo: when the gobject has a gsource
    # backend and the network is in-memory (igraph, not a dataStore),
    # write through to a parquetEdgeStore so the artifact lives in the
    # project vault. `type` is plumbed through so the resulting store's
    # @type slot reflects the actual network kind (kNN vs sNN) instead
    # of the storeWrite default.
    # Gated on `giotto` for the same reason as setExpression above — this
    # method dispatches on gAny.
    if (inherits(gobject, "giotto") && !is.null(gobject@source)) {
        gsrc <- .gsource(gobject)
        if (!inherits(x@network, "dataStore")) {
            store <- GiottoDisk::sourceWrite(gsrc, x@network,
                type = x@nn_type)
            x@network <- store
        }
    }

    gobject@nn_network[[spat_unit]][[feat_type]][[nn_type]][[name]] <- x
    if (isTRUE(initialize)) return(initialize(gobject))
    gobject
})
















## spatial network slot ####

#' @title Get spatial network
#' @name getSpatialNetwork
#' @description Function to get a spatial network
#' @inheritParams data_access_params
#' @param name name of spatial network
#' @param output object type to return as. Options:
#' 'spatialNetworkObj' (default),
#' 'networkDT' and 'networkDT_before_filter' for data.table outputs.
#' @param copy_obj whether to copy/duplicate when getting the
#' object (default = TRUE)
#' @param verbose be verbose
#' @param simplify logical. Whether or not to take object out of a list when
#' there is a length of 1.
#' @returns spatialNetworkObj of data.table
#' @family spatial network data accessor functions
#' @family functions to get data from giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' getSpatialNetwork(g)
#' @export
setGeneric("getSpatialNetwork",
    function(gobject, spat_unit = NULL, name = NULL, ...)
        standardGeneric("getSpatialNetwork"))

#' @rdname getSpatialNetwork
#' @export
setMethod("getSpatialNetwork", signature("giotto"), function(gobject,
    spat_unit = NULL,
    name = NULL,
    output = c("spatialNetworkObj", "igraph", "networkDT", "unfiltered", "outputObj"),
    set_defaults = TRUE,
    copy_obj = TRUE,
    verbose = TRUE,
    simplify = TRUE) {
    output <- match.arg(output, choices = c(
        "spatialNetworkObj", "igraph", "networkDT", "unfiltered", "outputObj"
    ))
    all_su <- identical(spat_unit, ":all:")

    if (isTRUE(set_defaults)) {
        spat_unit <- set_default_spat_unit(
            gobject = gobject, spat_unit = spat_unit
        )
    }

    data_type <- "spatial network"
    slotdata <- slot(gobject, "spatial_network")
    if (is.null(slotdata) || length(slotdata) == 0L) {
        stop(wrap_txt(sprintf(
            "No %ss in giotto object", data_type
        ), errWidth = TRUE), call. = FALSE)
    }

    # filter out length 0 spat_units
    slotdata <- slotdata[lengths(slotdata) > 0L]

    avail_su <- names(slotdata)
    missing_su <- spat_unit[!spat_unit %in% avail_su]
    if (length(missing_su) > 0L && !all_su) {
        stop(wrap_txt(sprintf(
            "No %ss for spat_unit(s): '%s'", data_type,
            paste(missing_su, collapse = "', '")
        ), errWidth = TRUE), call. = FALSE)
    }

    if (!all_su) slotdata <- slotdata[spat_unit]
    # list depth should be 2: 1. spat_unit(s), 2. spatialNetworkObj
    su_names <- names(slotdata)

    out_list <- lapply(su_names, function(su) {
        su_data <- slotdata[[su]]

        # if name not given, first available
        if (is.null(name)) return(su_data[[1L]])
        # all available
        if (identical(name, ":all:")) return(su_data)

        missing_snn <- name[!name %in% objName(su_data)]
        if (length(missing_snn) > 0L) {
            stop(wrap_txt(sprintf(
                "No %ss with name '%s' and spat_unit '%s'\n",
                data_type, paste(missing_snn, collapse = "', '"), su
            ), errWidth = TRUE), call. = FALSE)
        }

        su_data[name]
    })

    out <- Reduce("c", out_list) # collapse to depth 1
    if (!inherits(out, "list")) out <- list(out)
    names(out) <- NULL

    # spatialNetworkObj@network is an igraph (canonical as of 0.6.0), or a
    # GiottoDisk dataStore on a backed project. Both output shapes have to
    # handle the store: without this, `output = "networkDT"` hands a
    # parquetEdgeStore to as.data.table() and every downstream consumer of the
    # edge table -- annotateSpatialNetwork() and everything built on it --
    # fails on a backed object with "cannot coerce class parquetEdgeStore".
    # igraph is copy-on-modify so `copy_obj` is a no-op here.
    .read_store <- function(g, what) {
        package_check("GiottoDisk",
            repository = "github:giotto-suite/GiottoDisk")
        GiottoDisk::storeRead(g, output = what)
    }
    as_dt <- function(g) {
        if (inherits(g, "dataStore")) {
            dt <- data.table::as.data.table(.read_store(g, "tibble"))
            # edge stores name their endpoints from_id/to_id; the networkDT
            # contract is from/to
            for (nm in c("from", "to")) {
                old_nm <- paste0(nm, "_id")
                if (old_nm %in% names(dt) && !nm %in% names(dt)) {
                    data.table::setnames(dt, old_nm, nm)
                }
            }
            return(dt)
        }
        if (inherits(g, "igraph")) {
            return(data.table::as.data.table(
                igraph::as_data_frame(g, what = "edges")
            ))
        }
        g
    }
    as_ig <- function(g) {
        if (inherits(g, "dataStore")) return(.read_store(g, "igraph"))
        g
    }
    out <- lapply(out, function(x) {
        switch(output,
            "spatialNetworkObj" = x,
            "igraph" = as_ig(x[]),
            "networkDT" = as_dt(x[]),
            "unfiltered" = as_dt(x@unfiltered),
            "outputObj" = x@outputObj
        )
    })
    if (isTRUE(simplify)) out <- .simplify_list(out)
    out
})



#' @title Set spatial network
#' @name setSpatialNetwork
#' @description Function to set a spatial network
#' @inheritParams data_access_params
#' @param x spatialNetworkObj or list of spatialNetworkObj to set. Passing NULL
#' removes a specified set of spatial network information from the gobject.
#' @param name name of spatial network
#' @param provenance provenance name
#' @param verbose be verbose
#' @returns giotto object
#' @family spatial network data accessor functions
#' @family functions to set data in giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' spatnet <- getSpatialNetwork(g)
#'
#' setSpatialNetwork(gobject = g, x = spatnet)
#' @export
setGeneric("setSpatialNetwork",
    function(gobject, x, spat_unit = NULL, name = NULL, ...)
        standardGeneric("setSpatialNetwork"))

#' @rdname setSpatialNetwork
#' @export
setMethod("setSpatialNetwork", signature("giotto"), function(gobject,
    x,
    spat_unit = NULL,
    name = NULL,
    provenance = NULL,
    verbose = NULL,
    initialize = TRUE,
    ...) {
    if (!methods::hasArg(x)) {
        stop(wrap_txt("x param (data to set) must be given"))
    }

    # check hierarchical slots
    if (getOption("giotto.check_valid", TRUE)) {
        avail_sl <- list_spatial_locations(gobject)
        if (is.null(avail_sl)) {
            stop(wrap_txt("Add spatial location information first"))
        }
    }

    # Validate input type
    if (!inherits(x, c("spatialNetworkObj", "NULL", "list"))) {
        stop(wrap_txt(
            "Only spatialNetworkObj or lists of spatialNetworkObj accepted.
            For raw or external data, please first use readSpatNetData()"
        ))
    }

    # `nospec_*` are read from this frame by read_s4_nesting() to decide
    # whether to overwrite the subobject's nesting values with caller-supplied
    # ones, or vice versa.
    nospec_unit <- is.null(spat_unit)
    nospec_name <- is.null(name)

    # List input: validate items and iterate via self-recursion.
    if (inherits(x, "list")) {
        if (!all(vapply(x, inherits, "spatialNetworkObj",
                FUN.VALUE = logical(1L)))) {
            stop(wrap_txt(
                "Only spatialNetworkObj or lists of spatialNetworkObj
                accepted. For raw or external data, please first use
                readSpatNetData()"
            ))
        }
        base_args <- list(
            verbose = verbose,
            initialize = FALSE
        )
        if (!nospec_unit) base_args$spat_unit <- spat_unit
        if (!nospec_name) base_args$name <- name
        if (!is.null(provenance)) base_args$provenance <- provenance
        for (obj_i in seq_along(x)) {
            gobject <- do.call(setSpatialNetwork, c(
                list(gobject = gobject, x = x[[obj_i]]),
                base_args
            ))
        }
        if (isTRUE(initialize)) return(initialize(gobject))
        return(gobject)
    }

    # NULL: remove specified entry
    if (is.null(x)) {
        .vmsg_null_remove(verbose, "spatial network")
        gobject@spatial_network[[spat_unit]][[name]] <- NULL
        gobject@spatial_network <- .prune_nesting(
            gobject@spatial_network, spat_unit
        )

        if (isTRUE(initialize)) return(initialize(gobject))
        return(gobject)
    }

    # NOTE: read_s4_nesting modifies spat_unit / name / provenance in this
    # frame based on the nospec_* flags.
    x <- read_s4_nesting(x)

    # Notify on replacement
    potential_names <- list_spatial_networks_names(
        gobject = gobject,
        spat_unit = spat_unit
    )
    if (name %in% potential_names) {
        vmsg(.v = verbose,
            '> "', name,
            '" already exists and will be replaced with new
            spatial network'
        )
    }

    vmsg(.v = verbose,
        "Setting spatial network [", spatUnit(x), "] ",
        objName(x),
        sep = ""
    )

    # ensure legacy pre-0.6.0 schema migrates before storing
    x <- methods::initialize(x)

    # Write to disk if needed (see setNearestNetwork for rationale).
    # Always tagged "spatial" — the underlying graph construction
    # method (delaunay / kNN / Voronoi) is stored on the
    # spatialNetworkObj's @method slot, not the parquetEdgeStore.
    if (!is.null(gobject@source)) {
        gsrc <- .gsource(gobject)
        if (!inherits(x@network, "dataStore")) {
            store <- GiottoDisk::sourceWrite(gsrc, x@network,
                type = "spatial")
            x@network <- store
        }
    }

    slot(gobject, "spatial_network")[[spat_unit]][[name]] <- x
    if (isTRUE(initialize)) return(initialize(gobject))
    gobject
})




## spatial grid slot ####

#' @title Get spatial grid
#' @name getSpatialGrid
#' @description Function to get spatial grid
#' @inheritParams data_access_params
#' @param name name of spatial grid
#' @param return_grid_Obj return grid object (default = FALSE)
#' @returns spatialGridObj
#' @family spatial grid data accessor functions
#' @family functions to get data from giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' g <- createSpatialGrid(g, sdimx_stepsize = 5, sdimy_stepsize = 5)
#'
#' getSpatialGrid(g)
#' @export
getSpatialGrid <- function(gobject,
    spat_unit = NULL,
    feat_type = NULL,
    name = NULL,
    return_grid_Obj = FALSE,
    set_defaults = TRUE) {
    if (isTRUE(set_defaults)) {
        .set_default_nesting(gobject, spat_unit, feat_type)
    }

    # **To be deprecated** - check for old nesting
    if (is.null(
        names(gobject@spatial_grid[[spat_unit]][[feat_type]])[[1]]
    )) {
        # If gobject has nothing for this feat_type
        available <- list_spatial_grids(gobject,
            spat_unit = spat_unit
        )
        if (nrow(available) > 0 && is.null(available$feat_type)) {
            # If ANY old nesting objects are discovered (only reports old
            # nestings if any detected)
            if (is.null(name)) {
                gridObj <- gobject@spatial_grid[[
                    spat_unit
                ]][[available$name[[1]]]]
            } else {
                gridObj <- gobject@spatial_grid[[spat_unit]][[name]]
            }
            if (inherits(gridObj, "spatialGridObj")) {
                if (is.null(name)) {
                    message('The grid name was not specified, default to the
                            first: "', available$name[[1]], '"')
                }
                # S3 backwards compatibility
                if (!isS4(gridObj)) gridObj <- S3toS4spatialGridObj(gridObj)
                silent <- validObject(gridObj)
                # variable used to hide TRUE print

                gridDT <- slot(gridObj, "gridDT")
                if (return_grid_Obj == TRUE) {
                    return(gridObj)
                } else {
                    return(gridDT)
                }
            } else {
                stop(
                    'There is currently no spatial grid created for spatial
                    unit: "', spat_unit, '" and feature type "', feat_type,
                    '". First run createSpatialGrid()'
                )
            }
        }
    } # ** deprecation end**

    # Automatically select first grid for given spat_unit/feat_type combination
    if (is.null(name)) {
        name <- names(
            slot(gobject, "spatial_grid")[[spat_unit]][[feat_type]]
        )[[1]]
        message(
            'The grid name was not specified, default to the first: "',
            name, '"'
        )
    } else if (!is.element(name, names(slot(gobject, "spatial_grid")[[spat_unit]][[feat_type]]))) {
        message <- sprintf("spatial grid %s has not been created. Returning
                        NULL. check which spatial grids exist with
                        showGiottoSpatGrids()\n", name)
        warning(message)
        return(NULL)
    }

    # Get spatialGridObj
    gridObj <- slot(gobject, "spatial_grid")[[spat_unit]][[feat_type]][[name]]

    # S3 backwards compatibility
    if (!isS4(gridObj)) gridObj <- S3toS4spatialGridObj(gridObj)
    silent <- validObject(gridObj) # variable used to hide TRUE print

    gridDT <- slot(gridObj, "gridDT")

    if (return_grid_Obj == TRUE) {
        return(gridObj)
    } else {
        return(gridDT)
    }
}



#' @title Set spatial grid
#' @name setSpatialGrid
#' @description Function to set a spatial grid
#' @inheritParams data_access_params
#' @param spatial_grid spatial grid object
#' @param name name of spatial grid
#' @param verbose be verbose
#' @returns giotto object
#' @family spatial grid data accessor functions
#' @family functions to set data in giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' g <- createSpatialGrid(g, sdimx_stepsize = 5, sdimy_stepsize = 5)
#' sg <- getSpatialGrid(g, return_grid_Obj = TRUE)
#'
#' setSpatialGrid(gobject = g, spatial_grid = sg)
#' @export
setSpatialGrid <- function(gobject,
    spatial_grid,
    spat_unit = NULL,
    feat_type = NULL,
    name = NULL,
    verbose = NULL,
    set_defaults = TRUE,
    ...) {
    if (isTRUE(set_defaults)) {
        .set_default_nesting(gobject, spat_unit, feat_type)
    }

    if (is.null(spatial_grid)) {
        vmsg(.v = verbose,
            "NULL passed to spatial_grid. Removing specified entry."
        )
        gobject@spatial_grid[[spat_unit]][[feat_type]][[name]] <- NULL
        return(gobject)
    }

    if (!inherits(spatial_grid, "spatialGridObj")) {
        stop("spatial_grid must be a spatialGridObj")
    }

    spatial_grid <- read_s4_nesting(spatial_grid)

    potential_names <- names(
        slot(gobject, "spatial_grid")[[spat_unit]][[feat_type]]
    )
    if (name %in% potential_names) {
        vmsg(.v = verbose,
            '> "', name,
            '" already exists and will be replaced with new spatial grid \n'
        )
    }

    silent <- validObject(spatial_grid) # hide TRUE print

    slot(gobject, "spatial_grid")[[spat_unit]][[feat_type]][[name]] <-
        spatial_grid

    gobject
}

## polygon cell info ####

#' @title Get polygon info
#' @name getPolygonInfo
#' @description Get giotto polygon spatVector
#' @param gobject giotto object
#' @param name name of polygons. NULL (default) selects "cell" when available,
#' otherwise the first available
#' @param polygon_overlap include polygon overlap information
#' @param return_giottoPolygon (Defaults to FALSE) Return as giottoPolygon
#' S4 object
#' @param verbose be verbose
#' @param simplify logical. Whether or not to take object out of a list when
#' there is a length of 1.
#' @param polygon_name deprecated. Use \code{name}
#' @returns spatVector
#' @family polygon info data accessor functions
#' @family functions to get data from giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' getPolygonInfo(g)
#' @export
setGeneric("getPolygonInfo",
    function(gobject, name = NULL, ...)
        standardGeneric("getPolygonInfo"))

#' @rdname getPolygonInfo
#' @export
setMethod("getPolygonInfo", signature("giotto"), function(gobject,
    name = NULL,
    polygon_overlap = NULL,
    return_giottoPolygon = FALSE,
    verbose = TRUE,
    simplify = TRUE,
    ...,
    polygon_name = deprecated()) {
    name <- GiottoUtils::deprecate_param(polygon_name, name,
        fun = "getPolygonInfo", when = "0.5.2"
    )
    slotdata <- slot(gobject, "spatial_info")
    potential_names <- names(slotdata)

    if (is.null(potential_names)) {
        stop("Giotto object contains no polygon information")
    }

    if (is.null(name)) {
        if ("cell" %in% potential_names) {
            # Default to 'cell' if available
            name <- "cell"
        } else {
            # Otherwise the first available
            name <- potential_names[1]
            vmsg(.v = verbose, sprintf(
                "No polygon information named 'cell' discovered.
                Selecting first available ('%s')", name
            ))
        }
    }

    all_p <- identical(name, ":all:")
    missing_p <- name[!name %in% potential_names]
    if (length(missing_p) > 0L && !all_p) {
        stop(wrap_txtf(
            "No polygon information with name(s): '%s'",
            paste(missing_p, collapse = "', '"),
            errWidth = TRUE
        ), call. = FALSE)
    }

    if (!all_p) slotdata <- slotdata[name]

    names(slotdata) <- NULL
    out <- lapply(slotdata, function(x) {
        if (isTRUE(return_giottoPolygon)) return(x)
        if (!is.null(polygon_overlap)) {
            ovlp_data <- slot(x, "overlaps")
            potential_overlaps <- names(ovlp_data)
            if (!polygon_overlap %in% potential_overlaps) {
                stop(wrap_txtf(
                    "There is no polygon overlap information with name",
                    polygon_overlap,
                    errWidth = TRUE
                ), call. = FALSE)
            }
            return(ovlp_data[[polygon_overlap]])
        }
        x[] # poly geom
    })
    if (isTRUE(simplify)) out <- .simplify_list(out)
    out
})




#' @title Set polygon info
#' @name setPolygonInfo
#' @description Set polygon information into Giotto object
#' @inheritParams data_access_params
#' @param x single object or named list of objects to set as polygon
#' information (see details)
#' @param name (optional, character) name to assign to polygon and spatial unit
#' that polygon might define. Only used for single giottoPolygon objects. Names
#' are taken from a named list for multiple polygons. NULL (default) takes the
#' name from \code{x}, falling back to "cell" when \code{x} is unnamed
#' @param centroids_to_spatlocs if centroid information is discovered, whether
#' to additionally set them as a set of spatial locations (default = FALSE)
#' @param verbose be verbose
#' @details Inputs can be provided as either single objects or named lists of
#' objects. If the list is not named, then a generic name of the template
#' 'cell_i' will be applied. \cr
#' If an input is a character string, then it is assumed that it is a
#' filepath. \cr
#' For required formatting when reading tabular data or objects, see
#' \code{\link{createGiottoPolygonsFromDfr}} details.
#' @returns giotto object
#' @family polygon info data accessor functions
#' @family functions to set data in giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#' polyinfo <- getPolygonInfo(g, return_giottoPolygon = TRUE)
#'
#' setPolygonInfo(gobject = g, x = polyinfo)
#' @export
setGeneric("setPolygonInfo",
    function(gobject, x, name = NULL, ...)
        standardGeneric("setPolygonInfo"))

#' @rdname setPolygonInfo
#' @export
setMethod("setPolygonInfo", signature("giotto"), function(gobject,
    x,
    name = NULL,
    centroids_to_spatlocs = FALSE,
    verbose = NULL,
    initialize = TRUE,
    ...) {
    checkmate::assert_flag(centroids_to_spatlocs)

    if (!methods::hasArg(x)) {
        stop(wrap_txt("x param (data to be set) must be given"))
    }

    # Validate input type
    .validate_throw_err <- function() {
        stop(wrap_txt("Only giottoPolygon or lists of giottoPolygon accepted.
            For raw or external data, please first use readPolygonData()",
            errWidth = TRUE
        ), call. = FALSE)
    }
    if (!inherits(x, c("giottoPolygon", "NULL", "list"))) {
        .validate_throw_err()
    }

    # `nospec_name` is read by read_s4_nesting() to decide whether to override
    # the subobject's @name with the caller-supplied `name`.
    nospec_name <- is.null(name)

    # List input: validate items and iterate. Only forward `name` if the user
    # supplied it, so each subobj's own @name is honored by read_s4_nesting().
    if (inherits(x, "list")) {
        if (!all(vapply(x, inherits, "giottoPolygon", FUN.VALUE = logical(1L)))) {
            .validate_throw_err()
        }
        base_args <- list(
            centroids_to_spatlocs = centroids_to_spatlocs,
            verbose = verbose,
            initialize = FALSE
        )
        if (!nospec_name) base_args$name <- name
        for (obj_i in seq_along(x)) {
            gobject <- do.call(setPolygonInfo, c(
                list(gobject = gobject, x = x[[obj_i]]),
                base_args
            ))
        }
        if (isTRUE(initialize)) return(initialize(gobject))
        return(gobject)
    }

    # NULL: remove specified entry
    if (is.null(x)) {
        .vmsg_null_remove(verbose, "polygon information")
        gobject@spatial_info[[name]] <- NULL
        gobject@spatial_info <- .prune_nesting(
            gobject@spatial_info, character(0L)
        )

        if (isTRUE(initialize)) return(initialize(gobject))
        return(gobject)
    }

    # Single giottoPolygon
    # NOTE: read_s4_nesting modifies `name` in this frame based on
    # nospec_name (subobj's @name wins when caller didn't supply one).
    x <- read_s4_nesting(x)
    # Fallback for the rare case where neither the caller nor the
    # subobject supplied a name (objName(x) is NA).
    if (is.null(name)) name <- "cell"

    # Notify on replacement
    if (name %in% names(gobject@spatial_info)) {
        vmsg(.v = verbose, sprintf(
            "> '%s' already exists and will be replaced", name
        ))
    }

    vmsg(.v = verbose, sprintf("Setting polygon info [%s]", objName(x)))
  
    # Write to disk if needed
    if (!is.null(gobject@source)) {
        gsrc <- .gsource(gobject)
        if (!inherits(x[], "dataStore")) {
            store <- GiottoDisk::sourceWrite(gsrc, x[])
            x[] <- store
        }
    }
    gobject@spatial_info[[name]] <- x
  
    # Attach centroids as spatLocsObj if requested. Defer the gobject's
    # `initialize` until after the centroids are written so the parent
    # state stays consistent through both writes.
    if (centroids_to_spatlocs) {
        spatlocs <- .ctrs_to_spatlocs(x[],
            name = "raw",
            spat_unit = x@name, # tag same spat_unit as poly
            provenance = x@name,
            misc = NULL
        )
        gobject <- setSpatialLocations(gobject, spatlocs,
            verbose = verbose,
            initialize = FALSE
        )
    }

    if (isTRUE(initialize)) return(initialize(gobject))
    gobject
})
  
.ctrs_to_spatlocs <- function(x, ...) {
    sldf <- x[, "poly_ID"] |>
        centroids() |>
        as.data.frame(geom = "XY")
    data.table::setDT(sldf)
    data.table::setnames(sldf,
        old = c("poly_ID", "x", "y"),
        new = c("cell_ID", "sdimx", "sdimy")
    )
    createSpatLocsObj(sldf, ...)
}



## feature info ####

#' @title Get feature info
#' @name getFeatureInfo
#' @description Get giotto points spatVector
#' @inheritParams data_access_params
#' @param return_giottoPoints return as a giottoPoints object
#' @param simplify logical. Whether or not to take object out of a list when
#' there is a length of 1.
#' @returns giotto points spatVector
#' @family feature info data accessor functions
#' @family functions to get data from giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' getFeatureInfo(g)
#' @export
setGeneric("getFeatureInfo",
    function(gobject, feat_type = NULL, ...)
        standardGeneric("getFeatureInfo"))

#' @rdname getFeatureInfo
#' @export
setMethod("getFeatureInfo", signature("giotto"), function(gobject,
    feat_type = NULL,
    return_giottoPoints = FALSE,
    set_defaults = TRUE,
    simplify = TRUE) {
    if (isTRUE(set_defaults)) {
        feat_type <- set_default_feat_type(
            gobject = gobject,
            spat_unit = NULL,
            feat_type = feat_type
        )
    }

    slotdata <- slot(gobject, "feat_info")
    potential_names <- names(slotdata)

    if (is.null(potential_names)) {
        stop("Giotto object contains no feature point information",
            call. = FALSE
        )
    }

    all_fi <- identical(feat_type, ":all:")
    missing_p <- feat_type[!feat_type %in% potential_names]
    if (length(missing_p) > 0L && !all_fi) {
        stop(wrap_txtf(
            "No feature point information with name '%s'",
            paste(missing_p, collapse = "', '"),
            errWidth = TRUE
        ), call. = FALSE)
    }

    if (!all_fi) slotdata <- slotdata[feat_type]

    names(slotdata) <- NULL
    out <- lapply(slotdata, function(x) {
        if (isTRUE(return_giottoPoints)) return(x)
        x[] # spatVector
    })
    if (isTRUE(simplify)) out <- .simplify_list(out)
    out
})





#' @title Set feature info
#' @name setFeatureInfo
#' @description Set giotto polygon spatVector for features
#' @inheritParams data_access_params
#' @param x giottoPoints object or list of giottoPoints to set. Passing NULL
#' will remove the specified giottoPoints object from the giotto object
#' @param verbose be verbose
#' @returns giotto object
#' @family feature info data accessor functions
#' @family functions to set data in giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#' featinfo <- getFeatureInfo(g, return_giottoPoints = TRUE)
#'
#' setFeatureInfo(gobject = g, x = featinfo)
#' @export
setGeneric("setFeatureInfo",
    function(gobject, x, feat_type = NULL, ...)
        standardGeneric("setFeatureInfo"))

#' @rdname setFeatureInfo
#' @export
setMethod("setFeatureInfo", signature("giotto"), function(gobject, x,
    feat_type = NULL,
    verbose = NULL,
    initialize = TRUE,
    ...) {
    if (!methods::hasArg(x)) {
        stop(wrap_txt("x param (data to set) must be given"))
    }

    # `nospec_feat` is read from this frame by read_s4_nesting() to decide
    # whether to overwrite the subobject's feat_type with the caller's value.
    nospec_feat <- is.null(feat_type)

    # Validate input type
    .validate_throw_err <- function() {
        stop(wrap_txt("Only giottoPoints or lists of giottoPoints accepted.
            For raw or external data, please first use readFeatureInfo()"),
        call. = FALSE)
    }
    point_gclasses <- c("giottoPoints", "giottoBinPoints")
    accepted_inputs <- c(point_gclasses, "NULL", "list")
    if (!inherits(x, accepted_inputs)) {
        .validate_throw_err()
    }

    # List input: validate items and iterate via self-recursion
    if (inherits(x, "list")) {
        if (!all(vapply(x, inherits, point_gclasses, 
            FUN.VALUE = logical(1L)))) {
            .validate_throw_err()
        }
        for (obj_i in seq_along(x)) {
            gobject <- setFeatureInfo(
                gobject = gobject,
                x = x[[obj_i]],
                feat_type = feat_type,
                verbose = verbose,
                initialize = FALSE
            )
        }
        if (isTRUE(initialize)) return(initialize(gobject))
        return(gobject)
    }

    # NULL: remove specified feat_info entry
    if (is.null(x)) {
        vmsg(.v = verbose, "NULL passed to x.
            Removing specified feature information."
        )
        gobject@feat_info[[feat_type]] <- NULL
        if (length(gobject@feat_info) == 0L) gobject@feat_info <- NULL
        if (isTRUE(initialize)) return(initialize(gobject))
        return(gobject)
    }

    # Single giottoPoints / giottoBinPoints
    # read_s4_nesting() reads nospec_feat from this frame and writes feat_type
    # back if it was unset, or updates the subobject's feat_type otherwise.
    x <- read_s4_nesting(x)

    # Drop empty gpoints (e.g. split_keyword with no matches). Avoids creating
    # placeholder feat_info / feat_metadata entries that `initialize()` would
    # otherwise propagate.
    if (.gpoints_is_empty(x)) {
        warning(wrap_txt(
            "Skipping empty giottoPoints (0 features) for feat_type:",
            feat_type
        ), call. = FALSE)
        if (isTRUE(initialize)) return(initialize(gobject))
        return(gobject)
    }

    # Notify on replacement
    if (feat_type %in% names(gobject@feat_info)) {
        vmsg(.v = verbose, sprintf(
            "> '%s' already exists and will be replaced", feat_type
        ))
    }
  
    vmsg(.v = verbose, sprintf(
        "Setting feature info [%s]", featType(x)
    ))
  
    # write to disk if needed
    #
    # giottoBinPoints is held back deliberately. It has no `[` payload
    # accessor (only character/logical/numeric indices), so `x[]` errors, and
    # even with one the vault would take `@spatial` only -- while `@counts`
    # holds the bulk. Worse, `@spatial` has to stay a terra SpatVector:
    # calculateOverlap() and crop() call terra::extract() / terra::relate() on
    # it directly, neither of which accepts a store. Storing it would break
    # the bin1 overlap workflow. Revisit once giottoBinPoints has a
    # store-backed representation for both slots.
    if (!is.null(gobject@source)) {
        gsrc <- .gsource(gobject)
        if (inherits(x, "giottoBinPoints")) {
            vmsg(.v = verbose, sprintf(
                "giottoBinPoints [%s] kept in memory; no disk representation",
                featType(x)
            ))
        } else if (!inherits(x[], "dataStore")) {
            store <- GiottoDisk::sourceWrite(gsrc, x[])
            x[] <- store
        }
    }

    gobject@feat_info[[feat_type]] <- x
    if (isTRUE(initialize)) return(initialize(gobject))
    gobject
})

# Detect a 0-feature giottoPoints. Cheap path via cached IDs; falls back to
# nrow when the cache is empty/unpopulated (which may query disk for
# disk-backed representations).
#
# `unique_ID_cache` is a giottoPoints slot. giottoBinPoints reaches this from
# setFeatureInfo() too and has no such slot, so read it only when present and
# let that class take the nrow() path.
.gpoints_is_empty <- function(x) {
    if (is.null(x)) return(FALSE)
    cache <- if ("unique_ID_cache" %in% methods::slotNames(x)) {
        methods::slot(x, "unique_ID_cache")
    } else {
        character(0L)
    }
    if (length(cache) > 0L) return(FALSE)
    n <- try(nrow(x), silent = TRUE)
    if (inherits(n, "try-error")) return(TRUE)
    is.na(n) || n == 0L
}



## spatial enrichment slot ####


#' @title Get spatial enrichment
#' @name getSpatialEnrichment
#' @description Function to get a spatial enrichment data.table
#' @inheritParams data_access_params
#' @param name name of spatial enrichment results. Default "DWLS"
#' @returns spatEnrObj or data.table with fractions
#' @family spatial enrichment data accessor functions
#' @family functions to get data from giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' getSpatialEnrichment(g, spat_unit = "aggregate", name = "cluster_metagene")
#' @export
setGeneric("getSpatialEnrichment",
    function(gobject, spat_unit = NULL, feat_type = NULL, name = NULL, ...)
        standardGeneric("getSpatialEnrichment"))

#' @rdname getSpatialEnrichment
#' @export
setMethod("getSpatialEnrichment", signature("gAny"), function(gobject,
    spat_unit = NULL,
    feat_type = NULL,
    name = "DWLS",
    output = c("spatEnrObj", "data.table"),
    copy_obj = TRUE,
    set_defaults = TRUE) {
    output <- match.arg(output, choices = c("spatEnrObj", "data.table"))

    if (isTRUE(set_defaults)) {
        .set_default_nesting(gobject, spat_unit, feat_type)
    }

    # Fall back to first available result if no name supplied
    if (is.null(name)) {
        if (!is.null(gobject@spatial_enrichment)) {
            name <- list_spatial_enrichments_names(gobject,
                spat_unit = spat_unit,
                feat_type = feat_type
            )[[1]]
        } else {
            wrap_msg("No spatial enrichment results have been found")
            return(NULL)
        }
    }

    potential_names <- list_spatial_enrichments_names(gobject,
        spat_unit = spat_unit,
        feat_type = feat_type
    )

    if (is.null(potential_names)) {
        stop(wrap_txt(sprintf(
            "No spatial enrichments found for spat_unit: %s and feat_type: %s",
            spat_unit, feat_type
        )))
    }

    if (!name %in% potential_names) {
        stop(
            "The spatial enrichment result with name ", "'", name,
            "'", " can not be found \n"
        )
    }

    enr_res <- gobject@spatial_enrichment[[spat_unit]][[
        feat_type]][[name]]

    if (isTRUE(copy_obj)) enr_res[] <- data.table::copy(enr_res[])

    if (output == "spatEnrObj") return(enr_res)
    if (output == "data.table") return(enr_res[])
})







#' @title Set spatial enrichment
#' @name setSpatialEnrichment
#' @description Function to set a spatial enrichment slot
#' @inheritParams data_access_params
#' @param name name of spatial enrichment results. NULL (default) takes the
#' name from \code{x}, falling back to "enrichment" when \code{x} is unnamed
#' @param x spatEnrObj or list of spatEnrObj to set. Passing NULL will remove
#' a specified set of spatial enrichment information from the gobject.
#' @param provenance provenance information (optional)
#' @param verbose be verbose
#' @returns giotto object
#' @family spatial enrichment data accessor functions
#' @family functions to set data in giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#' spatenrich <- GiottoData::loadSubObjectMini("spatEnrObj")
#'
#' g <- setSpatialEnrichment(g, spatenrich)
#' @export
setGeneric("setSpatialEnrichment",
    function(gobject, x, spat_unit = NULL, feat_type = NULL, name = NULL, ...)
        standardGeneric("setSpatialEnrichment"))

#' @rdname setSpatialEnrichment
#' @export
setMethod("setSpatialEnrichment", signature("gAny"), function(gobject,
    x,
    spat_unit = NULL,
    feat_type = NULL,
    name = NULL,
    provenance = NULL,
    verbose = NULL,
    initialize = TRUE,
    ...) {
    if (!methods::hasArg(x)) {
        stop(wrap_txt("x param (data to set) must be given"))
    }

    # check hierarchical slots (spatial_locs check is giotto-only — giottoMulti
    # has no @spatial_locs slot; spatial info lives in @objects children)
    if (getOption("giotto.check_valid", TRUE)) {
        avail_ex <- list_expression(gobject)
        if (is.null(avail_ex)) {
            stop(wrap_txt("Add expression and spatial information first"))
        }
        if (inherits(gobject, "giotto")) {
            avail_sl <- list_spatial_locations(gobject)
            if (is.null(avail_sl)) {
                stop(wrap_txt("Add spatial location information first"))
            }
        }
    }

    # Validate input type
    if (!inherits(x, c("spatEnrObj", "NULL", "list"))) {
        stop(wrap_txt("Only spatEnrObj or lists of spatEnrObj accepted.
            For raw or external data, please first use readSpatEnrichData()"))
    }

    # `nospec_*` are read from this frame by read_s4_nesting() to decide
    # whether to overwrite the subobject's nesting values with caller-supplied
    # ones, or vice versa.
    nospec_unit <- is.null(spat_unit)
    nospec_feat <- is.null(feat_type)
    nospec_name <- is.null(name)

    # List input: validate items and iterate via self-recursion. Only forward
    # nesting args that the caller supplied.
    if (inherits(x, "list")) {
        if (!all(vapply(x, inherits, "spatEnrObj", FUN.VALUE = logical(1L)))) {
            stop(wrap_txt("Only spatEnrObj or lists of spatEnrObj accepted.
                For raw or external data, please first use readSpatEnrichData()"))
        }
        base_args <- list(
            verbose = verbose,
            initialize = FALSE
        )
        if (!nospec_unit) base_args$spat_unit <- spat_unit
        if (!nospec_feat) base_args$feat_type <- feat_type
        if (!nospec_name) base_args$name <- name
        if (!is.null(provenance)) base_args$provenance <- provenance
        for (obj_i in seq_along(x)) {
            gobject <- do.call(setSpatialEnrichment, c(
                list(gobject = gobject, x = x[[obj_i]]),
                base_args
            ))
        }
        if (isTRUE(initialize)) return(initialize(gobject))
        return(gobject)
    }

    # NULL: remove specified entry
    if (is.null(x)) {
        .vmsg_null_remove(verbose, "spatial enrichment")
        gobject@spatial_enrichment[[spat_unit]][[feat_type]][[name]] <- NULL
        gobject@spatial_enrichment <- .prune_nesting(
            gobject@spatial_enrichment, c(spat_unit, feat_type)
        )

        if (isTRUE(initialize)) return(initialize(gobject))
        return(gobject)
    }

    # NOTE: read_s4_nesting modifies spat_unit / feat_type / name /
    # provenance in this frame based on nospec_* flags.
    x <- read_s4_nesting(x)
    # Fallback for the rare case where neither the caller nor the
    # subobject supplied a name (objName(x) is NA).
    if (is.null(name)) name <- "enrichment"

    # Notify on replacement
    potential_names <- list_spatial_enrichments_names(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type
    )
    if (name %in% potential_names) {
        vmsg(.v = verbose,
            '> "', name,
            '" already exists and will be replaced with new spatial
                enrichment results'
        )
    }

    vmsg(.v = verbose,
        "Setting spatial enrichment [", spatUnit(x), "][",
        featType(x), "] ",
        objName(x),
        sep = ""
    )

    gobject@spatial_enrichment[[spat_unit]][[feat_type]][[name]] <- x
    if (isTRUE(initialize)) return(initialize(gobject))
    gobject
})




## all image slots ####


#' @title Get giotto image object
#' @name getGiottoImage
#' @description Get giotto one or more image objects from gobject
#' @param gobject giotto object
#' @param name character vector. Names giotto image object(s)
#' \code{\link{showGiottoImageNames}} to get
#' @returns a giotto image object
#' @family image data accessor functions
#' @family functions to get data from giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' getGiottoImage(gobject = g)
#' @export
setGeneric("getGiottoImage",
    function(gobject, name = NULL, ...)
        standardGeneric("getGiottoImage"))

#' @rdname getGiottoImage
#' @export
setMethod("getGiottoImage", signature("giotto"), function(gobject,
    name = NULL) {
    if (identical(name, ":all:")) {
        all_imgs <- gobject@images
        if (length(all_imgs) == 0L) all_imgs <- NULL
        return(all_imgs)
    }

    g_image_names <- list_images(gobject)$name
    if (is.null(g_image_names)) {
        stop("No images have been found \n")
    }

    if (is.null(name)) {
        name <- g_image_names[1L]
    }

    missing_names <- name[!name %in% g_image_names]
    if (length(missing_names) > 0) {
        stop(paste(missing_names, collapse = ", "), " not found in images.
            See showGiottoImageNames() \n")
    }

    g_img <- gobject@images[name]
    if (length(g_img) == 1) g_img <- g_img[[1L]]

    return(g_img)
})













#' @title Set giotto image object
#' @name setGiottoImage
#' @description Directly attach a giotto image to giotto object
#' @details \emph{\strong{Use with care!}} This function directly attaches
#' giotto image objects to the gobject without further modifications of
#' spatial positioning values within the image object that are generally
#' needed in order for them to plot in the correct location relative to the
#' other modalities of spatial data. \cr For the more general-purpose method
#' of attaching image objects, see \code{\link{addGiottoImage}}
#' @param gobject giotto object
#' @param x giotto image object to be attached without modification to the
#' giotto object
#' @param name name of giotto image object
#' @param verbose be verbose
#' @param image deprecated. Use \code{x}
#' @inheritParams data_access_params
#' @returns giotto object
#' @family image data accessor functions
#' @family functions to set data in giotto object
#' @seealso \code{\link{addGiottoImage}}
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#' gimg <- getGiottoImage(gobject = g)
#'
#' setGiottoImage(g, NULL, name = objName(gimg))
#' setGiottoImage(gobject = g, x = gimg)
#' @export
setGeneric("setGiottoImage",
    function(gobject, x, name = NULL, ...)
        standardGeneric("setGiottoImage"))

#' @rdname setGiottoImage
#' @export
setMethod("setGiottoImage", signature("giotto"), function(gobject,
    x,
    name = NULL,
    initialize = FALSE,
    verbose = NULL,
    ...,
    image = deprecated()) {
    x <- GiottoUtils::deprecate_param(image, x,
        fun = "setGiottoImage", when = "0.5.2"
    )
    if (is.null(x)) {
        if (!is.null(name)) { # image removal
            vmsg(.v = verbose, "NULL passed to `x` param
                removing specified image")
            gobject@images[[name]] <- x
            return(gobject)
        } else {
            stop("NULL passed to `x` param, but no specified `name`\n",
                call. = FALSE
            )
        }
    }

    if (!inherits(x, c("giottoImage", "giottoLargeImage"))) {
        stop(wrap_txt(
            "Unable to set non-giottoImage objects. Please ensure a
            giottoImage or giottoLargeImage is provided to this function.",
            errWidth = TRUE
        ))
    }

    # Default to name stored in object
    if (is.null(name)) name <- objName(x)

    # Find existing names
    potential_names <- list_images_names(gobject = gobject)

    if (name %in% potential_names) {
        vmsg(
            .v = verbose,
            sprintf("> image '%s' already exists and will be replaced", name)
        )
    }

    gobject@images[[name]] <- x
    return(gobject)
})



## spatValues getter ####

#' @name spatValues
#' @title Giotto object spatial values
#' @description
#' Retrieve specific values from the `giotto` object for a specific `spat_unit`
#' and `feat_type`. Values are returned as a data.table with the features
#' requested and a `cell_ID` column. This function may be updated in the future
#' to search in additional sets of information. To see the currently available
#' slot it checks, see details.
#' @param gobject `giotto` object
#' @param spat_unit character. spatial unit to check
#' @param feat_type character. feature type to check
#' @param feats character vector. One or more features or values to find within
#' the giotto object
#' @param expression_values character. (optional) Name of expression information
#' to use
#' @param spat_loc_name character. (optional) Name of spatial locations
#' information to use
#' @param spat_enr_name character. (optional) Name of spatial enrichments to
#' use
#' @param poly_info character. (optional) Name of polygons to use
#' @param dim_reduction_to_use character. (optional) Which type of dimension
#' reduction to use
#' @param dim_reduction_name character. (optional) Name of dimension reduction
#' to use
#' @param svkey use a `svkey`. Other params will be ignored. This is just
#' syntactic sugar for `svkey@get(gobject)`
#' @param verbose verbosity
#' @param debug logical. (default = FALSE) See details.
#' @returns A data.table with a cell_ID column and whichever feats were
#' requested
#' @details
#' **\[search\]**\cr
#' spatValues searches through the set of available information within the
#' `giotto` object for matches to `feats`. The current search order is
#' \enumerate{
#'   \item{cell expression}
#'   \item{cell metadata}
#'   \item{spatial locations}
#'   \item{spatial enrichment}
#'   \item{dimension reduction}
#'   \item{polygon info}
#' }
#' If a specific name for one of the types of information is provided via a
#' param such as `expression_values`, `spat_enr_name`, etc, then
#' the search will only be performed on that type of data.\cr\cr
#' **\[debug\]**\cr
#' This function uses Giotto's accessor functions which can usually throw errors
#' whenever a specific set of data or the features within that set do not
#' exist. This function muffles those errors, and only sends an error that the
#' data was not found when all getters fail. By setting `debug = TRUE`, you can
#' see the errors returned from each failed getter printed as messages for
#' easier debugging.
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#'
#' # expression
#' spatValues(g, spat_unit = "aggregate", feats = c("Mlc1", "Gfap"))
#' spatValues(g,
#'     spat_unit = "aggregate", feats = c("Mlc1", "Gfap"),
#'     expression_values = "normalized"
#' )
#'
#' # spatial enrichment
#' spatValues(g, spat_unit = "aggregate", feats = c("1", "3"))
#'
#' # polygon info
#' spatValues(g, spat_unit = "aggregate", feats = c("agg_n", "valid"))
#'
#' # cell meta
#' spatValues(g, spat_unit = "aggregate", feats = c("nr_feats"))
#'
#' @export
spatValues <- function(gobject,
    feats,
    spat_unit = NULL,
    feat_type = NULL,
    expression_values = NULL,
    spat_loc_name = NULL,
    spat_enr_name = NULL,
    poly_info = NULL,
    dim_reduction_to_use = NULL,
    dim_reduction_name = NULL,
    svkey = NULL,
    verbose = NULL,
    debug = FALSE) {
    checkmate::assert_class(gobject, "giotto")
    if (!is.null(svkey)) {
        checkmate::assert_class(svkey, "svkey")
        return(svkey@get(gobject))
    }
    checkmate::assert_character(feats)

    a <- get_args_list()

    # defaults
    .set_default_nesting(gobject, spat_unit, feat_type)

    # multi spat_unit access
    if (length(spat_unit) > 1) {
        dt_list <- lapply(spat_unit, function(spat) {
            a$spat_unit <- spat
            res <- do.call(spatValues, args = a)
            res[, spat_unit := spat]
        })
        combtable <- Reduce(rbind, dt_list)
        data.table::setcolorder(combtable, c("cell_ID", "spat_unit"))
        return(combtable)
    }


    # checker closures ------------------------------------------------- #
    check_expr <- function(vals) { # %%%%%%%%%%%%%%%%%%%%% EXPR %%%%%%
        if (!is.null(vals)) {
            return(vals)
        }
        e <- getExpression(
            gobject = gobject,
            spat_unit = spat_unit,
            feat_type = feat_type,
            values = expression_values,
            set_defaults = TRUE, # try to guess name if needed
            output = "exprObj"
        )
        if (is.null(e)) {
            return(NULL)
        }
        if (all(feats %in% featIDs(e))) {
            vals <- e[][feats, , drop = FALSE] |>
                t_flex() |>
                methods::as("dgCMatrix") |>
                as.matrix() |>
                data.table::as.data.table(keep.rownames = TRUE)
            data.table::setnames(vals, old = "rn", new = "cell_ID")
            vmsg(
                .v = verbose,
                sprintf(
                    "Getting values from [%s][%s][%s] expression",
                    spatUnit(e), featType(e), objName(e)
                )
            )
            return(vals)
        }
        return(NULL)
    }
    check_cellmeta <- function(vals) { # %%%%%%%%%%%% CELL META %%%%%%
        if (!is.null(vals)) {
            return(vals)
        }
        cx <- getCellMetadata(
            gobject = gobject,
            spat_unit = spat_unit,
            feat_type = feat_type,
            output = "cellMetaObj",
            copy_obj = FALSE,
            set_defaults = FALSE
        )
        if (is.null(cx)) {
            return(NULL)
        }
        if (all(feats %in% colnames(cx))) {
            vals <- cx[][, unique(c("cell_ID", feats)), with = FALSE]
            vmsg(
                .v = verbose,
                sprintf(
                    "Getting values from [%s][%s] cell metadata",
                    spatUnit(cx), featType(cx)
                )
            )
            return(vals)
        }
        return(NULL)
    }
    check_spatloc <- function(vals) { # %%%%%%%%%%%%%% SPAT LOC %%%%%%
        if (!is.null(vals)) {
            return(vals)
        }
        sl <- getSpatialLocations(
            gobject = gobject,
            spat_unit = spat_unit,
            name = spat_loc_name,
            output = "spatLocsObj",
            copy_obj = FALSE,
            set_defaults = TRUE # try to guess name
        )
        if (is.null(sl)) {
            return(NULL)
        }
        if (all(feats %in% colnames(sl[]))) {
            vals <- sl[][, unique(c("cell_ID", feats)), with = FALSE]
            vmsg(
                .v = verbose,
                sprintf(
                    "Getting values from [%s][%s] spatial locations",
                    spatUnit(sl), objName(sl)
                )
            )
            return(vals)
        }
        return(NULL)
    }
    check_spatenr <- function(vals) { # %%%%%%%%%%%%%% SPAT ENR %%%%%%
        if (!is.null(vals)) {
            return(vals)
        }
        enr <- getSpatialEnrichment(
            gobject = gobject,
            spat_unit = spat_unit,
            feat_type = feat_type,
            name = spat_enr_name,
            output = "spatEnrObj",
            copy_obj = FALSE,
            set_defaults = TRUE # try to guess name
        )
        if (is.null(enr)) {
            return(NULL)
        }
        if (all(feats %in% colnames(enr[]))) {
            vals <- enr[][, unique(c("cell_ID", feats)), with = FALSE]
            vmsg(
                .v = verbose,
                sprintf(
                    "Getting values from [%s][%s][%s] spatial enrichment",
                    spatUnit(enr), featType(enr), objName(enr)
                )
            )
            return(vals)
        }
        return(NULL)
    }
    check_dimred <- function(vals) { # %%%%%%%%%%%%%% DIM RED %%%%%%
        if (!is.null(vals)) {
            return(vals)
        }
        dr <- getDimReduction(
            gobject = gobject,
            spat_unit = spat_unit,
            feat_type = feat_type,
            reduction = "cells",
            name = dim_reduction_name,
            reduction_method = dim_reduction_to_use,
            output = "dimObj",
            set_defaults = TRUE # try to guess reduc. method and name
        )
        if (is.null(dr)) {
            return(NULL)
        }
        if (all(feats %in% colnames(dr[]))) {
            vals <- dr[][, feats, drop = FALSE] |>
                as.matrix() |>
                data.table::as.data.table(keep.rownames = TRUE)
            data.table::setnames(vals, old = "rn", new = "cell_ID")
            vmsg(
                .v = verbose,
                sprintf(
                    "Getting values from [%s][%s][%s][%s] dim reduction",
                    spatUnit(dr), featType(dr),
                    dr@reduction_method, objName(dr)
                )
            )
            return(vals)
        }
        return(NULL)
    }
    check_polyinfo <- function(vals) { # %%%%%%%%%%%% POLY INFO %%%%%%
        if (!is.null(vals)) {
            return(vals)
        }
        p <- getPolygonInfo(
            gobject = gobject,
            name = spat_unit,
            return_giottoPolygon = TRUE,
            verbose = FALSE
        )
        if (is.null(p)) {
            return(NULL)
        }
        sv <- p[]
        if (all(feats %in% names(sv))) {
            vals <- data.table::as.data.table(sv)
            data.table::setnames(vals, old = "poly_ID", new = "cell_ID")
            vmsg(
                .v = verbose,
                sprintf(
                    "Getting values from [%s] polygon info",
                    spatUnit(p)
                )
            )
            return(vals[, unique(c("cell_ID", feats)), with = FALSE])
        }
        return(NULL)
    }


    # [Getting the data]
    # Iterate through checks defined by `nextcheck` looking for the `feats`
    # desired.
    # If values of `feats` are not found after iterating through all
    # available checks, throw descriptive error.


    # set nextcheck if location is known -------------------------------- #
    nextcheck <- NULL
    if (!is.null(spat_enr_name)) nextcheck <- "spatial enrichment"
    if (!is.null(spat_loc_name)) nextcheck <- "spatial locations"
    if (!is.null(expression_values)) nextcheck <- "cell expression"
    if (!is.null(poly_info)) nextcheck <- "polygon info"
    if (!is.null(dim_reduction_name) || !is.null(dim_reduction_to_use)) {
        nextcheck <- "dimension reduction"
    }

    # set order of checks if location not known ------------------------- #
    if (is.null(nextcheck)) {
        nextcheck <- c(
            "cell expression",
            "cell metadata",
            "spatial locations",
            "spatial enrichment",
            "dimension reduction",
            "polygon info"
        )
    }


    # run check(s) ------------------------------------------------------ #
    vals <- NULL

    # requires error handling because the giotto accessors may normally throw
    # errors when the data you are looking for does not exist in the slot or
    # if the slot is simply empty.
    # Here we just silence those errors unless debug flag is TRUE.
    # Silenced errors pass NULL, triggering the loop to continue searching.
    err_handler <- function(fun, location) {
        qfun <- quote(fun)
        # eval(qfun)
        withRestarts(
            eval(qfun),
            muffleError = function() {
                invisible(NULL)
            }
        ) %>%
            withCallingHandlers(
                error = function(cond) {
                    if (isTRUE(debug)) {
                        message(sprintf(
                            "Caught an error at [%s]:\n%s\n",
                            location,
                            conditionMessage(cond)
                        ))
                    }
                    invokeRestart("muffleError")
                }
            )
    }

    for (data in nextcheck) {
        vals <- switch(data,
            "cell expression" = err_handler(check_expr(vals), data),
            "cell metadata" = err_handler(check_cellmeta(vals), data),
            "spatial enrichment" = err_handler(check_spatenr(vals), data),
            "polygon info" = err_handler(check_polyinfo(vals), data),
            "spatial locations" = err_handler(check_spatloc(vals), data),
            "dimension reduction" = err_handler(check_dimred(vals), data)
        )
    }



    if (is.null(vals)) {
        stop(wrap_txt(
            "features:", paste(feats, collapse = ", "), "not found in: ",
            paste(nextcheck, collapse = ", "),
            "for spat_unit", spat_unit, "and feat_type", feat_type
        ))
    }

    return(vals)
}


## svkey ####

#' @describeIn spatValues Create a `svkey` defining a `spatValues()` call
#' for deferred use. Handy for contexts where the full `giotto` object is not
#' available.
#' @export
svkey <- function(feats,
    spat_unit = NULL,
    feat_type = NULL,
    expression_values = NULL,
    spat_loc_name = NULL,
    poly_info = NULL,
    dim_reduction_to_use = NULL,
    dim_reduction_name = NULL,
    verbose = NULL) {
    if (missing(feats)) stop("'feats' to get must be provided", call. = FALSE)
    a <- get_args_list()
    svk <- do.call(new, c(list(Class = "svkey"), a))
    svk@get <- function(gobject) {
        spatValues(
            gobject = gobject,
            feats = svk@feats,
            spat_unit = svk@spat_unit,
            feat_type = svk@feat_type,
            expression_values = svk@expression_values,
            spat_loc_name = svk@spat_loc_name,
            poly_info = svk@poly_info,
            dim_reduction_to_use = svk@dim_reduction_to_use,
            dim_reduction_name = svk@dim_reduction_name,
            verbose = svk@verbose
        )
    }
    svk
}


# internals ####

.simplify_list <- function(x) {
    if (length(x) == 1L && inherits(x, "list")) {
        # if list of length 1, unlist
        return(x[[1L]])
    }
    return(x)
}

#' @include package_imports.R
#' @include classes-virtuals.R
#' @include classes-polygons.R
#' @include classes-points.R
#' @include classes-overlaps.R
#' @include classes-images.R
#' @include classes-utils.R
NULL











# CORE ####

## Giotto class ####


.versions_info <- function() {
    list(
        os_platform = get_os(),
        gclass = packageVersion("GiottoClass")
    )
}

.gversion <- function(gobject) {
    if (is.null(attr(gobject, "versions"))) { # apply default version 0.0.0
        return(as.package_version("0.0.0")) # untracked
    }
    gobject@versions$gclass
}

`.gversion<-` <- function(gobject, value) {
    gobject@versions$gclass <- value
    return(gobject)
}

.gsource <- function(gobject) {
    if (is.null(attr(gobject, "source"))) {
        return(NULL)
    }
    gobject@source
}

`.gsource<-` <- function(gobject, value) {
    if (!inherits(value, "gsource")) {
        stop("Backend source manager must extend class `gsource`\n", call. = FALSE)
    }
    gobject@source <- value
    gobject
}

#' @title Update giotto object
#' @name updateGiottoObject
#' @description Updates the giotto object for changes in structure for backwards
#' compatibility with earlier versions
#' @param gobject giotto object to update
#' @details
#' Supported updates:
#' \itemize{
#'   \item{3.2.0 update adding multiomics slot}
#'   \item{master branch to suite - TODO}
#' }
#' @returns giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' g <- updateGiottoObject(g)
#' @export
updateGiottoObject <- function(gobject) {
    if (!inherits(gobject, "giotto")) {
        stop(wrap_txt("This function is intended for updating giotto objects"))
    }

    # [Giotto versions (pre-modularization)] ----------------------------------#
    # 3.2.0 release adds multiomics slot
    if (is.null(attr(gobject, "multiomics"))) {
        attr(gobject, "multiomics") <- NA
        gobject@multiomics <- NULL
    }

    # 3.3.1 release adds h5_file slot
    if (is.null(attr(gobject, "h5_file"))) {
        attr(gobject, "h5_file") <- NA
        gobject@h5_file <- NULL
    }

    # ensure instructions are of correct type
    inst <- instructions(gobject)
    if (!inherits(inst, c("giottoInstructions", "NULL")) &&
        inherits(inst, "list")) {
        class(inst) <- c("giottoInstructions", "list")
        instructions(gobject, initialize = FALSE) <- inst
    }

    # [Switch to GiottoClass versioning] --------------------------------------#
    # GiottoClass 0.1.2 adds max_window and colors slots to giottoLargeImage
    # this update function has been moved to .update_image_slot() below

    # GiottoClass 0.1.4 supercedes @OS_platform with @versions slot
    if (!is.null(attr(gobject, "OS_platform"))) {
        attr(gobject, "OS_platform") <- NULL
    }
    if (is.null(attr(gobject, "versions"))) { # apply default version 0.0.0
        attr(gobject, "versions") <- .versions_info()
        gobject@versions$gclass <- "0.0.0" # untracked
    }

    # warn if gobject newer than package
    if (.gversion(gobject) >
        numeric_version(packageVersion("GiottoClass"))) {
        warning(
            call. = FALSE,
            sprintf(
                "This giotto object was created in a newer version of
                GiottoClass (v%s)",
                as.character(.gversion(gobject))
            )
        )
    }

    # [version-based updates] -------------------------------------------------#

    # GiottoClass 0.3.0 removes @largeImages slot
    if (.gversion(gobject) < numeric_version("0.3.0")) {
        gobject <- .update_image_slot(gobject)
    }

    # GiottoClass 0.1.2 image updates moved here
    # TODO remove in future update
    gobject@images <- lapply(gobject@images, .update_giotto_image)

    # GiottoClass 0.4.12 adds @misc slot
    if (.gversion(gobject) < numeric_version("0.4.12")) {
        attr(gobject, "misc") <- list()
    }
    
    # GiottoClass 0.5.1 adds @source slot
    if (.gversion(gobject) < "0.5.1") {
        gobject <- .update_source_slot(gobject)
    }

    # GiottoClass 0.6.0 renames spatNetData / nnData slots and switches
    # spatialNetworkObj storage to igraph (canonical). Migrate any
    # legacy serialized subobjects to the new schema + canonical form.
    if (.gversion(gobject) < "0.6.0") {
        gobject <- .update_network_slots(gobject)
    }

    # -------------------------------------------------------------------------#

    # subobject updates
    if (!is.null(attr(gobject, "feat_info"))) {
        info_list <- gobject[["feat_info"]]
        # update S4 object if needed
        info_list <- lapply(info_list, function(info) {
            try_val <- try(validObject(info), silent = TRUE)
            if (inherits(try_val, "try-error")) {
                info <- updateGiottoPointsObject(info)
            }
            return(info)
        })
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        gobject <- setFeatureInfo(
            gobject = gobject,
            x = info_list,
            verbose = FALSE,
            initialize = FALSE
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    }
    # GiottoClass 0.5.0: feat_ID_uniq on giottoPoints changes from string
    # (e.g. "gobject1-1") to sequential integer. Polygon overlap SpatVectors
    # referencing the old strings must be remapped using the gpoints SpatVector
    # as the authoritative lookup before updateGiottoPolygonObject converts them.
    if (.gversion(gobject) < numeric_version("0.5.0") &&
        !is.null(attr(gobject, "feat_info"))) {
        feat_uniq_maps <- list()
        for (ft in names(gobject@feat_info)) {
            gpts <- gobject@feat_info[[ft]]
            if (!inherits(gpts, "giottoPoints") ||
                is.null(gpts@spatVector)) next
            fid <- gpts@spatVector$feat_ID_uniq
            if (!is.character(fid)) next
            feat_uniq_maps[[ft]] <- setNames(
                seq_len(nrow(gpts@spatVector)), fid
            )
        }
        if (length(feat_uniq_maps) > 0L) {
            # remap overlap SpatVectors before they are converted
            if (!is.null(attr(gobject, "spatial_info"))) {
                for (poly_name in names(gobject@spatial_info)) {
                    gpoly <- gobject@spatial_info[[poly_name]]
                    if (is.null(gpoly@overlaps)) next
                    changed <- FALSE
                    for (ft in names(feat_uniq_maps)) {
                        ovlp <- gpoly@overlaps[[ft]]
                        if (!inherits(ovlp, "SpatVector")) next
                        ovlp$feat_ID_uniq <-
                            feat_uniq_maps[[ft]][ovlp$feat_ID_uniq]
                        gpoly@overlaps[[ft]] <- ovlp
                        changed <- TRUE
                    }
                    if (changed) gobject@spatial_info[[poly_name]] <- gpoly
                }
            }
            # reset gpoints feat_ID_uniq to sequential integers
            for (ft in names(feat_uniq_maps)) {
                gpts <- gobject@feat_info[[ft]]
                gpts@spatVector$feat_ID_uniq <- seq_len(nrow(gpts@spatVector))
                gobject@feat_info[[ft]] <- gpts
            }
        }
    }

    if (!is.null(attr(gobject, "spatial_info"))) {
        info_list <- gobject[["spatial_info"]]
        # update S4 object if needed
        info_list <- lapply(info_list, function(info) {
            try_val <- try(validObject(info), silent = TRUE)
            if (inherits(try_val, "try-error") ||
                .gversion(gobject) < "0.5.0") {
                info <- updateGiottoPolygonObject(info)
            }
            return(info)
        })
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        gobject <- setPolygonInfo(
            gobject = gobject,
            x = info_list,
            verbose = FALSE,
            centroids_to_spatlocs = FALSE,
            initialize = FALSE
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    }

    # set updated version number
    .gversion(gobject) <- packageVersion("GiottoClass")

    return(gobject)
}

# for updating pre-v0.3.0 objects
.update_image_slot <- function(x) {
    checkmate::assert_class(x, "giotto")
    # return early if no largeImages
    if (!methods::.hasSlot(x, "largeImages")) {
        return(x)
    }

    # transfer largeImages slot contents to images slot
    lgimg_list <- attr(x, "largeImages")

    # remove slot
    attr(x, "largeImages") <- NULL

    # if @largeImages was empty, expect `\001NULL\001` of class `name`
    # the object can be returned early now that @largeImages is stripped
    if (inherits(lgimg_list, "name")) {
        return(x)
    }

    # deal with same image and largeImage names
    lgnames <- names(lgimg_list)
    imgnames <- names(x@images)

    samename_bool <- imgnames %in% lgnames
    if (any(samename_bool)) {
        samenames <- imgnames[samename_bool]
        warning(wrap_txt(
            "GiottoClass v0.3.0 merges @images and @largeImages slots.
            image name(s):", paste(samenames, collapse = ", "),
            "\nare found in both slots.
            largeImages will be prioritized."
        ), call. = FALSE)

        # remove images with overlapped names
        x@images[samename_bool] <- NULL
    }

    x@images <- c(x@images, lgimg_list)

    return(x)
}

# for updating pre-0.5.1 objects
.update_source_slot <- function(x) {
    checkmate::assert_class(x, "giotto")
    attr(x, "source") <- list() # init slot
    x@source <- NULL
    x
}

# for updating pre-0.6.0 objects: slot renames on spatNetData / nnData
# plus DT -> igraph canonicalization of spatialNetworkObj content.
# Legacy data sits at attr() level under the OLD slot names because R's
# S4 deserialization preserves an object's stored representation even
# when the class definition has changed.
.update_network_slots <- function(gobject) {
    checkmate::assert_class(gobject, "giotto")
    sn_list <- gobject[["spatial_network"]]
    if (length(sn_list) > 0) {
        sn_list <- lapply(sn_list, .migrate_spatnet_obj)
        gobject <- setGiotto(gobject, sn_list, initialize = FALSE)
    }
    nn_list <- gobject[["nn_network"]]
    if (length(nn_list) > 0) {
        nn_list <- lapply(nn_list, .migrate_nn_net_obj)
        gobject <- setGiotto(gobject, nn_list, initialize = FALSE)
    }
    gobject
}

# True if `obj` already exposes `slot_name` via the new class def
# (legacy objects error on @-access of renamed slots).
.has_new_slot <- function(obj, slot_name) {
    tryCatch({
        slot(obj, slot_name)
        TRUE
    }, error = function(e) FALSE)
}

# Convert a legacy spatial-network data.table (with coord columns) into
# an undirected igraph carrying only distance/weight edge attributes.
.legacy_spatnet_dt_to_igraph <- function(dt) {
    if (is.null(dt) || !inherits(dt, "data.frame")) return(dt)
    cols <- intersect(c("from", "to", "distance", "weight"), names(dt))
    igraph::graph_from_data_frame(
        as.data.frame(dt)[, cols, drop = FALSE],
        directed = FALSE
    )
}

.migrate_spatnet_obj <- function(sn) {
    if (.has_new_slot(sn, "network")) return(sn) # idempotent
    old_net <- attr(sn, "networkDT", exact = TRUE)
    old_unf <- attr(sn, "networkDT_before_filter", exact = TRUE)
    create_spat_net_obj(
        name = sn@name,
        method = sn@method,
        parameters = sn@parameters,
        outputObj = sn@outputObj,
        network = .legacy_spatnet_dt_to_igraph(old_net),
        unfiltered = .legacy_spatnet_dt_to_igraph(old_unf),
        cellShapeObj = sn@cellShapeObj,
        crossSectionObjects = sn@crossSectionObjects,
        spat_unit = sn@spat_unit,
        provenance = sn@provenance,
        misc = sn@misc
    )
}

.migrate_nn_net_obj <- function(nn) {
    if (.has_new_slot(nn, "network")) return(nn) # idempotent
    old_ig <- attr(nn, "igraph", exact = TRUE)
    create_nn_net_obj(
        name = nn@name,
        nn_type = nn@nn_type,
        network = old_ig,
        spat_unit = nn@spat_unit,
        feat_type = nn@feat_type,
        provenance = nn@provenance,
        misc = nn@misc
    )
}



##### * Definition ####
# Giotto class
# ! Any slot modifications should also be reflected in packedGiotto class !

#' @title S4 giotto Class
#' @description Giotto's core object that encapsulates all the components
#' of a spatial-omic project and facilitates analyses.
#' @concept giotto object
#' @slot expression expression information
#' @slot expression_feat The different features or modalities such as rna,
#' protein, metabolites, ... that are provided in the expression slot.
#' @slot spatial_locs spatial location coordinates for cells/spots/grids
#' @slot spatial_info information about spatial units (Giotto spatVector)
#' @slot cell_metadata metadata for cells
#' @slot feat_metadata metadata for available features
#' @slot feat_info information about features (Giotto spatVector)
#' @slot cell_ID unique cell IDs
#' @slot feat_ID unique feature IDs for all features or modalities
#' @slot spatial_network spatial network in data.table/data.frame format
#' @slot spatial_grid spatial grid in data.table/data.frame format
#' @slot spatial_enrichment slot to save spatial enrichment-like results
#' @slot dimension_reduction slot to save dimension reduction coordinates
#' @slot nn_network nearest neighbor network in igraph format
#' @slot images slot to store giotto image objects
#' @slot parameters slot to save parameters that have been used
#' @slot instructions slot for global function instructions
#' @slot offset_file offset file used to stitch together image fields
#' @slot versions giotto object metadata and versioning info
#' @slot join_info information about joined Giotto objects
#' @slot multiomics multiomics integration results
#' @slot h5_file path to h5 file
#' @slot misc miscellaneous or unstructured data
#' @details
#'
#' \[**initialize**\]
#' The `giotto` class has a robust `initialize()` method that is automatically
#' called upon setting data into the object, updates of
#' the `giottoInstructions`, and loading of saved objects.
#' It performs the following steps:
#' 1. Update the object and subobjects for class definition changes if needed
#' 2. Ensure a set of `giottoInstructions` are available, otherwise generate
#' defaults
#' 3. Ensure a giotto python environment is accessible when the options
#'    giotto.has_conda and giotto.use_conda are TRUE
#' 4. Check the active spat_unit and feat_type
#' 5. Ensure spatial/cell ID consistency and initialize the cell_ID and feat_ID
#'    slots for the active spat_unit and feat_type, as well as cell and feature
#'    metadata if they do not exist. Values for IDs and metadata are pulled
#'    from any existing data in spatial_info/feat_info or expression slots,
#'    with a preference for the latter.
#' 6. Perform slot-specific and hierarchical checks that ensure dependent pieces
#'    of information are only added AFTER the data that they depend on and that
#'    existing information is consistent across slots.
#' 7. Object validity checking
#' @returns giotto object
#' @examples
#' giotto()
#' @export giotto
#' @exportClass giotto
giotto <- setClass(
    "giotto",
    contains = "gAny",
    slots = c(
        expression = "nullOrList",
        expression_feat = "nullOrChar",
        spatial_locs = "nullOrList",
        spatial_info = "nullOrList",
        cell_metadata = "nullOrList",
        feat_metadata = "nullOrList",
        feat_info = "nullOrList",
        cell_ID = "nullOrList",
        feat_ID = "nullOrList",
        spatial_network = "nullOrList",
        spatial_grid = "nullOrList",
        spatial_enrichment = "nullOrList",
        dimension_reduction = "nullOrList",
        nn_network = "nullOrList",
        images = "nullOrList",
        parameters = "ANY",
        instructions = "nullOrInstructions",
        offset_file = "ANY",
        versions = "list",
        join_info = "ANY",
        multiomics = "ANY",
        source = "ANY",
        h5_file = "ANY",
        misc = "list"
    ),
    prototype = list(
        expression = NULL,
        expression_feat = NULL,
        spatial_locs = NULL,
        spatial_info = NULL,
        cell_metadata = NULL,
        feat_metadata = NULL,
        feat_info = NULL,
        cell_ID = NULL,
        feat_ID = NULL,
        spatial_network = NULL,
        spatial_grid = NULL,
        spatial_enrichment = NULL,
        dimension_reduction = NULL,
        nn_network = NULL,
        images = NULL,
        parameters = list(),
        instructions = NULL,
        offset_file = NULL,
        versions = .versions_info(),
        join_info = NULL,
        multiomics = NULL,
        source = NULL,
        h5_file = NULL,
        misc = list()
    )

    # validity = check_giotto_obj
)


















# for use with wrap() generic
# not intended to be used until after unwrapped to giotto class
# does not inherit giotto to avoid any method inheritance
setClass(
    "packedGiotto",
    slots = c(
        packed_spatial_info = "ANY",
        packed_feat_info = "ANY",
        expression = "nullOrList",
        expression_feat = "ANY",
        spatial_locs = "ANY",
        cell_metadata = "ANY",
        feat_metadata = "ANY",
        cell_ID = "ANY",
        feat_ID = "ANY",
        spatial_network = "ANY",
        spatial_grid = "ANY",
        spatial_enrichment = "ANY",
        dimension_reduction = "ANY",
        nn_network = "ANY",
        images = "ANY",
        parameters = "ANY",
        instructions = "ANY",
        offset_file = "ANY",
        versions = "ANY",
        join_info = "ANY",
        multiomics = "ANY",
        h5_file = "ANY",
        misc = "list"
    ),
    prototype = list(
        packed_spatial_info = NULL,
        packed_feat_info = NULL,
        expression = NULL,
        expression_feat = NULL,
        spatial_locs = NULL,
        cell_metadata = NULL,
        feat_metadata = NULL,
        cell_ID = NULL,
        feat_ID = NULL,
        spatial_network = NULL,
        spatial_grid = NULL,
        spatial_enrichment = NULL,
        dimension_reduction = NULL,
        nn_network = NULL,
        images = NULL,
        parameters = NULL,
        instructions = NULL,
        offset_file = NULL,
        versions = NULL,
        join_info = NULL,
        multiomics = NULL,
        h5_file = NULL,
        misc = list()
    )
)







# EXPRESSION ####

## exprObj Class ####

## * Check ####
# exprObj Class

#' @title Check exprObj
#' @name .check_expr_obj
#' @description Check function for S4 exprObj
#' @param object S4 exprObj to check
#' @returns character or TRUE
#' @keywords internal
.check_expr_obj <- function(object) {
    errors <- character()

    # Check for expr info
    if (is.null(slot(object, "exprMat"))) {
        obj_info <- paste0(
            "exprObj ",
            'spat_unit "', slot(object, "spat_unit"), '", ',
            'feat_type "', slot(object, "feat_type"), '", ',
            'name "', slot(object, "name"), '": \n'
        )

        msg <- paste0(obj_info, "No expression information found.\n")
        errors <- c(errors, msg)
    }

    if (length(errors) == 0) TRUE else errors
}



## * Definition ####
# exprObj Class

#' @title S4 exprObj
#' @description Framework to store aggregated expression information
#' @slot name name of exprObj
#' @slot exprMat matrix of expression information
#' @slot spat_unit spatial unit of expression (e.g. 'cell')
#' @slot feat_type feature type of expression (e.g. 'rna', 'protein')
#' @slot provenance origin data of expression information (if applicable)
#' @slot misc misc
#' @returns exprObj
#' @examples
#' GiottoData::loadSubObjectMini("exprObj")
#' @exportClass exprObj
exprObj <- setClass("exprObj",
    contains = c(
        "nameData", "exprData", "spatFeatData", "miscData",
        "giottoSubobject"
    ),
    validity = .check_expr_obj
)











# METADATA ####


## cellMetaObj class ####

# * Check ####
#' @title Check cell metadata object
#' @name .check_cell_meta_obj
#' @description Function to check S4 cellMetaObj
#' @param object S4 cellMetaObj to check
#' @keywords internal
#' @returns character or TRUE
.check_cell_meta_obj <- function(object) {
    errors <- character()

    if (!"cell_ID" %in% colnames(object@metaDT)) {
        msg <- 'No "cell_ID" column found.'
        errors <- c(errors, msg)
    } else {
        if (!is.character(object@metaDT[["cell_ID"]])) {
            msg <- '"cell_ID" column must be of class character.'
            errors <- c(errors, msg)
        }

        if (colnames(object@metaDT)[[1]] != "cell_ID") {
            msg <- '"cell_ID" column should be the first column.'
            errors <- c(errors, msg)
        }
    }
    if (length(errors) == 0) TRUE else errors
}

# * Definition ####
#' @title S4 cellMetaObj
#' @description Framework to store cell metadata
#' @slot metaDT metadata info
#' @slot col_desc (optional) character vector describing columns of the metadata
#' @slot spat_unit spatial unit of aggregated expression (e.g. 'cell')
#' @slot feat_type feature type of aggregated expression (e.g. 'rna', 'protein')
#' @slot provenance origin data of aggregated expression
#' information (if applicable)
#' @returns cellMetaObj
#' @examples
#' GiottoData::loadSubObjectMini("cellMetaObj")
#' @exportClass cellMetaObj
cellMetaObj <- setClass("cellMetaObj",
    contains = c("metaData", "spatFeatData", "giottoSubobject"),
    validity = .check_cell_meta_obj
)




## featMetaObj class ####

# * Check ####
#' @title Check feature metadata object
#' @name .check_feat_meta_obj
#' @description Function to check S4 featMetaObj
#' @param object S4 featMetaObj to check
#' @keywords internal
#' @returns character or TRUE
.check_feat_meta_obj <- function(object) {
    errors <- character()

    if (!"feat_ID" %in% colnames(object@metaDT)) {
        msg <- 'No "feat_ID" column found.'
        errors <- c(errors, msg)
    } else {
        if (!is.character(object@metaDT[["feat_ID"]])) {
            msg <- '"feat_ID" column must be of class character.'
            errors <- c(errors, msg)
        }

        if (colnames(object@metaDT)[[1]] != "feat_ID") {
            msg <- '"feat_ID" column should be the first column.'
            errors <- c(errors, msg)
        }
    }
    if (length(errors) == 0) TRUE else errors
}

# * Definition ####
#' @title S4 featMetaObj
#' @description Framework to store feature metadata
#' @slot metaDT metadata info
#' @slot col_desc (optional) character vector describing columns of the metadata
#' @slot spat_unit spatial unit of aggregated expression (e.g. 'cell')
#' @slot feat_type feature type of aggregated expression (e.g. 'rna', 'protein')
#' @slot provenance origin data of aggregated expression
#' information (if applicable)
#' @returns featMetaObj
#' @examples
#' GiottoData::loadSubObjectMini("featMetaObj")
#' @exportClass featMetaObj
featMetaObj <- setClass("featMetaObj",
    contains = c("metaData", "spatFeatData", "giottoSubobject"),
    validity = .check_feat_meta_obj
)




# DIMENSION REDUCTION ####

## dimObj Class ####



##### * Check #####
# dimObj Class

#' @title Check dimObj
#' @name .check_dim_obj
#' @description check function for S4 dimObj
#' @param object S4 dimObj to check
#' @keywords internal
#' @returns character or TRUE
.check_dim_obj <- function(object) {
    errors <- character()
    length_reduction_method <- length(object@reduction_method)
    if (length_reduction_method > 1) {
        msg <- paste0(
            "reduction_method is length ", length_reduction_method,
            ". Should be 1"
        )
        errors <- c(errors, msg)
    }

    if (length_reduction_method == 0) {
        msg <- "A reduction_method must be given"
        errors <- c(errors, msg)
    }

    lastCols <- tail(colnames(object@coordinates), 2)
    col_dims <- all(grepl(pattern = "Dim.", x = lastCols))
    if (!isTRUE(col_dims)) {
        msg <- 'Dim reduction coordinates should be provided with dimensions
        ("Dim.#") as columns and samples as rows\n'
        errors <- c(errors, msg)
    }

    # This check applied using .check_dimension_reduction()
    # if(!inherits(rownames(object@coordinates, 'character'))) {
    #   msg = 'Dim reduction coordinate rownames must be character'
    #   errors = c(errors, msg)
    # }

    if (length(errors) == 0) TRUE else errors
}



## * Definition ####
# dimObj Class

#' @title S4 dimObj Class
#' @description Framework to store dimension reduction information
#' @slot name name of dimObject
#' @slot feat_type feature type of data
#' @slot spat_unit spatial unit of data
#' @slot provenance origin of aggregated information (if applicable)
#' @slot reduction whether reduction was performed on 'feats' or 'cells'
#' @slot reduction_method method used to generate dimension reduction
#' @slot coordinates embedding coordinates
#' @slot misc method-specific additional outputs
#' @returns dimObj
#' @examples
#' GiottoData::loadSubObjectMini("dimObj")
#' @exportClass dimObj
dimObj <- setClass("dimObj",
    contains = c("nameData", "spatFeatData", "giottoSubobject"),
    slots = c(
        reduction = "character",
        reduction_method = "character",
        coordinates = "ANY",
        misc = "ANY"
    ),
    prototype = list(
        reduction = NA_character_,
        reduction_method = NA_character_,
        coordinates = NULL,
        misc = NULL
    ),
    validity = .check_dim_obj
)






## * Additional functions ####
# dimObj Class

#' @title Dimension reductions
#' @name S3toS4dimObj
#' @description Convert S3 dimObj to S4
#' @param object S3 dimObj
#' @keywords internal
#' @returns S4 dimObj
S3toS4dimObj <- function(object) {
    if (!isS4(object)) {
        object <- new("dimObj",
            name = object$name,
            feat_type = object$feat_type,
            spat_unit = object$spat_unit,
            reduction_method = object$reduction_method,
            coordinates = object$coordinates,
            misc = object$misc
        )
    }
    object
}





## nnNetObj ####

## * Definition ####
# nnNetObj Class

#' @title S4 nnNetObj
#' @description Framework to store nearest neighbor network information
#' @slot name name of nnNetObj
#' @slot nn_type type of nearest neighbor network
#' @slot igraph igraph object containing network information
#' @slot feat_type feature type of data
#' @slot spat_unit spatial unit of data
#' @slot provenance origin of aggregated information (if applicable)
#' @slot misc misc
#' @returns nnNetObj
#' @examples
#' GiottoData::loadSubObjectMini("nnNetObj")
#' @exportClass nnNetObj
nnNetObj <- setClass("nnNetObj",
    contains = c(
        "nameData", "nnData", "spatFeatData", "miscData",
        "giottoSubobject"
    )
)

















# SPATIAL ####



## spatLocsObj Class ####

## * check ####
# spatLocsObj Class

#' @title Check spatLocsObj
#' @name .check_spat_locs_obj
#' @description Check function for S4 spatLocsObj
#' @param object S4 spatLocsObj to check
#' @keywords internal
#' @returns character or TRUE
.check_spat_locs_obj <- function(object) {
    errors <- character()

    if (!"sdimx" %in% colnames(slot(object, "coordinates"))) {
        msg <- 'Column "sdimx" for x spatial location was not found'
        errors <- c(errors, msg)
    }

    if (!"sdimy" %in% colnames(slot(object, "coordinates"))) {
        msg <- 'Column "sdimy" for y spatial location was not found'
        errors <- c(errors, msg)
    }

    # Allow .check_spatial_location_data() to compensate for missing cell_ID
    if (!"cell_ID" %in% colnames(slot(object, "coordinates"))) {
        msg <- 'Column "cell_ID" for cell ID was not found'
        errors <- c(errors, msg)
    }

    if (length(errors) == 0) TRUE else errors
}


## * definition ####
# spatLocsObj Class

#' @title S4 spatLocsObj Class
#' @description Framework to store spatial location information
#' @slot name name of spatLocsObj
#' @slot coordinates data.table of spatial coordinates/locations
#' @slot spat_unit spatial unit tag
#' @slot provenance origin of aggregated information (if applicable)
#' @returns spatLocsObj
#' @examples
#' GiottoData::loadSubObjectMini("spatLocsObj")
#' @exportClass spatLocsObj
spatLocsObj <- setClass("spatLocsObj",
    contains = c(
        "nameData", "coordDataDT", "spatData", "miscData",
        "giottoSubobject"
    ),
    validity = .check_spat_locs_obj
)














## spatialNetworkObj Class ####

### * check ####
# spatialNetworkObj Class

#' @title Check spatialNetworkObj
#' @name .check_spat_net_obj
#' @description Check function for S4 spatialNetworkObj
#' @param object S4 spatialNetworkObj to check
#' @keywords internal
#' @returns character or TRUE
.check_spat_net_obj <- function(object) {
    errors <- character()
    method_slot <- slot(object, "method")
    length_method <- length(method_slot)
    if (length_method > 1) {
        msg <- paste0("method is length ", length_method, ". Should be 1")
        errors <- c(errors, msg)
    }

    # if(is.null(method_slot)) {
    #   msg = 'A spatial network generation method must be given'
    #   errors = c(errors, msg)
    # }

    # An empty spatialNetworkObj is a valid intermediate state during
    # construction or migration from legacy schema. Substantive content
    # checks live in the constructors and the migration step in
    # updateGiottoObject().

    if (length(errors) == 0) TRUE else errors
}



### * definition ####
# spatialNetworkObj Class

#' @title S4 spatialNetworkObj Class
#' @description Framework to store spatial network information
#' @slot name name of spatialNetworkObj
#' @slot method method used to generate spatial network
#' @slot parameters additional method-specific parameters used during spatial
#' network generation
#' @slot outputObj network geometry object
#' @slot networkDT data.table of network connections, distances, and weightings
#' @slot networkDT_before_filter unfiltered data.table  of network connections,
#' distances, and weightings
#' @slot cellShapeObj network cell shape information
#' @slot crossSectionObjects crossSectionObjects
#' @slot spat_unit spatial unit tag
#' @slot provenance origin of aggregated information (if applicable)
#' @slot misc misc
#' @details The generic access operators work with the data within
#' the \code{networkDT}
#' slot (filtered).
#' @returns spatialNetworkObj
#' @examples
#' g <- GiottoData::loadSubObjectMini("spatialNetworkObj")
#' @export
setClass("spatialNetworkObj",
    contains = c(
        "nameData", "spatNetData", "spatData", "miscData",
        "giottoSubobject"
    ),
    slots = c(crossSectionObjects = "ANY"),
    prototype = list(crossSectionObjects = NULL),
    validity = .check_spat_net_obj
)






### * Additional functions ####


## crossSectionObj class ####
# See cross_section.R
# TODO







## spatialGridObj Class ####

### * check ####
# spatialGridObj Class

#' @title Check spatialGridObj
#' @name .check_spat_grid_obj
#' @description Check function for S4 spatialGridObj
#' @param object S4 spatialGridObj to check
#' @keywords internal
#' @returns character or TRUE
.check_spat_grid_obj <- function(object) {
    errors <- character()
    method_slot <- slot(object, "method")
    length_method <- length(method_slot)
    if (length_method > 1) {
        msg <- paste0("method is length ", length_method, ". Should be 1")
        errors <- c(errors, msg)
    }

    # if(is.null(method_slot)) {
    #   msg = 'A grid generation method must be given'
    #   errors = c(errors, msg)
    # }

    if (is.null(object@gridDT)) {
        msg <- "No data in gridDT slot.\nThis object contains no spatial
        grid information\n"
        errors <- c(errors, msg)
    }

    if (length(errors) == 0) TRUE else errors
}



### * definition ####
# spatialGridObj Class

#' @title S4 spatialGridObj Class
#' @description Framework to store spatial grid
#' @slot name name of spatialGridObj
#' @slot method method used to generate spatial grid
#' @slot parameters additional method-specific parameters used during spatial
#' grid generation
#' @slot gridDT data.table holding the spatial grid information
#' @slot spat_unit spatial unit
#' @slot feat_type feature type
#' @slot provenance origin of aggregated information (if applicable)
#' @slot misc misc
#' @details
#' This is an S4 object that defines a spatial grid. The structure of the grid
#' is stored as a \code{data.table} within the \code{gridDT} slot and is
#' defined by start and stop spatial locations along the spatial axes.
#' The \code{data.table} also includes names for each cell of the grid and
#' names for each of the spatial axis locations that make up the cell.
#' Grids can be annotated with both spatial and feature information
#' @returns spatialGridObj
#' @examples
#' g <- GiottoData::loadSubObjectMini("spatialGridObj")
#' @export
setClass("spatialGridObj",
    contains = c(
        "nameData", "spatGridData", "spatFeatData", "miscData",
        "giottoSubobject"
    ),
    validity = .check_spat_grid_obj
)







## * Additional functions ####
# spatialGridObj Class

# S3 to S4 backwards compatibility

#' @title Spatially Binned Data
#' @name S3toS4spatGridObj
#' @description convert S3 spatialGridObj to S4
#' @param object S3 spatialGridObj
#' @keywords internal
#' @returns S4 spatialGridObj
S3toS4spatialGridObj <- function(object) {
    if (!isS4(object)) {
        object <- new("spatialGridObj",
            name = object$name,
            method = object$method,
            parameters = object$parameters,
            gridDT = object$gridDT,
            misc = object$misc
        )
    }
    object
}



## spatEnrObj class ####

# * definition ####
# spatEnrObj class

#' @title S4 spatEnrObj Class
#' @description Framework to store spatial enrichment results
#' @slot name name of enrichment object
#' @slot method method used to perform spatial enrichment
#' @slot enrichDT spatial enrichment data.table
#' @slot spat_unit spatial unit
#' @slot feat_type feature type
#' @slot provenance provenance information
#' @slot misc misc
#' @returns spatEnrObj
#' @examples
#' g <- GiottoData::loadSubObjectMini("spatEnrObj")
#' @export
setClass("spatEnrObj",
    contains = c(
        "nameData", "enrData", "spatFeatData", "miscData",
        "giottoSubobject"
    )
)







# giottoSpatial ####

setClassUnion(
    name = "giottoSpatial", c("giottoPolygon", "giottoPoints", "spatLocsObj")
)

setClassUnion(
    name = "spatialClasses", c("giottoSpatial", "SpatVector")
)

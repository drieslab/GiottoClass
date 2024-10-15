#' @include package_imports.R
NULL

# MISC ####
## * Define class unions ####

#' @title NULL or char class union
#' @description class to allow either NULL or character
#' @keywords internal
#' @noRd
setClassUnion("nullOrChar", c("NULL", "character"))

#' @title NULL or list class union
#' @description class to allow either NULL or list
#' @keywords internal
#' @noRd
setClassUnion("nullOrList", c("NULL", "list"))

#' @title NULL or data.table class union
#' @description class to allow either NULL or data.table
#' @keywords internal
#' @noRd
setClassUnion("nullOrDatatable", c("NULL", "data.table"))

#' @title gIndex
#' @description
#' class for handling indices similar to `index` class from \pkg{Matrix}
#' simple class union (setClassUnion) of "numeric", "logical" and "character".
#' @keywords internal
#' @noRd
setClassUnion("gIndex", c("numeric", "logical", "character"))

# VIRTUAL CLASSES ####


# ** giottoSubobject Class ####
#' @keywords internal
#' @noRd
setClass(
    "giottoSubobject",
    contains = "VIRTUAL"
)

# ** gdtData Class ####
#' @description
#' umbrella class for referring to Giotto's normal data.table-based slots for
#' extraction purposes
#' @keywords internal
#' @noRd
setClass(
    "gdtData",
    contains = "VIRTUAL"
)


# ** nameData Class ####
#' @keywords internal
#' @noRd
setClass("nameData",
    contains = "VIRTUAL",
    slots = list(name = "character"),
    prototype = prototype(name = NA_character_)
)

# ** exprData Class ####
#' Basic class for classes with expression information
#' @keywords internal
#' @noRd
setClass("exprData",
    contains = "VIRTUAL",
    slots = list(exprMat = "ANY"),
    prototype = prototype(exprMat = NULL)
)



# ** coordData Class ####
#' Basic class for classes with coordinate information
#'
#' @description
#' coordDataDT is the specific flavor that deals with objects where the
#' coordinate information is stored within data.table objects and should work
#' similarly to data.table when interacting with some basic generic operators
#' for data retreival and setting.
#'
#' @keywords internal
#' @noRd
setClass("coordDataDT",
    contains = c("VIRTUAL", "gdtData"),
    slots = list(coordinates = "data.table"),
    prototype = prototype(coordinates = data.table::data.table())
)





# setClass('coordDataMT',
#          slots = list(coordinates = 'matrix'),
#          prototype = prototype(coordinates = matrix()))


# ** metaData Class ####
#' Basic class for classes with metadata information
#'
#' @description
#' Classes that inherit from this class will contain a metadata slot that
#' stores information in a data.table and should work similarly to data.table
#' when interacting with some basic generic operators for data retrieval and
#' setting
#' @keywords internal
#' @noRd
setClass("metaData",
    contains = c("VIRTUAL", "gdtData"),
    slots = list(
        metaDT = "data.table",
        col_desc = "character"
    ),
    prototype = methods::prototype(
        metaDT = data.table::data.table(),
        col_desc = NA_character_
    )
)





# ** enrData ####
#' enrData
#' @keywords internal
#' @noRd
setClass("enrData",
    contains = c("VIRTUAL", "gdtData"),
    slots = list(
        method = "character",
        enrichDT = "nullOrDatatable"
    ),
    prototype = methods::prototype(
        method = NA_character_,
        enrichDT = NULL
    )
)





# ** nnData ####
#' @keywords internal
#' @noRd
setClass("nnData",
    contains = "VIRTUAL",
    slots = list(
        nn_type = "character",
        igraph = "ANY"
    ),
    prototype = methods::prototype(
        nn_type = NA_character_,
        igraph = NULL
    )
)


# ** spatNetData ####
#' @keywords internal
#' @noRd
setClass("spatNetData",
    contains = "VIRTUAL",
    slots = list(
        method = "character",
        parameters = "ANY",
        outputObj = "ANY",
        networkDT = "nullOrDatatable",
        networkDT_before_filter = "nullOrDatatable",
        cellShapeObj = "ANY"
    ),
    prototype = methods::prototype(
        method = NA_character_,
        parameters = NULL,
        outputObj = NULL,
        networkDT = NULL,
        networkDT_before_filter = NULL,
        cellShapeObj = NULL
    )
)




# ** spatGridData ####
#' @keywords internal
#' @noRd
setClass("spatGridData",
    contains = "VIRTUAL",
    slots = list(
        method = "character",
        parameters = "ANY",
        gridDT = "nullOrDatatable"
    ),
    prototype = prototype(
        method = NA_character_,
        parameters = NULL,
        gridDT = NULL
    )
)





# ** provData Class ####
#' Basic class for classes with provenance information.
#'
#' @description
#' This kind of information is necessary when generating data that is
#' aggregated from multiple original sources of raw information. This could
#' refer to situations such as when producing cellxfeature expression matrices
#' from subcellular transcript information and polygons that are provided as
#' multiple z layers. Provenance is Giotto's method of mapping this aggregated
#' information back to the original z layers that were used in its generation.
#'
#' @keywords internal
#' @noRd
setClass("provData",
    contains = "VIRTUAL",
    slots = list(provenance = "ANY"),
    prototype = prototype(provenance = NULL)
)



# ** spatData Class ####
#' Basic class for classes with spatial information
#'
#' @description
#' Classes that inherit from this class will contain a spat_unit slot that
#' describes which spatial unit the data belongs to. This is most relevant
#' to aggregated information. Subcellular information such as poly data
#' in \code{spatial_info} slot essentially define their own spatial units.
#' Within slots that deal with classes that contain spatData,
#' there is a nesting structure that first nests by spatial unit.
#' @keywords internal
#' @noRd
setClass("spatData",
    contains = c("provData", "VIRTUAL"),
    slots = list(spat_unit = "character"), # not allowed to be NULL
    prototype = prototype(spat_unit = NA_character_)
)



# ** featData Class ####
#' @title Basic class for classes with feature information
#'
#' @description
#' Features in Giotto are a blanket term for any features that are detected,
#' covering modalities such as, but not limited to rna, protein, ATAC, and
#' even QC probes. Classes that inherit from this class will contain a
#' feat_type slot that describes which feature type the data is. Within slots
#' that deal with classes that contain featData, there is a nesting structure
#' that usually first nests by spatial unit and then by feature type.
#' @keywords internal
#' @noRd
setClass("featData",
    contains = "VIRTUAL",
    slots = list(feat_type = "character"), # not allowed to be NULL
    prototype = prototype(feat_type = NA_character_)
)



# ** miscData Class ####
#' @title Basic class for additional miscellaneous information
#'
#' @description
#' Classes (such as dimObj) that can hold information from multiple types of
#' methods use the misc slot to hold additional information specific to each
#' method. Information may be stored within as S3 structures.
#' @returns slot for miscellaneous information
#' @examples
#' g <- GiottoData::loadSubObjectMini("dimObj")
#'
#' slot(g, "misc")
setClass("miscData",
    contains = "VIRTUAL",
    slots = list(misc = "ANY"),
    prototype = prototype(misc = NULL)
)


# ** terraVectData Class ####
#' @title Basic class for terra SpatVector-based objects
#' @description
#' Classes that inherit from this class will contain a spatVector slot meant to
#' hold and work with terra SpatVector objects
#' @returns object with spatVector slot
terraVectData <- setClass(
    "terraVectData",
    contains = "VIRTUAL",
    slots = list(spatVector = "ANY"),
    prototype = prototype(spatVector = NULL)
)



# UTILITY ####

setClass(
    Class = "affine2d",
    slots = list(
        anchor = "ANY",
        affine = "matrix",
        order = "character",
        rotate = "numeric",
        shear = "numeric",
        scale = "numeric",
        translate = "numeric"
    ),
    prototype = list(
        anchor = c(-180, 180, -90, 90),
        affine = diag(rep(1, 2L)),
        order = c("rotate", "shear", "scale", "translate"),
        rotate = 0,
        shear = c(0, 0),
        scale = c(1, 1),
        translate = c(0, 0)
    )
)



# SUBCLASSES ####

# ** spatFeatData ####
#' @description Superclass for classes that contain both spatial and feature
#' data
#' @keywords internal
#' @noRd
setClass("spatFeatData",
    contains = c("spatData", "featData", "VIRTUAL")
)


# OLDCLASS ####
setOldClass("giottoInstructions")







# CORE ####

## Giotto class ####


.versions_info <- function() {
    list(
        os_platform = get_os(),
        gclass = packageVersion("GiottoClass")
    )
}

.gversion <- function(gobject) {
    gobject@versions$gclass
}

`.gversion<-` <- function(gobject, value) {
    gobject@versions$gclass <- value
    return(gobject)
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

    # [Switch to GiottoClass versioning] --------------------------------------#
    # GiottoClass 0.1.2 adds max_window and colors slots to giottoLargeImage
    # this update function has been moved to .update_image_slot() below

    # GiottoClass 0.1.4 supercedes @OS_platform with @versions slot
    if (!is.null(attr(gobject, "OS_platform"))) {
        attr(gobject, "OS_platform") <- NULL
    }
    if (is.null(attr(gobject, "versions"))) { # apply default version 0
        attr(gobject, "versions") <- .versions_info()
        gobject@versions$gclass <- 0 # untracked
    }

    # warn if gobject newer than package
    if (as.character(.gversion(gobject)) > as.character(packageVersion("GiottoClass"))) {
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
    if (.gversion(gobject) < "0.3.0") {
        gobject <- .update_image_slot(gobject)
    }

    # GiottoClass 0.1.2 image updates moved here
    # TODO remove in future update
    gobject@images <- lapply(gobject@images, .update_giotto_image)

    # -------------------------------------------------------------------------#

    # finally, set updated version number
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
    slots = c(
        expression = "nullOrList",
        expression_feat = "ANY",
        spatial_locs = "ANY",
        spatial_info = "ANY",
        cell_metadata = "ANY",
        feat_metadata = "ANY",
        feat_info = "ANY",
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
        versions = "list",
        join_info = "ANY",
        multiomics = "ANY",
        h5_file = "ANY"
        # mirai = 'list'
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
        h5_file = NULL
        # mirai = list()
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
        h5_file = "ANY"
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
        h5_file = NULL
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

    if (is.null(object@networkDT) && is.null(object@networkDT_before_filter)) {
        msg <- "No data in either networkDT or networkDT_before_filter slots.\n
        This object contains no network information.\n"
        errors <- c(errors, msg)
    }

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





# SUBCELLULAR ####

## giottoPolygon class ####

# * definition ####
# giottoPolygon class

#' @title S4 giotto polygon Class
#' @description Giotto class to store and operate on polygon-like data
#' @concept giotto polygon class
#' @slot name name of polygon shapes
#' @slot spatVector terra spatVector to store polygon shapes
#' @slot spatVectorCentroids centroids of polygon shapes
#' @slot overlaps information about overlapping points and polygons
#' @slot unique_ID_cache cached unique spatial IDs that should match the
#' spatVector slot
#' @details holds polygon data
#' @returns giottoPolygon
#' @examples
#' giottoPolygon()
#' @export
giottoPolygon <- setClass(
    Class = "giottoPolygon",
    contains = c("nameData", "terraVectData", "giottoSubobject"),
    slots = c(
        spatVectorCentroids = "ANY",
        overlaps = "ANY",
        unique_ID_cache = "character"
    ),
    prototype = list(
        spatVectorCentroids = NULL,
        overlaps = NULL,
        unique_ID_cache = NA_character_
    )
)




#' @title Update giotto polygon object
#' @name updateGiottoPolygonObject
#' @param gpoly giotto polygon object
#' @returns GiottoPolygonObject
#' @examples
#' g <- GiottoData::loadSubObjectMini("giottoPolygon")
#'
#' updateGiottoPolygonObject(g)
#' @export
updateGiottoPolygonObject <- function(gpoly) {
    if (!inherits(gpoly, "giottoPolygon")) {
        stop("This function is only for giottoPoints")
    }

    # 3.2.X adds cacheing of IDs
    if (is.null(attr(gpoly, "unique_ID_cache"))) {
        attr(gpoly, "unique_ID_cache") <- unique(
            as.list(gpoly@spatVector)$poly_ID
        )
    }

    gpoly
}






# for use with wrap() generic
setClass("packedGiottoPolygon",
    contains = c("nameData", "giottoSubobject"),
    slots = c(
        packed_spatVector = "ANY",
        packed_spatVectorCentroids = "ANY",
        packed_overlaps = "ANY",
        unique_ID_cache = "character"
    ),
    prototype = list(
        packed_spatVector = NULL,
        packed_spatVectorCentroids = NULL,
        packed_overlaps = NULL,
        unique_ID_cache = NA_character_
    )
)






## giottoPoints class ####


## * definition ####
# giottoPoints class

#' @title S4 giotto points Class
#' @description Giotto class to store and operate on points data
#' @concept giotto points class
#' @slot feat_type name of feature type
#' @slot spatVector terra spatVector to store point shapes
#' @slot networks feature networks
#' @slot unique_ID_cache cached unique feature IDs that should match the
#' spatVector slot
#' @details Contains vector-type feature data
#' @returns giottoPoints
#' @examples
#' giottoPoints()
#' @export
giottoPoints <- setClass(
    Class = "giottoPoints",
    contains = c("featData", "terraVectData", "giottoSubobject"),
    slots = c(
        networks = "ANY",
        unique_ID_cache = "character"
    ),
    prototype = list(
        networks = NULL,
        unique_ID_cache = NA_character_
    )
)




#' @title Update giotto points object
#' @name updateGiottoPointsObject
#' @param gpoints giotto points object
#' @returns GiottoPointsObject
#' @examples
#' g <- GiottoData::loadSubObjectMini("giottoPoints")
#'
#' updateGiottoPointsObject(g)
#' @export
updateGiottoPointsObject <- function(gpoints) {
    if (!inherits(gpoints, "giottoPoints")) {
        stop("This function is only for giottoPoints")
    }

    # 3.2.X adds cacheing of IDs
    if (is.null(attr(gpoints, "unique_ID_cache"))) {
        attr(gpoints, "unique_ID_cache") <- unique(
            as.list(gpoints@spatVector)$feat_ID
        )
    }

    gpoints
}











# for use with wrap() generic
setClass(
    "packedGiottoPoints",
    slots = c(
        feat_type = "character",
        packed_spatVector = "ANY",
        networks = "ANY",
        unique_ID_cache = "character"
    ),
    prototype = list(
        feat_type = NA_character_,
        packed_spatVector = NULL,
        networks = NULL,
        unique_ID_cache = NA_character_
    )
)









## featureNetwork class ####


## * definition ####
# featureNetwork class


#' @title S4 giotto feature network Class
#' @description Giotto class to store and operate on feature network
#' @concept giotto points network class
#' @slot name name of feature network
#' @slot network_datatable feature network in data.table format
#' @slot network_lookup_id table mapping numeric network ID to unique
#' feature numerical IDs
#' @slot full fully connected network
#' @details contains feature network information
#' @returns featureNetwork
#' @examples
#' featureNetwork()
#' @export
featureNetwork <- setClass(
    Class = "featureNetwork",
    contains = c("nameData", "giottoSubobject"),
    slots = c(
        network_datatable = "ANY",
        network_lookup_id = "ANY",
        full = "ANY"
    ),
    prototype = list(
        network_datatable = NULL,
        network_lookup_id = NULL,
        full = NULL
    )
)


# IMAGES ####

## giottoImage class ####

# * definition ####
# giottoImage class

#' @title S4 giottoImage Class
#' @description Framework of giotto object to store and work with spatial
#' expression data
#' @concept giotto image object
#' @slot name name of Giotto image
#' @slot mg_object magick image object
#' @slot minmax minimum and maximum of associated spatial location coordinates
#' @slot boundaries x and y coordinate adjustments (default to 0)
#' @slot scale_factor image scaling relative to spatial locations
#' @slot resolution spatial location units covered per pixel
#' @slot file_path file path to the image if given
#' @slot OS_platform Operating System to run Giotto analysis on
#' @details
#' \[\strong{mg_object}\] Core object is any image that can be read by the
#' magick package
#'
#' \[\strong{boundaries}\] Boundary adjustments can be used to manually or
#' automatically through a script adjust the image with the spatial data.
#'
#' @returns giottoImage
#' @examples
#' giottoImage()
#' @export
giottoImage <- setClass(
    Class = "giottoImage",
    slots = c(
        name = "character",
        mg_object = "ANY",
        minmax = "ANY",
        boundaries = "ANY",
        scale_factor = "ANY",
        resolution = "ANY",
        file_path = "ANY",
        OS_platform = "ANY"
    ),
    prototype = list(
        name = "test",
        mg_object = NULL,
        minmax = NULL,
        boundaries = NULL,
        scale_factor = NULL,
        resolution = NULL,
        file_path = NULL,
        OS_platform = NULL
    )
)






## giottoLargeImage class ####


## * definition ####
# giottoLargeImage class

#' @title S4 giottoLargeImage Class
#' @description Image class for Giotto that uses \pkg{terra} `SpatRaster` as
#' a backend. If images are loaded from a file on disk then they are worked
#' with lazily, where only the values needed at any moment are loaded/sampled
#' into memory. Since `SpatRaster` objects are C pointers, `giottoLargeImage`
#' and inheriting classes need to run `reconnect()` after loading from a
#' saved object.
#' @concept giotto object image
#' @slot name name of large Giotto image
#' @slot raster_object terra `SpatRaster` object
#' @slot extent tracks the extent of the raster object. Note that most
#' processes should rely on the extent of the raster object instead of this.
#' @slot overall_extent terra extent object covering the original extent of
#' image
#' @slot scale_factor image scaling relative to spatial locations
#' @slot resolution spatial location units covered per pixel
#' @slot max_intensity approximate maximum value
#' @slot min_intensity approximate minimum value
#' @slot max_window value to set as maximum intensity in color scaling
#' @slot colors color mappings in hex codes
#' @slot is_int values are integers
#' @slot file_path file path to the image if given
#' @slot OS_platform Operating System to run Giotto analysis on
#' @returns giottoLargeImage
#' @examples
#' giottoLargeImage()
#'
#' @export giottoLargeImage
#' @exportClass giottoLargeImage
giottoLargeImage <- setClass(
    Class = "giottoLargeImage",
    slots = c(
        name = "ANY",
        raster_object = "ANY",
        extent = "ANY", # REMOVE?
        overall_extent = "ANY", # REMOVE? New slot px_dims as replacement?
        scale_factor = "ANY",
        resolution = "ANY",
        max_intensity = "numeric",
        min_intensity = "numeric",
        max_window = "numeric", # NEW
        colors = "character", # NEW
        is_int = "ANY",
        file_path = "ANY",
        OS_platform = "ANY"
    ),
    prototype = list(
        name = NULL,
        raster_object = NULL,
        extent = NULL,
        overall_extent = NULL,
        scale_factor = NULL,
        resolution = NULL,
        max_intensity = NA_real_,
        min_intensity = NA_real_,
        max_window = NA_real_,
        colors = grDevices::grey.colors(n = 256, start = 0, end = 1, gamma = 1),
        is_int = NULL,
        file_path = NULL,
        OS_platform = NULL
    )
)

#' @title S4 giottoAffineImage Class
#' @description
#' Class extending `giottoLargeImage`. When `shear()` or `spin()` operations
#' are performed on
#'
#'
#' @slot affine contains `affine2d` object allowing lazily performed spatial
#' transforms
#' @slot funs list of functions associated with the object. Primarily to
#' perform the delayed/lazy operations
#' @returns giottoAffineImage
setClass(
    "giottoAffineImage",
    contains = c("giottoLargeImage"),
    slots = c(
        affine = "affine2d",
        funs = "list"
    )
)





# function for updating image objects if structure definitions have changed
.update_giotto_image <- function(x) {
    if (inherits(x, "giottoLargeImage")) {
        # 0.1.2 release adds colors & max_window slots
        if (is.null(attr(x, "colors"))) {
            attr(x, "colors") <- grDevices::grey.colors(
                n = 256, start = 0,
                end = 1, gamma = 1
            )
        }
        if (is.null(attr(x, "max_window"))) {
            # get a max intensity value
            if (!is.null(x@max_intensity)) {
                x@max_intensity <- .spatraster_intensity_range(
                    x@raster_object
                )[["max"]]
            }

            attr(x, "max_window") <- .bitdepth(
                x@max_intensity,
                return_max = TRUE
            )
        }

        # 0.1.x release adds giottoImageStack
        # deprecate
    }
    return(x)
}



## giottoImageStack class ####

## * definition ####
# giottoImageStack class

# giottoImageStack <- setClass(
#   Class = "giottoImageStack",
#
#   slots = c(
#     name = 'character',
#     images = 'giottoLargeImage',
#     weight = 'numeric'
#   )
# )






# giottoSpatial ####

setClassUnion(
    name = "giottoSpatial", c("giottoPolygon", "giottoPoints", "spatLocsObj")
)

setClassUnion(
    name = "spatialClasses", c("giottoSpatial", "SpatVector")
)


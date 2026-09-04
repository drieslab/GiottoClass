#' @include package_imports.R
#' @include classes.R
NULL


# Giotto Object Settings ####
# Methods and documentations found in methods-IDs.R
setGeneric("spatIDs", function(x, ...) standardGeneric("spatIDs"))
setGeneric("spatIDs<-", function(x, ..., value) standardGeneric("spatIDs<-"))
setGeneric("featIDs", function(x, ...) standardGeneric("featIDs"))

## instructions ####
# Methods and documentations found in methods-instructions.R
setGeneric(
    "instructions",
    function(gobject, param, ...) standardGeneric("instructions")
)
setGeneric(
    "instructions<-",
    function(gobject, param, initialize, ..., value) {
        standardGeneric("instructions<-")
    }
)

## set defaults ####
# Methods and documentations found in methods-instructions.R
setGeneric(
    "activeSpatUnit",
    function(gobject, ...) standardGeneric("activeSpatUnit")
)
setGeneric(
    "activeSpatUnit<-",
    function(gobject, ..., value) standardGeneric("activeSpatUnit<-")
)



# Object creation ####
setGeneric(
    "createGiottoPoints",
    function(x, ...) standardGeneric("createGiottoPoints")
)
setGeneric(
    "createGiottoPolygon",
    function(x, ...) standardGeneric("createGiottoPolygon")
)


# Object reconnection ####
# Some objects may operate base on on-disk files.
setGeneric("reconnect", function(x, ...) standardGeneric("reconnect"))

# copy() S4 generic ####
setGeneric("copy",
    function(x) standardGeneric("copy"),
    useAsDefault = data.table::copy
)

# lazy operations ####
setGeneric("doDeferred", function(x, ...) standardGeneric("doDeferred"))

# Object Characteristics ####

## nrow() S4 generic ####
if (!isGeneric("nrow")) setOldClass("nrow")
if (!isGeneric("ncol")) setOldClass("ncol")
if (!isGeneric("dim")) setOldClass("dim")

## colnames and rownames generics ####
if (!isGeneric("colnames")) setOldClass("colnames")
if (!isGeneric("rownames")) setOldClass("rownames")

#' @title Data Processing
#' @name processData
#' @description Generic for processing an object containing measured values.
#' Specific methods should be defined for this generic to
#' perform pre or post processing specific to a data class type. No methods
#' are exported from \pkg{GiottoClass}. The methods, which may
#' differ depending on the input data, are attached from other packages which
#' focus on analyses and/or alternative data representations with specific ways
#' to implement those analyses.
#' @param x a data object
#' @param param a [processParam-class] inheriting object
#' @param ... additional arguments, for use in specific methods
#' @returns An object of the same class containing the processed values
#' @export
setGeneric("processData", function(x, param, ...) standardGeneric("processData"))
#' @title Data Clustering
#' @name clusterData
#' @description Generic for clustering numeric matrix-like data. `param` accepts
#' param classes from the bioconductor \pkg{bluster} framework.
#' No methods are exported from \pkg{GiottoClass}. The methods, which may
#' differ depending on the input data, are attached from other packages which
#' focus on analyses and/or alternative data representations with specific ways
#' to implement those analyses.
#' @param x a data object
#' @param param a [bluster::BlusterParam-class] inheriting object
#' @param ... additional arguments, for use in specific methods
#' @returns Clustering results. Exact outputs may depend on param settings
#' @export
setGeneric("clusterData", function(x, param, ...) standardGeneric("clusterData"))
#' @title Data Analysis
#' @name analyzeData
#' @description Generic for analyzing an object containing measured values,
#' producing computed outputs or summary statistics about the data rather than
#' transforming it. Specific methods should be defined for this generic to
#' perform analyses specific to a data class type. No methods are exported
#' from \pkg{GiottoClass}. The methods, which may differ depending on the
#' input data, are attached from other packages which focus on analyses and/or
#' alternative data representations with specific ways to implement those
#' analyses.
#' @param x a data object
#' @param param a [analyzeParam-class] inheriting object
#' @param ... additional arguments, for use in specific methods
#' @returns A \code{data.table} of computed values or summary statistics
#' @export
setGeneric("analyzeData", function(x, param, ...) standardGeneric("analyzeData"))
#' @title Data Filter
#' @name filterData
#' @description Generic for filtering an object containing measured values,
#' producing a selection (typically a list of IDs to keep) rather than
#' transformed data or summary statistics. Specific methods should be
#' defined for this generic to perform filtering specific to a data class
#' type. No methods are exported from \pkg{GiottoClass}. The methods, which
#' may differ depending on the input data, are attached from other packages
#' which focus on filtering and/or alternative data representations with
#' specific ways to implement those filters.
#' @param x a data object
#' @param param a [filterParam-class] inheriting object
#' @param ... additional arguments, for use in specific methods
#' @returns A selection (typically a list of character ID vectors)
#' @export
setGeneric("filterData", function(x, param, ...) standardGeneric("filterData"))
#' @title Data Reduction
#' @name reduceData
#' @description Generic for reducing an object containing measured values
#' to a lower-dimensional decomposition or embedding (PCA, UMAP, tSNE,
#' ...), distinct from [processData()] (same-shape transform),
#' [analyzeData()] (summary stats), and [filterData()] (selection).
#' Specific methods should be defined for this generic to perform
#' reductions specific to a data class type. No methods are exported from
#' \pkg{GiottoClass}. The methods, which may differ depending on the input
#' data, are attached from other packages which focus on reduction methods
#' and/or alternative data representations with specific ways to implement
#' those reductions.
#' @param x a data object
#' @param param a [reduceParam-class] inheriting object
#' @param ... additional arguments, for use in specific methods
#' @returns A decomposition (typically a list of matrices/vectors,
#' e.g. `list(u, d, v, sdev, eigenvalues)` for PCA)
#' @export
setGeneric("reduceData", function(x, param, ...) standardGeneric("reduceData"))
#' @title Create a Network
#' @name createNetwork
#' @description Generic for constructing a network (graph) from
#' coordinates, features, or an embedding. Methods dispatch on the input
#' data class and a [networkParam-class]-inheriting object that selects
#' the algorithm (kNN, sNN, Delaunay, ...). Part of the `create<Noun>`
#' object-construction family. Distinct from analysis-stage operations
#' such as [processData()], [filterData()], [reduceData()], and
#' [analyzeData()].
#' @param x a data object (matrix, [spatLocsObj-class], [dimObj-class],
#' [giotto-class], or a GiottoDisk `fileStore`)
#' @param param a [networkParam-class]-inheriting object
#' @param ... additional arguments, for use in specific methods
#' @returns A network. Concrete type depends on the Param's `output` slot
#' and any supplied `backend`: `"data.table"` of edges, `igraph`, or a
#' GiottoDisk `parquetEdgeStore`.
#' @export
setGeneric("createNetwork", function(x, param, ...) standardGeneric("createNetwork"))

# spatial operations ####
setGeneric(
    "calculateOverlap",
    function(x, y, ...) standardGeneric("calculateOverlap")
)
setGeneric(
    "overlapToMatrix",
    function(x, ...) standardGeneric("overlapToMatrix")
)

# Methods and documentations found in methods-spatShift.R

setGeneric("spatShift", function(x, ...) standardGeneric("spatShift"))
setGeneric("affine", function(x, y, ...) standardGeneric("affine"))
setGeneric("shear", function(x, ...) standardGeneric("shear"))
setGeneric("XY", function(x, ...) standardGeneric("XY"))
setGeneric("XY<-", function(x, ..., value) standardGeneric("XY<-"))
setGeneric("settleGeom", function(x, ...) standardGeneric("settleGeom"))
setGeneric("combineGeom", function(x, ...) standardGeneric("combineGeom"))
setGeneric("splitGeom", function(x, ...) standardGeneric("splitGeom"))


# Methods and documentations found in methods-overlaps.R
setGeneric("overlaps", function(x, ...) standardGeneric("overlaps"))


# Methods and documentations found in methods-relate.R
setGeneric(
    "spatRelate",
    function(x, y, relation = "intersects", ...) standardGeneric("spatRelate")
)



# Giotto subnesting ####
# All methods and documentations found in methods-nesting.R

## prov() S4 generic ####
setGeneric("prov", function(x) standardGeneric("prov"))
setGeneric("prov<-", function(x, value) standardGeneric("prov<-"))

## spatUnit() S4 generic ####
setGeneric("spatUnit", function(x) standardGeneric("spatUnit"))
setGeneric("spatUnit<-", function(x, ..., value) standardGeneric("spatUnit<-"))

## featType() S4 generic ####
setGeneric("featType", function(x) standardGeneric("featType"))
setGeneric("featType<-", function(x, ..., value) standardGeneric("featType<-"))

## objName() generic ####
setGeneric("objName", function(x) standardGeneric("objName"))
setGeneric("objName<-", function(x, value) standardGeneric("objName<-"))

## objManifest() generic ####
# Documentation and methods found in manifest.R
setGeneric("objManifest", function(x, ...) standardGeneric("objManifest"))

## setGiotto() generic ####
setGeneric("setGiotto", function(gobject, x, ...) standardGeneric("setGiotto"))


# coerce ####
setGeneric("as.sf", function(x, ...) standardGeneric("as.sf"))
setGeneric("as.sp", function(x, ...) standardGeneric("as.sp"))
setGeneric("as.stars", function(x, ...) standardGeneric("as.stars"))
setGeneric("as.terra", function(x, ...) standardGeneric("as.terra"))

# # docs ----------------------------------------------------------- #
#' @title Wrap giotto terra pointer information
#' @name wrap
#' @aliases vect
#' @description Extension of wrap methods from terra for Giotto's terra-based S4
#' objects. Allows pointer information to be packaged into memory so that it can
#' be passed over a connection (e.g. nodes on a computer cluster)
#' @param x giottoPolygon or giottoPoints
#' @returns wrapped giottoPolygon or giottoPoints
#' @examples
#' g <- GiottoData::loadSubObjectMini("giottoPoints")
#'
#' wrap(g)
NULL
# ---------------------------------------------------------------- #

# terra-based object serialization ####
## wrap methods ####

#' @describeIn wrap Wrap giottoPolygon
#' @export
setMethod(
    "wrap", signature(x = "giottoPolygon"),
    function(x) {
        pgp <- new("packedGiottoPolygon")
        pgp@name <- x@name
        pgp@unique_ID_cache <- x@unique_ID_cache
        pgp@packed_spatVector <- terra::wrap(x@spatVector)
        if (!is.null(x@spatVectorCentroids)) {
            pgp@packed_spatVectorCentroids <- terra::wrap(x@spatVectorCentroids)
        }
        if (!is.null(x@overlaps)) {
            pgp@packed_overlaps <- lapply(x@overlaps, function(sv) {
                if (inherits(sv, "SpatVector")) {
                    terra::wrap(sv)
                } else {
                    sv
                }
            })
        }
        return(pgp)
    }
)


#' @describeIn wrap Wrap giotto
#' @export
setMethod(
    "wrap", signature(x = "giotto"),
    function(x) {
        pg <- new("packedGiotto")
        g_slots <- methods::slotNames("giotto")
        g_slots <- g_slots[!g_slots %in% c("spatial_info", "feat_info")]
        for (g_slot in g_slots) {
            slot(pg, g_slot) <- slot(x, g_slot)
        }
        pg@packed_spatial_info <- lapply(x@spatial_info, wrap)
        pg@packed_feat_info <- lapply(x@feat_info, wrap)
        return(pg)
    }
)


#' @describeIn wrap Wrap giottoPoints
#' @export
setMethod(
    "wrap", signature(x = "giottoPoints"),
    function(x) {
        pgp <- new("packedGiottoPoints")
        pgp@feat_type <- x@feat_type
        pgp@unique_ID_cache <- x@unique_ID_cache
        pgp@packed_spatVector <- terra::wrap(x@spatVector)
        pgp@networks <- x@networks
        return(pgp)
    }
)







## unwrap methods ####
# For compatibility before terra 1.6.41, vect will be used

#' @describeIn wrap Unwrap giottoPolygon
#' @export
setMethod(
    "vect", signature(x = "packedGiottoPolygon"),
    function(x) {
        gp <- new("giottoPolygon")
        gp@name <- x@name
        gp@spatVector <- terra::vect(x@packed_spatVector)

        # new cache slot
        if (!is.null(attr(x, "unique_ID_cache"))) {
            gp@unique_ID_cache <- x@unique_ID_cache
        } else {
            gp@unique_ID_cache <- spatIDs(gp)
        }

        if (!is.null(x@packed_spatVectorCentroids)) {
            gp@spatVectorCentroids <- terra::vect(x@packed_spatVectorCentroids)
        }
        if (length(x@packed_overlaps) > 0) {
            gp@overlaps <- lapply(x@packed_overlaps, function(sv) {
                if (inherits(sv, "PackedSpatVector")) {
                    terra::vect(sv)
                } else {
                    sv
                }
            })
        }
        return(gp)
    }
)


#' @describeIn wrap Unwrap giottoPolygon
#' @export
setMethod(
    "vect", signature(x = "packedGiottoPoints"),
    function(x) {
        gp <- new("giottoPoints")
        gp@feat_type <- x@feat_type
        gp@spatVector <- terra::vect(x@packed_spatVector)

        # new cache slot
        if (!is.null(attr(x, "unique_ID_cache"))) {
            gp@unique_ID_cache <- x@unique_ID_cache
        } else {
            gp@unique_ID_cache <- featIDs(gp)
        }

        gp@networks <- x@networks
        return(gp)
    }
)


#' @describeIn wrap Unwrap giotto
#' @export
setMethod(
    "vect", signature(x = "packedGiotto"),
    function(x) {
        gobj <- new("giotto")
        g_slots <- methods::slotNames("giotto")
        g_slots <- g_slots[!g_slots %in% c("spatial_info", "feat_info")]
        for (g_slot in g_slots) {
            slot(gobj, g_slot) <- slot(x, g_slot)
        }
        gobj@spatial_info <- lapply(x@packed_spatial_info, vect)
        gobj@feat_info <- lapply(x@packed_feat_info, vect)
        return(gobj)
    }
)

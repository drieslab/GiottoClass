#' @include generics.R
NULL

# docs ----------------------------------------------------------- #
#' @title Combine objects by rows (Giotto-related)
#' @name rbind-generic
#' @description row bind two objects
#' @param x item 1 to rbind
#' @param y item 2 to rbind
#' @param add_list_ID whether to generate a list_ID column when giottoPolygons
#' to append have different names
#' @param \dots additional params to pass to methods
#' @returns object with appended rows
#' @examples
#' g <- GiottoData::loadSubObjectMini("giottoPolygon")
#'
#' rbind2(g, g)
NULL
# ---------------------------------------------------------------- #



#' @describeIn rbind-generic Append giottoPolygon objects
#' @export
setMethod(
    "rbind2", signature(x = "giottoPolygon", y = "giottoPolygon"),
    function(x, y, add_list_ID = TRUE, ...) {
        # determine homo or hetero
        poly_names <- c(slot(x, "name"), slot(y, "name"))
        homo <- identical(poly_names[[1L]], poly_names[[2L]])
        if (!isTRUE(homo)) {
            new_name <- paste0(mixedsort(poly_names), collapse = "-")
            out <- rbind2_giotto_polygon_hetero(
                x = x, y = y,
                new_name = new_name,
                add_list_ID = add_list_ID
            )
        } else {
            out <- rbind2_giotto_polygon_homo(x = x, y = y)
        }

        return(out)
    }
)


if (!isGeneric("rbind")) setGeneric("rbind", signature = "...")

setMethod("rbind", "giottoPolygon", function(..., deparse.level = 1) {
    if (nargs() <= 2L) {
        rbind2(...)
    } else {
        xs <- list(...)
        rbind2(xs[[1L]], do.call(Recall, xs[-1L]))
    }
})






# internals ####
#' @title Append giotto polygons of the same name
#' @name rbind2_giotto_polygon_homo
#' @description Append two giotto polygons together of the same name.
#' Performed recursively through \code{rbind2} and \code{rbind}
#' @param x \code{giottoPolygon} 1
#' @param y \code{giottoPolygon} 2
#' @returns giottoPolygon
#' @keywords internal
rbind2_giotto_polygon_homo <- function(x, y) {
    if (is.null(slot(x, "spatVector"))) {
        slot(x, "spatVector") <- slot(y, "spatVector")
    } else {
        slot(x, "spatVector") <- rbind(
            slot(x, "spatVector"),
            slot(y, "spatVector")
        )
    }

    if (is.null(slot(x, "spatVectorCentroids"))) {
        slot(x, "spatVectorCentroids") <- slot(y, "spatVectorCentroids")
    } else {
        slot(x, "spatVectorCentroids") <- rbind(
            slot(x, "spatVectorCentroids"),
            slot(y, "spatVectorCentroids")
        )
    }

    if (is.null(slot(x, "overlaps"))) {
        slot(x, "overlaps") <- slot(y, "overlaps")
    } else {
        slot(x, "overlaps") <- rbind(slot(x, "overlaps"), slot(y, "overlaps"))
    }

    slot(x, "unique_ID_cache") <- unique(c(spatIDs(x), spatIDs(y)))
    x
}


#' @title Append giotto polygons of different names
#' @name rbind2_giotto_polygon_hetero
#' @description Append two giotto polygons together of different names
#' Performed recursively through \code{rbind2} and \code{rbind}. Generates an
#' additional list_ID attribute based on the original names. The object name
#' also becomes a combination of both previous names
#' @param x \code{giottoPolygon} 1
#' @param y \code{giottoPolygon} 2
#' @param poly_names sorted polygon names to be used in the
#' combined \code{giottoPolygon} object
#' @param add_list_ID whether to include the name of the
#' origin \code{giottoPolygons} as a new 'list_ID' attribute
#' @returns giottoPolygon
#' @keywords internal
rbind2_giotto_polygon_hetero <- function(x, y, new_name, add_list_ID = TRUE) {
    # handle as homo but different name if add_list_ID = FALSE
    if (!isTRUE(add_list_ID)) {
        gpoly <- rbind2_giotto_polygon_homo(x, y)
        slot(gpoly, "name") <- new_name
        return(gpoly)
    }

    null_xsv <- null_xsvc <- null_xovlp <- FALSE

    # Add list_ID
    if (!is.null(slot(x, "spatVector"))) {
        if (!"list_ID" %in% names(slot(x, "spatVector"))) {
            slot(x, "spatVector")$list_ID <- slot(x, "name")
        }
    } else {
        null_xsv <- TRUE
    }
    if (!is.null(y@spatVector)) {
        if (!"list_ID" %in% names(slot(y, "spatVector"))) {
            slot(y, "spatVector")$list_ID <- slot(y, "name")
        }
    }

    if (!is.null(slot(x, "spatVectorCentroids"))) {
        if (!"list_ID" %in% names(slot(x, "spatVectorCentroids"))) {
            slot(x, "spatVectorCentroids")$list_ID <- slot(x, "name")
        }
    } else {
        null_xsvc <- TRUE
    }
    if (!is.null(y@spatVectorCentroids)) {
        if (!"list_ID" %in% names(slot(y, "spatVectorCentroids"))) {
            slot(y, "spatVectorCentroids")$list_ID <- slot(y, "name")
        }
    }

    if (!is.null(slot(x, "overlaps"))) {
        if (!"list_ID" %in% names(slot(x, "overlaps"))) {
            slot(x, "overlaps")$list_ID <- slot(x, "name")
        }
    } else {
        null_xovlp <- TRUE
    }
    if (!is.null(y@overlaps)) {
        if (!"list_ID" %in% names(slot(y, "overlaps"))) {
            slot(y, "overlaps")$list_ID <- slot(y, "name")
        }
    }

    # Perform rbinds
    if (isTRUE(null_xsv)) {
        new_sv <- slot(y, "spatVector")
    } else {
        new_sv <- rbind(slot(x, "spatVector"), slot(y, "spatVector"))
    }

    if (isTRUE(null_xsvc)) {
        new_svc <- slot(y, "spatVectorCentroids")
    } else {
        new_svc <- rbind(
            slot(x, "spatVectorCentroids"),
            slot(y, "spatVectorCentroids")
        )
    }

    if (isTRUE(null_xovlp)) {
        new_ovlp <- slot(y, "overlaps")
    } else {
        new_ovlp <- rbind(slot(x, "overlaps"), slot(y, "overlaps"))
    }

    new_poly <- create_giotto_polygon_object(
        name = new_name,
        spatVector = new_sv,
        spatVectorCentroids = new_svc,
        overlaps = new_ovlp,
        unique_IDs = unique(c(spatIDs(x), spatIDs(y)))
    )
    new_poly
}

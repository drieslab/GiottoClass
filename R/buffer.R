#' @include generics.R
NULL

# Documentations ------------------------------------------------------------ #
#' @name buffer
#' @title Create a buffer around vector geometries
#' @description Calculate a buffer around all geometries of a `SpatVector`
#' @inheritParams terra::buffer
#' @param \dots additional params to pass
#' @param settle logical. Settle the borders between polygons by cutting them
#' where they touch based on voronoi boundaries.
#' @returns `giottoPolygon` of buffer polygons
#' @examples
#' sl <- GiottoData::loadSubObjectMini("spatLocsObj")
#' slb <- buffer(sl, 30)
#' plot(slb)
NULL

#' @name settleGeom
#' @title Settle polygon bounds
#' @description Settle the boundaries between polygons when they overlap by
#' splitting both at the point where they touch. Works through intersection
#' with the voronoi of the centroids.
#' @param x a `SpatVector` of type "polygons" or object inheriting from 
#' `giottoPolygon`
#' @returns same class as `x`, with the contained polygons borders settled
#' in relation to each other.
#' @examples
#' svp <- GiottoData::loadSubObjectMini("giottoPolygon")[]
#' svp <- buffer(svp, 5)
#' plot(svp)
#' svp <- settleGeom(svp)
#' plot(svp)
NULL
# --------------------------------------------------------------------------- #




# buffer ####
#' @rdname buffer
#' @export
setMethod("buffer", "spatLocsObj", function(x, width, ..., settle = TRUE) {
    x_use <- as.points(x)
    res <- buffer(x = x_use, width = width, ...)
    if (settle) res <- settleGeom(res)
    gpoly <- createGiottoPolygon(res, verbose = FALSE)
    return(gpoly)
})

#' @rdname buffer
#' @export
setMethod("buffer", signature("giottoPoints"), function(x, width, ..., settle = TRUE) {
    x_use <- x[]
    res <- buffer(x = x_use, width = width, ...)
    if (settle) res <- settleGeom(res)
    res$poly_ID <- sprintf("poly_", seq_len(nrow(res)))
    gpoly <- createGiottoPolygon(res, verbose = FALSE)
    return(gpoly)
})

#' @rdname buffer
#' @export
setMethod("buffer", signature("giottoPolygon"), function(x, width, ..., settle = TRUE) {
    x_use <- x[]
    res <- buffer(x = x_use, width = width, ...)
    if (settle) res <- settleGeom(res)
    x[] <- res
    return(x)
})


# settleGeom ####

#' @rdname settleGeom
#' @export
setMethod("settleGeom", signature("giottoPolygon"), function(x) {
    x[] <- settleGeom(x[])
})

#' @rdname settleGeom
#' @export
setMethod("settleGeom", signature("SpatVector"), function(x) {
    if (!terra::geomtype(x) == "polygons") {
        stop("`settleGeom()` can only be used with polygon geometries")
    }

    orig_names <- names(x)
    # apply index
    x$.idx <- seq_len(nrow(x))
    names_keep <- c(orig_names, ".idx")
    
    # Find overlapping circles
    overlaps <- relate(x, relation = "overlaps", pairs = TRUE) |>
        as.vector() |>
        unique()
    if(length(overlaps) == 0L) return(x)  # If no overlaps, return original polys
    
    # Create Voronoi polygons for the points
    # Note: extend parameter ensures Voronoi polygons cover all buffer areas
    vor <- voronoi(centroids(x), bnd = ext(x) * 1.2)
    # voronoi does not return values in order. Reorder with index
    vor <- terra::sort(vor, v = ".idx")
    
    # Process each buffer
    reslist <- lapply(overlaps, function(i) {
        terra::intersect(vor[i], x[i])
    })

    res <- do.call(rbind, reslist)
    names(res)[seq_along(names_keep)] <- names_keep
    res <- res[, names_keep] # drop extra fields from intersect
    x <- x[-res$.idx] # drop polys to edit
    x <- rbind(x, res)
    x <- terra::sort(x, v = ".idx")
    x <- x[, orig_names]
    
    return(x)
})


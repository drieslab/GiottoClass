# docs ####
#' @name tileIterator-class
#' @title Spatial Tile Iterator
#' @aliases tileIterator
#' @description
#' Utility class that simplifies the setup of tiles across a spatial extent.
#' Tiles are stored in a lightweight format safe to be passed to child
#' processes. Tile `SpatExtent` objects can be extracted on-demand.
#'
#' @section setup and basic characteristics:
#' A `tileIterator` needs both a spatial extent to tile across and also a
#' request for a certain number of tiles.
#'
#' * `tileIterator()` is used to create a `tileIterator` instance.
#' * `ext()<-` can be used to set up the spatial extent.
#' * `ext()` is used to check extent.
#' * `length()<-` is used to request a number of tiles.
#' * `length()` can be used to find out how many tiles there are.
#' * `dim()`/`nrow()`/`ncol()` basic generics are implemented and return
#' information about how the tiles are arranged.
#'
#' Note that the number of requested tiles may not be the actual length, since
#' a grid pattern must be followed. However, the number of generated tiles will
#' be AT LEAST the number that is requested. Generated tiles will have as square
#' a shape as possible.
#'
#' @section Getting tile extent:
#' `[i]` and `[i, j]` indexing can be used to select tiles, similarly to a
#' matrix. `[]` without any indexing will return the entire set of extents
#' as a list.
#'
#' @section padding:
#' `+`/`-` can be used to add or subtract padding to each of the tiles.
#'
#' @section previewing tiles:
#' `plot()` can be used to check the layout of the tiles.
#'
#' @section metadata:
#' The `tileIterator` object can contain metadata. By default after extent and
#' tiles setup, a column called `"tile"` will be set up that simply records
#' which tile it is.
#'
#' * `$` can be used to view a specific type of metadata
#' * `$<-` can be used to set additional metadata items
#' * `[[i]]` selection will pull specific metadata rows corresponding to the
#' selected tiles.
#'
#' @examples
#' x <- tileIterator()
#' force(x)
#' ext(x) <- c(0, 100, 0, 100)
#' length(x) <- 8 # generated tiles will be AT LEAST this value
#' force(x)
#'
#' length(x) # how many were actually generated?
#' dim(x)
#' nrow(x)
#' ncol(x)
#'
#' # previewing
#' plot(x)
#'
#' # tile padding
#' y <- x + 10
#' plot(y, alpha = 0.3)
#' plot(ext(x), add = TRUE, border = "red")
#'
#' z <- x - 5
#' plot(ext(x), border = "red")
#' plot(z, add = TRUE)
#'
#' # tile selection
#' x[5]
#' x[1, 2:3]
#'
#' # metadata
#' x$tile
#' x$fname <- sprintf("tile_%03d.tif", x$tile)
#' x[[5]]
#' x[[1:3]]$fname
NULL

# methods ####

setMethod("initialize", signature("tileIterator"), \(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)

    # initialize n tiles
    if (length(.Object@n) == 0L) {
        .Object@n <- 0
    }

    # return early if extent not provided
    if (length(.Object@extent) == 0L) {
        return(.Object)
    }

    # check extent validity
    if (length(.Object@extent) != 4L) {
        stop("tileIterator: invalid extent information", call. = FALSE)
    }

    # return early if n tiles = 0
    if (.Object@n == 0) {
        return(.Object)
    }

    # generate tile extent array
    n_desired <- .Object@n
    e <- terra::ext(.Object@extent)
    .Object@tiles <- .chunk_plan(e, min_chunks = n_desired)
    .Object@n <- length(.Object)

    # initialize metadata
    if (nrow(.Object@metadata) == 0L || nrow(.Object@metadata) != .Object@n) {
        .Object@metadata <- data.frame(tile = seq_len(.Object@n))
    }

    return(.Object)
})

setMethod("$<-", signature("tileIterator", "ANY"), function(x, name, value) {
    x@metadata[[name]] <- value
    x
})

setMethod("$", signature("tileIterator"), function(x, name) {
    x@metadata[[name]]
})

setMethod(
    "plot", signature(x = "tileIterator", y = "missing"),
    function(x, values = "tile", color_as_factor = FALSE, ...) {
        if (length(x@tiles) == 0L) {
            stop("No tiles to plot.\nTry requesting tiles with `length()`")
        }
        values <- x@metadata[[values]]
        if (isTRUE(color_as_factor)) values <- as.factor(values)

        .preview_chunk_plan(x[], values = values, ...)
    }
)

setMethod("show", signature("tileIterator"), function(object) {
    cat("Object of class", class(object), "\n")

    # no extent, return early
    e <- object@extent
    if (length(e) == 0L) {
        cat("<empty>")
        return(invisible())
    }

    d <- dim(object)
    plist <- list(
        extent = sprintf(
            "%s (xmin, xmax, ymin, ymax)",
            paste(.ext_to_num_vec(e), collapse = ", ")
        ),
        dim = paste(dim(x), collapse = " ")
    )
    print_list(plist)
})

setMethod("nrow", signature("tileIterator"), function(x) {
    nrow(x@tiles)
})

setMethod("ncol", signature("tileIterator"), function(x) {
    res <- ncol(x@tiles)
    if (is.na(res)) {
        res <- 0 # catch for when `x@tiles` is not an array
    }
    return(res)
})

setMethod("length", signature("tileIterator"), function(x) {
    nrow(x) * ncol(x)
})

setMethod("length<-", signature("tileIterator"), function(x, value) {
    x@n <- value
    return(initialize(x))
})

setMethod("dim", signature("tileIterator"), function(x) {
    c(nrow(x), ncol(x))
})

setMethod("ext", signature("tileIterator"), function(x, ...) {
    if (length(x@extent) == 0L) {
        stop("tileIterator: No extent set", call. = FALSE)
    }
    ext(x@extent, ...)
})

setMethod("ext<-", signature("tileIterator"), function(x, value) {
    x@extent <- .ext_to_num_vec(ext(value))
    return(initialize(x))
})

setMethod("[", signature(x = "tileIterator", i = "numeric", j = "missing", drop = "missing"), function(x, i) {
    i <- as.integer(i)
    if (any(i > length(x) | i <= 0)) stop("tileIterator: subscript out of bounds", call. = FALSE)
    ij <- .tile_idx_to_ij(x, i)
    x[ij[[1]], ij[[2]]] # pass to numeric/numeric method
})

setMethod(
    "[<-",
    signature(x = "tileIterator", i = "numeric", j = "missing", value = "numeric"),
    function(x, i, j, ..., value) {
        i <- as.integer(i)
        if (any(i > length(x) | i <= 0)) stop("tileIterator: subscript out of bounds", call. = FALSE)
        ij <- .tile_idx_to_ij(x, i)
        x@tiles[ij[[1]], ij[[2]], ] <- .ext_to_num_vec(value)
        x
    }
)

setMethod("[", signature(x = "tileIterator", i = "numeric", j = "numeric", drop = "missing"), function(x, i, j) {
    mapply(function(i, j) {
        n <- ((i - 1) * ncol(x)) + j
        meta <- as.list(x@metadata[n, ])
        e <- ext(x@tiles[i, j, ])
        for (metadata in names(meta)) {
            attr(e, metadata) <- meta[[metadata]]
        }
        e
    }, i, j)
})

setMethod("[", signature(x = "tileIterator", i = "missing", j = "missing", drop = "missing"), function(x) {
    x[seq_len(length(x))] # pass to numeric/missing method
})

setMethod("[[", signature("tileIterator", i = "numeric", j = "missing"), function(x, i, j, ...) {
    x@metadata[i, ]
})

setMethod("+", signature("tileIterator", "numeric"), function(e1, e2) {
    # return without modification if tiles are not set up yet
    if (length(e1@tiles) == 0L) {
        return(e1)
    }
    e1@tiles[, , 1L] <- e1@tiles[, , 1L] - e2
    e1@tiles[, , 2L] <- e1@tiles[, , 2L] + e2
    e1@tiles[, , 3L] <- e1@tiles[, , 3L] - e2
    e1@tiles[, , 4L] <- e1@tiles[, , 4L] + e2
    e1
})

setMethod("-", signature("tileIterator", "numeric"), function(e1, e2) {
    e1 + -e2
})

# helpers ####

.DollarNames.tileIterator <- function(x, pattern) {
    colnames(x@metadata)
}

#' @name .preview_chunk_plan
#' @title Plot a preview of the chunk plan
#' @description
#' Plots the output from \code{\link{.chunk_plan}} as a set of polygons to preview.
#' Can be useful for debugging. Invisibly returns the planned chunks as a SpatVector
#' of polygons
#' @param extent_list list of extents from \code{.chunk_plan}
#' @keywords internal
#' @noRd
.preview_chunk_plan <- function(extent_list, values, mode = c("poly", "bound"), ...) {
    checkmate::assert_list(extent_list, types = "SpatExtent")
    mode <- match.arg(mode, choices = c("poly", "bound"))

    switch(mode,
        "poly" = {
            poly_list <- sapply(extent_list, terra::as.polygons)
            poly_bind <- do.call(rbind, poly_list)
            terra::plot(poly_bind, values = values, col = hcl.colors(100), ...)
            return(invisible(poly_bind))
        },
        "bound" = {
            xlim <- c(extent_list[[1]]$xmin, extent_list[[length(extent_list)]]$xmax)
            ylim <- c(extent_list[[1]]$ymin, extent_list[[length(extent_list)]]$ymax)
            # initiate plot
            plot(x = NULL, y = NULL, asp = 1L, xlim = xlim, ylim = ylim, ...)
            # plot extent bounds
            for (e in extent_list) {
                rect(e$xmin, e$ymin, e$xmax, e$ymax)
            }
            return(invisible())
        }
    )
}

.tile_idx_to_ij <- function(x, i) {
    i_idx <- floor(i / ncol(x)) + 1L
    no_resid <- i %% ncol(x) == 0L
    i_idx[no_resid] <- i_idx[no_resid] - 1L
    j_idx <- i %% ncol(x)
    j_idx[j_idx == 0L] <- ncol(x)
    list(i_idx, j_idx)
}

.get_dim_n_chunks <- function(n, e) {
    # find x to y ratio as 'r'
    e <- e[]
    r <- (e[["xmax"]] - e[["xmin"]]) / (e[["ymax"]] - e[["ymin"]])

    # x * y = n = ... ry^2 = n
    y <- ceiling(sqrt(n / r))
    x <- ceiling(n / y)

    return(c(y, x))
}

.chunk_plan <- function(extent, min_chunks = NULL, nrows = NULL, ncols = NULL) {
    checkmate::assert_class(extent, "SpatExtent")
    if (!is.null(nrows)) {
        checkmate::assert_true(length(c(nrows, ncols)) == 2L)
    } else {
        checkmate::assert_numeric(min_chunks)
        res <- .get_dim_n_chunks(n = min_chunks, e = extent)
        nrows <- res[1L]
        ncols <- res[2L]
    }

    x_stops <- seq(
        from = terra::xmin(extent),
        to = terra::xmax(extent),
        length.out = ncols + 1L
    )
    y_stops <- seq(
        from = terra::ymin(extent),
        to = terra::ymax(extent),
        length.out = nrows + 1L
    )

    # vector of extent values
    e_vec <- c()
    for (i in seq_len(nrows)) {
        for (j in seq_len(ncols)) {
            e_vec <- c(
                e_vec,
                x_stops[j],
                x_stops[j + 1L],
                y_stops[i],
                y_stops[i + 1L]
            )
        }
    }

    a <- array(e_vec, dim = c(4, ncols, nrows))
    a <- aperm(a, perm = c(3, 2, 1))
    # reverse order of rows so tiles count from top to bottom
    a <- a[seq(from = nrow(a), to = 1), , , drop = FALSE]

    # validate array output
    if (!length(dim(a)) == 3) {
        stop("in .chunk_plan() output: invalid dims", call. = FALSE)
    }

    return(a)
}

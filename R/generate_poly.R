# simple polygon generation ####


#' @title Spatial polygons stamp
#' @name polyStamp
#' @description Takes a given stamp polygon and places it at each spatial
#' location provided.
#' @param stamp_dt data.table with x and y vertices for a polygon to be stamped.
#' Column names are expected to be 'x' and 'y' respectively
#' @param spatlocs spatial locations with x and y coordinates where polygons
#' should be stamped. Column names are 'cell_ID', 'sdimx' and 'sdimy' by default
#' @param id_col column in spatlocs to use as IDs (default is 'cell_ID')
#' @param x_col column in spatlocs to use as x locations (default is 'sdimx')
#' @param y_col column in spatlocs to use as y locations (default is 'sdimy')
#' @param verbose be verbose
#' @returns a data.table of polygon vertices
#' @family polygon stamping
#' @examples
#'
#' # stamp shapes
#' hex <- hexVertices(radius = 1)
#' spatlocs <- data.table::data.table(
#'     sdimx = rnorm(10, mean = 5, sd = 20),
#'     sdimy = rnorm(10, mean = 5, sd = 20),
#'     cell_ID = paste0("spot_", seq_len(10))
#' )
#' random_hex <- polyStamp(hex, spatlocs)
#' random_hex_poly <- createGiottoPolygon(random_hex)
#' plot(random_hex_poly)
#'
#' # make a grid of shapes
#' e <- c(0, 1000, 0, 1000)
#' tg <- triGrid(extent = e, ccd = 100, id_prefix = "bin_")
#'
#' r <- rectVertices(dims = c(x = 60, y = 50))
#'
#' rect_grid_dt <- polyStamp(stamp_dt = r, spatlocs = tg)
#' rect_poly <- createGiottoPolygon(rect_grid_dt)
#' plot(rect_poly)
#'
#' @seealso [generate_grid] [tessellate]
#' @export
polyStamp <- function(stamp_dt,
    spatlocs,
    id_col = "cell_ID",
    x_col = "sdimx",
    y_col = "sdimy",
    verbose = TRUE) {
    # data.table vars
    spatlocs_idx <- rel_vertices_idx <- poly_ID <- NULL

    if (!all(c(id_col, x_col, y_col) %in% colnames(spatlocs))) {
        stop(wrap_txt("Not all colnames found in spatlocs"))
    }

    # define polys relative to centroid of stamp_dt
    # add necessary columns to make a polygon from matrix
    stamp_dt$geom <- 1
    stamp_dt$part <- 0
    stamp_dt$hole <- 0

    data.table::setcolorder(stamp_dt,
        neworder = c("geom", "part", "x", "y", "hole")
    )
    stamp_poly <- terra::vect(as.matrix(stamp_dt), type = "polygons")

    centroid_dt <- data.table::as.data.table(terra::centroids(stamp_poly),
        geom = "XY",
        include_values = FALSE
    )

    stamp_centroid <- c(
        x = centroid_dt$x,
        y = centroid_dt$y
    )

    rel_vertices <- data.table::data.table(
        x = stamp_dt$x - stamp_centroid[["x"]],
        y = stamp_dt$y - stamp_centroid[["y"]]
    )

    # generate poly vertices around given spatlocs
    poly_dt <- data.table::CJ(seq_len(nrow(spatlocs)),
        seq_len(nrow(rel_vertices)),
        sorted = FALSE
    )
    colnames(poly_dt) <- c("spatlocs_idx", "rel_vertices_idx")

    # compute the absolute coordinates of the polygon vertices for each
    # spatial location
    poly_dt[, c(x_col, y_col) := {
        spat_row <- spatlocs[spatlocs_idx]
        rel_row <- rel_vertices[rel_vertices_idx]
        list(spat_row[[x_col]] + rel_row$x, spat_row[[y_col]] + rel_row$y)
    }]

    # add a new column 'poly_ID' to 'poly_dt' with the ID of each spatial
    # location.
    poly_dt[, poly_ID := spatlocs[poly_dt$spatlocs_idx, id_col, with = FALSE]]
    poly_dt$poly_ID <- as.character(poly_dt$poly_ID)

    if (isTRUE(verbose)) wrap_msg(nrow(spatlocs), "polygons generated")

    res <- poly_dt[, c(x_col, y_col, "poly_ID"), with = FALSE]
    setnames(res, c(x_col, y_col), c("x", "y"))

    return(res)
}


#' @title Generate circle polygon vertices
#' @name circleVertices
#' @description Generates vertex coordinates for a circle around (0,0) with the
#' given radius. Modified from \pkg{packcircles}.
#' @param radius radius of circle to be drawn
#' @param npoints number of vertices to generate
#' @returns a data.table of circle vertices
#' @family polygon stamping
#' @seealso [generate_grid]
#' @examples
#' circleVertices(radius = 10)
#' @export
circleVertices <- function(radius,
    npoints = 25) {
    a <- seq(0, 2 * pi, length.out = npoints + 1)
    x <- radius * cos(a)
    y <- radius * sin(a)
    m <- data.table::data.table(x = x, y = y)
    return(m)
}


#' @title Generate rectangular polygon vertices
#' @name rectVertices
#' @description Generates vertex coordinates for a rectangle with dimensions
#' given through \code{dims} param.
#' @param dims named vector in the style of c(x = \code{numeric},
#' y = \code{numeric}) that defines the width (x) and height (y) of the
#' generated rectangle polygon.
#' @returns a data.table of rectangle vertices
#' @family polygon stamping
#' @seealso [generate_grid]
#' @examples
#' rectVertices(c(x = 1, y = 2))
#' @export
rectVertices <- function(dims) {
    if (length(dims) == 1) {
        xdim <- ydim <- dims
    } else {
        xdim <- dims[["x"]]
    }
    ydim <- dims[["y"]]

    m <- data.table::data.table(
        x = c(0, 0, xdim, xdim),
        y = c(0, ydim, ydim, 0)
    )
    return(m)
}


#' @title Generate regular hexagon vertices
#' @name hexVertices
#' @description Generates vertex coordinates for a regular hexagon.
#' @param radius radius of the hexagon
#' @param  major_axis orientation of the major axis 'v' is vertical (default)
#' and 'h' is horizontal
#' @returns a data.table of regular hexagon vertices
#' @family polygon stamping
#' @seealso [generate_grid]
#' @examples
#' hexVertices(radius = 10)
#' @export
hexVertices <- function(radius, major_axis = c("v", "h")) {
    major_axis <- match.arg(major_axis, choices = c("v", "h"))
    r <- radius
    v <- data.table::data.table(
        # counter clockwise
        x = c(
            0, # A
            (sqrt(3) * r) / 2, # B
            (sqrt(3) * r) / 2, # C
            0, # D
            -(sqrt(3) * r) / 2, # E
            -(sqrt(3) * r) / 2 # F
        ),
        y = c(
            r, # A
            r / 2, # B
            -r / 2, # C
            -r, # D
            -r / 2, # E
            r / 2 # F
        )
    )
    if (major_axis == "v") {
        return(v)
    }
    if (major_axis == "h") {
        h <- data.table::data.table()
        h$x <- v$y
        h$y <- v$x
        return(h)
    }
}






# array generation ####


#' @title Tessellated grid of polygons
#' @name tessellate
#' @aliases tesselate
#' @description Generates a tessellated grid of polygons within the provided
#' spatial extent
#' @param extent SpatExtent or anything else a SpatExtent can be extracted or
#' created from
#' @param shape Shape of the tessellation grid. Available options are "hexagon"
#' and "square".
#' @param shape_size numeric. Size of shape to tessellate. (x-axis width for
#' hexagons, side length for squares)
#' @param name name of giottoPolygons grid to make
#' @param gap numeric. Shrink polygons to add a gap between tessellated polygons
#' @param id_prefix character. prefix to add to poly_ID names generated
#' @param radius deprecated. numeric. Radius size of the tessellation grid.
#' @details This function generates a tessellated grid of spatial locations
#' based on the input spatial locations. The shape of the tessellation grid
#' can be either hexagonal or square. The shape_size parameter determines the
#' size of the grid cells or the bin size.
#' @returns A giottoPolygon
#' @examples
#' # Create an extent across which to generate tessellated polygons
#' e <- ext(0, 100, 0, 100)
#' # produce hexagons with a diameter of 10
#' x <- tessellate(extent = e, shape = "hexagon", shape_size = 10)
#' plot(x)
#'
#' # same size grid, but now with a gap
#' x <- tessellate(extent = e, shape = "hexagon", shape_size = 10, gap = 1)
#' plot(x)
#'
#' # square grid with a gap
#' x <- tessellate(extent = e, shape = "square", shape_size = 10, gap = 1)
#' plot(x)
#' @concept spatial location
#' @export
tessellate <- function(extent,
    shape = c("hexagon", "square"),
    shape_size = NULL,
    gap = 0,
    radius = NULL,
    id_prefix = "ID_",
    name = "grid") {
    if (is.null(radius) && is.null(shape_size)) stop("shape_size must be given")
    if (!is.null(radius)) shape_size <- radius * 2

    shape <- match.arg(shape, choices = c("hexagon", "square"))
    if (shape == "hexagon") grid <- "triangular"
    if (shape == "square") grid <- "orthogonal"
    e <- ext(extent)[]
    checkmate::assert_numeric(shape_size)
    checkmate::assert_character(name)

    # Calculate the minimum difference between the x and y coordinates of the
    # points in spat_locs
    x_range <- c(e[["xmin"]], e[["xmax"]])
    y_range <- c(e[["ymin"]], e[["ymax"]])

    # Check if shape_size size exceeds x,y range
    if ((diff(x_range) / shape_size < 1) && (diff(y_range) / shape_size < 1)) {
        stop(wrap_txt("Please choose a smaller shape_size for tessellation."))
    }

    # generate shape to tessellate
    stamp_dt <- switch(shape,
        "hexagon" = hexVertices(
            radius = (shape_size / 2 / sqrt(0.75)) - gap, major_axis = "v"
        ),
        "square" = rectVertices(dims = c(
            x = (shape_size - gap),
            y = (shape_size - gap)
        ))
    )

    # get grid centers to tessellate
    centers <- switch(grid,
        "triangular" = triGrid(extent, ccd = shape_size, id_prefix = id_prefix),
        "orthogonal" = orthoGrid(extent,
            ccd = shape_size,
            id_prefix = id_prefix
        )
    )


    # Call polyStamp function to generate the tessellated grid
    res <- polyStamp(stamp_dt, centers)

    createGiottoPolygonsFromDfr(
        res,
        name = name,
        verbose = FALSE,
        skip_eval_dfr = TRUE,
        copy_dt = FALSE
    )
}





#' @title Spatial grids
#' @name generate_grid
#' @description
#' Generate a spatial grid of spatial locations
#' @param extent SpatExtent or anything else a SpatExtent can be extracted or
#' created from
#' @param ccd center to center distance
#' @param id_prefix character. prefix to add to ID names generated
#' @returns spatial grid
#' @examples
#' e <- ext(0, 100, 0, 100)
#' x <- triGrid(extent = e, ccd = 10)
#' plot(x$sdimx, x$sdimy, asp = 1L)
#'
#' x <- orthoGrid(extent = e, ccd = 10)
#' plot(x$sdimx, x$sdimy, asp = 1L)
#' @concept spatial location
NULL

#' @rdname generate_grid
#' @export
triGrid <- function(extent, ccd, id_prefix = "ID_") {
    e <- ext(extent)[]
    # Create a tessellation grid of points where the hexagons will be centered
    # Adjust the y-sequence spacing to be 1.5/2*ccd for hexagonal packing
    y_seq <- seq(e[["ymin"]] + (0.5 * ccd), e[["ymax"]] - (0.5 * ccd),
        by = ccd * sqrt(0.75)
    )
    centers <- data.table::rbindlist(lapply(seq_along(y_seq), function(i) {
        x_start <- if (i %% 2 == 0) {
            e[["xmin"]] + (0.5 * ccd)
        } else {
            e[["xmin"]] + ccd
        }
        x_seq <- seq(x_start, e[["xmax"]] - (0.5 * ccd), by = ccd)
        data.table::data.table(sdimx = x_seq, sdimy = y_seq[i])
    }))

    centers$cell_ID <- paste0(id_prefix, seq(nrow(centers)))
    return(centers)
}


#' @rdname generate_grid
#' @export
orthoGrid <- function(extent, ccd, id_prefix = "ID_") {
    e <- ext(extent)[]
    # Create a tessellation grid of points where the squares will be centered
    x_seq <- seq(e[["xmin"]] + (0.5 * ccd), e[["xmax"]] - (0.5 * ccd), by = ccd)
    y_seq <- seq(e[["ymin"]] + (0.5 * ccd), e[["ymax"]] - (0.5 * ccd), by = ccd)
    centers <- expand.grid(sdimx = x_seq, sdimy = y_seq)
    centers$cell_ID <- paste0(id_prefix, seq(nrow(centers)))
    setDT(centers)
    return(centers)
}













#' @title makePseudoVisium
#' @name makePseudoVisium
#' @description Generates a pseudo-visium grid of spots across a provided
#' spatial extent
#' @param extent SpatExtent or anything else a SpatExtent can be extracted or
#' created from
#' @param micron_size size of a micrometer relative to spatial coordinates
#' @param name character. (default is 'pseudo_visium') Name of giottoPolygon
#' object to create
#' @details This function generates a pseudo-Visium grid of spots based on the
#' input spatial locations. The micron_size param is used to determine the size
#' of the spots
#' @returns A giottoPolygon for the pseudo-visium spots.
#' @examples
#' e <- ext(0, 2000, 0, 2000)
#' x <- makePseudoVisium(extent = e, micron_size = 1)
#' plot(x)
#' @concept spatial location
#' @export
makePseudoVisium <- function(extent = NULL,
    micron_size = 1,
    name = "pseudo_visium") {
    e <- ext(extent)[]

    # Visium default scale parameters
    visium_radius_um <- 27.5
    visium_center_center_dist_um <- 100
    visium_gap_um <- 45

    # Compute metrics to visium scale
    radius <- visium_radius_um / micron_size
    gap <- (visium_gap_um / visium_radius_um) * radius

    # Define a data.table with the vertices of a circle centered around (0,0)
    stamp_dt <- circleVertices(radius = radius, npoints = 100)

    # Create a grid of y points where the circles will be centered
    y_seq <- seq(e[["ymin"]] + radius, e[["ymax"]] - radius,
        by = 2 * radius + gap
    )

    # Stagger center point of circles to match visium staggered grid
    centers <- data.table::rbindlist(lapply(seq_len(length(y_seq)), function(i) {
        x_start <- if (i %% 2 == 0) {
            e[["xmin"]] + radius + (2 * radius + gap) / 2
        } else {
            e[["xmin"]] + radius
        }
        x_seq <- seq(x_start, e[["xmax"]] - radius, by = 2 * radius + gap)
        data.table::data.table(sdimx = x_seq, sdimy = y_seq[i])
    }))
    centers$cell_ID <- paste0("spot_", seq_len(nrow(centers)))

    # Call polyStamp function on centers to generate the pseudo-visium grid
    res <- polyStamp(stamp_dt, centers)

    createGiottoPolygonsFromDfr(res,
        name = name,
        skip_eval_dfr = TRUE,
        copy_dt = FALSE
    )
}

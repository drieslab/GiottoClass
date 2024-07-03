#' @include generics.R
NULL



# createNetwork ####

#' @name createNetwork
#' @title Create a network
#' @description Create networks from node values. This is a hub function for
#' many different methods of finding nearest neighbors. See details for
#' additional params important for generating specific types of networks.
#' @param x matrix. Data to treat as nodes. Examples include expression
#' information, PCA matrix, spatial locations xy(z) coordinates.
#' @param type type of network to create. Currently: "sNN", "kNN", or "delaunay"
#' @param method method used to create the type of network requested.
#' One of "dbscan" for sNN and kNN or "geometry", "RTriangle", or "deldir"
#' for delaunay.
#' @param node_ids character. Node ID values to assign. If NULL, integer indices
#' will be used as node IDs.
#' @param include_weight logical. include edge weight attribute in output
#' @param include_distance logical. include edge distance attribute in output
#' @param as.igraph logical. Whether to return as `igraph`. Otherwise returns
#' as `data.table`
#' @param verbose be verbose. Default = NULL (uses giotto verbosity option)
#' @param ... additional params to pass. See details section
#' @details
#' Additional params are described below. Items in parenthesis refer to which
#' network types and/or methods the params are specific to.
#' - \[**`k`**\] numeric. (*sNN, kNN*) number of neighbors to find. Default is
#' 30
#' - \[**`minimum_shared`**\] numeric. (*sNN*) minimum shared neighbors allowed
#'   per edge
#' - \[**`top_shared`**\] numeric. (*sNN*) keep at least this many edges per
#' node, where kept edges are top ranked in terms of number of shared neighbors.
#' - \[**`filter`**\] logical. (*kNN*) whether to filter for only unique
#'   edges and apply `minimum_k` and `maximum_distance` filters. Should be set
#'   `TRUE` when generating a spatial kNN network. Default is `FALSE.`
#' - \[**`minimum_k`**\] (*delaunay, kNN*) minimum nearest neighbours if
#'   `maximum_distance != NULL`
#' - \[**`maximum_distance`**\] (*delaunay, kNN*) edge maximum euclidean
#'   distance allowed
#' - \[**`Y`**\] (*RTriangle*) If TRUE prohibits the insertion of Steiner points
#'   on the mesh boundary. Default is TRUE
#' - \[**`j`**\] (*RTriangle*) If TRUE jettisons vertices that are not part of
#'   the final triangulation from the output. Default is TRUE
#' - \[**`S`**\] (*RTriangle*) Specifies the maximum number of added Steiner
#'   points. Default is 0
#' - \[**`options`**\] (*geometry*) default is "Pp". See [geometry::delaunayn]
#' - \[**`weight_fun`**\] function to calculate weights based on distance if
#'   `include_weight = TRUE`. Default is \eqn{weight = 1 / (1 + distance)} for
#'   `"kNN"` and `"sNN"` types and \eqn{weight = 1 / distance} for `delaunay`
#'   type
#' networks
#' @returns Either `igraph` if `as.igraph = TRUE` and `data.table` otherwise.
#' @examples
#' pca <- GiottoData::loadSubObjectMini("dimObj")[]
#' sl <- GiottoData::loadSubObjectMini("spatLocsObj")[]
#'
#' # Delaunay via geometry::delaunayn()
#' del_geom <- createNetwork(
#'     x = as.matrix(sl[, .(sdimx, sdimy)]),
#'     type = "delaunay",
#'     method = "geometry",
#'     include_weight = TRUE,
#'     weight_fun = function(d) 1 / d,
#'     as.igraph = FALSE,
#'     node_ids = sl$cell_ID
#' )
#'
#' # Delaunay via RTriangle::triangulate()
#' del_rt <- createNetwork(
#'     x = as.matrix(sl[, .(sdimx, sdimy)]),
#'     type = "delaunay",
#'     method = "RTriangle",
#'     include_weight = TRUE,
#'     weight_fun = function(d) 1 / d,
#'     as.igraph = FALSE,
#'     node_ids = sl$cell_ID
#' )
#'
#' # Delaunay via deldir::deldir()
#' del_dd <- createNetwork(
#'     x = as.matrix(sl[, .(sdimx, sdimy)]),
#'     type = "delaunay",
#'     method = "deldir",
#'     include_weight = TRUE,
#'     weight_fun = function(d) 1 / d,
#'     as.igraph = FALSE,
#'     node_ids = sl$cell_ID
#' )
#'
#' # kNN spatial network
#' kNN_spat <- createNetwork(
#'     x = as.matrix(sl[, .(sdimx, sdimy)]),
#'     type = "kNN",
#'     method = "dbscan",
#'     include_weight = TRUE,
#'     weight_fun = function(d) 1 / d, # not the default
#'     as.igraph = FALSE,
#'     node_ids = sl$cell_ID,
#'     k = 4L,
#'     maximum_distance = NULL,
#'     minimum_k = 0L
#' )
#'
#' # kNN NN network
#' kNN <- createNetwork(
#'     pca[, seq_len(10)],
#'     type = "kNN",
#'     method = "dbscan",
#'     node_ids = rownames(pca),
#'     as.igraph = TRUE
#' )
#'
#' # sNN NN network
#' sNN <- createNetwork(
#'     pca[, seq_len(10)],
#'     type = "sNN",
#'     method = "dbscan",
#'     node_ids = rownames(pca),
#'     as.igraph = TRUE
#' )
#'
#' # using defaults for sNN with index IDs to create igraph
#' sNN_idx <- createNetwork(pca[, seq_len(10)])
NULL

#' @rdname createNetwork
#' @export
createNetwork <- function(
        x,
        type = c("sNN", "kNN", "delaunay"),
        method = c("dbscan", "geometry", "RTriangle", "deldir"),
        node_ids = NULL,
        include_distance = TRUE,
        include_weight = TRUE,
        as.igraph = TRUE,
        verbose = NULL,
        ...) {
    # NSE vars
    from <- to <- NULL

    # check params
    type <- match.arg(type, choices = c("sNN", "kNN", "delaunay"))
    method <- switch(type,
        "sNN" = match.arg(method, choices = c("dbscan"), several.ok = TRUE),
        "kNN" = match.arg(method, choices = c("dbscan"), several.ok = TRUE),
        "delaunay" = match.arg(
            method,
            choices = c("geometry", "RTriangle", "deldir"),
            several.ok = TRUE
        )
    )

    # get common params
    alist <- list(
        x = x,
        include_weight = include_weight,
        ...
    )

    # generate network data.table
    network_dt <- switch(sprintf("%s:%s", type, method),
        "kNN:dbscan" = do.call(.net_dt_knn, args = alist),
        "sNN:dbscan" = do.call(.net_dt_snn, args = alist),
        "delaunay:deldir" = do.call(.net_dt_del_deldir,
            args = alist
        )$delaunay_network_DT,
        "delaunay:RTriangle" = do.call(.net_dt_del_rtriangle,
            args = alist
        )$delaunay_network_DT,
        "delaunay:geometry" = do.call(.net_dt_del_geometry,
            args = alist
        )$delaunay_network_DT
    )

    # replace indices with node_ids if desired
    if (!is.null(node_ids)) {
        # if node_ids are provided, include as extra cols
        names(node_ids) <- seq_along(node_ids)
        network_dt[, "from" := node_ids[from]]
        network_dt[, "to" := node_ids[to]]
    }

    ## outputs ##

    # cols to include in output
    keep_cols <- c("from", "to")

    all_index <- network_dt[, unique(unlist(.SD)), .SDcols = keep_cols]
    if (include_weight) keep_cols <- c(keep_cols, "weight")
    if (include_distance) keep_cols <- c(keep_cols, "distance")

    if (type == "sNN") keep_cols <- c(keep_cols, "shared", "rank")

    # return early if igraph not required
    network_dt_final <- network_dt[, keep_cols, with = FALSE]
    if (!as.igraph) {
        return(network_dt_final)
    }

    ## convert to igraph object
    out <- igraph::graph_from_data_frame(
        network_dt_final,
        directed = TRUE,
        vertices = all_index
    )

    return(out)
}


# x input is a matrix
.net_dt_knn <- function(
        x, k = 30L, include_weight = TRUE, include_distance = TRUE,
        filter = FALSE,
        maximum_distance = NULL, minimum_k = 0L,
        weight_fun = function(d) 1 / (1 + d),
        verbose = NULL, ...) {
    # NSE vars
    from <- to <- distance <- NULL

    k <- as.integer(k)

    if (k >= nrow(x)) {
        k <- (nrow(x) - 1L)
        vmsg(.v = verbose, "k is higher than total number of cells.
         Adjusted to (total number of cells - 1)")
    }
    # distances must be calculated when a limit is set
    if (!is.null(maximum_distance)) include_distance <- TRUE

    nn_network <- dbscan::kNN(x = x, k = k, sort = TRUE, ...)

    nn_network_dt <- data.table::data.table(
        from = rep(seq_len(nrow(nn_network$id)), k),
        to = as.vector(nn_network$id)
    )

    # optional info
    if (include_distance || include_weight) {
        if (!is.null(maximum_distance)) {
            # maximum_distance flag treated as a flag to use this function for
            # spatial network purposes.
            #
            # Use the input matrix coords instead of those exported from dbscan
            # needed for filtering
            nn_network_dt[, "distance" := edge_distances(x, .SD),
                .SDcols = c("from", "to")
            ]
        } else {
            nn_network_dt[, "distance" := as.vector(nn_network$dist)]
        }
    }
    if (include_weight) {
        nn_network_dt[, "weight" := weight_fun(distance)]
    }

    # filtering by distance and min k is done when maximum_distance is not NULL
    if (filter) {
        nn_network_dt <- .filter_network(
            networkDT = nn_network_dt,
            maximum_distance = maximum_distance,
            minimum_k = minimum_k
        )
    }

    return(nn_network_dt)
}

# x input is a matrix
.net_dt_snn <- function(
        x, k = 30L, include_weight = TRUE, include_distance = TRUE,
        top_shared = 3L, minimum_shared = 5L,
        weight_fun = function(d) 1 / (1 + d),
        verbose = NULL, ...) {
    # NSE vars
    from <- to <- shared <- distance <- NULL

    k <- as.integer(k)
    top_shared <- as.integer(top_shared)
    minimum_shared <- as.integer(minimum_shared)

    if (k >= nrow(x)) {
        k <- (nrow(x) - 1L)
        vmsg(.v = verbose, "k is higher than total number of cells.
         Adjusted to (total number of cells - 1)")
    }

    nn_network <- dbscan::kNN(x = x, k = k, sort = TRUE, ...)
    snn_network <- dbscan::sNN(x = nn_network, k = k, kt = NULL, ...)

    snn_network_dt <- data.table::data.table(
        from = rep(seq_len(nrow(snn_network$id)), k),
        to = as.vector(snn_network$id),
        shared = as.vector(snn_network$shared)
    )
    snn_network_dt <- snn_network_dt[stats::complete.cases(snn_network_dt)]

    # optional info
    if (include_distance || include_weight) {
        snn_network_dt[, "distance" := as.vector(snn_network$dist)]
    }
    if (include_weight) {
        snn_network_dt[, "weight" := weight_fun(distance)]
    }


    # rank snn. LOWER ranking means MORE shared per source
    data.table::setorder(snn_network_dt, from, -shared)
    snn_network_dt[, rank := seq_len(.N), by = from]

    # filter snn
    # keep at at least `top_shared` - 1 interactions where the ones selected
    # should have more connections than the cutoff. Also keep any interactions
    # with more shared than `minimum_shared`
    snn_network_dt <- snn_network_dt[rank <= top_shared |
        shared >= minimum_shared]

    return(snn_network_dt)
}

.net_dt_del_geometry <- function(
        x, include_weight = TRUE, options = "Pp", maximum_distance = "auto",
        minimum_k = 0L, weight_fun = function(d) 1 / d,
        ...) {
    package_check("geometry", repository = "CRAN:geometry")

    # data.table variables
    from <- to <- distance <- NULL

    delaunay_simplex_mat <- geometry::delaunayn(
        p = x, options = options, ...
    )

    geometry_obj <- list("delaunay_simplex_mat" = delaunay_simplex_mat)
    edge_combs <- utils::combn(x = ncol(delaunay_simplex_mat), m = 2L)
    delaunay_edges <- data.table::as.data.table(apply(
        edge_combs,
        MARGIN = 1L, function(comb) delaunay_simplex_mat[, comb]
    ))

    ### making sure of no duplication ###
    delaunay_edges_dedup <- unique(delaunay_edges)
    igraph_obj <- igraph::graph_from_edgelist(as.matrix(delaunay_edges_dedup))
    adj_obj <- igraph::as_adjacency_matrix(igraph_obj)
    igraph_obj2 <- igraph::graph.adjacency(adj_obj)
    delaunay_edges_dedup2 <- igraph::get.data.frame(igraph_obj2)
    delaunay_network_dt <- data.table::as.data.table(delaunay_edges_dedup2)
    delaunay_network_dt[, from := as.integer(from)]
    delaunay_network_dt[, to := as.integer(to)]
    data.table::setorder(delaunay_network_dt, from, to)

    # needed for filtering
    delaunay_network_dt[, "distance" := edge_distances(x, .SD),
        .SDcols = c("from", "to")
    ]

    # optional cols
    if (include_weight) {
        delaunay_network_dt[, "weight" := weight_fun(distance)]
    }

    delaunay_network_dt <- .filter_network(
        networkDT = delaunay_network_dt,
        maximum_distance = maximum_distance,
        minimum_k = minimum_k
    )

    out_object <- list(
        "geometry_obj" = geometry_obj,
        "delaunay_network_DT" = delaunay_network_dt
    )
    return(out_object)
}

.net_dt_del_rtriangle <- function(
        x, include_weight = TRUE, maximum_distance = "auto", minimum_k = 0L,
        Y = TRUE, j = TRUE, S = 0, weight_fun = function(d) 1 / d,
        ...) {
    # NSE vars
    from <- to <- distance <- NULL

    package_check("RTriangle", repository = "CRAN:RTriangle")

    rtriangle_obj <- RTriangle::triangulate(
        RTriangle::pslg(x),
        Y = Y, j = j, S = S,
        ...
    )

    delaunay_network_dt <- data.table::data.table(
        from = rtriangle_obj$E[, 1],
        to = rtriangle_obj$E[, 2]
    )

    data.table::setorder(delaunay_network_dt, from, to)

    # needed for filtering
    delaunay_network_dt[, "distance" := edge_distances(x, .SD),
        .SDcols = c("from", "to")
    ]

    # optional cols
    if (include_weight) {
        delaunay_network_dt[, "weight" := weight_fun(distance)]
    }

    delaunay_network_dt <- .filter_network(
        networkDT = delaunay_network_dt,
        maximum_distance = maximum_distance,
        minimum_k = minimum_k
    )

    out_object <- list(
        "RTriangle_obj" = rtriangle_obj,
        "delaunay_network_DT" = delaunay_network_dt
    )
    return(out_object)
}

.net_dt_del_deldir <- function(
        x, include_weight = TRUE, maximum_distance = "auto", minimum_k = 0L,
        weight_fun = function(d) 1 / d,
        ...) {
    # NSE variables
    from <- to <- distance <- NULL

    if (ncol(x) > 2L) {
        .gstop("\'deldir\' delaunay method only applies to 2D data.
           use method \'geometry\' or \'RTriangle\' instead")
    }

    deldir_obj <- deldir::deldir(x = x, ...)

    delaunay_network_dt <- data.table::data.table(
        from = deldir_obj$delsgs$ind1,
        to = deldir_obj$delsgs$ind2
    )

    data.table::setorder(delaunay_network_dt, from, to)

    delaunay_network_dt[, "distance" := edge_distances(x, .SD),
        .SDcols = c("from", "to")
    ]

    # optional cols
    if (include_weight) {
        delaunay_network_dt[, "weight" := weight_fun(distance)]
    }

    delaunay_network_dt <- .filter_network(
        networkDT = delaunay_network_dt,
        maximum_distance = maximum_distance,
        minimum_k = minimum_k
    )

    out_object <- list(
        "deldir_obj" = deldir_obj,
        "delaunay_network_DT" = delaunay_network_dt
    )
    return(out_object)
}



# distances calculation ####

#' @name edge_distances
#' @title Calculate network edge euclidean distances
#' @param x matrix of nodes info with coords. Rows should be samples, Cols
#' should be variables
#' @param y network data.table with `from` and `to` cols. Usually integer
#' indices matching the rows of x.
#' @param x_node_ids if y is indexed by character in from and to cols, then the
#' node IDs that apply to the coords in x must be supplied as a character vector
#' @returns numeric
#' @examples
#' m <- matrix(c(0, 0, 0, 1, 1, 1, 3, 2, 4), byrow = TRUE, nrow = 3)
#' edges <- data.table::data.table(
#'     from = c(1, 1),
#'     to = c(2, 3)
#' )
#'
#' edge_distances(m, edges)
#' @export
edge_distances <- function(x, y, x_node_ids = NULL) {
    .calc_edge_dist(.edge_coords_array(x, y))
}



# Nodes row order is assumed to be the same as the network indices
#' @title Numerical array of edge start and end
#' @name .edge_coords_array
#' @description
#' Generate a \eqn{2} x \eqn{j} x \eqn{k} numerical array of edge start and end
#' coordinates. Rows correspond  to start and end. Cols are for each variable
#' ie x, y, (z) or whatever other variable is used to measure sample location
#' in graph space. The third dim is for each sample. This layout makes it easy
#' to iterate across matrix slices of this array with `[stats::dist()]`.
#' @param x matrix of nodes info with coords
#' @param y network data.table with `from` and `to` cols
#' @param x_node_ids if y is indexed by character in from and to cols, then the
#' node IDs that apply to the coords in x must be supplied as a character vector
#' @returns numeric
#' @keywords internal
.edge_coords_array <- function(x, y, x_node_ids = NULL) {
    # NSE vars
    from <- to <- NULL

    checkmate::assert_matrix(x)
    checkmate::assert_data_table(y)

    # if indexed by character
    if (y[, is.character(from) && is.character(to)]) {
        # try to match against the cell_ID col in nodes info
        if (is.null(x_node_ids)) {
            .gstop("y is indexed by node ID.
            Node IDs for x must be provided as a vector to 'x_node_ids'")
        }
        # convert to int indexing (should match x by row)
        y <- data.table::copy(y)
        y[, from := match(from, x_node_ids)]
        y[, to := match(to, x_node_ids)]
    }

    edge_coords_array <- array(
        dim = c(nrow(y), ncol(x), 2),
        dimnames = list(
            c(),
            paste0("dim_", seq(ncol(x))),
            c("start", "end")
        )
    )

    edge_coords_array[, , 1] <- x[y$from, ]
    edge_coords_array[, , 2] <- x[y$to, ]
    edge_coords_array <- aperm(edge_coords_array, perm = c(3, 2, 1))
    class(edge_coords_array) <- c("edge_coords_array", class(edge_coords_array))

    return(edge_coords_array)
}

# x should be an edge_coords_array
.calc_edge_dist <- function(x, method = "euclidean", ...) {
    checkmate::assert_class(x, "edge_coords_array")

    vapply(
        seq(dim(x)[3L]),
        function(pair_i) stats::dist(x[, , pair_i], method = method, ...),
        FUN.VALUE = numeric(1L)
    )
}





# original implementations ####


#' @title createNearestNetwork
#' @name createNearestNetwork
#' @description create a nearest neighbour (NN) network
#' @inheritParams data_access_params
#' @param type sNN or kNN
#' @param dim_reduction_to_use dimension reduction method to use
#' @param dim_reduction_name name of dimension reduction set to use
#' @param dimensions_to_use number of dimensions to use as input
#' @param name arbitrary name for NN network. Defaults to
#' \[type\].\[dim_reduction_to_use\]
#' @param feats_to_use if dim_reduction_to_use = NULL, which genes to use
#' @param expression_values expression values to use
#' @param return_gobject boolean: return giotto object (default = TRUE)
#' @param k number of k neighbors to use
#' @param minimum_shared minimum shared neighbors
#' @param top_shared keep at ...
#' @param verbose be verbose
#' @param ... additional parameters for kNN and sNN functions from dbscan
#' @returns giotto object with updated NN network
#' @details This function creates a k-nearest neighbour (kNN) or shared
#' nearest neighbour (sNN) network based on the provided dimension reduction
#' space. To run it directly on the gene expression matrix
#' set \emph{dim_reduction_to_use = NULL}.
#'
#' See also \code{\link[dbscan]{kNN}} and \code{\link[dbscan]{sNN}} for more
#' information about how the networks are created.
#'
#' Output for kNN:
#'   * **from:** cell_ID for source cell
#'   * **to:** cell_ID for target cell
#'   * **distance:** distance between cells
#'   * **weight:** \eqn{1/(1 + distance)}
#'
#' Output for sNN:
#'   * **from:** cell_ID for source cell
#'   * **to:** cell_ID for target cell
#'   * **distance:** distance between cells
#'   * **weight:** \eqn{1/(1 + distance)}
#'   * **shared:** number of shared neighbours
#'   * **rank:** ranking of pairwise cell neighbours
#' 
#' For sNN networks two additional parameters can be set:
#'   * **minimum_shared:** minimum number of shared neighbours needed
#'   * **top_shared:** keep this number of the top shared neighbours,
#'   irrespective of minimum_shared setting
#' 
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' createNearestNetwork(g)
#' @export
createNearestNetwork <- function(
        gobject,
        spat_unit = NULL,
        feat_type = NULL,
        type = c("sNN", "kNN"),
        dim_reduction_to_use = "pca",
        dim_reduction_name = NULL,
        dimensions_to_use = seq_len(10),
        feats_to_use = NULL,
        expression_values = c("normalized", "scaled", "custom"),
        name = NULL,
        return_gobject = TRUE,
        k = 30,
        minimum_shared = 5,
        top_shared = 3,
        verbose = TRUE,
        ...) {
    # Set feat_type and spat_unit
    spat_unit <- set_default_spat_unit(
        gobject = gobject,
        spat_unit = spat_unit
    )
    feat_type <- set_default_feat_type(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type
    )

    # specify dim_reduction_name tailored to feat_type
    if (is.null(dim_reduction_name)) {
        if (feat_type == "rna") {
            dim_reduction_name <- "pca"
        } else {
            dim_reduction_name <- paste0(feat_type, ".", "pca")
        }
    }

    # type of NN network
    type <- match.arg(type, c("sNN", "kNN"))

    ## using dimension reduction ##
    if (!is.null(dim_reduction_to_use)) {
        ## check if reduction exists
        dim_red_names <- list_dim_reductions_names(
            gobject = gobject, data_type = "cells",
            spat_unit = spat_unit, feat_type = feat_type,
            dim_type = dim_reduction_to_use
        )

        if (!dim_reduction_name %in% dim_red_names) {
            stop(sprintf(
                "\n dimension reduction: %s or dimension reduction name:
                %s is not available \n",
                dim_reduction_to_use,
                dim_reduction_name
            ))
        }

        # check = gobject@dimension_reduction[['cells']][[spat_unit
        # ]][[dim_reduction_to_use]][[dim_reduction_name]]
        # if(is.null(check)) stop('dimension reduction does not exist,
        # check if you did ', dim_reduction_to_use,
        # ' and if ', dim_reduction_name, ' was the name used')

        # use only available dimensions if dimensions < dimensions_to_use

        dim_obj <- get_dimReduction(
            gobject = gobject,
            spat_unit = spat_unit,
            feat_type = feat_type,
            reduction = "cells",
            reduction_method = dim_reduction_to_use,
            name = dim_reduction_name,
            output = "dimObj"
        )

        provenance <- prov(dim_obj)

        dim_coord <- dim_obj[]
        dimensions_to_use <- dimensions_to_use[dimensions_to_use %in%
            seq_len(ncol(dim_coord))]
        matrix_to_use <- dim_coord[, dimensions_to_use]
    } else {
        ## using original matrix ##
        # expression values to be used
        values <- match.arg(
            expression_values,
            unique(c(
                "normalized", "scaled", "custom",
                expression_values
            ))
        )
        expr_obj <- get_expression_values(
            gobject = gobject,
            feat_type = feat_type,
            spat_unit = spat_unit,
            values = values,
            output = "exprObj"
        )

        provenance <- prov(expr_obj)
        expr_values <- expr_obj[] # extract matrix

        # subset expression matrix
        if (!is.null(feats_to_use)) {
            expr_values <- expr_values[rownames(expr_values) %in%
                feats_to_use, ]
        }

        # features as columns & cells as rows
        matrix_to_use <- t_flex(expr_values)
    }

    # vector for cell_ID
    cell_names <- rownames(matrix_to_use)
    names(cell_names) <- seq_len(nrow(matrix_to_use))

    ## run nearest-neighbour algorithm ##
    if (k >= nrow(matrix_to_use)) {
        k <- (nrow(matrix_to_use) - 1)
        vmsg(.v = verbose, "k is higher than total number of cells.
         Adjusted to (total number of cells - 1)")
    }

    nn_network <- dbscan::kNN(x = matrix_to_use, k = k, sort = TRUE, ...)

    # data.table variables
    from <- to <- weight <- distance <- from_cell_ID <- to_cell_ID <-
        shared <- NULL

    nn_network_dt <- data.table::data.table(
        from = rep(seq_len(nrow(nn_network$id)), k),
        to = as.vector(nn_network$id),
        weight = 1 / (1 + as.vector(nn_network$dist)),
        distance = as.vector(nn_network$dist)
    )
    nn_network_dt[, from_cell_ID := cell_names[from]]
    nn_network_dt[, to_cell_ID := cell_names[to]]


    if (type == "sNN") {
        snn_network <- dbscan::sNN(x = nn_network, k = k, kt = NULL, ...)
        snn_network_dt <- data.table::data.table(
            from = rep(seq_len(nrow(snn_network$id)), k),
            to = as.vector(snn_network$id),
            weight = 1 / (1 + as.vector(snn_network$dist)),
            distance = as.vector(snn_network$dist),
            shared = as.vector(snn_network$shared)
        )
        snn_network_dt <- snn_network_dt[stats::complete.cases(snn_network_dt)]
        snn_network_dt[, from_cell_ID := cell_names[from]]
        snn_network_dt[, to_cell_ID := cell_names[to]]

        # rank snn
        data.table::setorder(snn_network_dt, from, -shared)
        snn_network_dt[, rank := seq_len(.N), by = from]

        # filter snn
        snn_network_dt <- snn_network_dt[rank <= top_shared |
            shared >= minimum_shared]
    }

    ## convert to igraph object
    all_index <- unique(x = c(
        nn_network_dt$from_cell_ID,
        nn_network_dt$to_cell_ID
    ))


    if (type == "kNN") {
        nn_network_igraph <- igraph::graph_from_data_frame(
            nn_network_dt[, .(from_cell_ID, to_cell_ID, weight, distance)],
            directed = TRUE, vertices = all_index
        )
    } else if (type == "sNN") {
        # TODO never returned?
        missing_indices <- all_index[!all_index %in%
            unique(snn_network_dt$from)]
        nn_network_igraph <- igraph::graph_from_data_frame(
            snn_network_dt[
                ,
                .(from_cell_ID, to_cell_ID, weight, distance, shared, rank)
            ],
            directed = TRUE, vertices = all_index
        )
    }




    # set default name
    if (is.null(name)) name <- paste0(type, ".", dim_reduction_to_use)

    if (return_gobject == TRUE) {
        nn_names <- names(gobject@nn_network[[spat_unit]][[type]])

        if (name %in% nn_names) {
            wrap_msg(name, " has already been used, will be overwritten")
        }

        nnObj <- create_nn_net_obj(
            name = name,
            nn_type = type,
            igraph = nn_network_igraph,
            spat_unit = spat_unit,
            feat_type = feat_type,
            provenance = provenance,
            misc = NULL
        )

        gobject <- set_NearestNetwork(
            gobject = gobject,
            spat_unit = spat_unit,
            feat_type = feat_type,
            nn_network_to_use = type,
            network_name = name,
            nn_network = nnObj
        )

        ## update parameters used ##
        gobject <- update_giotto_params(gobject, description = "_nn_network")

        return(gobject)
    } else {
        return(nn_network_igraph)
    }
}



#' @title addNetworkLayout
#' @name addNetworkLayout
#' @description Add a network layout for a selected nearest neighbor network
#' @param gobject giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param nn_network_to_use kNN or sNN
#' @param network_name name of NN network to be used
#' @param layout_type layout algorithm to use
#' @param options_list list of options for selected layout
#' @param layout_name name for layout
#' @param return_gobject boolean: return giotto object (default = TRUE)
#' @returns giotto object with updated layout for selected NN network
#' @details This function creates layout coordinates based on the provided
#' kNN or sNN.
#' Currently only the force-directed graph layout "drl",
#' see \code{\link[igraph]{layout_with_drl}}, is implemented.
#' This provides an alternative to tSNE or UMAP based visualizations.
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' addNetworkLayout(g)
#' @export
addNetworkLayout <- function(gobject,
    spat_unit = NULL,
    feat_type = NULL,
    nn_network_to_use = "sNN",
    network_name = "sNN.pca",
    layout_type = c("drl"),
    options_list = NULL,
    layout_name = "layout",
    return_gobject = TRUE) {
    ## checks
    if (is.null(nn_network_to_use) | is.null(network_name)) {
        stop("\n first create a nearest network \n")
    }

    # Set feat_type and spat_unit
    spat_unit <- set_default_spat_unit(
        gobject = gobject,
        spat_unit = spat_unit
    )
    feat_type <- set_default_feat_type(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type
    )

    ig_object <- get_NearestNetwork(
        gobject = gobject,
        spat_unit = spat_unit,
        nn_network_to_use = nn_network_to_use,
        network_name = network_name, output = "igraph"
    )

    # ig_object = gobject@nn_network[[spat_unit]][[nn_network_to_use
    # ]][[network_name]][['igraph']]

    layout_type <- match.arg(arg = layout_type, c("drl"))

    if (layout_type == "drl") {
        if (is.null(options_list)) {
            layout_options <- igraph::drl_defaults$default
        } else {
            layout_options <- options_list
        }
        layout_coord <- igraph::layout_with_drl(
            graph = ig_object,
            options = layout_options
        )
    }


    if (return_gobject == TRUE) {
        nn_names <- names(gobject@nn_network[[spat_unit]][[nn_network_to_use]])
        if (layout_name %in% nn_names) {
            wrap_msg(layout_name, " has already been used, will be overwritten")
        }

        gobject@nn_network[[spat_unit]][[
            nn_network_to_use
        ]][[network_name]][["layout"]] <- layout_coord

        ## update parameters used ##
        gobject <- update_giotto_params(gobject,
            description = "_nn_network_layout"
        )
        return(gobject)
    } else {
        return(layout_coord)
    }
}


#' @title nnDT_to_kNN
#' @name nnDT_to_kNN
#' @description Convert a nearest network data.table to a kNN object
#' @param nnDT nearest neighbor network in data.table format
#' @keywords internal
#' @returns kNN object
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' g_nn <- getNearestNetwork(g, output = "data.table", name = "custom_NN")
#'
#' nnDT_to_kNN(g_nn)
#' @export
nnDT_to_kNN <- function(nnDT) {
    # data.table variable
    from <- NULL

    k <- unique(table(nnDT$from))

    if (length(k) > 1) {
        stop("\n k is not the same for all cells \n")
    }

    nnDT[, rank := seq_len(.N), by = from]

    # distance matrix
    dist_prep <- data.table::dcast.data.table(
        nnDT,
        formula = from ~ rank, value.var = "distance"
    )
    dist_prep[, from := NULL]
    dist_matrix <- as.matrix(dist_prep)

    # id matrix
    id_prep <- data.table::dcast.data.table(
        nnDT,
        formula = from ~ rank, value.var = "to"
    )
    id_prep[, from := NULL]
    id_matrix <- as.matrix(id_prep)

    return(structure(
        list(
            dist = dist_matrix,
            id = id_matrix,
            k = k,
            sort = TRUE
        ),
        class = c("kNN", "NN")
    ))
}

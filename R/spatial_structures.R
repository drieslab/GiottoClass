## Spatial structure helper functions ####








#' @title get_distance
#' @name get_distance
#' @description estimate average distance between neighboring cells with network
#' table as input
#' @param networkDT networkDT
#' @param method method
#' @returns numeric
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' spat_net <- getSpatialNetwork(g, output = "networkDT")
#'
#' get_distance(spat_net, method = "mean")
#' @export
get_distance <- function(networkDT,
    method = c("mean", "median")) {
    distance <- switch(method,
        "median" = stats::median(networkDT$distance),
        "mean" = mean(networkDT$distance)
    )
    return(distance)
}





#' @title Filter spatial network
#' @name .filter_network
#' @description Filter a spatial network by spatial characteristics
#' @param networkDT spatial network in data.table format
#' @param maximum_distance maximum distance between cell centroids
#' @param minimum_k minimum number of neighbors
#' @keywords internal
#' @returns data.table
.filter_network <- function(networkDT = NULL,
    maximum_distance = NULL,
    minimum_k = 0L) {
    # data.table variables
    distance <- rank_from <- rank_to <- from <- to <- NULL

    if (is.null(maximum_distance)) return(networkDT)

    dt <- data.table::copy(networkDT)

    # per-side distance rank — lets the `minimum_k` floor apply
    # symmetrically (each node retains at least k neighbours) without
    # expanding the canonical (from < to) representation.
    data.table::setorder(dt, from, distance)
    dt[, rank_from := seq_len(.N), by = "from"]
    data.table::setorder(dt, to, distance)
    dt[, rank_to := seq_len(.N), by = "to"]

    cutoff <- if (isTRUE(maximum_distance == "auto")) {
        grDevices::boxplot.stats(dt$distance)$stats[5]
    } else maximum_distance

    dt <- dt[distance <= cutoff | rank_from <= minimum_k | rank_to <= minimum_k]
    dt[, c("rank_from", "rank_to") := NULL][]
}







#' @title Compatible spatial network
#' @name compatible_spatial_network
#' @description Function to evaluate if a spatial network is compatible
#' with a provided expression matrix
#' @param spatial_network spatial network to evaluate
#' @param expression_matrix expression to compare against
#' @returns TRUE or character
#' @keywords internal
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' spat_net <- getSpatialNetwork(g, output = "networkDT")
#' expr_m <- getExpression(g)
#'
#' compatible_spatial_network(spat_net, expr_m)
#' @export
compatible_spatial_network <- function(spatial_network,
    expression_matrix) {
    # first evaluate spatial network
    spatial_network <- .evaluate_spatial_network(spatial_network)

    # compatible network
    # all network nodes need to be found back in the column names

    network_ids <- unique(spatial_network$from, spatial_network$to)
    cell_ids <- colnames(expression_matrix)

    missing_network_ids <- network_ids[!network_ids %in% cell_ids]

    if (length(missing_network_ids) > 0) {
        stop(
            "Spatial network ids missing in expression matrix: ",
            list(missing_network_ids)
        )
    } else {
        return(TRUE)
    }
}






#' @title Convert spatialNetworkObj to igraph
#' @name spat_net_to_igraph
#' @description Convert a `spatialNetworkObj` to a non-directed igraph
#' representation.
#' @param attr columns to include as edge attributes.
#' @param spatialNetworkObj spatialNetworkObj
#' @returns igraph
#' @examples
#' sn <- GiottoData::loadSubObjectMini("spatialNetworkObj")
#' # only name attribute
#' g <- spat_net_to_igraph(sn)
#'
#' # view other column info besides to and from cols
#' head(sn[], 1)
#'
#' # include distance and weight col info
#' g <- spat_net_to_igraph(sn, attr = c("distance", "weight"))
#' @export
spat_net_to_igraph <- function(spatialNetworkObj, attr = NULL) {
    net_list <- as.list(spatialNetworkObj[])
    cell_ids <- spatIDs(spatialNetworkObj)

    igraph::make_empty_graph(directed = FALSE) %>%
        igraph::add_vertices(
            nv = length(cell_ids),
            attr = list(name = cell_ids)
        ) %>%
        igraph::add_edges(
            edges = rbind(
                net_list$from,
                net_list$to
            ),
            attr = net_list[attr]
        )
}







#' @title Create a spatial Delaunay network
#' @name createSpatialDelaunayNetwork
#' @description Create a spatial Delaunay network based on cell centroid
#' physical distances.
#' @param gobject giotto object
#' @param name name for spatial network (default = 'delaunay_network')
#' @param feat_type feature type
#' @param spat_unit spatial unit
#' @param spat_loc_name name of spatial locations
#' @param method package to use to create a Delaunay network
#' @param spat_loc_name name of spatial locations
#' @param dimensions which spatial dimensions to use. Use
#' "sdimx" (spatial dimension x), "sdimy", "sdimz" respectively to refer to
#' X (or the 1st), Y (or the 2nd) and Z(or the 3rd) dimension, see
#' details. (default = all)
#' @param maximum_distance distance cuttof for Delaunay neighbors to consider.
#' If "auto", "upper whisker" value of the distance vector between neighbors
#' is used; see the [graphics::boxplot()] documentation for more
#' details.(default = "auto")
#' @param minimum_k minimum number of neighbours if maximum_distance != NULL
#' @param options (geometry) String containing extra control options for the
#' underlying Qhull command; see the
#' [Qhull documentation](http://www.qhull.org/html/qdelaun.htm) for the
#' available options. (default = 'Pp', do not report precision problems)
#' @param Y (RTriangle) If TRUE prohibits the insertion of Steiner points on
#' the mesh boundary.
#' @param j (RTriangle) If TRUE jettisons vertices that are not part of the
#' final triangulation from the output.
#' @param S (RTriangle) Specifies the maximum number of added Steiner points.
#' @inheritParams createSpatialNetwork
#' @param \dots Other additional parameters
#' @returns giotto object with updated spatial network slot
#' @details Creates a spatial Delaunay network as explained
#' in \code{\link[geometry]{delaunayn}} (default), \code{\link[deldir]{deldir}},
#' or \code{\link[RTriangle]{triangulate}}.
#' @section Choosing a Delaunay backend:
#' All three backends compute the same exact triangulation -- on 50,000 uniform
#' points each returns the identical 149,978 edges -- so the choice is purely
#' one of implementation speed, and `deldir` is by a wide margin the slowest:
#'
#' | points | `deldir` | `delaunayn_geometry` |
#' |---|---|---|
#' | 20,000 | 3.7 s | 0.14 s |
#' | 50,000 | 21.1 s | 0.20 s |
#' | 200,000 | ~363 s | 0.94 s |
#'
#' `deldir` remains the default for backward compatibility, but
#' **`delaunay_method = "delaunayn_geometry"` is strongly preferred above a few
#' thousand points**. It requires the \pkg{geometry} package, and it is also the
#' only backend that handles 3D.
#'
#' Two caveats. Qhull can struggle with exactly cocircular input -- a
#' grid-aligned platform such as Visium -- in which case pass
#' `options = "Qbb Qc Qz"`; a 10,000-point exact grid worked with the default
#' `"Pp"` in testing, but the failure mode is degenerate input rather than
#' size. And `deldir` peaks around 2.9 GB of memory at 200,000 points against
#' `geometry`'s 0.24 GB, so on a large section it is the memory, not just the
#' wait, that bites.
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' createSpatialDelaunayNetwork(g)
#' @export
createSpatialDelaunayNetwork <- function(gobject,
    name = "Delaunay_network",
    spat_unit = NULL,
    feat_type = NULL,
    spat_loc_name = NULL,
    method = c("deldir", "delaunayn_geometry", "RTriangle"),
    dimensions = "all",
    maximum_distance = "auto",
    minimum_k = 0,
    options = "Pp",
    Y = TRUE, j = TRUE, S = 0,
    verbose = TRUE,
    return_gobject = TRUE,
    output = c("spatialNetworkObj", "data.table"),
    ...) {
    # Thin wrapper over createNetwork() + spatialNetworkObj construction.
    method <- match.arg(method, c("deldir", "delaunayn_geometry", "RTriangle"))
    output <- match.arg(output, c("spatialNetworkObj", "data.table"))
    spat_unit <- set_default_spat_unit(gobject, spat_unit = spat_unit)

    # alias "delaunayn_geometry" -> "geometry" for the new Param API
    param_method <- if (method == "delaunayn_geometry") "geometry" else method

    # Pull spatLocsObj to capture provenance, then dispatch via the
    # matrix method (lets us honour the legacy `dimensions` arg).
    sl <- getSpatialLocations(gobject,
        spat_unit = spat_unit, name = spat_loc_name,
        output = "spatLocsObj"
    )
    provenance <- prov(sl)
    sl_dt <- sl[]
    coord_cols <- intersect(c("sdimx", "sdimy", "sdimz"), names(sl_dt))
    if (!identical(dimensions, "all")) coord_cols <- coord_cols[dimensions]
    if (length(coord_cols) == 3L && method != "delaunayn_geometry") {
        stop(method, " method only applies to 2D data, ",
            "use delaunayn_geometry, see details \n", call. = FALSE)
    }
    coords <- as.matrix(sl_dt[, coord_cols, with = FALSE])
    node_ids <- sl_dt$cell_ID

    param <- delaunayNetworkParam(
        method = param_method,
        maximum_distance = maximum_distance,
        minimum_k = minimum_k,
        output = "igraph",
        options = options, Y = Y, j = j, S = S
    )
    g_net <- createNetwork(coords, param,
        node_ids = node_ids, verbose = verbose, ...
    )

    if (output == "data.table" && !return_gobject) {
        return(data.table::as.data.table(
            igraph::as_data_frame(g_net, what = "edges")
        ))
    }

    sn_obj <- create_spat_net_obj(
        name = name,
        method = method,
        parameters = list(
            maximum_distance = maximum_distance,
            minimum_k = minimum_k,
            dimensions = dimensions
        ),
        network = g_net,
        spat_unit = spat_unit,
        provenance = provenance
    )

    if (!return_gobject) return(sn_obj)

    spn_names <- list_spatial_networks_names(gobject, spat_unit = spat_unit)
    if (name %in% spn_names) {
        vmsg(.v = verbose, name, " has already been used, will be overwritten")
    }
    gobject <- setSpatialNetwork(gobject,
        x = sn_obj, spat_unit = spat_unit, name = name,
        verbose = verbose
    )
    gobject <- update_giotto_params(gobject, description = "_spatial_network", toplevel = 1L)
    gobject
}












#' @title createSpatialKNNnetwork
#' @name createSpatialKNNnetwork
#' @description Create a spatial knn network.
#' @param gobject giotto object
#' @param feat_type feature type
#' @param spat_unit spatial unit
#' @param name name for spatial network (default = 'spatial_network')
#' @param method method to create kNN network
#' @param spat_unit spatial unit
#' @param spat_loc_name name of spatial locations
#' @param dimensions which spatial dimensions to use (default = all)
#' @param k number of nearest neighbors based on physical distance
#' @param maximum_distance distance cuttof for nearest neighbors to consider
#' for kNN network
#' @param minimum_k minimum nearest neigbhours if maximum_distance != NULL
#' @param verbose verbose
#' @param return_gobject boolean: return giotto object (default = TRUE)
#' @inheritParams createSpatialNetwork
#' @param \dots additional arguments to the selected method function
#' @returns giotto object with updated spatial network slot
#'
#' \strong{dimensions: } default = 'all' which takes all possible dimensions.
#' Alternatively you can provide a character vector that specififies the
#' spatial dimensions to use, e.g. c("sdimx', "sdimy")
#' or a numerical vector, e.g. 2:3
#'
#' \strong{maximum_distance: } this is a post-filter on the k neighbours the
#' search already found, not a constraint on the search itself. To build a
#' network on distance alone, prefer [radiusNetworkParam()], which searches
#' only within the radius; the older advice of setting `k` very high (e.g.
#' `k = 100`) and filtering still works, but it finds a hundred neighbours per
#' cell in order to discard most of them.
#'
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' createSpatialKNNnetwork(g)
#'
#' @export
createSpatialKNNnetwork <- function(gobject,
    method = "dbscan",
    spat_unit = NULL,
    feat_type = NULL,
    spat_loc_name = NULL,
    dimensions = "all",
    name = "knn_network",
    k = 4,
    maximum_distance = NULL,
    minimum_k = 0,
    verbose = FALSE,
    return_gobject = TRUE,
    output = c("spatialNetworkObj", "data.table"),
    ...) {
    # Thin wrapper over createNetwork() + spatialNetworkObj construction.
    method <- match.arg(method, c("dbscan"))
    output <- match.arg(output, c("spatialNetworkObj", "data.table"))
    spat_unit <- set_default_spat_unit(gobject, spat_unit = spat_unit)

    sl <- getSpatialLocations(gobject,
        spat_unit = spat_unit, name = spat_loc_name,
        output = "spatLocsObj"
    )
    provenance <- prov(sl)
    sl_dt <- sl[]
    coord_cols <- intersect(c("sdimx", "sdimy", "sdimz"), names(sl_dt))
    if (!identical(dimensions, "all")) coord_cols <- coord_cols[dimensions]
    coords <- as.matrix(sl_dt[, coord_cols, with = FALSE])
    node_ids <- sl_dt$cell_ID

    param <- kNNNetworkParam(
        k = k,
        filter = TRUE,
        maximum_distance = maximum_distance,
        minimum_k = minimum_k,
        output = "igraph"
    )
    g_net <- createNetwork(coords, param,
        node_ids = node_ids, verbose = verbose, ...
    )

    if (output == "data.table" && !return_gobject) {
        return(data.table::as.data.table(
            igraph::as_data_frame(g_net, what = "edges")
        ))
    }

    sn_obj <- create_spat_net_obj(
        name = name,
        method = method,
        parameters = list(
            k = k,
            maximum_distance = maximum_distance,
            minimum_k = minimum_k,
            dimensions = dimensions
        ),
        network = g_net,
        spat_unit = spat_unit,
        provenance = provenance
    )

    if (!return_gobject) return(sn_obj)

    spn_names <- list_spatial_networks_names(gobject, spat_unit = spat_unit)
    if (name %in% spn_names) {
        vmsg(.v = verbose, name, " has already been used, will be overwritten")
    }
    gobject <- setSpatialNetwork(gobject,
        x = sn_obj, spat_unit = spat_unit, name = name,
        verbose = verbose
    )
    gobject <- update_giotto_params(gobject, description = "_spatial_network", toplevel = 1L)
    gobject
}









## spatial network ####

#' @title Create spatial network
#' @name createSpatialNetwork
#' @description Create a spatial network based on cell centroids. These networks
#' are often used when determining cell-cell connectivities and spatial
#' relationships.
#' There are several types of spatial networks and multiple methods to generate
#' them. Method-specific params are labeled with the name of the method within
#' parentheses in their descriptions.
#' @param gobject giotto object
#' @param name name for spatial network (default = 'spatial_network')
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param spat_loc_name name of spatial locations to use
#' @param dimensions which spatial dimensions to use (default = all)
#' @param method which method to use to create a spatial network. One of
#' `"Delaunay"` (default), `"kNN"`, or `"radius"`. `"radius"` connects every
#' pair of cells closer together than `radius`, so unlike kNN it gives dense
#' regions more neighbours than sparse ones.
#' @param delaunay_method method to use to generate Delaunay network. All
#' three give the identical triangulation; `"delaunayn_geometry"` is far faster
#' above a few thousand points (0.94 s vs ~363 s at 200,000) and is the only
#' one that handles 3D. `"deldir"` remains the default for backward
#' compatibility. See the *Choosing a Delaunay backend* section of
#' [createSpatialDelaunayNetwork()].
#' @param maximum_distance_delaunay distance cutoff for nearest neighbors to
#' consider for Delaunay network. If "auto", "upper whisker" value of the
#' distance vector between neighbors is used; see the [grDevices::boxplot.stats]
#' documentation for more details.(default = "auto")
#' @param options (geometry) String containing extra control options for the
#' underlying Qhull command; see the
#' [Qhull documentation](http://www.qhull.org/html/qdelaun.htm) for the
#' available options. (default = 'Pp', do not report precision problems)
#' @param Y (RTriangle) If TRUE prohibits the insertion of Steiner points on
#' the mesh boundary.
#' @param j (RTriangle) If TRUE jettisons vertices that are not part of the
#' final triangulation from the output.
#' @param S (RTriangle) Specifies the maximum number of added Steiner points.
#' @param knn_method method to create kNN network
#' @param k number of nearest neighbors based on physical distance
#' @param minimum_k minimum nearest neighbours if maximum_distance != NULL
#' @param maximum_distance_knn distance cutoff for nearest neighbors to consider
#' for kNN network
#' @param radius (radius) distance cutoff, in the units of the spatial
#' locations. Every pair of cells within this distance of each other is
#' connected. Required when `method = "radius"`.
#' @param verbose be verbose
#' @param return_gobject logical. return giotto object (default = TRUE)
#' @param output character. Object type to return spatial network as when
#' `return_gobject = FALSE`. (default: 'spatialNetworkObj')
#' @param \dots Additional parameters for the selected function
#' @returns giotto object with updated spatial network slot
#' @details Creates a spatial network connecting single-cells based on their
#' physical distance to each other.
#' For Delaunay method, neighbors will be decided by Delaunay triangulation and
#' a maximum distance criteria. For kNN method, number of neighbors can be
#' determined by k, or maximum distance from each cell with or without
#' setting a minimum k for each cell.
#'
#' **dimensions: ** default = 'all' which takes all possible dimensions.
#' Alternatively you can provide a character vector that specifies the spatial
#' dimensions to use, e.g. c("sdimx', "sdimy") or a numerical vector, e.g. 2:3
#'
#' @md
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' createSpatialNetwork(g)
#' @export
createSpatialNetwork <- function(gobject,
    name = NULL,
    spat_unit = NULL,
    feat_type = NULL,
    spat_loc_name = NULL,
    dimensions = "all",
    method = c("Delaunay", "kNN", "radius"),
    delaunay_method = c("deldir", "delaunayn_geometry", "RTriangle"),
    maximum_distance_delaunay = "auto",
    options = "Pp",
    Y = TRUE,
    j = TRUE,
    S = 0,
    minimum_k = 0,
    knn_method = "dbscan",
    k = 4,
    maximum_distance_knn = NULL,
    radius = NULL,
    verbose = FALSE,
    return_gobject = TRUE,
    output = c("spatialNetworkObj", "data.table"),
    ...) {
    # get paramters
    method <- match.arg(method, c("Delaunay", "kNN", "radius"))


    if (method == "kNN") {
        if (is.null(name)) {
            name <- paste0(method, "_", "network")
        }

        knn_method <- match.arg(knn_method, c("dbscan"))

        out <- createSpatialKNNnetwork(
            gobject = gobject,
            spat_unit = spat_unit,
            feat_type = feat_type,
            method = knn_method,
            spat_loc_name = spat_loc_name,
            dimensions = dimensions,
            k = k,
            maximum_distance = maximum_distance_knn,
            minimum_k = minimum_k,
            name = name,
            verbose = verbose,
            return_gobject = return_gobject,
            output = output,
            ...
        )
    } else if (method == "Delaunay") {
        delaunay_method <- match.arg(
            delaunay_method,
            c("deldir", "delaunayn_geometry", "RTriangle")
        )
        if (is.null(name)) {
            name <- paste0(method, "_", "network")
        }
        out <- createSpatialDelaunayNetwork(
            gobject = gobject,
            spat_unit = spat_unit,
            feat_type = feat_type,
            spat_loc_name = spat_loc_name,
            method = delaunay_method,
            dimensions = dimensions,
            name = name,
            maximum_distance = maximum_distance_delaunay,
            options = options,
            minimum_k = minimum_k,
            Y = Y,
            j = j,
            S = S,
            verbose = verbose,
            return_gobject = return_gobject,
            output = output,
            ...
        )
    } else if (method == "radius") {
        if (is.null(radius)) {
            stop(wrap_txt(
                'method = "radius" needs a `radius` (a distance in the units',
                "of the spatial locations)."
            ), call. = FALSE)
        }
        if (is.null(name)) name <- "radius_network"

        out <- .create_spatial_network_from_param(
            gobject = gobject,
            param = radiusNetworkParam(
                eps = radius, minimum_k = minimum_k, output = "igraph"
            ),
            method = "radius",
            parameters = list(
                eps = radius, minimum_k = minimum_k, dimensions = dimensions
            ),
            spat_unit = spat_unit,
            spat_loc_name = spat_loc_name,
            dimensions = dimensions,
            name = name,
            verbose = verbose,
            return_gobject = return_gobject,
            output = output,
            ...
        )
    }

    return(out)
}


# Build a spatial network on cell centroids from an already-constructed
# networkParam, and do the gobject plumbing around it.
#
# createSpatialKNNnetwork() and createSpatialDelaunayNetwork() each carry their
# own copy of this plumbing; they are not migrated onto it here because they
# are the tested paths and this is a bug-fix commit. New spatial network
# methods should route through this instead of adding a fourth copy.
.create_spatial_network_from_param <- function(gobject,
    param,
    method,
    parameters,
    spat_unit = NULL,
    spat_loc_name = NULL,
    dimensions = "all",
    name,
    verbose = FALSE,
    return_gobject = TRUE,
    output = c("spatialNetworkObj", "data.table"),
    ...) {
    output <- match.arg(output, c("spatialNetworkObj", "data.table"))
    spat_unit <- set_default_spat_unit(gobject, spat_unit = spat_unit)

    sl <- getSpatialLocations(gobject,
        spat_unit = spat_unit, name = spat_loc_name,
        output = "spatLocsObj"
    )
    provenance <- prov(sl)
    sl_dt <- sl[]
    coord_cols <- intersect(c("sdimx", "sdimy", "sdimz"), names(sl_dt))
    if (!identical(dimensions, "all")) coord_cols <- coord_cols[dimensions]
    coords <- as.matrix(sl_dt[, coord_cols, with = FALSE])

    g_net <- createNetwork(coords, param,
        node_ids = sl_dt$cell_ID, verbose = verbose, ...
    )

    if (output == "data.table" && !return_gobject) {
        return(data.table::as.data.table(
            igraph::as_data_frame(g_net, what = "edges")
        ))
    }

    sn_obj <- create_spat_net_obj(
        name = name,
        method = method,
        parameters = parameters,
        network = g_net,
        spat_unit = spat_unit,
        provenance = provenance
    )

    if (!return_gobject) return(sn_obj)

    spn_names <- list_spatial_networks_names(gobject, spat_unit = spat_unit)
    if (name %in% spn_names) {
        vmsg(.v = verbose, name, " has already been used, will be overwritten")
    }
    gobject <- setSpatialNetwork(gobject,
        x = sn_obj, spat_unit = spat_unit, name = name, verbose = verbose
    )
    update_giotto_params(gobject,
        description = "_spatial_network", toplevel = 1L
    )
}





#' @title annotateSpatialNetwork
#' @name annotateSpatialNetwork
#' @description Annotate spatial network with cell metadata information.
#' @param gobject giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param spatial_network_name name of spatial network to use
#' @param cluster_column name of column to use for clusters
#' @param create_full_network convert from reduced to full network
#' representation
#' @returns annotated network in data.table format
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' annotateSpatialNetwork(g, cluster_column = "leiden_clus")
#' @export
annotateSpatialNetwork <- function(gobject,
    spat_unit = NULL,
    feat_type = NULL,
    spatial_network_name = "Delaunay_network",
    cluster_column,
    create_full_network = FALSE) {
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

    # get network
    if (!spatial_network_name %in%
        list_spatial_networks_names(gobject, spat_unit)) {
        stop(
            "\n spatial network with name: ",
            spatial_network_name, " does not exist \n"
        )
    }
    spatial_network <- getSpatialNetwork(
        gobject = gobject,
        spat_unit = spat_unit,
        name = spatial_network_name,
        output = "networkDT"
    )

    if (isTRUE(create_full_network)) {
        # expand canonical (from, to) to both directions
        rev <- data.table::copy(spatial_network)
        data.table::setnames(rev, c("from", "to"), c("to", "from"))
        spatial_network <- unique(rbind(spatial_network, rev))
    }

    # Attach sdim*_begin / sdim*_end coords from spatLocsObj. Networks
    # no longer cache coords (as of 0.6.0), so we join them here for
    # downstream consumers that draw line segments. These are read from
    # the live spatLocsObj so any spatial transforms automatically
    # propagate.
    sl_dt <- getSpatialLocations(gobject,
        spat_unit = spat_unit, output = "data.table"
    )
    coord_cols <- intersect(c("sdimx", "sdimy", "sdimz"), names(sl_dt))
    sl_keys <- sl_dt[, c("cell_ID", coord_cols), with = FALSE]
    begin_cols <- paste0(coord_cols, "_begin")
    end_cols <- paste0(coord_cols, "_end")

    spatial_network <- merge(
        spatial_network,
        data.table::setnames(
            data.table::copy(sl_keys),
            c("cell_ID", coord_cols),
            c("from", begin_cols)
        ),
        by = "from"
    )
    spatial_network <- merge(
        spatial_network,
        data.table::setnames(
            data.table::copy(sl_keys),
            c("cell_ID", coord_cols),
            c("to", end_cols)
        ),
        by = "to"
    )



    # cell metadata
    cell_metadata <- getCellMetadata(gobject,
        feat_type = feat_type,
        spat_unit = spat_unit,
        output = "data.table",
        copy_obj = TRUE
    )
    if (!cluster_column %in% colnames(cell_metadata)) {
        stop("\n the cluster column does not exist in pDataDT(gobject) \n")
    }
    cluster_type_vector <- cell_metadata[[cluster_column]]
    names(cluster_type_vector) <- cell_metadata[["cell_ID"]]

    # data.table variables
    to_cell_type <- to <- from_cell_type <- from <- type_int <- from_to <- NULL

    spatial_network_annot <- data.table::copy(spatial_network)
    spatial_network_annot[, to_cell_type := cluster_type_vector[to]]
    spatial_network_annot[, from_cell_type := cluster_type_vector[from]]
    spatial_network_annot[
        ,
        type_int := ifelse(to_cell_type == from_cell_type, "homo", "hetero")
    ]

    # specific direction
    spatial_network_annot[
        ,
        from_to := paste0(from_cell_type, "-", to_cell_type)
    ]

    # unified direction, due to 'sort'
    spatial_network_annot <- dt_sort_combine_two_columns(spatial_network_annot,
        column1 = "from_cell_type",
        column2 = "to_cell_type",
        myname = "unified_int"
    )

    return(spatial_network_annot)
}





# spatial weight matrix ####
# TODO move to Giotto?

#' @title Create a spatial weight matrix
#' @name createSpatialWeightMatrix
#' @description Generate spatial weight matrix based on the strength of spatial
#' interactions between nodes. Requires spatial networks to be first generated.
#' @param gobject giotto object
#' @param spat_unit spatial unit
#' @param spatial_network_to_use spatial network information to use
#' @param method type of weighted matrix to generate. See details
#' @param wm_name name to assign the weight matrix values
#' @param return_gobject (default = TRUE) whether to return as the giotto object
#' with attached results or the bare weighted matrix
#' @param verbose be verbose
#' @returns spatial weight matrix
#' @details
#' \itemize{
#'   \item{\code{"distance"} method is calculated using 1/(1+distance) to
#'   create an inverse weighting based on the distance between nodes.}
#'   \item{\code{"adjacency"} method is a binary matrix with 1 signifying that
#'   two nodes are connected in the spatial network and 0 indicating that
#'   they are not.}
#' }
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' createSpatialWeightMatrix(g, spatial_network_to_use = "spatial_network")
#' @export
createSpatialWeightMatrix <- function(gobject,
    spat_unit = NULL,
    spatial_network_to_use = "kNN_network",
    method = c("distance", "adjacency"),
    wm_name = "spat_weights",
    return_gobject = TRUE,
    verbose = TRUE) {
    # 1. setup
    spat_unit <- set_default_spat_unit(
        gobject = gobject,
        spat_unit = spat_unit
    )

    method <- match.arg(method, choices = c("distance", "adjacency"))

    sn <- getSpatialNetwork(
        gobject = gobject,
        spat_unit = spat_unit,
        name = spatial_network_to_use,
        output = "spatialNetworkObj"
    )
    if (is.null(sn)) stop("Specified spatial network not found")

    # 2. calculate weights — sn[] is the canonical igraph
    g <- sn[]
    wm <- switch(method,
        "distance"  = igraph::as_adjacency_matrix(g, attr = "weight", sparse = TRUE),
        "adjacency" = igraph::as_adjacency_matrix(g, sparse = TRUE)
    )

    # 3. return results
    if (isTRUE(return_gobject)) {
        sn@misc$weight_matrix[[wm_name]] <- wm
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        if (isTRUE(verbose)) {
            wrap_msg("Attaching weight matrix to", spatial_network_to_use)
        }
        gobject <- setSpatialNetwork(
            gobject = gobject,
            x = sn,
            set_defaults = FALSE,
            verbose = FALSE
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        return(gobject)
    } else {
        return(wm)
    }
}







## Spatial grid ####

#' @title .find_grid_3d
#' @name .find_grid_3d
#' @description find grid location in 3D
#' @keywords internal
#' @returns character
.find_grid_3d <- function(grid_DT, x_loc, y_loc, z_loc) {
    # data.table variables
    x_start <- x_end <- y_start <- y_end <- z_start <- z_end <- NULL

    name <- grid_DT[x_loc > x_start & x_loc < x_end & y_loc > y_start &
        y_loc < y_end & z_loc > z_start & z_loc < z_end]$gr_name
    return(name)
}

#' @title .find_grid_2d
#' @name .find_grid_2d
#' @description find grid location in 2D
#' @keywords internal
#' @returns character
.find_grid_2d <- function(grid_DT, x_loc, y_loc) {
    # data.table variables
    x_start <- x_end <- y_start <- y_end <- NULL

    name <- grid_DT[x_loc > x_start & x_loc < x_end & y_loc > y_start &
        y_loc < y_end]$gr_name
    return(name)
}

#' @title .find_grid_x
#' @name .find_grid_x
#' @description find grid location on x-axis
#' @keywords internal
#' @returns character
.find_grid_x <- function(grid_DT, x_loc) {
    # data.table variables
    x_start <- x_end <- gr_x_name <- NULL

    grid_DT_x <- unique(grid_DT[, .(x_start, x_end, gr_x_name)])
    name_x <- grid_DT_x[x_loc > x_start & x_loc < x_end]$gr_x_name
    return(name_x)
}

#' @title .find_grid_y
#' @name .find_grid_y
#' @description find grid location on y-axis
#' @keywords internal
#' @returns character
.find_grid_y <- function(grid_DT, y_loc) {
    # data.table variables
    y_start <- y_end <- gr_y_name <- NULL

    grid_DT_y <- unique(grid_DT[, .(y_start, y_end, gr_y_name)])
    name_y <- grid_DT_y[y_loc > y_start & y_loc < y_end]$gr_y_name
    return(name_y)
}

#' @title .find_grid_z
#' @name .find_grid_z
#' @description find grid location on z-axis
#' @keywords internal
#' @returns character
.find_grid_z <- function(grid_DT, z_loc) {
    # data.table variables
    z_start <- z_end <- gr_z_name <- NULL

    grid_DT_z <- unique(grid_DT[, .(z_start, z_end, gr_z_name)])
    name_z <- grid_DT_z[z_loc > z_start & z_loc < z_end]$gr_z_name
    return(name_z)
}



#' @title .create_spatialgrid_default_2d
#' @description create a 2D spatial grid
#' @keywords internal
#' @returns 2D spatial grid
.create_spatialgrid_default_2d <- function(gobject,
    spat_unit = NULL,
    spat_loc_name = "raw",
    sdimx_stepsize = NULL,
    sdimy_stepsize = NULL,
    minimum_padding = 1) {
    # data.table variables
    gr_name <- gr_x_name <- gr_y_name <- gr_x_loc <- gr_y_loc <- gr_loc <- NULL

    spat_unit <- set_default_spat_unit(
        gobject = gobject,
        spat_unit = spat_unit
    )

    spatlocs <- getSpatialLocations(
        gobject = gobject,
        spat_unit = spat_unit,
        name = spat_loc_name,
        output = "data.table",
        copy_obj = FALSE
    )

    if (is.null(spatlocs)) {
        stop("\n spatial locations are needed to create a spatial grid \n")
    }

    ## calculate sequences for desired stepsize
    # x-axis
    x_range <- range(spatlocs$sdimx)
    x_start <- x_range[[1]] - minimum_padding
    x_end <- x_range[[2]] + minimum_padding
    dimx_steps <- ceiling((x_end - x_start) / sdimx_stepsize)
    dimx_start <- mean(c(x_start, x_end)) - ((dimx_steps / 2) * sdimx_stepsize)
    dimx_end <- mean(c(x_start, x_end)) + ((dimx_steps / 2) * sdimx_stepsize)
    my_x_seq <- seq(from = dimx_start, to = dimx_end, by = sdimx_stepsize)

    # y-axis
    y_range <- range(spatlocs$sdimy)
    y_start <- y_range[[1]] - minimum_padding
    y_end <- y_range[[2]] + minimum_padding
    dimy_steps <- ceiling((y_end - y_start) / sdimy_stepsize)
    dimy_start <- mean(c(y_start, y_end)) - ((dimy_steps / 2) * sdimy_stepsize)
    dimy_end <- mean(c(y_start, y_end)) + ((dimy_steps / 2) * sdimy_stepsize)
    my_y_seq <- seq(from = dimy_start, to = dimy_end, by = sdimy_stepsize)


    ## create grid with starts and ends
    grid_starts <- data.table::as.data.table(expand.grid(
        my_x_seq[-length(my_x_seq)],
        my_y_seq[-length(my_y_seq)]
    ))
    colnames(grid_starts) <- c("x_start", "y_start")
    grid_ends <- data.table::as.data.table(expand.grid(
        my_x_seq[-1],
        my_y_seq[-1]
    ))
    colnames(grid_ends) <- c("x_end", "y_end")
    spatgrid <- cbind(grid_starts, grid_ends)


    ## first label the grid itself ##
    spatgrid[, gr_name := paste0("gr_", seq_len(.N))]

    # x-axis
    x_labels <- sort(unique(spatgrid$x_start))
    x_gr_names <- paste0("gr_x_", seq_len(length(x_labels)))
    names(x_gr_names) <- x_labels
    x_gr_names_vector <- x_gr_names[as.character(spatgrid$x_start)]
    spatgrid[, gr_x_name := x_gr_names_vector]

    # y-axis
    y_labels <- sort(unique(spatgrid$y_start))
    y_gr_names <- paste0("gr_y_", seq_len(length(y_labels)))
    names(y_gr_names) <- y_labels
    y_gr_names_vector <- y_gr_names[as.character(spatgrid$y_start)]
    spatgrid[, gr_y_name := y_gr_names_vector]

    ## for all dimensions ##
    # converter
    gr_dim_names <- spatgrid$gr_name
    names(gr_dim_names) <- paste0(spatgrid$gr_x_name, "-", spatgrid$gr_y_name)


    return(spatgrid)
}


#' @title .create_spatialgrid_default_3d
#' @description create a 3D spatial grid
#' @keywords internal
#' @returns 3D spatial grid
.create_spatialgrid_default_3d <- function(gobject,
    spat_unit = NULL,
    spat_loc_name = "raw",
    sdimx_stepsize = NULL,
    sdimy_stepsize = NULL,
    sdimz_stepsize = NULL,
    minimum_padding = 1) {
    # data.table variables
    gr_name <- gr_x_name <- gr_y_name <- gr_z_name <- gr_x_loc <-
        gr_y_loc <- gr_z_loc <- gr_loc <- NULL

    spat_unit <- set_default_spat_unit(
        gobject = gobject,
        spat_unit = spat_unit
    )

    spatlocs <- getSpatialLocations(
        gobject = gobject,
        spat_unit = spat_unit,
        name = spat_loc_name,
        output = "data.table",
        copy_obj = FALSE
    )

    if (is.null(spatlocs)) {
        stop("\n spatial locations are needed to create a spatial grid \n")
    }

    ## calculate sequences for desired stepsize
    # x-axis
    x_range <- range(spatlocs$sdimx)
    x_start <- x_range[[1]] - minimum_padding
    x_end <- x_range[[2]] + minimum_padding
    dimx_steps <- ceiling((x_end - x_start) / sdimx_stepsize)
    dimx_start <- mean(c(x_start, x_end)) - ((dimx_steps / 2) * sdimx_stepsize)
    dimx_end <- mean(c(x_start, x_end)) + ((dimx_steps / 2) * sdimx_stepsize)
    my_x_seq <- seq(from = dimx_start, to = dimx_end, by = sdimx_stepsize)

    # y-axis
    y_range <- range(spatlocs$sdimy)
    y_start <- y_range[[1]] - minimum_padding
    y_end <- y_range[[2]] + minimum_padding
    dimy_steps <- ceiling((y_end - y_start) / sdimy_stepsize)
    dimy_start <- mean(c(y_start, y_end)) - ((dimy_steps / 2) * sdimy_stepsize)
    dimy_end <- mean(c(y_start, y_end)) + ((dimy_steps / 2) * sdimy_stepsize)
    my_y_seq <- seq(from = dimy_start, to = dimy_end, by = sdimy_stepsize)

    # z-axis
    z_range <- range(spatlocs$sdimz)
    z_start <- z_range[[1]] - minimum_padding
    z_end <- z_range[[2]] + minimum_padding
    dimz_steps <- ceiling((z_end - z_start) / sdimz_stepsize)
    dimz_start <- mean(c(z_start, z_end)) - ((dimz_steps / 2) * sdimz_stepsize)
    dimz_end <- mean(c(z_start, z_end)) + ((dimz_steps / 2) * sdimz_stepsize)
    my_z_seq <- seq(from = dimz_start, to = dimz_end, by = sdimz_stepsize)

    ## create grid with starts and ends
    grid_starts <- data.table::as.data.table(expand.grid(
        my_x_seq[-length(my_x_seq)],
        my_y_seq[-length(my_y_seq)],
        my_z_seq[-length(my_z_seq)]
    ))
    colnames(grid_starts) <- c("x_start", "y_start", "z_start")
    grid_ends <- data.table::as.data.table(expand.grid(
        my_x_seq[-1],
        my_y_seq[-1],
        my_z_seq[-1]
    ))
    colnames(grid_ends) <- c("x_end", "y_end", "z_end")
    spatgrid <- cbind(grid_starts, grid_ends)


    ## first label the grid itself ##
    spatgrid[, gr_name := paste0("gr_", seq_len(.N))]

    # x-axis
    x_labels <- sort(unique(spatgrid$x_start))
    x_gr_names <- paste0("gr_x_", seq_len(length(x_labels)))
    names(x_gr_names) <- x_labels
    x_gr_names_vector <- x_gr_names[as.character(spatgrid$x_start)]
    spatgrid[, gr_x_name := x_gr_names_vector]

    # y-axis
    y_labels <- sort(unique(spatgrid$y_start))
    y_gr_names <- paste0("gr_y_", seq_len(length(y_labels)))
    names(y_gr_names) <- y_labels
    y_gr_names_vector <- y_gr_names[as.character(spatgrid$y_start)]
    spatgrid[, gr_y_name := y_gr_names_vector]

    # z-axis
    z_labels <- sort(unique(spatgrid$z_start))
    z_gr_names <- paste0("gr_z_", seq_len(length(z_labels)))
    names(z_gr_names) <- z_labels
    z_gr_names_vector <- z_gr_names[as.character(spatgrid$z_start)]
    spatgrid[, gr_z_name := z_gr_names_vector]

    ## for all dimensions ##
    # converter
    gr_dim_names <- spatgrid$gr_name
    names(gr_dim_names) <- paste0(
        spatgrid$gr_x_name, "-",
        spatgrid$gr_y_name, "-", spatgrid$gr_z_name
    )

    return(spatgrid)
}



#' @title createSpatialDefaultGrid
#' @name createSpatialDefaultGrid
#' @description Create a spatial grid using the default method
#' @param gobject giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param spat_loc_name spatial location name
#' @param sdimx_stepsize stepsize along the x-axis
#' @param sdimy_stepsize stepsize along the y-axis
#' @param sdimz_stepsize stepsize along the z-axis
#' @param minimum_padding minimum padding on the edges
#' @param name name for spatial grid (default = 'spatial_grid')
#' @param return_gobject boolean: return giotto object (default = TRUE)
#' @returns giotto object with updated spatial grid slot
#' @details Creates a spatial grid with defined x, y (and z) dimensions.
#' The dimension units are based on the provided spatial location units.
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' createSpatialDefaultGrid(g, sdimx_stepsize = 5, sdimy_stepsize = 5)
#' @export
createSpatialDefaultGrid <- function(gobject,
    spat_unit = NULL,
    feat_type = NULL,
    spat_loc_name = "raw",
    sdimx_stepsize = NULL,
    sdimy_stepsize = NULL,
    sdimz_stepsize = NULL,
    minimum_padding = 1,
    name = NULL,
    return_gobject = TRUE) {
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

    # check parameters
    if (is.null(name)) {
        name <- "spatial_grid"
    }

    if (length(c(sdimx_stepsize, sdimy_stepsize, sdimz_stepsize)) == 3) {
        resultgrid <- .create_spatialgrid_default_3d(
            gobject = gobject,
            spat_unit = spat_unit,
            spat_loc_name = spat_loc_name,
            sdimx_stepsize = sdimx_stepsize,
            sdimy_stepsize = sdimy_stepsize,
            sdimz_stepsize = sdimz_stepsize,
            minimum_padding = minimum_padding
        )
    } else if (!is.null(sdimx_stepsize) & !is.null(sdimy_stepsize)) {
        resultgrid <- .create_spatialgrid_default_2d(
            gobject = gobject,
            spat_unit = spat_unit,
            spat_loc_name = spat_loc_name,
            sdimx_stepsize = sdimx_stepsize,
            sdimy_stepsize = sdimy_stepsize,
            minimum_padding = minimum_padding
        )
    } else {
        stop("\n the stepsize for the x-axis (sdimx) and y-axis (sdimy) is
            the minimal requirement \n\n Additionally for a 3D spatial grid
            the z-axis (sdimz) is also required \n")
    }


    # object return
    if (return_gobject == TRUE) {
        # 1. check if name has already been used
        spg_names <- list_spatial_grids_names(
            gobject = gobject,
            spat_unit = spat_unit,
            feat_type = feat_type
        )

        if (name %in% spg_names) {
            wrap_msg(name, " has already been used, will be overwritten")
        }

        # 2. create spatial grid object
        parameters <- list(
            "sdimx_stepsize" = sdimx_stepsize,
            "sdimy_stepsize" = sdimy_stepsize,
            "sdimz_stepsize" = sdimz_stepsize,
            "minimum_padding" = minimum_padding
        )

        spatgridobj <- new("spatialGridObj",
            name = name,
            method = "default",
            parameters = parameters,
            gridDT = resultgrid,
            # outputObj = NULL, # NULL with default
            # (from original S3 definition)
            spat_unit = spat_unit,
            feat_type = feat_type,
            misc = NULL
        )

        # 3. assign spatial grid object
        gobject <- setSpatialGrid(
            gobject = gobject,
            spat_unit = spat_unit,
            feat_type = feat_type,
            name = name,
            spatial_grid = spatgridobj
        )

        # 4. update log
        ## update parameters used ##

        # parent function name
        cl <- sys.call(-1)


        if (is.null(cl)) {
            gobject <- update_giotto_params(gobject, description = "_grid")
        } else {
            fname <- as.character(cl[[1]])
            if (fname == "createSpatialGrid") {
                gobject <- update_giotto_params(gobject,
                    description = "_grid",
                    toplevel = 3
                )
            } else {
                gobject <- update_giotto_params(gobject, description = "_grid")
            }
        }

        return(gobject)
    } else {
        return(resultgrid)
    }
}





#' @title createSpatialGrid
#' @name createSpatialGrid
#' @description Create a spatial grid using the default method
#' @param gobject giotto object
#' @param spat_unit spatial unit
#' @param spat_loc_name spatial location name
#' @param name name for spatial grid
#' @param method method to create a spatial grid
#' @param sdimx_stepsize stepsize along the x-axis
#' @param sdimy_stepsize stepsize along the y-axis
#' @param sdimz_stepsize stepsize along the z-axis
#' @param minimum_padding minimum padding on the edges
#' @param return_gobject boolean: return giotto object (default = TRUE)
#' @returns giotto object with updated spatial grid slot
#' @details Creates a spatial grid with defined x, y (and z) dimensions.
#' The dimension units are based on the provided spatial location units.
#'   * **default method:** \code{\link{createSpatialDefaultGrid}}
#'
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' createSpatialGrid(g, sdimx_stepsize = 5, sdimy_stepsize = 5)
#'
#' @export
createSpatialGrid <- function(gobject,
    spat_unit = NULL,
    spat_loc_name = "raw",
    name = NULL,
    method = c("default"),
    sdimx_stepsize = NULL,
    sdimy_stepsize = NULL,
    sdimz_stepsize = NULL,
    minimum_padding = 1,
    return_gobject = TRUE) {
    # get parameters
    method <- match.arg(method, c("default"))

    if (method == "default") {
        out <- createSpatialDefaultGrid(
            gobject = gobject,
            spat_unit = spat_unit,
            spat_loc_name = spat_loc_name,
            sdimx_stepsize = sdimx_stepsize,
            sdimy_stepsize = sdimy_stepsize,
            sdimz_stepsize = sdimz_stepsize,
            minimum_padding = minimum_padding,
            name = name,
            return_gobject = return_gobject
        )
    }

    return(out)
}







#' @title annotate_spatlocs_with_spatgrid_2D
#' @description annotate spatial locations with 2D spatial grid information
#' @param spatloc spatial_locs slot from giotto object
#' @param spatgrid selected spatial_grid slot from giotto object
#' @returns annotated spatial location data.table
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' g <- createSpatialGrid(g, sdimx_stepsize = 5, sdimy_stepsize = 5)
#' g_spatloc <- getSpatialLocations(g, output = "data.table")
#' g_spatgrid <- getSpatialGrid(g)
#'
#' annotate_spatlocs_with_spatgrid_2D(
#'     spatloc = g_spatloc,
#'     spatgrid = g_spatgrid
#' )
#' @export
annotate_spatlocs_with_spatgrid_2D <- function(spatloc,
    spatgrid) {
    ## second label the spatial locations ##
    spatlocs <- data.table::copy(spatloc)

    # data.table variables
    gr_x_loc <- gr_y_loc <- gr_loc <- NULL

    x_vector <- spatlocs$sdimx
    x_breaks <- sort(unique(spatgrid$x_end))
    x_breaks_labels <- paste0("gr_x_", seq_len(length(x_breaks)))
    minimum_x <- min(spatgrid$x_start)
    my_x_gr <- cut(
        x = x_vector, breaks = c(minimum_x, x_breaks),
        include.lowest = TRUE, right = TRUE, labels = x_breaks_labels
    )
    spatlocs[, gr_x_loc := as.character(my_x_gr)]

    y_vector <- spatlocs$sdimy
    y_breaks <- sort(unique(spatgrid$y_end))
    y_breaks_labels <- paste0("gr_y_", seq_len(length(y_breaks)))
    minimum_y <- min(spatgrid$y_start)
    my_y_gr <- cut(
        x = y_vector, breaks = c(minimum_y, y_breaks),
        include.lowest = TRUE, right = TRUE, labels = y_breaks_labels
    )
    spatlocs[, gr_y_loc := as.character(my_y_gr)]


    ## for all dimensions ##
    # converter
    gr_dim_names <- spatgrid$gr_name
    names(gr_dim_names) <- paste0(spatgrid$gr_x_name, "-", spatgrid$gr_y_name)

    indiv_dim_names <- paste0(spatlocs$gr_x_loc, "-", spatlocs$gr_y_loc)
    my_gr <- gr_dim_names[indiv_dim_names]
    spatlocs[, gr_loc := as.character(my_gr)]

    return(spatlocs)
}


#' @title annotate_spatlocs_with_spatgrid_3D
#' @description annotate spatial locations with 3D spatial grid information
#' @param spatloc spatial_locs slot from giotto object
#' @param spatgrid selected spatial_grid slot from giotto object
#' @returns annotated spatial location data.table
#' @examples
#' g <- GiottoData::loadGiottoMini("starmap")
#' g_spatloc <- getSpatialLocations(g, output = "data.table")
#' g_spatgrid <- getSpatialGrid(g)
#'
#' annotate_spatlocs_with_spatgrid_3D(
#'     spatloc = g_spatloc,
#'     spatgrid = g_spatgrid
#' )
#' @export
annotate_spatlocs_with_spatgrid_3D <- function(spatloc,
    spatgrid) {
    ## second label the spatial locations ##
    spatlocs <- data.table::copy(spatloc)

    # data.table variables
    gr_x_loc <- gr_y_loc <- gr_z_loc <- gr_loc <- NULL

    x_vector <- spatlocs$sdimx
    x_breaks <- sort(unique(spatgrid$x_end))
    x_breaks_labels <- paste0("gr_x_", seq_len(length(x_breaks)))
    minimum_x <- min(spatgrid$x_start)
    my_x_gr <- cut(
        x = x_vector, breaks = c(minimum_x, x_breaks),
        include.lowest = TRUE, right = TRUE, labels = x_breaks_labels
    )
    spatlocs[, gr_x_loc := as.character(my_x_gr)]

    y_vector <- spatlocs$sdimy
    y_breaks <- sort(unique(spatgrid$y_end))
    y_breaks_labels <- paste0("gr_y_", seq_len(length(y_breaks)))
    minimum_y <- min(spatgrid$y_start)
    my_y_gr <- cut(
        x = y_vector, breaks = c(minimum_y, y_breaks),
        include.lowest = TRUE, right = TRUE, labels = y_breaks_labels
    )
    spatlocs[, gr_y_loc := as.character(my_y_gr)]

    z_vector <- spatlocs$sdimz
    z_breaks <- sort(unique(spatgrid$z_end))
    z_breaks_labels <- paste0("gr_z_", seq_len(length(z_breaks)))
    minimum_z <- min(spatgrid$z_start)
    my_z_gr <- cut(
        x = z_vector, breaks = c(minimum_z, z_breaks),
        include.lowest = TRUE, right = TRUE, labels = z_breaks_labels
    )
    spatlocs[, gr_z_loc := as.character(my_z_gr)]


    ## for all dimensions ##
    # converter
    gr_dim_names <- spatgrid$gr_name
    names(gr_dim_names) <- paste0(
        spatgrid$gr_x_name, "-", spatgrid$gr_y_name, "-", spatgrid$gr_z_name
    )

    indiv_dim_names <- paste0(
        spatlocs$gr_x_loc, "-", spatlocs$gr_y_loc, "-", spatlocs$gr_z_loc
    )
    my_gr <- gr_dim_names[indiv_dim_names]
    spatlocs[, gr_loc := as.character(my_gr)]

    return(spatlocs)
}




#' @title annotateSpatialGrid
#' @name annotateSpatialGrid
#' @description annotate spatial grid with cell ID and cell metadata (optional)
#' @param gobject Giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param spat_loc_name name of spatial locations
#' @param spatial_grid_name name of spatial grid,
#' see \code{\link{showGiottoSpatGrids}}
#' @param cluster_columns names of cell metadata, see \code{\link{pDataDT}}
#' @returns annotated spatial grid data.table
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' g <- createSpatialGrid(g, sdimx_stepsize = 5, sdimy_stepsize = 5)
#'
#' annotateSpatialGrid(g)
#' @export
annotateSpatialGrid <- function(gobject,
    spat_unit = NULL,
    feat_type = NULL,
    spat_loc_name = "raw",
    spatial_grid_name = "spatial_grid",
    cluster_columns = NULL) {
    # get grid
    spatial_grid <- getSpatialGrid(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type,
        name = spatial_grid_name
    )
    spatial_locs <- getSpatialLocations(
        gobject = gobject,
        spat_unit = spat_unit,
        name = spat_loc_name,
        output = "data.table",
        copy_obj = FALSE
    ) # copy happens anyways in step 1

    # 1. annotate spatial grid with spatial locations
    if (all(c("sdimx", "sdimy", "sdimz") %in% colnames(spatial_locs))) {
        annotgrid_locs <- annotate_spatlocs_with_spatgrid_3D(
            spatloc = spatial_locs, spatgrid = spatial_grid
        )
    } else if (all(c("sdimx", "sdimy") %in% colnames(spatial_locs))) {
        annotgrid_locs <- annotate_spatlocs_with_spatgrid_2D(
            spatloc = spatial_locs, spatgrid = spatial_grid
        )
    }

    # 2.select metadata
    cell_metadata <- pDataDT(gobject,
        spat_unit = spat_unit,
        feat_type = feat_type
    )

    if (!is.null(cluster_columns)) {
        annotation_vector <- cluster_columns
        possible_annotations <- colnames(cell_metadata)

        missing_annotation <- annotation_vector[!annotation_vector %in%
            possible_annotations]
        if (length(missing_annotation) > 0) {
            wrap_msg("These annotations were not found back in the cell metadata
                (pDataDT): \n", missing_annotation)
        }

        annotation_vector_found <- annotation_vector[annotation_vector %in%
            possible_annotations]
        cell_meta_selected <- cell_metadata[,
            c("cell_ID", annotation_vector_found),
            with = FALSE
        ]

        annotated_grid <- data.table::merge.data.table(
            x = annotgrid_locs, y = cell_meta_selected, by = "cell_ID"
        )

        return(annotated_grid)
    } else {
        return(annotgrid_locs)
    }
}

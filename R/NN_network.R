#' @include generics.R
NULL



# networkParam classes ####

#' @rdname NNNetworkParam-class
#' @title NNNetworkParam — Nearest-Neighbour Network Param Classes
#' @description Virtual parent for [kNNNetworkParam-class] and
#' [sNNNetworkParam-class].
#' @exportClass NNNetworkParam
setClass("NNNetworkParam", contains = c("networkParam", "VIRTUAL"))

#' @rdname kNNNetworkParam-class
#' @title kNNNetworkParam — k-Nearest-Neighbour Network Param
#' @description
#' Constructor and class for k-Nearest-Neighbour network parameters. Pass
#' to [createNetwork()] to build a kNN graph. kNN edges are inherently
#' asymmetric (`a`'s k-nearest neighbours are not necessarily those for
#' which `a` is among the k-nearest), so the resulting graph is
#' **directed** when promoted to igraph.
#' @slot k integer. number of nearest neighbours per node.
#' @slot filter logical. apply `minimum_k`/`maximum_distance` post-filter.
#' @slot maximum_distance numeric or NULL. drop edges longer than this.
#' @slot minimum_k integer. minimum neighbours per node when filtering.
#' @slot weight_fun function. weight = `weight_fun(distance)`.
#' @slot include_weight,include_distance logical. include columns.
#' @slot output character. one of `"auto"`, `"data.table"`, `"igraph"`,
#'   `"parquet"`. See [createNetwork()].
#' @exportClass kNNNetworkParam
setClass("kNNNetworkParam",
    contains = "NNNetworkParam",
    slots = list(
        k = "integer",
        filter = "logical",
        maximum_distance = "ANY",
        minimum_k = "integer",
        weight_fun = "function",
        include_weight = "logical",
        include_distance = "logical",
        output = "character",
        engine = "character",
        ef = "numeric",
        n_threads_build = "ANY"
    )
)


#' @name radiusNetworkParam-class
#' @title Fixed-radius network parameters
#' @description
#' Every pair of nodes within `eps` of each other is joined. Unlike kNN, node
#' degree is not fixed -- it follows local density, which is usually what is
#' meant by "cells that touch" in a tissue with varying cell density.
#'
#' Previously the only way to get this was `kNN` with a large `k` and a
#' `maximum_distance` filter, which searches for neighbours you then throw
#' away. This searches for the ones you asked for.
#'
#' The graph is symmetric by construction, so it is **undirected**.
#' @slot eps numeric. radius; nodes closer than this are joined.
#' @slot minimum_k integer. keep this many nearest neighbours per node even
#'   when they lie beyond `eps`, so no node is left isolated. `0` disables.
#' @slot weight_fun function. weight = `weight_fun(distance)`.
#' @slot include_weight,include_distance logical. include columns.
#' @slot output character. one of `"auto"`, `"data.table"`, `"igraph"`,
#'   `"parquet"`. See [createNetwork()].
#' @section Performance:
#' Backed by `dbscan::frNN`, which is exact. At 200,000 points with mean degree
#' 6 the search takes ~2 s. `spatstat.geom::closepairs()` is roughly 28x faster
#' and returns flat index vectors rather than the per-point lists `frNN` has to
#' be flattened out of, but \pkg{spatstat.geom} is not a Giotto dependency and
#' is 2D-only, so it is mentioned rather than used.
#' @exportClass radiusNetworkParam
setClass("radiusNetworkParam",
    contains = "NNNetworkParam",
    slots = list(
        eps = "numeric",
        minimum_k = "integer",
        weight_fun = "function",
        include_weight = "logical",
        include_distance = "logical",
        output = "character"
    )
)

#' @rdname sNNNetworkParam-class
#' @title sNNNetworkParam — Shared-Nearest-Neighbour Network Param
#' @description
#' Constructor and class for shared-Nearest-Neighbour network parameters.
#' sNN edges are symmetric by definition (`|N(a) ∩ N(b)| = |N(b) ∩ N(a)|`),
#' so the resulting graph is **undirected** — one edge per pair.
#' @slot k integer. number of nearest neighbours used to compute sharing.
#' @slot top_shared integer. keep at least this many edges per node.
#' @slot minimum_shared integer. keep edges with at least this many shared
#'   neighbours.
#' @slot weight_fun function. weight = `weight_fun(distance)`.
#' @slot include_weight,include_distance logical. include columns.
#' @slot output character. See [createNetwork()].
#' @exportClass sNNNetworkParam
setClass("sNNNetworkParam",
    contains = "NNNetworkParam",
    slots = list(
        k = "integer",
        top_shared = "integer",
        minimum_shared = "integer",
        weight_fun = "function",
        include_weight = "logical",
        include_distance = "logical",
        output = "character",
        engine = "character",
        ef = "numeric",
        n_threads_build = "ANY"
    )
)

#' @rdname delaunayNetworkParam-class
#' @title delaunayNetworkParam — Delaunay Network Param
#' @description
#' Constructor and class for Delaunay triangulation network parameters.
#' Delaunay edges are an undirected geometric relation, so the resulting
#' graph is **undirected** — one edge per pair.
#' @slot method character. backend: `"deldir"`, `"RTriangle"`, or
#'   `"geometry"`.
#' @slot maximum_distance numeric, `"auto"`, or NULL.
#' @slot minimum_k integer. minimum neighbours per node when filtering.
#' @slot weight_fun function. weight = `weight_fun(distance)`.
#' @slot include_weight,include_distance logical. include columns.
#' @slot output character. See [createNetwork()].
#' @slot options character. *geometry only.* passed to `geometry::delaunayn`.
#' @slot Y,j logical; S numeric. *RTriangle only.* passed to
#'   `RTriangle::triangulate`.
#' @exportClass delaunayNetworkParam
setClass("delaunayNetworkParam",
    contains = "networkParam",
    slots = list(
        method = "character",
        maximum_distance = "ANY",
        minimum_k = "integer",
        weight_fun = "function",
        include_weight = "logical",
        include_distance = "logical",
        output = "character",
        # geometry-only
        options = "character",
        # RTriangle-only
        Y = "logical",
        j = "logical",
        S = "numeric"
    )
)


# networkParam constructors ####

#' @rdname kNNNetworkParam-class
#' @param k number of neighbours
#' @param filter apply `minimum_k`/`maximum_distance` post-filter
#' @param maximum_distance maximum edge length
#' @param minimum_k minimum neighbours per node when filtering
#' @param weight_fun function mapping distance to weight
#' @param include_weight,include_distance include columns in output
#' @param engine character. kNN search backend. `"dbscan"` (default) is exact
#'   and single-threaded, and is the better choice on small data where it is
#'   also faster. `"hnsw"` uses an approximate HNSW index via [hnswKNN()] --
#'   multithreaded and free of the kd-tree's degradation at PCA
#'   dimensionality, which is what makes it worth reaching for on large
#'   datasets. On 158,662 cells at `k = 30` it reproduced 99.995% of the exact
#'   network's undirected edges in 11.8s against 79.9s. Requires
#'   \pkg{RcppHNSW}. An `"auto"` setting that picks by dataset size is planned.
#' @param ef integer. `"hnsw"` only, ignored otherwise. Search beam width,
#'   default 200 -- the recall/speed dial. Higher values search more of the
#'   graph, moving the result closer to the exact `"dbscan"` answer at the cost
#'   of query time.
#' @param n_threads_build integer or `NULL`. `"hnsw"` only, ignored otherwise.
#'   Threads for the index build, default `1`. A parallel build is not
#'   reproducible -- insertion order varies, so neighbours differ slightly
#'   between runs and that propagates to clustering even with a fixed seed.
#'   `NULL` inherits the search thread count and trades reproducibility for
#'   speed.
#' @param output one of `"auto"`, `"data.table"`, `"igraph"`, `"parquet"`
#' @export
kNNNetworkParam <- function(k = 30L, filter = FALSE,
        maximum_distance = NULL, minimum_k = 0L,
        weight_fun = function(d) 1 / (1 + d),
        include_weight = TRUE, include_distance = TRUE,
        output = c("auto", "data.table", "igraph", "parquet"),
        engine = c("dbscan", "hnsw"), ef = 200, n_threads_build = 1L) {
    output <- match.arg(output)
    engine <- match.arg(engine)
    new("kNNNetworkParam",
        k = as.integer(k), filter = filter,
        maximum_distance = maximum_distance,
        minimum_k = as.integer(minimum_k),
        weight_fun = weight_fun,
        include_weight = include_weight,
        include_distance = include_distance,
        output = output,
        engine = engine,
        ef = as.numeric(ef),
        n_threads_build = n_threads_build
    )
}

#' @rdname sNNNetworkParam-class
#' @param k number of neighbours used to compute sharing
#' @param top_shared keep at least this many edges per node
#' @param minimum_shared keep edges with at least this many shared neighbours
#' @param weight_fun function mapping distance to weight
#' @param include_weight,include_distance include columns in output
#' @param engine character. kNN search backend. `"dbscan"` (default) is exact
#'   and single-threaded, and is the better choice on small data where it is
#'   also faster. `"hnsw"` uses an approximate HNSW index via [hnswKNN()] --
#'   multithreaded and free of the kd-tree's degradation at PCA
#'   dimensionality, which is what makes it worth reaching for on large
#'   datasets. On 158,662 cells at `k = 30` it reproduced 99.995% of the exact
#'   network's undirected edges in 11.8s against 79.9s. Requires
#'   \pkg{RcppHNSW}. An `"auto"` setting that picks by dataset size is planned.
#' @param ef integer. `"hnsw"` only, ignored otherwise. Search beam width,
#'   default 200 -- the recall/speed dial. Higher values search more of the
#'   graph, moving the result closer to the exact `"dbscan"` answer at the cost
#'   of query time.
#' @param n_threads_build integer or `NULL`. `"hnsw"` only, ignored otherwise.
#'   Threads for the index build, default `1`. A parallel build is not
#'   reproducible -- insertion order varies, so neighbours differ slightly
#'   between runs and that propagates to clustering even with a fixed seed.
#'   `NULL` inherits the search thread count and trades reproducibility for
#'   speed.
#' @param output one of `"auto"`, `"data.table"`, `"igraph"`, `"parquet"`
#' @export
sNNNetworkParam <- function(k = 30L, top_shared = 3L, minimum_shared = 5L,
        weight_fun = function(d) 1 / (1 + d),
        include_weight = TRUE, include_distance = TRUE,
        output = c("auto", "data.table", "igraph", "parquet"),
        engine = c("dbscan", "hnsw"), ef = 200, n_threads_build = 1L) {
    output <- match.arg(output)
    engine <- match.arg(engine)
    new("sNNNetworkParam",
        k = as.integer(k),
        top_shared = as.integer(top_shared),
        minimum_shared = as.integer(minimum_shared),
        weight_fun = weight_fun,
        include_weight = include_weight,
        include_distance = include_distance,
        output = output,
        engine = engine,
        ef = as.numeric(ef),
        n_threads_build = n_threads_build
    )
}

#' @rdname delaunayNetworkParam-class
#' @param method backend: `"deldir"`, `"RTriangle"`, or `"geometry"`
#' @param maximum_distance maximum edge length, or `"auto"`, or `NULL`
#' @param minimum_k minimum neighbours per node when filtering
#' @param weight_fun function mapping distance to weight
#' @param include_weight,include_distance include columns in output
#' @param output one of `"auto"`, `"data.table"`, `"igraph"`, `"parquet"`
#' @param options *geometry only.* passed to `geometry::delaunayn`
#' @param Y,j,S *RTriangle only.* passed to `RTriangle::triangulate`
#' @export
delaunayNetworkParam <- function(
        method = c("deldir", "RTriangle", "geometry"),
        maximum_distance = "auto", minimum_k = 0L,
        weight_fun = function(d) 1 / d,
        include_weight = TRUE, include_distance = TRUE,
        output = c("auto", "data.table", "igraph", "parquet"),
        options = "Pp", Y = TRUE, j = TRUE, S = 0) {
    method <- match.arg(method)
    output <- match.arg(output)
    new("delaunayNetworkParam",
        method = method,
        maximum_distance = maximum_distance,
        minimum_k = as.integer(minimum_k),
        weight_fun = weight_fun,
        include_weight = include_weight,
        include_distance = include_distance,
        output = output,
        options = options, Y = Y, j = j, S = S
    )
}

#' @rdname radiusNetworkParam-class
#' @param eps numeric. Radius within which nodes are joined. There is no
#'   default -- the right value is set by the data's units and cell spacing.
#'   A reasonable starting point is a high quantile of the edge lengths of a
#'   Delaunay network on the same points.
#' @param minimum_k integer. Retain this many nearest neighbours per node even
#'   if they fall outside `eps`. Guards against isolated nodes in sparse
#'   regions, which otherwise drop out of the network entirely. Default `0`.
#' @param weight_fun function mapping distance to weight
#' @param include_weight,include_distance logical
#' @param output one of `"auto"`, `"data.table"`, `"igraph"`, `"parquet"`
#' @returns a `radiusNetworkParam` object
#' @examples
#' p <- radiusNetworkParam(eps = 25)
#' @export
radiusNetworkParam <- function(eps,
        minimum_k = 0L,
        weight_fun = function(d) 1 / (1 + d),
        include_weight = TRUE, include_distance = TRUE,
        output = c("auto", "data.table", "igraph", "parquet")) {
    output <- match.arg(output)
    checkmate::assert_number(eps, lower = 0, finite = TRUE)
    new("radiusNetworkParam",
        eps = as.numeric(eps),
        minimum_k = as.integer(minimum_k),
        weight_fun = weight_fun,
        include_weight = include_weight,
        include_distance = include_distance,
        output = output
    )
}


#' @title networkParam — Dispatcher constructor
#' @name networkParam
#' @description
#' Returns the appropriate concrete `*NetworkParam` based on `type`.
#' Equivalent to calling [kNNNetworkParam()], [sNNNetworkParam()], or
#' [delaunayNetworkParam()] directly.
#' @param type one of `"kNN"`, `"sNN"`, `"delaunay"`, `"radius"`
#' @param ... arguments forwarded to the type-specific constructor
#' @returns a [networkParam-class]-inheriting object
#' @examples
#' p <- networkParam("kNN", k = 30)
#' @export
networkParam <- function(type = c("kNN", "sNN", "delaunay", "radius"), ...) {
    type <- match.arg(type)
    switch(type,
        kNN      = kNNNetworkParam(...),
        sNN      = sNNNetworkParam(...),
        delaunay = delaunayNetworkParam(...),
        radius   = radiusNetworkParam(...)
    )
}


# createNetwork methods ####

# Internal helper: post-process the edge data.table from a backend helper
# and emit per the param's `output` slot. Centralizes node_id substitution,
# column trim, directionality handling, and output dispatch.
.finalize_network <- function(network_dt, x, node_ids, type, directed, param,
        backend = NULL) {
    # NSE vars
    from <- to <- NULL

    # default node_ids are input matrix rownames
    if (is.null(node_ids) && !is.null(rownames(x))) {
        node_ids <- rownames(x)
    }
    if (!is.null(node_ids)) {
        names(node_ids) <- seq_along(node_ids)
        network_dt[, "from" := node_ids[from]]
        network_dt[, "to" := node_ids[to]]
    }

    # cols to keep
    keep_cols <- c("from", "to")
    all_index <- network_dt[, unique(unlist(.SD)), .SDcols = keep_cols]

    # A node with no surviving edge is not a vertex of the graph built below,
    # so it disappears from every downstream result -- proximity enrichment,
    # motifs, neighbourhood composition -- with nothing to say it went. That is
    # easy to cause by accident: the Delaunay default is
    # maximum_distance = "auto", which trims long edges and on a clustered
    # section can strand a few hundred cells. Say so rather than letting the
    # cell count quietly disagree with the input.
    n_dropped <- nrow(x) - length(all_index)
    if (n_dropped > 0L) {
        vmsg(sprintf(
            "%d of %d node(s) have no edges and are omitted from the network",
            n_dropped, nrow(x)
        ))
    }

    if (isTRUE(param@include_weight)) keep_cols <- c(keep_cols, "weight")
    if (isTRUE(param@include_distance)) keep_cols <- c(keep_cols, "distance")
    if (type == "sNN") keep_cols <- c(keep_cols, "shared")
    network_dt <- network_dt[, keep_cols, with = FALSE]

    # resolve output
    output <- param@output
    if (output == "auto") {
        output <- if (is.null(backend)) "data.table" else "parquet"
    }

    if (output == "data.table") {
        return(network_dt)
    }
    if (output == "igraph") {
        return(igraph::graph_from_data_frame(
            network_dt, directed = directed, vertices = all_index
        ))
    }
    if (output == "parquet") {
        package_check("GiottoDisk", repository = "github:giotto-suite/GiottoDisk")
        if (is.null(backend)) {
            # storeCreate handles dump-path resolution when no path supplied
            store <- GiottoDisk::storeCreate(type = "parquetEdgeStore")
            return(GiottoDisk::storeWrite(store, network_dt))
        }
        # write through the supplied source backend
        return(GiottoDisk::sourceWrite(backend, network_dt,
            store_type = "parquetEdgeStore"))
    }
    stop(sprintf("[createNetwork] unknown output: %s", output))
}

#' @rdname createNetwork
setMethod("createNetwork", signature("matrix", "kNNNetworkParam"),
    function(x, param, node_ids = NULL, verbose = NULL, backend = NULL, ...) {
        if (length(x) == 0L) {
            stop(wrap_txt(errWidth = TRUE,
                "[createNetwork] empty matrix provided.
                No network can be generated"
            ))
        }
        dt <- .net_dt_knn(
            x = x, k = param@k, filter = param@filter,
            maximum_distance = param@maximum_distance,
            minimum_k = param@minimum_k,
            weight_fun = param@weight_fun,
            include_weight = param@include_weight,
            include_distance = param@include_distance,
            engine = param@engine,
            ef = param@ef,
            n_threads_build = param@n_threads_build,
            verbose = verbose, ...
        )
        .finalize_network(dt, x = x, node_ids = node_ids,
            type = "kNN", directed = TRUE, param = param, backend = backend)
    }
)


#' @rdname createNetwork
setMethod("createNetwork", signature("matrix", "radiusNetworkParam"),
    function(x, param, node_ids = NULL, verbose = NULL, backend = NULL, ...) {
        if (length(x) == 0L) {
            stop(wrap_txt(errWidth = TRUE,
                "[createNetwork] empty matrix provided.
                No network can be generated"
            ))
        }
        dt <- .net_dt_radius(
            x = x, eps = param@eps,
            minimum_k = param@minimum_k,
            weight_fun = param@weight_fun,
            include_weight = param@include_weight,
            include_distance = param@include_distance,
            verbose = verbose, ...
        )
        .finalize_network(dt, x = x, node_ids = node_ids,
            type = "radius", directed = FALSE, param = param,
            backend = backend)
    }
)

#' @rdname createNetwork
setMethod("createNetwork", signature("matrix", "sNNNetworkParam"),
    function(x, param, node_ids = NULL, verbose = NULL, backend = NULL, ...) {
        if (length(x) == 0L) {
            stop(wrap_txt(errWidth = TRUE,
                "[createNetwork] empty matrix provided.
                No network can be generated"
            ))
        }
        dt <- .net_dt_snn(
            x = x, k = param@k,
            top_shared = param@top_shared,
            minimum_shared = param@minimum_shared,
            weight_fun = param@weight_fun,
            include_weight = param@include_weight,
            include_distance = param@include_distance,
            engine = param@engine,
            ef = param@ef,
            n_threads_build = param@n_threads_build,
            verbose = verbose, ...
        )
        # sNN: symmetric relation, collapse to undirected unique pairs.
        # `rank` is per-source and ill-defined after symmetrization — dropped.
        dt <- .undirected_unique(dt)
        .finalize_network(dt, x = x, node_ids = node_ids,
            type = "sNN", directed = FALSE, param = param, backend = backend)
    }
)

#' @rdname createNetwork
setMethod("createNetwork", signature("matrix", "delaunayNetworkParam"),
    function(x, param, node_ids = NULL, verbose = NULL, backend = NULL, ...) {
        if (length(x) == 0L) {
            stop(wrap_txt(errWidth = TRUE,
                "[createNetwork] empty matrix provided.
                No network can be generated"
            ))
        }
        helper <- switch(param@method,
            deldir    = .net_dt_del_deldir,
            RTriangle = .net_dt_del_rtriangle,
            geometry  = .net_dt_del_geometry
        )
        # Build per-backend arg list. Each helper accepts only a subset;
        # don't forward irrelevant Param slots.
        helper_args <- list(
            x = x,
            include_weight = param@include_weight,
            maximum_distance = param@maximum_distance,
            minimum_k = param@minimum_k,
            weight_fun = param@weight_fun
        )
        if (param@method == "geometry") {
            helper_args$options <- param@options
        } else if (param@method == "RTriangle") {
            helper_args$Y <- param@Y
            helper_args$j <- param@j
            helper_args$S <- param@S
        }
        dt <- do.call(helper, c(helper_args, list(...)))$delaunay_network_DT
        .finalize_network(dt, x = x, node_ids = node_ids,
            type = "delaunay", directed = FALSE, param = param,
            backend = backend)
    }
)

# Collapse a directed (from, to) edge table to an undirected unique pair
# table by canonicalizing each pair as (min, max) and applying `unique()`
# on the (from, to) key.
.undirected_unique <- function(dt) {
    from <- to <- NULL
    if (nrow(dt) == 0L) return(dt)
    swap <- dt$from > dt$to
    if (any(swap)) {
        tmp <- dt$from[swap]
        data.table::set(dt, which(swap), "from", dt$to[swap])
        data.table::set(dt, which(swap), "to", tmp)
    }
    unique(dt, by = c("from", "to"))
}

# Legacy string-arg shim — translates the pre-Param signature
# `createNetwork(x, type = ..., method = ..., as.igraph = ..., ...)`
# into the appropriate Param and dispatches.
#' @rdname createNetwork
setMethod("createNetwork", signature("matrix", "missing"),
    function(x, param,
            type = c("sNN", "kNN", "delaunay"),
            method = c("dbscan", "geometry", "RTriangle", "deldir"),
            node_ids = NULL,
            include_distance = TRUE,
            include_weight = TRUE,
            as.igraph = TRUE,
            verbose = NULL,
            backend = NULL,
            ...) {
        if (length(x) == 0L) {
            stop(wrap_txt(errWidth = TRUE,
                "[createNetwork] empty matrix provided.
                No network can be generated"
            ))
        }
        type <- match.arg(type, choices = c("sNN", "kNN", "delaunay"))
        mdef <- c("dbscan", "geometry", "RTriangle", "deldir")
        mchoices <- if (type %in% c("sNN", "kNN")) {
            "dbscan"
        } else {
            c("deldir", "RTriangle", "geometry")
        }
        if (identical(method, mdef)) method <- mchoices[[1L]]
        method <- match.arg(method, choices = mchoices)

        # translate as.igraph → output
        output <- if (isTRUE(as.igraph)) "igraph" else "data.table"

        # build Param, splitting `...` into Param ctor args vs method args
        param_ctor <- switch(type,
            kNN = kNNNetworkParam, sNN = sNNNetworkParam,
            delaunay = delaunayNetworkParam
        )
        ctor_formals <- names(formals(param_ctor))
        dots <- list(...)
        ctor_args <- dots[intersect(names(dots), ctor_formals)]
        method_args <- dots[setdiff(names(dots), ctor_formals)]
        ctor_args$include_weight <- include_weight
        ctor_args$include_distance <- include_distance
        ctor_args$output <- output
        if (type == "delaunay") ctor_args$method <- method
        param <- do.call(param_ctor, ctor_args)

        do.call(createNetwork, c(
            list(x = x, param = param, node_ids = node_ids,
                verbose = verbose, backend = backend),
            method_args
        ))
    }
)


# Higher-level dispatches ####

#' @rdname createNetwork
setMethod("createNetwork", signature("spatLocsObj", "networkParam"),
    function(x, param, node_ids = NULL, ...) {
        sl_dt <- x[]
        coord_cols <- intersect(c("sdimx", "sdimy", "sdimz"), names(sl_dt))
        coords <- as.matrix(sl_dt[, coord_cols, with = FALSE])
        if (is.null(node_ids) && "cell_ID" %in% names(sl_dt)) {
            node_ids <- sl_dt$cell_ID
        }
        createNetwork(coords, param, node_ids = node_ids, ...)
    }
)

#' @rdname createNetwork
#' @param dimensions_to_use integer vector; columns of the `dimObj` matrix
#'   to keep when building the network. `NULL` (default) keeps all.
setMethod("createNetwork", signature("dimObj", "networkParam"),
    function(x, param, dimensions_to_use = NULL, ...) {
        mat <- x[]
        if (!is.null(dimensions_to_use)) {
            dimensions_to_use <- dimensions_to_use[
                dimensions_to_use %in% seq_len(ncol(mat))
            ]
            mat <- mat[, dimensions_to_use, drop = FALSE]
        }
        createNetwork(mat, param, ...)
    }
)

#' @rdname createNetwork
#' @param spat_unit spatial unit (`giotto` method)
#' @param feat_type feature type (`giotto` method, NN networks)
#' @param space for NN networks on a `giotto` object: which space the
#'   neighborhood is defined in. Default `"expression"` (pulls a
#'   dimension reduction such as PCA). Set to `"spatial"` to build a
#'   spatial kNN/sNN from cell coordinates.
#' @param dim_reduction_to_use name of the reduction family to pull from the
#'   `giotto` object (default `"pca"`). Only used when `space = "expression"`.
#' @param dim_reduction_name specific reduction name. Only used when
#'   `space = "expression"`.
#' @param spat_loc_name spatial-locations name. Used by the Delaunay method
#'   and by NN methods when `space = "spatial"`.
setMethod("createNetwork", signature("giotto", "NNNetworkParam"),
    function(x, param,
            spat_unit = NULL, feat_type = NULL,
            space = c("expression", "spatial"),
            dim_reduction_to_use = "pca", dim_reduction_name = NULL,
            dimensions_to_use = seq_len(10L),
            spat_loc_name = "raw", ...) {
        space <- match.arg(space)
        spat_unit <- set_default_spat_unit(x, spat_unit = spat_unit)

        if (space == "spatial") {
            sl <- getSpatialLocations(x,
                spat_unit = spat_unit,
                name = spat_loc_name,
                output = "spatLocsObj"
            )
            return(createNetwork(sl, param, ...))
        }

        feat_type <- set_default_feat_type(x,
            spat_unit = spat_unit, feat_type = feat_type
        )
        if (is.null(dim_reduction_name)) {
            dim_reduction_name <- if (feat_type == "rna") {
                dim_reduction_to_use
            } else {
                paste0(feat_type, ".", dim_reduction_to_use)
            }
        }
        dim_obj <- getDimReduction(x,
            spat_unit = spat_unit, feat_type = feat_type,
            reduction = "cells", reduction_method = dim_reduction_to_use,
            name = dim_reduction_name, output = "dimObj"
        )
        createNetwork(dim_obj, param,
            dimensions_to_use = dimensions_to_use, ...)
    }
)

#' @rdname createNetwork
#' @details
#' `radiusNetworkParam` defaults to `space = "spatial"` rather than
#' `"expression"`. A radius is a distance with units, and for a radius graph
#' those units are almost always the tissue's -- "cells within 25 um". Reaching
#' it through the general NN method would have silently measured `eps` in PCA
#' units instead. Pass `space = "expression"` to get the PCA-space behaviour
#' back.
setMethod("createNetwork", signature("giotto", "radiusNetworkParam"),
    function(x, param, space = c("spatial", "expression"), ...) {
        space <- match.arg(space)
        callNextMethod(x = x, param = param, space = space, ...)
    }
)

#' @rdname createNetwork
setMethod("createNetwork", signature("giotto", "delaunayNetworkParam"),
    function(x, param,
            spat_unit = NULL, spat_loc_name = "raw", ...) {
        spat_unit <- set_default_spat_unit(x, spat_unit = spat_unit)
        sl <- getSpatialLocations(x,
            spat_unit = spat_unit,
            name = spat_loc_name,
            output = "spatLocsObj"
        )
        createNetwork(sl, param, ...)
    }
)


# x input is a matrix
# kNN search backend.
#
# "dbscan" is exact and single-threaded, and is the default: on small data it
# is both faster and exact, since building an HNSW index is pure overhead
# there. "hnsw" uses hnswKNN(), approximate but multithreaded and free of the
# kd-tree's degradation as dimensionality rises -- which is the dominant cost
# of createNearestNetwork() at PCA dimensionality, and why it pays off on large
# datasets. Both return the same `c("kNN", "NN")` shape, so everything
# downstream is identical.
.nn_search <- function(x, k, engine = c("dbscan", "hnsw"), ...) {
    engine <- match.arg(engine)
    if (identical(engine, "dbscan")) {
        # ef / n_threads_build are hnsw-only; accepting and ignoring them here
        # lets a caller set them once and switch engines freely.
        dots <- list(...)
        dots[c("ef", "n_threads_build")] <- NULL
        return(do.call(dbscan::kNN,
            c(list(x = x, k = k, sort = TRUE), dots)))
    }
    hnswKNN(x = x, k = k, ...)
}


.net_dt_knn <- function(
        x, k = 30L, include_weight = TRUE, include_distance = TRUE,
        filter = FALSE,
        maximum_distance = NULL, minimum_k = 0L,
        weight_fun = function(d) 1 / (1 + d),
        engine = c("dbscan", "hnsw"),
        ef = 200,
        n_threads_build = 1L,
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

    nn_network <- .nn_search(x, k = k, engine = engine, ef = ef,
        n_threads_build = n_threads_build, ...)

    nn_network_dt <- data.table::data.table(
        from = rep(seq_len(nrow(nn_network$id)), k),
        to = as.vector(nn_network$id)
    )

    # optional info
    if (include_distance || include_weight) {
        # Use the distances the search itself returned, on both branches.
        #
        # This previously recomputed them from the coordinate matrix whenever
        # maximum_distance was set -- an 11x penalty triggered precisely by
        # asking for a cutoff, since the recompute went one stats::dist() call
        # per edge. dbscan::kNN already returns exact euclidean distances;
        # measured max deviation from a recompute is 4e-16.
        #
        # Beyond the speed, this is the self-consistent choice: whatever metric
        # the engine searched under is the metric the cutoff now filters on. The
        # old branch could mix metrics if a non-euclidean engine were wired in.
        nn_network_dt[, "distance" := as.vector(nn_network$dist)]
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
# Fixed-radius neighbour network.
#
# dbscan::frNN searches only within eps, so unlike the kNN-with-a-big-k
# workaround this does not find neighbours in order to discard them. It returns
# per-node lists, which are flattened here; the result is symmetric by
# construction (if a is within eps of b then b is within eps of a), so it is
# canonicalized to one row per undirected pair.
#
# spatstat.geom::closepairs() is ~28x faster than frNN and returns flat index
# vectors directly, but spatstat.geom is not a Giotto dependency and is 2D
# only. dbscan is already a hard Imports, so frNN is what ships.
.net_dt_radius <- function(
        x, eps, minimum_k = 0L,
        include_weight = TRUE, include_distance = TRUE,
        weight_fun = function(d) 1 / (1 + d),
        verbose = NULL, ...) {
    # NSE vars
    from <- to <- distance <- NULL

    fr <- dbscan::frNN(x = x, eps = eps, sort = TRUE, ...)

    lens <- lengths(fr$id)
    dt <- data.table::data.table(
        from = rep.int(seq_along(fr$id), lens),
        to = unlist(fr$id, use.names = FALSE),
        distance = unlist(fr$dist, use.names = FALSE)
    )

    # minimum_k rescues nodes whose eps-ball is empty or sparse. frNN cannot
    # supply those neighbours -- they are outside eps by definition -- so they
    # come from a separate kNN search, run only when asked for and only wide
    # enough to cover the floor.
    if (minimum_k > 0L) {
        short <- which(lens < minimum_k)
        if (length(short) > 0L) {
            kk <- min(as.integer(minimum_k), nrow(x) - 1L)
            # Query points are rows of x, and dbscan does not know that, so it
            # returns each one's nearest neighbour as itself at distance 0.
            # Ask for one extra and drop the self-match, or the rescue is
            # silently removed again by the from != to filter below.
            nn <- dbscan::kNN(x = x, k = kk + 1L,
                query = x[short, , drop = FALSE], sort = TRUE)
            add <- data.table::data.table(
                from = rep.int(short, kk + 1L),
                to = as.vector(nn$id),
                distance = as.vector(nn$dist)
            )
            add <- add[from != to]
            # keep the kk nearest genuine neighbours per rescued node
            add <- add[, utils::head(.SD, kk), by = "from"]
            dt <- rbind(dt, add)
        }
    }

    dt <- dt[from != to]
    if (nrow(dt) > 0L) {
        # symmetric by construction -> keep one row per undirected pair
        dt <- .undirected_unique(dt)
        data.table::setorder(dt, from, to)
    }

    if (include_weight) {
        dt[, "weight" := weight_fun(distance)]
    }
    if (!include_distance) {
        dt[, "distance" := NULL]
    }
    dt[]
}


.net_dt_snn <- function(
        x, k = 30L, include_weight = TRUE, include_distance = TRUE,
        top_shared = 3L, minimum_shared = 5L,
        weight_fun = function(d) 1 / (1 + d),
        engine = c("dbscan", "hnsw"),
        ef = 200,
        n_threads_build = 1L,
        nn_network = NULL,
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

    # The kNN search is the expensive half of an sNN build. A caller that
    # already has one -- from a previous createNetwork(type = "kNN"), or
    # shared with a UMAP that reuses the same neighbours -- passes it here
    # instead of paying for it twice.
    if (is.null(nn_network)) {
        nn_network <- .nn_search(x, k = k, engine = engine, ef = ef,
        n_threads_build = n_threads_build, ...)
    } else {
        if (!inherits(nn_network, "kNN")) {
            stop(wrap_txt(errWidth = TRUE,
                "[createNetwork] `nn_network` must be a kNN object, as
                returned by dbscan::kNN() or GiottoDisk::hnswKNN()."
            ), call. = FALSE)
        }
        if (ncol(nn_network$id) < k) {
            stop(wrap_txtf(
                "[createNetwork] `nn_network` has %d neighbours per node,
                fewer than the requested k = %d.",
                ncol(nn_network$id), k
            ), call. = FALSE)
        }
        if (nrow(nn_network$id) != nrow(x)) {
            stop(wrap_txtf(
                "[createNetwork] `nn_network` covers %d nodes but `x` has %d.",
                nrow(nn_network$id), nrow(x)
            ), call. = FALSE)
        }
    }
    snn_network <- dbscan::sNN(x = nn_network, k = k, kt = NULL)

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

    # expand each simplex into its (d+1 choose 2) edges
    edge_combs <- combn(x = ncol(delaunay_simplex_mat), m = 2L)
    from_idx <- as.vector(delaunay_simplex_mat[, edge_combs[1L, ]])
    to_idx <- as.vector(delaunay_simplex_mat[, edge_combs[2L, ]])

    # canonicalize as undirected pairs (from < to), then dedupe
    swap <- from_idx > to_idx
    tmp <- from_idx[swap]
    from_idx[swap] <- to_idx[swap]
    to_idx[swap] <- tmp
    delaunay_network_dt <- unique(data.table::data.table(
        from = from_idx, to = to_idx
    ))
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
    checkmate::assert_matrix(x)
    checkmate::assert_data_table(y)
    y <- .edge_int_index(y, x_node_ids)

    # One vectorized pass over the endpoint rows. The general machinery below
    # (.edge_coords_array + .calc_edge_dist) supports any stats::dist method,
    # but does so with one dist() call per edge -- measured 556x slower, and
    # this function has only ever asked for euclidean. Non-euclidean callers
    # still have .calc_edge_dist().
    #
    # Index into the original coordinates rather than reusing any coordinates
    # a triangulation backend hands back: deldir's delsgs$x1/y1/x2/y2 are
    # precision-reduced by its Fortran core and give distances differing by up
    # to ~1e-6, which would shift maximum_distance filtering at the margin.
    sqrt(rowSums(
        (x[y$from, , drop = FALSE] - x[y$to, , drop = FALSE])^2
    ))
}


# Resolve a network table's from/to to integer row indices of the coord matrix.
# Shared by edge_distances() and .edge_coords_array() so the character-index
# handling has one implementation.
#' @keywords internal
#' @noRd
.edge_int_index <- function(y, x_node_ids = NULL) {
    # NSE vars
    from <- to <- NULL

    if (y[, is.character(from) && is.character(to)]) {
        if (is.null(x_node_ids)) {
            .gstop("y is indexed by node ID.
            Node IDs for x must be provided as a vector to 'x_node_ids'")
        }
        y <- data.table::copy(y)
        y[, from := match(from, x_node_ids)]
        y[, to := match(to, x_node_ids)]
    }
    y
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
    checkmate::assert_matrix(x)
    checkmate::assert_data_table(y)

    y <- .edge_int_index(y, x_node_ids)

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
#' @param engine character. kNN search backend. `"dbscan"` (default) is exact
#'   and single-threaded, and is the better choice on small data where it is
#'   also faster. `"hnsw"` uses an approximate HNSW index via [hnswKNN()] --
#'   multithreaded and free of the kd-tree's degradation at PCA
#'   dimensionality, which is what makes it worth reaching for on large
#'   datasets. On 158,662 cells at `k = 30` it reproduced 99.995% of the exact
#'   network's undirected edges in 11.8s against 79.9s. Requires
#'   \pkg{RcppHNSW}. An `"auto"` setting that picks by dataset size is planned.
#' @param ef integer. `"hnsw"` only, ignored otherwise. Search beam width,
#'   default 200 -- the recall/speed dial. Higher values search more of the
#'   graph, moving the result closer to the exact `"dbscan"` answer at the cost
#'   of query time.
#' @param n_threads_build integer or `NULL`. `"hnsw"` only, ignored otherwise.
#'   Threads for the index build, default `1`. A parallel build is not
#'   reproducible -- insertion order varies, so neighbours differ slightly
#'   between runs and that propagates to clustering even with a fixed seed.
#'   `NULL` inherits the search thread count and trades reproducibility for
#'   speed.
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
        engine = c("dbscan", "hnsw"),
        ef = 200,
        n_threads_build = 1L,
        verbose = TRUE,
        ...) {
    # NB: thin wrapper over createNetwork() + nnNetObj construction.
    # Legacy expression-matrix path (dim_reduction_to_use = NULL) goes
    # through the matrix method; PCA / dim-reduction path goes through
    # the giotto/NN method.

    type <- match.arg(type, c("sNN", "kNN"))
    engine <- match.arg(engine)

    spat_unit <- set_default_spat_unit(gobject, spat_unit = spat_unit)
    feat_type <- set_default_feat_type(gobject,
        spat_unit = spat_unit, feat_type = feat_type
    )

    # default dim_reduction_name
    if (is.null(dim_reduction_name)) {
        dim_reduction_name <- if (feat_type == "rna") {
            dim_reduction_to_use
        } else {
            paste0(feat_type, ".", dim_reduction_to_use)
        }
    }

    # build Param; output = "igraph" because nnNetObj wraps an igraph
    param <- if (type == "kNN") {
        kNNNetworkParam(k = k, output = "igraph", engine = engine,
            ef = ef, n_threads_build = n_threads_build)
    } else {
        sNNNetworkParam(k = k,
            minimum_shared = minimum_shared, top_shared = top_shared,
            output = "igraph", engine = engine,
            ef = ef, n_threads_build = n_threads_build
        )
    }

    if (!is.null(dim_reduction_to_use)) {
        # PCA / dim-reduction source
        dim_obj <- getDimReduction(gobject,
            spat_unit = spat_unit, feat_type = feat_type,
            reduction = "cells", reduction_method = dim_reduction_to_use,
            name = dim_reduction_name, output = "dimObj"
        )
        provenance <- prov(dim_obj)
        nn_igraph <- createNetwork(dim_obj, param,
            dimensions_to_use = dimensions_to_use, verbose = verbose, ...
        )
    } else {
        # legacy: build NN from raw expression matrix
        expression_values <- match.arg(
            expression_values,
            unique(c("normalized", "scaled", "custom", expression_values))
        )
        expr_obj <- getExpression(gobject,
            feat_type = feat_type, spat_unit = spat_unit,
            values = expression_values, output = "exprObj"
        )
        provenance <- prov(expr_obj)
        expr_mat <- expr_obj[]
        if (!is.null(feats_to_use)) {
            expr_mat <- expr_mat[rownames(expr_mat) %in% feats_to_use, ]
        }
        matrix_to_use <- t_flex(expr_mat) # cells as rows
        nn_igraph <- createNetwork(matrix_to_use, param,
            node_ids = rownames(matrix_to_use), verbose = verbose, ...
        )
    }

    if (is.null(name)) name <- paste0(type, ".", dim_reduction_to_use)

    if (!return_gobject) {
        return(nn_igraph)
    }

    # wrap and store
    nnObj <- create_nn_net_obj(
        name = name,
        nn_type = type,
        network = nn_igraph,
        spat_unit = spat_unit,
        feat_type = feat_type,
        provenance = provenance
    )
    nn_names <- names(gobject@nn_network[[spat_unit]][[type]])
    if (name %in% nn_names) {
        vmsg(.v = verbose, name, "has already been used, will be overwritten")
    }
    gobject <- setNearestNetwork(gobject,
        x = nnObj,
        spat_unit = spat_unit, feat_type = feat_type,
        nn_type = type, name = name,
        verbose = verbose
    )
    gobject <- update_giotto_params(gobject, description = "_nn_network", toplevel = 1L)
    gobject
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

    ig_object <- getNearestNetwork(
        gobject = gobject,
        spat_unit = spat_unit,
        nn_type = nn_network_to_use,
        name = network_name, output = "igraph"
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

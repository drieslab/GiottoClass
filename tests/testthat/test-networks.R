# Ignore internal usage of deprecated accessors
lifecycle_opt <- getOption("lifecycle_verbosity")
options("lifecycle_verbosity" = "quiet")

# ignore conda
options("giotto.use_conda" = FALSE)


# load data to test
g <- GiottoData::loadGiottoMini("viz")
activeSpatUnit(g) <- "aggregate"


test_that("spatial weight matrix can be created", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    test <- createSpatialWeightMatrix(g, spat_unit = "aggregate", return_gobject = TRUE)
    mat <- getSpatialNetwork(test, spat_unit = "aggregate", name = "kNN_network")@misc$weight_matrix$spat_weights

    expect_true(inherits(mat, c("matrix", "Matrix")))
})


# Cross-implementation parity tests between the gobject wrappers
# (createSpatialNetwork / createNearestNetwork) and the canonical
# createNetwork() were removed when both paths were unified — coverage
# of the underlying behaviours is now in test_10_create_network.R.
# A small integration test per wrapper remains here to catch regressions
# in the wiring between the gobject method and createNetwork.

test_that("createSpatialNetwork(Delaunay) returns gobject with igraph-backed spatialNetworkObj", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    g2 <- createSpatialNetwork(g, method = "Delaunay", verbose = FALSE)
    sn <- getSpatialNetwork(g2, name = "Delaunay_network",
                            output = "spatialNetworkObj")
    expect_s4_class(sn, "spatialNetworkObj")
    expect_true(inherits(sn@network, "igraph"))
    expect_gt(igraph::ecount(sn@network), 0)
})

test_that("createSpatialNetwork(kNN) returns gobject with igraph-backed spatialNetworkObj", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    g2 <- createSpatialNetwork(g, method = "kNN", k = 4, verbose = FALSE)
    sn <- getSpatialNetwork(g2, name = "kNN_network",
                            output = "spatialNetworkObj")
    expect_s4_class(sn, "spatialNetworkObj")
    expect_true(inherits(sn@network, "igraph"))
    expect_gt(igraph::ecount(sn@network), 0)
})

test_that("createNearestNetwork returns gobject with igraph-backed nnNetObj", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    g2 <- createNearestNetwork(g, type = "kNN", dimensions_to_use = 1:10)
    nn <- getNearestNetwork(g2, nn_type = "kNN", name = "kNN.pca",
                            output = "nnNetObj")
    expect_s4_class(nn, "nnNetObj")
    expect_true(inherits(nn@network, "igraph"))
    expect_gt(igraph::ecount(nn@network), 0)
})


# Backend-aware auto-write on network setters. When the gobject has a
# gsource backend attached and the incoming network is in-mem (igraph),
# setNearestNetwork / setSpatialNetwork route through GiottoDisk to
# disk-back the @network slot as a parquetEdgeStore. Mirrors the
# setExpression / setPolygonInfo pattern.

test_that("setNearestNetwork auto-writes igraph to parquetEdgeStore on backed gobject", {
    skip_if_not_installed("GiottoDisk")
    rlang::local_options(lifecycle_verbosity = "quiet")

    gdir <- file.path(tempdir(), paste0("nn_autowrite_", basename(tempfile())))
    on.exit(unlink(gdir, recursive = TRUE), add = TRUE)
    mat <- matrix(rpois(20 * 50, 2), nrow = 50, ncol = 20,
                  dimnames = list(paste0("g_", 1:50),
                                  paste0("c_", 1:20)))
    gb <- createGiottoObject(expression = mat, backend = gdir)
    expect_false(is.null(gb@source))

    ig <- igraph::sample_gnm(20, 50, directed = FALSE)
    igraph::V(ig)$name <- paste0("c_", seq_len(20))
    nn <- methods::new("nnNetObj", network = ig, nn_type = "sNN",
        name = "sNN.test", spat_unit = "cell", feat_type = "rna",
        provenance = "cell")
    rlang::local_options(giotto.check_valid = FALSE)
    gb <- setNearestNetwork(gb, nn, verbose = FALSE)

    nn_back <- getNearestNetwork(gb, output = "nnNetObj",
        spat_unit = "cell", feat_type = "rna",
        nn_type = "sNN", name = "sNN.test")
    expect_s4_class(nn_back@network, "parquetEdgeStore")
})

test_that("setSpatialNetwork auto-writes igraph to parquetEdgeStore on backed gobject", {
    skip_if_not_installed("GiottoDisk")
    rlang::local_options(lifecycle_verbosity = "quiet")

    gdir <- file.path(tempdir(), paste0("sn_autowrite_", basename(tempfile())))
    on.exit(unlink(gdir, recursive = TRUE), add = TRUE)
    mat <- matrix(rpois(20 * 50, 2), nrow = 50, ncol = 20,
                  dimnames = list(paste0("g_", 1:50),
                                  paste0("c_", 1:20)))
    sl_dt <- data.table::data.table(
        cell_ID = paste0("c_", 1:20),
        sdimx = runif(20), sdimy = runif(20)
    )
    sl <- createSpatLocsObj(coordinates = sl_dt, spat_unit = "cell",
                            provenance = "cell")
    gb <- createGiottoObject(expression = mat, backend = gdir)
    gb <- setSpatialLocations(gb, sl, verbose = FALSE)

    ig <- igraph::sample_gnm(20, 50, directed = FALSE)
    igraph::V(ig)$name <- paste0("c_", seq_len(20))
    sn <- methods::new("spatialNetworkObj", network = ig,
        name = "delaunay.network", spat_unit = "cell", provenance = "cell")
    rlang::local_options(giotto.check_valid = FALSE)
    gb <- setSpatialNetwork(gb, sn, verbose = FALSE)

    sn_back <- getSpatialNetwork(gb, output = "spatialNetworkObj",
        spat_unit = "cell", name = "delaunay.network")
    expect_s4_class(sn_back@network, "parquetEdgeStore")
})

test_that("network setters plumb @type + @directed correctly to parquetEdgeStore", {
    skip_if_not_installed("GiottoDisk")
    rlang::local_options(lifecycle_verbosity = "quiet")

    gdir <- file.path(tempdir(), paste0("nn_typedir_", basename(tempfile())))
    on.exit(unlink(gdir, recursive = TRUE), add = TRUE)
    cells <- paste0("c_", 1:10)
    mat <- matrix(rpois(10 * 20, 2), nrow = 20, ncol = 10,
                  dimnames = list(paste0("g", 1:20), cells))
    sl_dt <- data.table::data.table(cell_ID = cells,
                                    sdimx = runif(10),
                                    sdimy = runif(10))
    sl <- createSpatLocsObj(coordinates = sl_dt, spat_unit = "cell",
                            provenance = "cell")
    g <- createGiottoObject(expression = mat, backend = gdir)
    g <- setSpatialLocations(g, sl, verbose = FALSE)

    # sNN — undirected
    ig_snn <- igraph::sample_gnm(10, 15, directed = FALSE)
    igraph::V(ig_snn)$name <- cells
    nn_snn <- methods::new("nnNetObj", network = ig_snn, nn_type = "sNN",
        name = "sNN.t", spat_unit = "cell", feat_type = "rna",
        provenance = "cell")
    # kNN — directed
    ig_knn <- igraph::sample_gnm(10, 15, directed = TRUE)
    igraph::V(ig_knn)$name <- cells
    nn_knn <- methods::new("nnNetObj", network = ig_knn, nn_type = "kNN",
        name = "kNN.t", spat_unit = "cell", feat_type = "rna",
        provenance = "cell")
    # spatial — undirected
    ig_sp <- igraph::sample_gnm(10, 15, directed = FALSE)
    igraph::V(ig_sp)$name <- cells
    sn <- methods::new("spatialNetworkObj", network = ig_sp,
        name = "delaunay.t", spat_unit = "cell", provenance = "cell")

    rlang::local_options(giotto.check_valid = FALSE)
    g <- setNearestNetwork(g, nn_snn, verbose = FALSE)
    g <- setNearestNetwork(g, nn_knn, verbose = FALSE)
    g <- setSpatialNetwork(g, sn, verbose = FALSE)

    snn_store <- getNearestNetwork(g, output = "nnNetObj",
        spat_unit = "cell", feat_type = "rna",
        nn_type = "sNN", name = "sNN.t")@network
    expect_equal(snn_store@type, "sNN")
    expect_false(snn_store@directed)

    knn_store <- getNearestNetwork(g, output = "nnNetObj",
        spat_unit = "cell", feat_type = "rna",
        nn_type = "kNN", name = "kNN.t")@network
    expect_equal(knn_store@type, "kNN")
    expect_true(knn_store@directed)

    sn_store <- getSpatialNetwork(g, output = "spatialNetworkObj",
        spat_unit = "cell", name = "delaunay.t")@network
    expect_equal(sn_store@type, "spatial")
    expect_false(sn_store@directed)
})

test_that("network setters leave in-mem igraphs untouched on unbacked gobject", {
    skip_if_not_installed("GiottoDisk")
    rlang::local_options(lifecycle_verbosity = "quiet")

    mat <- matrix(rpois(20 * 50, 2), nrow = 50, ncol = 20,
                  dimnames = list(paste0("g_", 1:50),
                                  paste0("c_", 1:20)))
    gb <- createGiottoObject(expression = mat)
    expect_null(gb@source)

    ig <- igraph::sample_gnm(20, 50, directed = FALSE)
    igraph::V(ig)$name <- paste0("c_", seq_len(20))
    nn <- methods::new("nnNetObj", network = ig, nn_type = "sNN",
        name = "sNN.test", spat_unit = "cell", feat_type = "rna",
        provenance = "cell")
    rlang::local_options(giotto.check_valid = FALSE)
    gb <- setNearestNetwork(gb, nn, verbose = FALSE)

    nn_back <- getNearestNetwork(gb, output = "nnNetObj",
        spat_unit = "cell", feat_type = "rna",
        nn_type = "sNN", name = "sNN.test")
    expect_s3_class(nn_back@network, "igraph")  # not promoted
})


# spatIDs delegates to dataStore-backed @network -----------------------------
# When @network is a parquetEdgeStore (GiottoDisk), the spatIDs methods on
# nnNetObj / spatialNetworkObj must delegate via dispatch instead of calling
# igraph functions directly.

test_that("spatIDs(nnNetObj) delegates to parquetEdgeStore when @network is one", {
    skip_if_not_installed("GiottoDisk")
    rlang::local_options(lifecycle_verbosity = "quiet")

    gdir <- file.path(tempdir(), paste0("spatids_", basename(tempfile())))
    on.exit(unlink(gdir, recursive = TRUE), add = TRUE)
    src <- GiottoDisk::gDirSource(gdir)

    ig <- igraph::make_graph(c(1, 2, 2, 3, 3, 4, 4, 5), directed = FALSE)
    igraph::V(ig)$name <- letters[1:5]
    igraph::E(ig)$weight <- c(0.9, 0.7, 0.5, 0.3)
    igraph::E(ig)$distance <- 1 / igraph::E(ig)$weight

    pes <- GiottoDisk::sourceWrite(src, ig, type = "sNN")
    nn <- methods::new("nnNetObj", network = pes, nn_type = "sNN",
        name = "sNN.test")

    expect_s4_class(nn@network, "parquetEdgeStore")
    expect_setequal(spatIDs(nn), letters[1:5])
})

test_that("spatIDs(spatialNetworkObj) delegates to parquetEdgeStore when @network is one", {
    skip_if_not_installed("GiottoDisk")
    rlang::local_options(lifecycle_verbosity = "quiet")

    gdir <- file.path(tempdir(), paste0("spatids_sn_", basename(tempfile())))
    on.exit(unlink(gdir, recursive = TRUE), add = TRUE)
    src <- GiottoDisk::gDirSource(gdir)

    ig <- igraph::make_graph(c(1, 2, 2, 3, 3, 4), directed = FALSE)
    igraph::V(ig)$name <- letters[1:4]
    igraph::E(ig)$weight <- c(0.9, 0.7, 0.5)
    igraph::E(ig)$distance <- 1 / igraph::E(ig)$weight

    pes <- GiottoDisk::sourceWrite(src, ig, type = "spatial")
    sn <- methods::new("spatialNetworkObj", network = pes, name = "sn.test")

    expect_s4_class(sn@network, "parquetEdgeStore")
    expect_setequal(spatIDs(sn), letters[1:4])
})


# --- the in-memory igraph path -------------------------------------------
#
# Both accessors below still read `@network` as the from/to data.table it held
# before 0.6.0. The disk-backed branches were covered (above) and stayed
# correct; the canonical in-memory path had no test at all, which is how
# `spatIDs()` came to return character(0) for every ordinary network and
# `spat_net_to_igraph()` came to fail outright -- taking Giotto's
# spatialSplitCluster() and identifyTMAcores(), its only two callers, with it.

.sn_fixture <- function(method = "Delaunay", n = 100L, seed = 7L, ...) {
    rlang::local_options(lifecycle_verbosity = "quiet",
                         .local_envir = parent.frame())
    set.seed(seed)
    locs <- data.table::data.table(
        cell_ID = sprintf("c%03d", seq_len(n)),
        sdimx = runif(n, 0, 500), sdimy = runif(n, 0, 500)
    )
    m <- matrix(rpois(6L * n, 5), nrow = 6L,
                dimnames = list(paste0("g", 1:6), locs$cell_ID))
    gg <- createGiottoObject(expression = m, spatial_locs = locs)
    gg <- createSpatialNetwork(gg, method = method, name = "n1", ...)
    getSpatialNetwork(gg, name = "n1")
}

test_that("spatIDs(spatialNetworkObj) returns the nodes of an in-memory network", {
    sn <- .sn_fixture()
    net <- sn[]
    expect_s3_class(net, "igraph")
    expect_gt(igraph::ecount(net), 0L)

    ids <- spatIDs(sn)
    expect_type(ids, "character")
    expect_equal(length(ids), igraph::vcount(net))
    expect_setequal(ids, names(igraph::V(net)))
    # every endpoint of every edge is among them
    ends <- igraph::as_data_frame(net, what = "edges")
    expect_true(all(c(ends$from, ends$to) %in% ids))
})

test_that("spat_net_to_igraph returns the stored graph, undirected and bare", {
    sn <- .sn_fixture()
    net <- sn[]

    g1 <- spat_net_to_igraph(sn)
    expect_s3_class(g1, "igraph")
    expect_false(igraph::is_directed(g1))
    expect_equal(igraph::vcount(g1), igraph::vcount(net))
    expect_equal(igraph::ecount(g1), igraph::ecount(net))
    expect_setequal(names(igraph::V(g1)), names(igraph::V(net)))

    # `attr = NULL` means no edge attributes, as it always has -- the stored
    # graph carries weight and distance, so this is not a free pass-through
    expect_true("weight" %in% igraph::edge_attr_names(net))
    expect_length(igraph::edge_attr_names(g1), 0L)

    g2 <- spat_net_to_igraph(sn, attr = c("distance", "weight"))
    expect_setequal(igraph::edge_attr_names(g2), c("distance", "weight"))
    expect_equal(igraph::E(g2)$weight, igraph::E(net)$weight)

    g3 <- spat_net_to_igraph(sn, attr = "distance")
    expect_identical(igraph::edge_attr_names(g3), "distance")
})

test_that("spat_net_to_igraph undirects a kNN network without losing edges", {
    sn <- .sn_fixture(method = "kNN", k = 4)
    net <- sn[]
    # kNN is asymmetric, so the stored graph is directed -- the case that
    # makes the "non-directed" contract more than a formality
    expect_true(igraph::is_directed(net))

    g <- spat_net_to_igraph(sn)
    expect_false(igraph::is_directed(g))
    # `mode = "each"`: reciprocal pairs stay two edges, as they did when every
    # row of the old from/to table was added to an undirected graph
    expect_equal(igraph::ecount(g), igraph::ecount(net))
    expect_equal(igraph::vcount(g), igraph::vcount(net))
})

# as.igraph ####

test_that("as.igraph returns the graph the @network slot holds", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    g2 <- createSpatialNetwork(g, method = "Delaunay", verbose = FALSE)
    sn <- getSpatialNetwork(g2, name = "Delaunay_network",
                            output = "spatialNetworkObj")
    nn <- getNearestNetwork(g, output = "nnNetObj")

    # an accessor, not a construction -- identity, not merely equality
    expect_identical(igraph::as.igraph(sn), slot(sn, "network"))
    expect_identical(igraph::as.igraph(nn), slot(nn, "network"))
})

test_that("as.igraph re-dispatches when @network is backed", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    # stands in for a GiottoDisk store: any class registering its own
    # as.igraph method. GiottoClass must not need to name the backend.
    setClass("fakeBackedNet", representation(g = "ANY"))
    on.exit(removeClass("fakeBackedNet"), add = TRUE)
    registerS3method("as.igraph", "fakeBackedNet", function(x, ...) x@g,
        envir = asNamespace("igraph"))

    ring <- igraph::make_ring(7)
    g2 <- createSpatialNetwork(g, method = "Delaunay", verbose = FALSE)
    sn <- getSpatialNetwork(g2, name = "Delaunay_network",
                            output = "spatialNetworkObj")
    slot(sn, "network") <- new("fakeBackedNet", g = ring)

    expect_identical(igraph::as.igraph(sn), ring)
})


options("lifecycle_verbosity" = lifecycle_opt)
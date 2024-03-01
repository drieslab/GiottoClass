# Ignore internal usage of deprecated accessors
lifecycle_opt <- getOption("lifecycle_verbosity")
options("lifecycle_verbosity" = "quiet")

# ignore conda
options("giotto.has_conda" = FALSE)


# load data to test
g <- GiottoData::loadGiottoMini("viz")
sn <- GiottoData::loadSubObjectMini("spatialNetworkObj")


test_that("full spatial network can be created and reverted", {
    # reduced to full
    n_edges <- nrow(sn)
    full <- convert_to_full_spatial_network(sn[])
    expect_equal(n_edges * 2, nrow(full))

    # revert from full to reduced
    reduced <- convert_to_reduced_spatial_network(full)
    expect_equal(n_edges, nrow(reduced))
})

test_that("spatial weight matrix can be created", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    test <- createSpatialWeightMatrix(g, spat_unit = "aggregate", return_gobject = TRUE)
    mat <- getSpatialNetwork(test, spat_unit = "aggregate", name = "kNN_network")@misc$weight_matrix$spat_weights

    expect_true(inherits(mat, c("matrix", "Matrix")))
})


# network generation ####

e <- GiottoData::loadSubObjectMini("exprObj")
pca <- GiottoData::loadSubObjectMini("dimObj")
sl <- GiottoData::loadSubObjectMini("spatLocsObj")

# make gobject
options("giotto.use_conda" = FALSE)
# ignore warnings & messages from no py usage + obj setup
g <- suppressMessages(suppressWarnings({
    giotto() %>%
        setGiotto(e) %>%
        setGiotto(pca) %>%
        setGiotto(sl)
}))
options("giotto.use_conda" = TRUE)

# createNetwork is current separately implemented from Giotto's
# gobject-based network generation functions. These tests currently check
# for parity with the gobject-based functions which have been battle-tested.
#
# TODO update tests after switch to the createNetwork methods for giotto
# functions

test_that("delaunay [geometry] produces expected results", {
    rlang::local_options(lifecycle_verbosity = "quiet")

    # gobject
    g2 <- createSpatialNetwork(g, delaunay_method = "delaunayn_geometry")
    del_geom <- getSpatialNetwork(g2)[]

    del_geom2 <- createNetwork(
        x = as.matrix(sl[][, .(sdimx, sdimy)]),
        type = "delaunay",
        method = "geometry",
        include_weight = TRUE,
        weight_fun = function(d) 1 / d,
        as.igraph = FALSE,
        node_ids = sl$cell_ID
    )

    expect_identical(del_geom$from, del_geom2$from)
    expect_identical(del_geom$to, del_geom2$to)
    expect_identical(del_geom$weight, del_geom2$weight)
    expect_identical(del_geom$distance, del_geom2$distance)
})

test_that("delaunay [RTriangle] produces expected results", {
    rlang::local_options(lifecycle_verbosity = "quiet")

    # gobject
    g2 <- createSpatialNetwork(g, delaunay_method = "RTriangle")
    del_rt <- getSpatialNetwork(g2)[]

    del_rt2 <- createNetwork(
        x = as.matrix(sl[][, .(sdimx, sdimy)]),
        type = "delaunay",
        method = "RTriangle",
        include_weight = TRUE,
        weight_fun = function(d) 1 / d,
        as.igraph = FALSE,
        node_ids = sl$cell_ID
    )

    expect_identical(del_rt$from, del_rt2$from)
    expect_identical(del_rt$to, del_rt2$to)
    expect_identical(del_rt$weight, del_rt2$weight)
    expect_identical(del_rt$distance, del_rt2$distance)
})

test_that("delaunay [deldir] produces expected results", {
    rlang::local_options(lifecycle_verbosity = "quiet")

    # gobject
    g2 <- createSpatialNetwork(g, delaunay_method = "deldir")
    del_dd <- getSpatialNetwork(g2)[]

    del_dd2 <- createNetwork(
        x = as.matrix(sl[][, .(sdimx, sdimy)]),
        type = "delaunay",
        method = "deldir",
        include_weight = TRUE,
        weight_fun = function(d) 1 / d,
        as.igraph = FALSE,
        node_ids = sl$cell_ID
    )

    expect_identical(del_dd$from, del_dd2$from)
    expect_identical(del_dd$to, del_dd2$to)

    # deldir original implementation generates distances from x and y values
    # that are reported by the deldir object. The new createNetwork()
    # implementation uses the spatiallocations like the other delaunay methods do.
    # A very slight difference less than 1e-4 is expected
    cutoff <- 1e-4
    expect_true(all((del_dd$weight - del_dd2$weight) < cutoff))
    expect_true(all((del_dd$distance - del_dd2$distance) < cutoff))
})

test_that("knn <spatial> [dbscan] produces expected results", {
    rlang::local_options(lifecycle_verbosity = "quiet")

    # gobject
    g2 <- createSpatialNetwork(
        g,
        method = "kNN",
        knn_method = "dbscan",
        k = 8L,
        maximum_distance_knn = NULL, # (900 by itself)
        minimum_k = 0L # no change by itself, 1488 with max_dist_knn
    )
    kNN_spat <- getSpatialNetwork(g2)[]

    kNN_spat2 <- createNetwork(
        x = as.matrix(sl[][, .(sdimx, sdimy)]),
        type = "kNN",
        method = "dbscan",
        include_weight = TRUE,
        weight_fun = function(d) 1 / d, # not the default
        as.igraph = FALSE,
        node_ids = sl$cell_ID,
        k = 8L,
        filter = TRUE,
        maximum_distance = NULL,
        minimum_k = 0L
    )

    data.table::setorder(kNN_spat, from, to)
    data.table::setorder(kNN_spat2, from, to)

    expect_identical(kNN_spat$from, kNN_spat2$from)
    expect_identical(kNN_spat$to, kNN_spat2$to)
    # expect_identical(kNN_spat$weight, kNN_spat2$weight) # expected to be different
    expect_identical(kNN_spat$distance, kNN_spat2$distance)
})

test_that("kNN <NN> [dbscan] produces expected results", {
    rlang::local_options(lifecycle_verbosity = "quiet")

    # gobject
    g2 <- createNearestNetwork(g, type = "kNN", dimensions_to_use = 1:10)
    kNN_g <- suppressMessages(getNearestNetwork(g2)[])

    # low level
    kNN_cn <- createNetwork(
        pca[][, 1:10],
        type = "kNN",
        method = "dbscan",
        node_ids = rownames(pca),
        as.igraph = TRUE,
    )

    expect_true(igraph::identical_graphs(kNN_g, kNN_cn))
})

test_that("sNN <NN> [dbscan] produces expected results", {
    rlang::local_options(lifecycle_verbosity = "quiet")

    # gobject
    g2 <- createNearestNetwork(g, type = "sNN", dimensions_to_use = 1:10)
    sNN_g <- suppressMessages(getNearestNetwork(g2)[])

    # low level
    sNN_cn <- createNetwork(
        pca[][, 1:10],
        type = "sNN",
        method = "dbscan",
        node_ids = rownames(pca),
        as.igraph = TRUE,
    )

    expect_true(igraph::identical_graphs(sNN_g, sNN_cn))
})

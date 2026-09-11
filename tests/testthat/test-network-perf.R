# Regression tests for the network-construction performance work. Every change
# under test is meant to be a no-op on results, so these compare against a
# reference rather than against a stored snapshot: the previous implementation
# inlined below, a brute-force answer, or the other backend.

# The implementation edge_distances() had before it was vectorized: one
# stats::dist() call per edge, through a 2 x d x E array. Kept here so the
# equivalence claim is checked rather than asserted.
.ref_edge_distances <- function(x, y) {
    a <- array(dim = c(nrow(y), ncol(x), 2))
    a[, , 1] <- x[y$from, ]
    a[, , 2] <- x[y$to, ]
    a <- aperm(a, perm = c(3, 2, 1))
    vapply(seq(dim(a)[3L]),
        function(i) stats::dist(a[, , i]),
        FUN.VALUE = numeric(1L)
    )
}

# Not bit-identical, and the test says so: stats::dist() and
# sqrt(rowSums(...)) accumulate in a different order, so results differ in the
# last ulp (~1e-16 absolute). That is small enough not to matter except for an
# edge sitting exactly on a maximum_distance cutoff, which is the same
# tolerance question the dbscan distances already raise.
test_that("edge_distances agrees with the per-edge implementation to ~1e-15", {
    set.seed(1)
    for (d in 2:3) {
        x <- matrix(runif(400 * d), ncol = d)
        y <- data.table::data.table(
            from = sample(400, 900, TRUE), to = sample(400, 900, TRUE)
        )
        expect_equal(
            edge_distances(x, y), .ref_edge_distances(x, y),
            tolerance = 1e-14, info = paste0(d, "D")
        )
    }
})

test_that("edge_distances handles the degenerate shapes", {
    x <- matrix(c(0, 0, 3, 4, 3, 4), ncol = 2, byrow = TRUE)

    # a single edge
    one <- data.table::data.table(from = 1L, to = 2L)
    expect_identical(edge_distances(x, one), 5)

    # duplicate points -> distance 0, not NA
    dup <- data.table::data.table(from = 2L, to = 3L)
    expect_identical(edge_distances(x, dup), 0)

    # no edges at all
    none <- data.table::data.table(from = integer(), to = integer())
    expect_identical(edge_distances(x, none), numeric(0))

    # 1D coordinates
    x1 <- matrix(c(0, 7), ncol = 1)
    expect_identical(
        edge_distances(x1, data.table::data.table(from = 1L, to = 2L)), 7
    )
})

test_that("edge_distances honours x_node_ids", {
    # this silently ignored x_node_ids and errored on any character-indexed
    # edge table, which is exactly what the argument exists for
    x <- matrix(c(0, 0, 3, 4), ncol = 2, byrow = TRUE)
    ids <- c("a", "b")
    y <- data.table::data.table(from = "a", to = "b")

    expect_identical(edge_distances(x, y, x_node_ids = ids), 5)
    expect_error(edge_distances(x, y), "indexed by node ID")

    # and it must not modify the caller's table by reference
    y2 <- data.table::data.table(from = "a", to = "b")
    invisible(edge_distances(x, y2, x_node_ids = ids))
    expect_type(y2$from, "character")
})

test_that("kNN distances no longer depend on whether a cutoff was asked for", {
    # the maximum_distance branch used to recompute distances from coordinates
    # instead of using the ones dbscan returned; the two must agree
    set.seed(4)
    x <- matrix(runif(300 * 2, 0, 100), ncol = 2)
    a <- GiottoClass:::.net_dt_knn(x, k = 5L, filter = FALSE)
    b <- GiottoClass:::.net_dt_knn(x, k = 5L, filter = FALSE,
        maximum_distance = 1e9
    )
    expect_equal(a$distance, b$distance, tolerance = 1e-12)
    expect_equal(nrow(a), nrow(b))
})

test_that("deldir and geometry produce the same Delaunay graph", {
    skip_if_not_installed("geometry")
    set.seed(9)
    x <- matrix(runif(600 * 2, 0, 100), ncol = 2)
    key <- function(dt) {
        paste0(pmin(dt$from, dt$to), "|", pmax(dt$from, dt$to))
    }
    a <- GiottoClass:::.net_dt_del_deldir(x)$delaunay_network_DT
    b <- GiottoClass:::.net_dt_del_geometry(x)$delaunay_network_DT
    expect_setequal(key(a), key(b))
    expect_equal(nrow(a), nrow(b))
})

# --- radius network --------------------------------------------------------

test_that("the radius network matches a brute-force answer", {
    set.seed(6)
    n <- 250L
    x <- matrix(runif(n * 2, 0, 100), ncol = 2)
    eps <- 12

    dt <- GiottoClass:::.net_dt_radius(x, eps = eps)

    full <- as.matrix(stats::dist(x))
    want <- which(full <= eps & upper.tri(full), arr.ind = TRUE)
    want_key <- sort(paste0(
        pmin(want[, 1], want[, 2]), "|", pmax(want[, 1], want[, 2])
    ))
    got_key <- sort(paste0(pmin(dt$from, dt$to), "|", pmax(dt$from, dt$to)))

    expect_identical(got_key, want_key)
    expect_true(all(dt$distance <= eps + 1e-9))
})

test_that("the radius network is undirected and self-loop free", {
    set.seed(7)
    x <- matrix(runif(200 * 2, 0, 100), ncol = 2)
    dt <- GiottoClass:::.net_dt_radius(x, eps = 15)
    expect_true(all(dt$from < dt$to)) # canonical, one row per pair
    expect_false(any(dt$from == dt$to))
    expect_equal(anyDuplicated(paste0(dt$from, "|", dt$to)), 0L)
})

test_that("minimum_k rescues nodes that eps leaves isolated", {
    # two tight clusters far apart, plus one outlier beyond eps of everything
    x <- rbind(
        matrix(c(0, 0, 1, 0, 0, 1, 1, 1), ncol = 2, byrow = TRUE),
        matrix(c(500, 500), ncol = 2)
    )
    bare <- GiottoClass:::.net_dt_radius(x, eps = 3)
    expect_false(5L %in% c(bare$from, bare$to)) # the outlier has no edges

    rescued <- GiottoClass:::.net_dt_radius(x, eps = 3, minimum_k = 1L)
    expect_true(5L %in% c(rescued$from, rescued$to))
    # and the within-eps edges are all still there
    expect_true(all(
        paste0(bare$from, "|", bare$to) %in%
            paste0(rescued$from, "|", rescued$to)
    ))
})

test_that("radiusNetworkParam validates and dispatches", {
    p <- radiusNetworkParam(eps = 25)
    expect_s4_class(p, "radiusNetworkParam")
    expect_identical(p@eps, 25)
    expect_identical(p@minimum_k, 0L)

    expect_error(radiusNetworkParam(eps = -1))
    expect_error(radiusNetworkParam(eps = Inf))
    expect_error(radiusNetworkParam(eps = c(1, 2)))

    expect_s4_class(networkParam("radius", eps = 10), "radiusNetworkParam")
})

test_that("createNetwork builds a radius network end to end", {
    set.seed(8)
    x <- matrix(runif(300 * 2, 0, 100), ncol = 2)
    rownames(x) <- sprintf("c%03d", seq_len(300))

    g <- createNetwork(x, radiusNetworkParam(eps = 14, output = "igraph"))
    expect_s3_class(g, "igraph")
    expect_false(igraph::is_directed(g))

    dt <- createNetwork(x, radiusNetworkParam(eps = 14, output = "data.table"))
    expect_s3_class(dt, "data.table")
    expect_true(all(c("from", "to", "weight", "distance") %in% names(dt)))
    expect_equal(igraph::ecount(g), nrow(dt))
})

test_that("a radius network has density-following degree, unlike kNN", {
    # the reason to have it: one dense cluster and one sparse, same eps
    set.seed(12)
    dense <- cbind(rnorm(200, 0, 1), rnorm(200, 0, 1))
    sparse <- cbind(rnorm(200, 100, 12), rnorm(200, 100, 12))
    x <- rbind(dense, sparse)

    dt <- GiottoClass:::.net_dt_radius(x, eps = 2)
    deg <- table(factor(c(dt$from, dt$to), levels = seq_len(400)))
    expect_gt(mean(deg[1:200]), mean(deg[201:400]))

    # kNN would give every node the same degree by construction
    knn <- GiottoClass:::.net_dt_knn(x, k = 6L)
    kdeg <- table(factor(knn$from, levels = seq_len(400)))
    expect_true(all(kdeg == 6L))
})

# --- disk-backed networks through the accessor -----------------------------
#
# On a backed project @network holds a GiottoDisk parquetEdgeStore, not an
# igraph. getSpatialNetwork() has to serve both, because everything downstream
# reads the edge table through it -- annotateSpatialNetwork(), and
# cellProximityEnrichment() on top of that. Before this, output = "networkDT"
# handed the store straight to as.data.table() and failed with "cannot coerce
# class parquetEdgeStore", so pairwise proximity analysis simply did not run on
# a backed object.

test_that("getSpatialNetwork serves a disk-backed network in every output", {
    skip_if_not_installed("GiottoDisk")
    withr::local_options(giotto.check_valid = FALSE, giotto.verbose = FALSE)

    set.seed(3)
    n <- 150L
    locs <- data.table::data.table(
        cell_ID = sprintf("c%03d", seq_len(n)),
        sdimx = runif(n, 0, 100), sdimy = runif(n, 0, 100)
    )
    m <- matrix(1, nrow = 2L, ncol = n,
                dimnames = list(c("g1", "g2"), locs$cell_ID))
    dir <- file.path(withr::local_tempdir(), "proj")
    g <- createGiottoObject(expression = m, spatial_locs = locs, backend = dir)
    g <- createSpatialNetwork(g, method = "Delaunay", name = "Delaunay_network")

    sn <- getSpatialNetwork(g, name = "Delaunay_network",
                            output = "spatialNetworkObj")[]
    expect_true(inherits(sn, "dataStore"))

    dt <- getSpatialNetwork(g, name = "Delaunay_network", output = "networkDT")
    expect_s3_class(dt, "data.table")
    # the networkDT contract is from/to, not the store's from_id/to_id
    expect_true(all(c("from", "to") %in% names(dt)))
    expect_false(any(c("from_id", "to_id") %in% names(dt)))
    expect_type(dt$from, "character")
    expect_gt(nrow(dt), 0L)

    ig <- getSpatialNetwork(g, name = "Delaunay_network", output = "igraph")
    expect_s3_class(ig, "igraph")
    expect_equal(igraph::ecount(ig), nrow(dt))
})

test_that("annotateSpatialNetwork works on a disk-backed network", {
    skip_if_not_installed("GiottoDisk")
    withr::local_options(giotto.check_valid = FALSE, giotto.verbose = FALSE)

    set.seed(4)
    n <- 150L
    locs <- data.table::data.table(
        cell_ID = sprintf("c%03d", seq_len(n)),
        sdimx = runif(n, 0, 100), sdimy = runif(n, 0, 100)
    )
    m <- matrix(1, nrow = 2L, ncol = n,
                dimnames = list(c("g1", "g2"), locs$cell_ID))
    dir <- file.path(withr::local_tempdir(), "proj")
    g <- createGiottoObject(expression = m, spatial_locs = locs, backend = dir)
    g <- addCellMetadata(g, new_metadata = data.frame(
        cell_ID = locs$cell_ID, ct = sample(c("A", "B", "C"), n, TRUE)),
        by_column = TRUE, column_cell_ID = "cell_ID")
    g <- createSpatialNetwork(g, method = "Delaunay", name = "Delaunay_network")

    ann <- annotateSpatialNetwork(g, spatial_network_name = "Delaunay_network",
                                  cluster_column = "ct")
    expect_s3_class(ann, "data.table")
    expect_true(all(c("from", "to", "from_cell_type", "to_cell_type",
                      "unified_int") %in% names(ann)))
    expect_gt(nrow(ann), 0L)
})

test_that("getNearestNetwork serves a disk-backed NN network", {
    skip_if_not_installed("GiottoDisk")
    withr::local_options(giotto.check_valid = FALSE, giotto.verbose = FALSE)

    # getSpatialNetwork() learned to read a store; getNearestNetwork() was
    # left behind, so both of its non-object outputs handed a
    # parquetEdgeStore to igraph and failed with "Must provide a graph
    # object". Both accessors now share .network_as_dt()/.network_as_igraph().
    set.seed(5)
    n <- 150L
    locs <- data.table::data.table(
        cell_ID = sprintf("c%03d", seq_len(n)),
        sdimx = runif(n, 0, 100), sdimy = runif(n, 0, 100)
    )
    m <- matrix(rpois(6 * n, 5), nrow = 6L,
                dimnames = list(paste0("g", 1:6), locs$cell_ID))
    dir <- file.path(withr::local_tempdir(), "proj")
    g <- createGiottoObject(expression = m, spatial_locs = locs, backend = dir)
    emb <- matrix(rnorm(n * 5), nrow = n, dimnames = list(locs$cell_ID, NULL))
    g <- setDimReduction(g, create_dim_obj(
        coordinates = emb, name = "pca", reduction_method = "pca",
        spat_unit = "cell", feat_type = "rna"
    ))
    g <- createNearestNetwork(g,
        dim_reduction_to_use = "pca", k = 5, name = "sNN.pca"
    )

    nn <- getNearestNetwork(g, name = "sNN.pca", output = "nnNetObj")
    expect_true(inherits(nn[], "dataStore"))

    dt <- getNearestNetwork(g, name = "sNN.pca", output = "data.table")
    expect_s3_class(dt, "data.table")
    expect_true(all(c("from", "to") %in% names(dt)))
    expect_false(any(c("from_id", "to_id") %in% names(dt)))
    expect_gt(nrow(dt), 0L)

    ig <- getNearestNetwork(g, name = "sNN.pca", output = "igraph")
    expect_s3_class(ig, "igraph")
    expect_equal(igraph::ecount(ig), nrow(dt))
})


# --- radiusNetworkParam reaching a giotto object ----------------------------
#
# radiusNetworkParam inherits NNNetworkParam, whose giotto method defaults to
# space = "expression". A radius is a distance with units, so that silently
# measured eps in PCA units instead of the tissue's. A more specific method
# flips the default; the PCA-space behaviour stays reachable explicitly.

test_that("radiusNetworkParam on a giotto defaults to spatial coordinates", {
    withr::local_options(giotto.check_valid = FALSE, giotto.verbose = FALSE)

    set.seed(3)
    n <- 200L
    locs <- data.table::data.table(
        cell_ID = sprintf("c%03d", seq_len(n)),
        sdimx = runif(n, 0, 1000), sdimy = runif(n, 0, 1000)
    )
    m <- matrix(rpois(6 * n, 5), nrow = 6L,
                dimnames = list(paste0("g", 1:6), locs$cell_ID))
    g <- createGiottoObject(expression = m, spatial_locs = locs)
    emb <- matrix(rnorm(n * 5, sd = 2), nrow = n,
                  dimnames = list(locs$cell_ID, NULL))
    g <- setDimReduction(g, create_dim_obj(
        coordinates = emb, name = "pca", reduction_method = "pca",
        spat_unit = "cell", feat_type = "rna"
    ))

    # a dedicated method exists rather than inheriting the NN one
    expect_identical(
        selectMethod("createNetwork", c("giotto", "radiusNetworkParam"))@defined[[2L]],
        "radiusNetworkParam"
    )

    spatial <- createNetwork(g,
        radiusNetworkParam(eps = 60, output = "data.table")
    )
    # every edge is within eps of the *spatial* coordinates
    expect_lte(max(spatial$distance), 60)
    ref <- as.matrix(stats::dist(as.matrix(locs[, .(sdimx, sdimy)])))
    expect_equal(nrow(spatial), sum(ref > 0 & ref <= 60) / 2L)

    # the PCA-space behaviour is still reachable, but only on request
    expr <- createNetwork(g,
        radiusNetworkParam(eps = 3, output = "data.table"),
        space = "expression"
    )
    ref_pca <- as.matrix(stats::dist(emb))
    expect_equal(nrow(expr), sum(ref_pca > 0 & ref_pca <= 3) / 2L)
})

test_that("createSpatialNetwork gives radiusNetworkParam a user path", {
    withr::local_options(giotto.check_valid = FALSE, giotto.verbose = FALSE)

    set.seed(3)
    n <- 200L
    locs <- data.table::data.table(
        cell_ID = sprintf("c%03d", seq_len(n)),
        sdimx = runif(n, 0, 1000), sdimy = runif(n, 0, 1000)
    )
    m <- matrix(rpois(6 * n, 5), nrow = 6L,
                dimnames = list(paste0("g", 1:6), locs$cell_ID))
    g <- createGiottoObject(expression = m, spatial_locs = locs)

    g2 <- createSpatialNetwork(g, method = "radius", radius = 60)
    expect_true("radius_network" %in%
        list_spatial_networks_names(g2, spat_unit = "cell"))

    sn <- getSpatialNetwork(g2, name = "radius_network")
    expect_identical(sn@method, "radius")
    expect_identical(sn@parameters$eps, 60)

    dt <- createSpatialNetwork(g, method = "radius", radius = 60,
        return_gobject = FALSE, output = "data.table")
    expect_s3_class(dt, "data.table")
    expect_equal(nrow(dt), igraph::ecount(sn[]))
    expect_lte(max(dt$distance), 60)

    # the cutoff is not optional for this method
    expect_error(createSpatialNetwork(g, method = "radius"), "radius")

    # and the two established methods are untouched
    expect_identical(
        nrow(createSpatialNetwork(g, method = "Delaunay",
            return_gobject = FALSE, output = "data.table")),
        558L
    )
    expect_identical(
        nrow(createSpatialNetwork(g, method = "kNN", k = 4,
            return_gobject = FALSE, output = "data.table")),
        800L
    )
})

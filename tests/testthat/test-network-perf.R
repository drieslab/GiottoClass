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

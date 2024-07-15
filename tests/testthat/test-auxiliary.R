
# dummy gobject
options("giotto.use_conda" = FALSE)
g <- suppressWarnings(giotto()) # hide no python warning

# dummy expression
m <- matrix(
    seq(9), ncol = 3,
    dimnames = list(
        sprintf("gene_%s", letters[seq(3)]),
        sprintf("cell_%d", seq(3))
    )
)

# createMetafeats ####
e <- exprObj(
    exprMat = m,
    spat_unit = "cell",
    provenance = "cell",
    feat_type = "test_feat",
    name = "test"
)
g <- setGiotto(g, e)

num_vec_clus <- c(1, 1, 1, 2, 2, 3)
names(num_vec_clus) <- paste0("gene_", c("a", "b", "c", "a", "c", "b"))

df_clus <- data.frame(
    clus = c(1, 1, 1, 2, 2, 3),
    feat = paste0("gene_", c("a", "b", "c", "a", "c", "b"))
)

df_clus_weight <- data.frame(
    clus = c(1, 1, 1, 2, 2, 3),
    feat = paste0("gene_", c("a", "b", "c", "a", "c", "b")),
    w = c(rep(2, 3), 0.5, 0.1, 1)
)

test_that("createMetafeat can calculate mean values", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_m <- matrix(rep(c(2, 5, 8), 3), nrow = 3)

    g <- createMetafeats(
        g, stat = "mean",
        expression_values = "test",
        feat_clusters = num_vec_clus,
        name = "chara_vec_mean",
        verbose = FALSE
    )
    enr <- getSpatialEnrichment(g, name = "chara_vec_mean",
                                output = "data.table")

    test_m_num <- as.matrix(enr[, 1:3])
    dimnames(test_m_num) <- NULL
    expect_identical(test_m_num, expect_m)

    g <- createMetafeats(
        g, stat = "mean",
        expression_values = "test",
        feat_clusters = df_clus,
        name = "df_mean",
        verbose = FALSE
    )
    enr <- getSpatialEnrichment(g, name = "df_mean", output = "data.table")

    test_m_df <- as.matrix(enr[, 1:3])
    dimnames(test_m_df) <- NULL
    expect_identical(test_m_df, expect_m)
})

test_that("createMetafeat can calculate sum values", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_m <- matrix(c(6, 15, 24, 4, 10, 16, 2, 5, 8), nrow = 3)

    g <- createMetafeats(
        g, stat = "sum",
        expression_values = "test",
        feat_clusters = num_vec_clus,
        name = "sum",
        verbose = FALSE
    )
    enr <- getSpatialEnrichment(g, name = "sum", output = "data.table")

    test_m_num <- as.matrix(enr[, 1:3])
    dimnames(test_m_num) <- NULL
    expect_identical(test_m_num, expect_m)
})

test_that("createMetafeat can calculate min values", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_m <- matrix(c(1L, 4L, 7L, 1L, 4L, 7L, 2L, 5L, 8L), nrow = 3)

    g <- createMetafeats(
        g, stat = "min",
        expression_values = "test",
        feat_clusters = num_vec_clus,
        name = "min",
        verbose = FALSE
    )
    enr <- getSpatialEnrichment(g, name = "min", output = "data.table")

    test_m_num <- as.matrix(enr[, 1:3])
    dimnames(test_m_num) <- NULL
    expect_identical(test_m_num, expect_m)
})

test_that("createMetafeat can calculate max values", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_m <- matrix(c(3L, 6L, 9L, 3L, 6L, 9L, 2L, 5L, 8L), nrow = 3)

    g <- createMetafeats(
        g, stat = "max",
        expression_values = "test",
        feat_clusters = num_vec_clus,
        name = "max",
        verbose = FALSE
    )
    enr <- getSpatialEnrichment(g, name = "max", output = "data.table")

    test_m_num <- as.matrix(enr[, 1:3])
    dimnames(test_m_num) <- NULL
    expect_identical(test_m_num, expect_m)
})

test_that("createMetafeat can use weights", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_m <- matrix(c(4, 10, 16, 0.4, 1.3, 2.2, 2, 5, 8), nrow = 3)

    g <- createMetafeats(
        g, stat = "mean",
        expression_values = "test",
        feat_clusters = df_clus_weight,
        name = "weighted_means",
        verbose = FALSE
    )
    enr <- getSpatialEnrichment(g, name = "weighted_means",
                                output = "data.table")

    test_m_num <- as.matrix(enr[, 1:3])
    dimnames(test_m_num) <- NULL
    expect_identical(test_m_num, expect_m)
})

test_that("createMetafeat can use rescale", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_m <- matrix(rep(c(0, 0.5, 1), 3), nrow = 3)

    g <- createMetafeats(
        g, stat = "mean",
        expression_values = "test",
        feat_clusters = num_vec_clus,
        rescale_to = c(0, 1),
        name = "scaled_means",
        verbose = FALSE
    )
    enr <- getSpatialEnrichment(g, name = "scaled_means",
                                output = "data.table")

    test_m_num <- as.matrix(enr[, 1:3])
    dimnames(test_m_num) <- NULL
    expect_identical(test_m_num, expect_m)
})


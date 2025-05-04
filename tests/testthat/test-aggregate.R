random_pts_names <- function(n, species = 20) {
    GiottoUtils::local_seed(1234)
    sampleset <- c(letters, LETTERS, seq(from = 0, to = 9))
    nameset <- vapply(seq_len(species), FUN = function(i) {
        paste(sample(sampleset, size = 8, replace = TRUE), collapse = "")

    }, FUN.VALUE = character(1L))
    sample(nameset, replace = TRUE, size = n, prob = runif(species))
}

random_points_gen <- function(n = 500, extent = ext(gpoly)) {
    GiottoUtils::local_seed(1234)
    evect <- as.numeric(ext(extent)[])
    count <- abs(round(rnorm(n, 0, sd = 0.8))) + 1
    data.table::data.table(
        id = random_pts_names(n),
        x = runif(n, min = evect[[1]], max = evect[[2]]),
        y = runif(n, min = evect[[3]], max = evect[[4]]),
        count = count
    )
}

g <- GiottoData::loadGiottoMini("vizgen")
gpoly <- g[["spatial_info", "aggregate"]][[1]]
gpoly@overlaps = NULL
gpoly@spatVectorCentroids <- NULL
gpts <- createGiottoPoints(random_points_gen(80000), verbose = FALSE)
imglist <- g[["images",]]
img <- imglist[[1]]

# calculateOverlap ####
## --- poly vs pts level tests

# these tests can change if the source test dataset changes

test_that("calculateOverlap works for points", {
    res_rast <- calculateOverlap(gpoly, gpts, verbose = FALSE)
    expect_identical(names(res_rast@overlaps), "rna")
    ovlp_rast <- overlaps(res_rast, "rna")
    checkmate::expect_class(ovlp_rast, "overlapInfo")
    expect_equal(nrow(ovlp_rast@data), 12383)
    expect_identical(as.numeric(ovlp_rast@data[100,]), c(385, 685, 12))
    res_vect <- calculateOverlap(gpoly, gpts,
        verbose = FALSE, method = "vector"
    )

    # larger due to double counts being possible with vector method
    ovlp_vect <- overlaps(res_vect, "rna")
    expect_equal(nrow(ovlp_vect@data), 12311)
    expect_identical(as.numeric(ovlp_vect@data[100,]), c(12, 671, 3))

    # with counts info
    res_vect_cts <- calculateOverlap(gpoly, gpts,
        feat_count_column = "count", verbose = FALSE, method = "vector"
    )
    ovlp_vect_cts <- overlaps(res_vect_cts, "rna")
    expect_identical(
        names(ovlp_vect_cts@data),
        c("poly", "feat", "feat_id_index", "count")
    )
    expect_identical(as.numeric(ovlp_vect_cts@data[100,]), c(12, 671, 3, 2))
})

test_that("calculateOverlap works for basic images", {
    res <- calculateOverlap(gpoly, img, name = "image", verbose = FALSE, )
    o <- overlaps(res, "image")
    vals <- c(514207.812, 576698.000, 593017.562)
    expect_equal(head(o@data[[2]], 3), vals)
})

test_that("calculateOverlap works for affine images", {
    res_base <- calculateOverlap(gpoly, img)
    aimg <- spin(imglist[[1]], 45, x0 = 0, y0 = 0)
    spin_gpoly <- spin(gpoly, 45, x0 = 0, y0 = 0)
    res_aff <- calculateOverlap(spin_gpoly, aimg,
        name = "affine_image", verbose = FALSE
    )
    ovlp_base <- overlaps(res_base, "dapi_z0")
    ovlp_aff <- overlaps(res_aff, "affine_image")
    # original vs affine calculates same value
    expect_equal(ovlp_base@data$mini_dataset_dapi_z0,
                 ovlp_aff@data$mini_dataset_dapi_z0)
})


# overlapToMatrix ####
## --- poly level tests

# test data



test_that("overlapToMatrix works for point overlaps", {
    res_vect <- calculateOverlap(gpoly, gpts,
        verbose = FALSE,
        method = "vector"
    )
    ovlp_vect <- overlaps(res_vect, "rna")
    expect_identical(names(ovlp_vect@data),
                     c("poly", "feat", "feat_id_index"))

    # with a counts column summation
    res_vect_cts <- calculateOverlap(gpoly, gpts,
        feat_count_column = "count",
        verbose = FALSE,
        method = "vector"
    )
    ovlp_vect_cts <- overlaps(res_vect_cts, "rna")
    expect_identical(names(ovlp_vect_cts@data),
                     c("poly", "feat", "feat_id_index", "count"))

    # matrix without counts
    mat <- as.matrix(ovlp_vect)
    mat_val <- mat["Bp5vKRUi", "104929374280991324709110264935409912418"]
    expect_equal(mat_val, 4)
    # matrix with counts
    mat <- as.matrix(ovlp_vect_cts, feat_count_column = "count")
    mat_val <- mat["Bp5vKRUi", "104929374280991324709110264935409912418"]
    expect_equal(mat_val, 6)
    # matrix with counts, calculated differently
    ovlp_val_table <- ovlp_vect_cts@data[
        poly == match("104929374280991324709110264935409912418",
                      ovlp_vect_cts@spat_ids) &
            feat_id_index == match("Bp5vKRUi", ovlp_vect_cts@feat_ids)
    ]
    ovlp_val <- sum(ovlp_val_table$count)
    expect_equal(ovlp_val, 6)
})

test_that("overlapToMatrix works for intensity overlaps", {
    res_rast <- calculateOverlap(gpoly, img, verbose = FALSE)
    ovlp_rast <- overlaps(res_rast, "dapi_z0")
    m <- as.matrix(ovlp_rast)
    expect_equal(m[1,10], 536529.94)
})


# overlap point object tests ####

res_vect <- calculateOverlap(gpoly, gpts,
    verbose = FALSE,
    method = "vector"
)
ovlp <- overlaps(res_vect, "rna")

test_that("overlap `[]` subset works", {
    # expect the feat_ID_uniq overlapped by poly 9
    fuid9 <- ovlp[9]
    expected_fuid9 <- c(
        1949, 4234, 6934, 9360, 11891, 12037, 13671, 13766, 14633, 14732,
        22670, 25184, 27910, 30742, 37026, 44881, 45922, 50053, 53519, 67618,
        68997, 69003, 69911, 70336, 72325, 74205, 76817
    )
    expect_equal(fuid9, expected_fuid9)
    # expect the feat_ID_uniq overlapped by poly 40
    fuid40 <- ovlp[40]
    expected_fuid40 <- c(
        2184, 3710, 3870, 5862, 12664, 19589, 24959, 25368, 26082, 29026,
        36516, 45774, 49727, 50094, 50514, 53664, 53834, 54290, 54365, 54534,
        55520, 59178, 66336, 66812, 70350, 73361, 75951
    )
    expect_equal(fuid40, expected_fuid40)

    # expect poly overlapping specific features
    feat_idx <- match("Bp5vKRUi", ovlp@feat_ids)
    poly_idx_by_fidx <- head(ovlp[, feat_idx])
    poly_idx_by_fname <- head(ovlp[, "Bp5vKRUi"])
    expect_identical(poly_idx_by_fidx, poly_idx_by_fname)
    expected_idx <- c(459, 215, 11, 30, 294, 301)
    expect_equal(poly_idx_by_fidx, expected_idx)
})

test_that("overlap `as.data.frame` works", {
    res <- as.data.frame(ovlp)
    expect_equal(ncol(res), 3)
    expect_equal(nrow(res), 12311)
    checkmate::expect_character(res$poly_ID)
    checkmate::expect_character(res$feat_ID)
    checkmate::expect_integer(res$feat_ID_uniq)
})

# aggregateFeatures ####
## --- gobject-level checks

test_that("aggregateFeatures works and generates exprObj", {
    g <- aggregateFeatures(g,
        spat_info = "z0",
        feat_info = "rna",
        new_spat_unit = "test_spat",
        new_feat_type = "test_feat",
        name = "test_mat",
        verbose = FALSE,
        return_gobject = TRUE
    )
    e <- g@expression$test_spat$test_feat$test_mat
    expect_s4_class(e, "exprObj")
    m <- e[]
    expect_s4_class(m, "dgCMatrix")
    expect_identical(head(colnames(m)), c(
        "1132915601442719251817312578799507532",
        "1900302715660571356090444774019116326",
        "2467888014556719520437642348850497467",
        "2582675971475731682260721390861103474",
        "3151102251621248215463444891373423271",
        "3188909098022617910378369321966273882"
    ))
    expect_identical(head(rownames(m)), c(
        "Abcc9", "Ackr1", "Ackr3", "Adcyap1r1", "Adgra1", "Adgra2"
    ))
    expect_identical(objName(e), "test_mat")
    expect_identical(spatUnit(e), "test_spat")
    expect_identical(featType(e), "test_feat")
    expect_equal(dim(e), c(337, 498))
})




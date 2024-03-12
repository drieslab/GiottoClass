require(testthat)

# Load subobjects
ex <- GiottoData::loadSubObjectMini("exprObj")
sl <- GiottoData::loadSubObjectMini("spatLocsObj")
cm <- GiottoData::loadSubObjectMini("cellMetaObj")
fm <- GiottoData::loadSubObjectMini("featMetaObj")
sn <- GiottoData::loadSubObjectMini("spatialNetworkObj")
enr <- GiottoData::loadSubObjectMini("spatEnrObj")
nn <- GiottoData::loadSubObjectMini("nnNetObj")
dr <- GiottoData::loadSubObjectMini("dimObj")
gpoly <- GiottoData::loadSubObjectMini("giottoPolygon")
gpoints <- GiottoData::loadSubObjectMini("giottoPoints")


# Ignore internal usage of deprecated accessors
lifecycle_opt <- getOption("lifecycle_verbosity")
options("lifecycle_verbosity" = "quiet")


#  TEST SUBOBJECT CREATION ####
## ------------------------------------------------------------------------ ##

# giottoPolygon ####
sv <- gpoly[]
DT <- .spatvector_to_dt(sv)
DF <- data.table::setDF(DT)
gp_IDs <- spatIDs(gpoly)

test_that("giottoPolygon is created from SpatVector", {
    gp <- createGiottoPolygonsFromDfr(sv)
    expect_no_error(validObject(gp))
    expect_s4_class(gp, "giottoPolygon")
    expect_setequal(gp_IDs, spatIDs(gp))
})

test_that("giottoPolygon is created from data.frame", {
    gp <- createGiottoPolygonsFromDfr(DT)
    expect_no_error(validObject(gp))
    expect_s4_class(gp, "giottoPolygon")
    expect_setequal(gp_IDs, spatIDs(gp))
})

test_that("giottoPolygon is created from data.table", {
    gp <- createGiottoPolygonsFromDfr(DF)
    expect_no_error(validObject(gp))
    expect_s4_class(gp, "giottoPolygon")
    expect_setequal(gp_IDs, spatIDs(gp))
})


test_that("giottoPolygon is created from maskfile", {
    # make a faux mask (DO NOT DELETE COMMENTED CODE HERE)
    # a <- circleVertices(2)
    b <- data.table::data.table(
        sdimx = c(5, 10, 20, 10, 25, 22, 6),
        sdimy = c(5, 3, 8, 10, 3, 10, 8),
        cell_ID = letters[seq(7)]
    )
    # x <- createGiottoPolygon(polyStamp(a, b))[]
    # x$idx <- rev(4:10)
    # r <- terra::rast(ncol = 100, nrow = 100)
    # ext(r) <- c(0, 30, 0, 13)
    # mask_multi <- terra::rasterize(x, r, field = "idx")
    # terra::writeRaster(mask_multi,
    #                    filename = "inst/extdata/toy_mask_multi.tif",
    #                    gdal = "COG",
    #                    overwrite = TRUE)
    # mask_single <- terra::rasterize(x, r)
    # terra::writeRaster(mask_single,
    #                    filename = "inst/extdata/toy_mask_single.tif",
    #                    gdal = "COG",
    #                    overwrite = TRUE)
    # terra::writeVector(x,
    #                    filename = "inst/extdata/toy_poly.shp",
    #                    overwrite = TRUE)

    m <- system.file("extdata/toy_mask_multi.tif", package = "GiottoClass")
    s <- system.file("extdata/toy_mask_single.tif", package = "GiottoClass")

    # expect all 7 polys
    gpm <- createGiottoPolygonsFromMask(m,
        flip_vertical = FALSE,
        flip_horizontal = FALSE,
        shift_horizontal_step = FALSE,
        shift_vertical_step = FALSE,
        ID_fmt = "id_test_%03d",
        name = "multi_test",
        verbose = FALSE
    )
    expect_equal(nrow(gpm), 7)
    gpm_centroids_dt <- data.table::as.data.table(centroids(gpm), geom = "XY")
    expect_identical(gpm_centroids_dt$poly_ID, sprintf("id_test_%03d", 4:10))
    # compare against reversed values from spatlocs DT since values were applied
    # in reverse (from idx col)
    expect_identical(round(gpm_centroids_dt$x), rev(b$sdimx))
    expect_identical(round(gpm_centroids_dt$y), rev(b$sdimy))

    # expect 5 polys
    gps <- createGiottoPolygonsFromMask(s,
        flip_vertical = FALSE,
        flip_horizontal = FALSE,
        shift_horizontal_step = FALSE,
        shift_vertical_step = FALSE,
        ID_fmt = "id_test_%03d",
        name = "single_test",
        verbose = FALSE
    )
    expect_equal(nrow(gps), 5)
    gps_centroids_dt <- data.table::as.data.table(centroids(gps), geom = "XY")
    expect_identical(gps_centroids_dt$poly_ID, sprintf("id_test_%03d", seq(1:5)))
    # ordering from readin for "single" is ordered first by row then col
    data.table::setkeyv(b, c("sdimy", "sdimx")) # note that y ordering is still inverted
    singles_x <- c(b$sdimx[6], mean(b$sdimx[c(7, 5)]), mean(b$sdimx[c(3, 4)]), b$sdimx[c(1, 2)])
    singles_y <- c(b$sdimy[6], mean(b$sdimy[c(7, 5)]), mean(b$sdimy[c(3, 4)]), b$sdimy[c(1, 2)])

    expect_identical(round(gps_centroids_dt$x, digits = 1), singles_x)
    expect_identical(round(gps_centroids_dt$y, digits = 1), singles_y)

    # try again with specified poly_ID values --------------------------------- #

    gpm2 <- createGiottoPolygonsFromMask(m,
        flip_vertical = FALSE,
        flip_horizontal = FALSE,
        shift_horizontal_step = FALSE,
        shift_vertical_step = FALSE,
        poly_IDs = letters[1:7],
        ID_fmt = "id_test_%03d", # ignored
        name = "multi_test",
        verbose = FALSE
    )
    expect_identical(gpm2$poly_ID, letters[1:7])
    gpm2_centroids_dt <- data.table::as.data.table(centroids(gpm2), geom = "XY")
    data.table::setkey(b, cell_ID)
    expect_identical(round(gpm2_centroids_dt$x), rev(b$sdimx))
    expect_identical(round(gpm2_centroids_dt$y), rev(b$sdimy))

    gps2 <- createGiottoPolygonsFromMask(s,
        flip_vertical = FALSE,
        flip_horizontal = FALSE,
        shift_horizontal_step = FALSE,
        shift_vertical_step = FALSE,
        poly_IDs = LETTERS[1:5],
        ID_fmt = "id_test_%03d", # ignored
        name = "single_test",
        verbose = FALSE
    )
    expect_identical(gps2$poly_ID, LETTERS[1:5])
    gps2_centroids_dt <- data.table::as.data.table(centroids(gps2), geom = "XY")
    data.table::setkeyv(b, c("sdimy", "sdimx")) # note that y ordering is still inverted
    singles_x <- c(b$sdimx[6], mean(b$sdimx[c(7, 5)]), mean(b$sdimx[c(3, 4)]), b$sdimx[c(1, 2)])
    singles_y <- c(b$sdimy[6], mean(b$sdimy[c(7, 5)]), mean(b$sdimy[c(3, 4)]), b$sdimy[c(1, 2)])

    expect_identical(round(gps2_centroids_dt$x, digits = 1), singles_x)
    expect_identical(round(gps2_centroids_dt$y, digits = 1), singles_y)
})



# exprObj ####

a <- as.array(ex[])
m <- as.matrix(ex[])
dgC <- ex[]
ex_IDs <- spatIDs(ex)

test_that("exprObj is created from array", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    exprObj <- create_expr_obj(exprMat = a)
    expect_no_error(validObject(exprObj))
    expect_s4_class(exprObj, "exprObj")
    expect_setequal(ex_IDs, spatIDs(exprObj))
})

test_that("exprObj is created from matrix", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    exprObj <- create_expr_obj(exprMat = m)
    expect_no_error(validObject(exprObj))
    expect_s4_class(exprObj, "exprObj")
    expect_setequal(ex_IDs, spatIDs(exprObj))
})

test_that("exprObj is created from dgCMatrix", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    exprObj <- create_expr_obj(exprMat = dgC)
    expect_no_error(validObject(exprObj))
    expect_s4_class(exprObj, "exprObj")
    expect_setequal(ex_IDs, spatIDs(exprObj))
})



















# Nearest Network ####





# SETUP
ig <- nn[]
nnDT <- nnDT_min <- data.table::setDT(igraph::as_data_frame(nn[]))
nnDT_min[, c("weight", "shared", "rank") := NULL]


## nnNetObj ####

### input evaluation ####

test_that("Eval of nnNetObj returns nnNetObj", {
    expect_identical(.evaluate_nearest_networks(nn), nn)
})






## data.table ####

### input evaluation ####

test_that("Eval with missing info throws error", {
    nnDT_min <- data.table::copy(nnDT_min)
    nnDT_min[, distance := NULL]
    expect_error(.evaluate_nearest_networks(nnDT_min), regexp = "Unable to coerce")
})

test_that("Eval with minimum info works", {
    ig_new <- expect_no_error(.evaluate_nearest_networks(nnDT_min))
    expect_s3_class(ig_new, "igraph")
    expect_true(all(c("distance", "weight") %in% igraph::edge_attr_names(ig_new)))
    expect_true("name" %in% igraph::vertex_attr_names(ig_new))
})






## igraph ####

### input evaluation ####
test_that("Eval of igraph returns igraph", {
    expect_s3_class(.evaluate_nearest_networks(ig), "igraph")
})

test_that("Eval of igraph with no distance attr fails", {
    ig_nodist <- igraph::delete_edge_attr(ig, "distance")
    expect_error(.evaluate_nearest_networks(ig_nodist), regexp = "distance")
})

test_that("Eval of igraph with no name attr fails", {
    ig_noname <- igraph::delete_vertex_attr(ig, "name")
    expect_error(.evaluate_nearest_networks(ig_noname), regexp = "name")
})

test_that("Eval of minimal igraph adds weight attr", {
    ig_min <- igraph::delete_edge_attr(ig, "weight")
    ig_min <- .evaluate_nearest_networks(ig_min)
    expect_s3_class(ig_min, "igraph")
    expect_true(all(c("distance", "weight") %in% igraph::edge_attr_names(ig_min)))
    expect_true("name" %in% igraph::vertex_attr_names(ig_min))
    expect_true("weight" %in% igraph::edge_attr_names(ig_min))
})






## list reading ####


test_that("Read returns nnNetObj list directly", {
    # this warning can't be tested properly
    read_list <- suppressWarnings(readNearestNetData(list(nn, nn)))
    expect_true(is.list(read_list))
    expect_true(all(sapply(read_list, class) == "nnNetObj"))
})


test_that("Depth 1 works", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    read_list <- readNearestNetData(list(ig, ig))
    expect_true(all(sapply(read_list, featType) == "rna"))
    expect_true(all(sapply(read_list, spatUnit) == "cell"))
    expect_identical(sapply(read_list, objName), c("nn_1", "nn_2"))
    expect_identical(sapply(read_list, function(x) x@nn_type), c("nn_1", "nn_2"))
})


test_that("Depth 2 works", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    read_list <- readNearestNetData(list(
        test_feat = list(ig, ig),
        list(test = ig)
    ))
    expect_identical(sapply(read_list, featType), c("test_feat", "test_feat", "feat_2"))
    expect_identical(sapply(read_list, spatUnit), c("cell", "cell", "cell"))
    expect_identical(sapply(read_list, objName), c("nn_1", "nn_2", "test"))
    expect_identical(sapply(read_list, function(x) x@nn_type), c("nn_1", "nn_2", "test"))
})

test_that("Depth 3 works", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    read_list <- readNearestNetData(list(
        test_unit = list(
            test_feat = list(a = ig, ig),
            list(ig)
        ),
        list(list(b = ig))
    ))
    expect_identical(sapply(read_list, spatUnit), c("test_unit", "test_unit", "test_unit", "unit_2"))
    expect_identical(sapply(read_list, featType), c("test_feat", "test_feat", "feat_2", "feat_1"))
    expect_identical(sapply(read_list, objName), c("a", "nn_2", "nn_1", "b"))
    expect_identical(sapply(read_list, function(x) x@nn_type), c("a", "nn_2", "nn_1", "b"))
})

test_that("Depth 4 works", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    read_list <- readNearestNetData(list(
        test_unit = list(
            test_feat = list(list(a = ig),
                test_meth2 = list(x = ig)
            ),
            list(test_meth = list(ig))
        ),
        list(list(list(b = ig)))
    ))
    expect_identical(sapply(read_list, spatUnit), c("test_unit", "test_unit", "test_unit", "unit_2"))
    expect_identical(sapply(read_list, featType), c("test_feat", "test_feat", "feat_2", "feat_1"))
    expect_identical(sapply(read_list, objName), c("a", "x", "nn_1", "b"))
    expect_identical(sapply(read_list, function(x) x@nn_type), c("method_1", "test_meth2", "test_meth", "method_1"))
})










# dimension reduction ####





### list reading ####
drm <- dr[]

test_that("Read returns dimObj list directly", {
    read_list <- suppressWarnings(readDimReducData(list(dr, dr)))
    expect_true(is.list(read_list))
    expect_true(all(sapply(read_list, class) == "dimObj"))
})


test_that("Depth 1 works", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    read_list <- readDimReducData(list(drm, drm))
    expect_true(all(sapply(read_list, featType) == "rna"))
    expect_true(all(sapply(read_list, spatUnit) == "cell"))
    expect_identical(sapply(read_list, objName), c("dimRed_1", "dimRed_2"))
    expect_identical(sapply(read_list, function(x) x@reduction_method), c("dimRed_1", "dimRed_2"))
})


test_that("Depth 2 works", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    read_list <- readDimReducData(list(
        test_feat = list(drm, drm),
        list(test = drm)
    ))
    expect_identical(sapply(read_list, featType), c("test_feat", "test_feat", "feat_2"))
    expect_identical(sapply(read_list, spatUnit), c("cell", "cell", "cell"))
    expect_identical(sapply(read_list, objName), c("dimRed_1", "dimRed_2", "test"))
    expect_identical(sapply(read_list, function(x) x@reduction_method), c("dimRed_1", "dimRed_2", "test"))
})

test_that("Depth 3 works", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    read_list <- readDimReducData(list(
        test_unit = list(
            test_feat = list(a = drm, drm),
            list(drm)
        ),
        list(list(b = drm))
    ))
    expect_identical(sapply(read_list, spatUnit), c("test_unit", "test_unit", "test_unit", "unit_2"))
    expect_identical(sapply(read_list, featType), c("test_feat", "test_feat", "feat_2", "feat_1"))
    expect_identical(sapply(read_list, objName), c("a", "dimRed_2", "dimRed_1", "b"))
    expect_identical(sapply(read_list, function(x) x@reduction_method), c("a", "dimRed_2", "dimRed_1", "b"))
})

test_that("Depth 4 works", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    read_list <- readDimReducData(list(
        test_unit = list(
            test_feat = list(list(a = drm),
                test_meth2 = list(x = drm)
            ),
            list(test_meth = list(drm))
        ),
        list(list(list(b = drm)))
    ))
    expect_identical(sapply(read_list, spatUnit), c("test_unit", "test_unit", "test_unit", "unit_2"))
    expect_identical(sapply(read_list, featType), c("test_feat", "test_feat", "feat_2", "feat_1"))
    expect_identical(sapply(read_list, objName), c("a", "x", "dimRed_1", "b"))
    expect_identical(sapply(read_list, function(x) x@reduction_method), c("method_1", "test_meth2", "test_meth", "method_1"))
})




# spatial enrichment ####



## list reading ####

enrDT <- enr[]

test_that("Read returns spatEnrObj list directly", {
    read_list <- suppressWarnings(readSpatEnrichData(list(enr, enr)))
    expect_true(is.list(read_list))
    expect_true(all(sapply(read_list, class) == "spatEnrObj"))
})


test_that("Depth 1 works", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    read_list <- readSpatEnrichData(list(enrDT, enrDT))
    expect_true(all(sapply(read_list, featType) == "rna"))
    expect_true(all(sapply(read_list, spatUnit) == "cell"))
    expect_identical(sapply(read_list, objName), c("enr_1", "enr_2"))
    expect_identical(sapply(read_list, function(x) x@method), c("enr_1", "enr_2"))
})


test_that("Depth 2 works", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    read_list <- readSpatEnrichData(list(
        test_feat = list(enrDT, enrDT),
        list(test = enrDT)
    ))
    expect_identical(sapply(read_list, featType), c("test_feat", "test_feat", "feat_2"))
    expect_identical(sapply(read_list, spatUnit), c("cell", "cell", "cell"))
    expect_identical(sapply(read_list, objName), c("enr_1", "enr_2", "test"))
    expect_identical(sapply(read_list, function(x) x@method), c("enr_1", "enr_2", "test"))
})

test_that("Depth 3 works", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    read_list <- readSpatEnrichData(list(
        test_unit = list(
            test_feat = list(a = enrDT, enrDT),
            list(enrDT)
        ),
        list(list(b = enrDT))
    ))
    expect_identical(sapply(read_list, spatUnit), c("test_unit", "test_unit", "test_unit", "unit_2"))
    expect_identical(sapply(read_list, featType), c("test_feat", "test_feat", "feat_2", "feat_1"))
    expect_identical(sapply(read_list, objName), c("a", "enr_2", "enr_1", "b"))
    expect_identical(sapply(read_list, function(x) x@method), c("a", "enr_2", "enr_1", "b"))
})


test_that("Depth 4 works", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    read_list <- readSpatEnrichData(list(
        test_unit = list(
            test_feat = list(list(a = enrDT),
                test_meth2 = list(x = enrDT)
            ),
            list(test_meth = list(enrDT))
        ),
        list(list(list(b = enrDT)))
    ))
    expect_identical(sapply(read_list, spatUnit), c("test_unit", "test_unit", "test_unit", "unit_2"))
    expect_identical(sapply(read_list, featType), c("test_feat", "test_feat", "feat_2", "feat_1"))
    expect_identical(sapply(read_list, objName), c("a", "x", "enr_1", "b"))
    expect_identical(sapply(read_list, function(x) x@method), c("method_1", "test_meth2", "test_meth", "method_1"))
})







# spatial network ####


## list reading ####

snDT <- sn[]

test_that("Read returns spatialNetworkObj list directly", {
    read_list <- suppressWarnings(readSpatNetData(list(sn, sn)))
    expect_true(is.list(read_list))
    expect_true(all(sapply(read_list, class) == "spatialNetworkObj"))
})


test_that("Depth 1 works", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    read_list <- readSpatNetData(list(snDT, snDT))
    expect_true(all(sapply(read_list, spatUnit) == "cell"))
    expect_identical(sapply(read_list, objName), c("sn_1", "sn_2"))
    expect_identical(sapply(read_list, function(x) x@method), c("sn_1", "sn_2"))
})


test_that("Depth 2 works", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    read_list <- readSpatNetData(list(
        test_unit = list(snDT, snDT),
        list(test = snDT)
    ))
    expect_identical(sapply(read_list, spatUnit), c("test_unit", "test_unit", "unit_2"))
    expect_identical(sapply(read_list, objName), c("sn_1", "sn_2", "test"))
    expect_identical(sapply(read_list, function(x) x@method), c("sn_1", "sn_2", "test"))
})




# spatial locations ####


## list reading ####

slDT <- sl[]

test_that("Read returns spatLocsObj list directly", {
    read_list <- suppressWarnings(readSpatLocsData(list(sl, sl)))
    expect_true(is.list(read_list))
    expect_true(all(sapply(read_list, class) == "spatLocsObj"))
})

test_that("Depth 1 works", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    read_list <- readSpatLocsData(list(slDT, slDT))
    expect_true(all(sapply(read_list, spatUnit) == "cell"))
    expect_identical(sapply(read_list, objName), c("coord_1", "coord_2"))
})


test_that("Depth 2 works", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    read_list <- readSpatLocsData(list(
        test_unit = list(slDT, slDT),
        list(test = slDT)
    ))
    expect_identical(sapply(read_list, spatUnit), c("test_unit", "test_unit", "unit_2"))
    expect_identical(sapply(read_list, objName), c("coord_1", "coord_2", "test"))
})













# expression ####


## list reading ####

exMat <- ex[]

test_that("Read returns dimObj list directly", {
    read_list <- suppressWarnings(readExprData(list(ex, ex)))
    expect_true(is.list(read_list))
    expect_true(all(sapply(read_list, class) == "exprObj"))
})


test_that("Depth 1 works", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    read_list <- readExprData(list(exMat, exMat))
    expect_true(all(sapply(read_list, featType) == "rna"))
    expect_true(all(sapply(read_list, spatUnit) == "cell"))
    expect_identical(sapply(read_list, objName), c("data_1", "data_2"))
})


test_that("Depth 2 works", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    read_list <- readExprData(list(
        test_feat = list(exMat, exMat),
        list(test = exMat)
    ))
    expect_identical(sapply(read_list, featType), c("test_feat", "test_feat", "feat_2"))
    expect_identical(sapply(read_list, spatUnit), c("cell", "cell", "cell"))
    expect_identical(sapply(read_list, objName), c("data_1", "data_2", "test"))
})

test_that("Depth 3 works", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    read_list <- readExprData(list(
        test_unit = list(
            test_feat = list(a = exMat, exMat),
            list(exMat)
        ),
        list(list(b = exMat))
    ))
    expect_identical(sapply(read_list, spatUnit), c("test_unit", "test_unit", "test_unit", "unit_2"))
    expect_identical(sapply(read_list, featType), c("test_feat", "test_feat", "feat_2", "feat_1"))
    expect_identical(sapply(read_list, objName), c("a", "data_2", "data_1", "b"))
})


# cell metadata ####

## list reading ####





# feat metadata ####

## list reading ####








options("lifecycle_verbosity" = lifecycle_opt)

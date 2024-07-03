# silence deprecated internal functions
rlang::local_options(lifecycle_verbosity = "quiet")
# Gobject can be generated without conda env, but will send warning
options("giotto.use_conda" = FALSE)

# create test object
suppressWarnings({
    giotto_object <- giotto()
})




# GETTERS ####

## missing cases ####

test_that("Not found exprObj returns error", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_error(
        getExpression(giotto_object,
            spat_unit = "none",
            feat_type = "none", values = "raw"
        )
    )
})

test_that("Not found CellMetadata returns error", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_error(
        getCellMetadata(
            giotto_object,
            spat_unit = "none",
            feat_type = "none",
            values = "raw"
        )
    )
})

test_that("Not found DimReduction returns error", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_error(
        getDimReduction(
            giotto_object,
            spat_unit = "none",
            feat_type = "none",
            name = "raw"
        )
    )
})

test_that("Not found FeatureInfo returns error", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_error(
        getFeatureInfo(giotto_object,
            feat_type = "none"
        )
    )
})

test_that("Not found FeatureMetadata returns error", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_error(
        getFeatureMetadata(giotto_object,
            spat_unit = "none",
            feat_type = "none"
        )
    )
})

test_that("Not found GiottoImage returns error", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_error(
        getGiottoImage(giotto_object,
            name = "none"
        )
    )
})

test_that("Not found Multiomics returns error", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_error(
        getMultiomics(giotto_object,
            spat_unit = "none",
            feat_type = "none",
            result_name = "use_this",
            integration_method = "wnn"
        )
    )
})

test_that("Not found NearestNetwork returns error", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_error(
        getNearestNetwork(giotto_object,
            spat_unit = "none",
            feat_type = "none",
            nn_type = "knn",
            name = "random"
        )
    )
})

test_that("Not found PolygonInfo returns error", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_error(
        getPolygonInfo(giotto_object,
            polygon_name = "none"
        )
    )
})

test_that("Not found SpatialEnrichment returns error", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_error(
        getSpatialEnrichment(giotto_object,
            spat_unit = "none",
            feat_type = "none",
            name = "metafeat"
        )
    )
})

test_that("Not found SpatialGrid returns error", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_error(
        getSpatialGrid(giotto_object,
            spat_unit = "none",
            feat_type = "none",
            name = "raw"
        )
    )
})

test_that("Not found SpatialLocations returns error", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_error(
        getSpatialLocations(giotto_object,
            spat_unit = "none",
            name = "raw"
        )
    )
})

test_that("Not found SpatialNetwork returns error", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_error(
        getSpatialNetwork(giotto_object,
            spat_unit = "none", name = "raw"
        )
    )
})

## expect information ####

### load pre-processed Giotto object
giotto_object <- GiottoData::loadGiottoMini("vizgen")

test_that("Finds exprObj", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_class(getExpression(giotto_object), "exprObj")
})

test_that("Finds CellMetadata", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_class(getCellMetadata(giotto_object), "cellMetaObj")
})

test_that("Finds DimReduction", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_class(
        getDimReduction(giotto_object,
            spat_unit = "aggregate",
            feat_type = "rna"
        ),
        "dimObj"
    )
})

test_that("Finds FeatureInfo", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_class(getFeatureInfo(giotto_object), "SpatVector")
})

test_that("Finds FeatureMetadata", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_class(getFeatureMetadata(giotto_object), "featMetaObj")
})

test_that("Finds NearestNetwork", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_class(
        getNearestNetwork(giotto_object,
            spat_unit = "aggregate",
            feat_type = "rna",
            nn_type = "sNN",
            name = "sNN.pca"
        ),
        "nnNetObj"
    )
})

test_that("Finds PolygonInfo", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_class(getPolygonInfo(giotto_object,
                                polygon_name = "z0"),
                 "SpatVector")
})

test_that("Finds SpatialEnrichment", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_class(
        getSpatialEnrichment(giotto_object,
            spat_unit = "aggregate",
            feat_type = "rna",
            name = "cluster_metagene"
        ),
        "spatEnrObj"
    )
})

test_that("Finds SpatialLocations", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_class(getSpatialLocations(giotto_object), "spatLocsObj")
})

test_that("Finds SpatialNetwork", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_class(
        getSpatialNetwork(giotto_object,
            spat_unit = "aggregate"
        ),
        "spatialNetworkObj"
    )
})

# SETTERS ####

### create empty test object
suppressWarnings({
    giotto_empty <- giotto()
})


x <- getExpression(giotto_object,
    spat_unit = "z0",
    feat_type = "rna"
)
giotto_empty <- setExpression(giotto_empty,
    spat_unit = "z0",
    feat_type = "rna",
    x = x
)

x <- getExpression(giotto_object,
    spat_unit = "z1",
    feat_type = "rna"
)
giotto_empty <- setExpression(giotto_empty,
    spat_unit = "z1",
    feat_type = "rna",
    x = x
)

x <- getExpression(giotto_object,
    spat_unit = "aggregate",
    feat_type = "rna"
)
giotto_empty <- setExpression(giotto_empty,
    spat_unit = "aggregate",
    feat_type = "rna",
    x = x
)

test_that("Sets exprObj", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_class(getExpression(giotto_empty), "exprObj")
})


x <- getCellMetadata(giotto_object)
giotto_empty <- setCellMetadata(giotto_empty,
    x = x
)

test_that("Sets CellMetadata", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_class(getCellMetadata(giotto_empty), "cellMetaObj")
})

x <- getDimReduction(giotto_object,
    spat_unit = "aggregate",
    feat_type = "rna"
)
giotto_empty <- setDimReduction(giotto_empty,
    spat_unit = "aggregate",
    feat_type = "rna",
    x = x
)

test_that("Sets DimReduction", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_class(
        getDimReduction(giotto_empty,
            spat_unit = "aggregate",
            feat_type = "rna"
        ),
        "dimObj"
    )
})

x <- getFeatureInfo(giotto_object)
giotto_empty <- setFeatureInfo(giotto_empty,
    x = createGiottoPoints(x)
)

test_that("Sets FeatureInfo", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_class(getFeatureInfo(giotto_empty), "SpatVector")
})

x <- getFeatureMetadata(giotto_object)
giotto_empty <- setFeatureMetadata(giotto_empty,
    x = x
)

test_that("Sets FeatureMetadata", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_class(getFeatureMetadata(giotto_empty), "featMetaObj")
})

x <- getNearestNetwork(giotto_object,
    spat_unit = "aggregate",
    feat_type = "rna"
)
giotto_empty <- setNearestNetwork(giotto_empty,
    spat_unit = "aggregate",
    feat_type = "rna",
    x = x
)

test_that("Sets NearestNetwork", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_class(
        getNearestNetwork(giotto_empty,
            spat_unit = "aggregate",
            feat_type = "rna"
        ),
        "nnNetObj"
    )
})

x <- getPolygonInfo(giotto_object)
x_polygon <- GiottoClass:::create_giotto_polygon_object(
    name = "z0",
    spatVector = x
)
giotto_empty <- setPolygonInfo(giotto_empty,
    x = x_polygon,
    name = "z0"
)

test_that("Sets PolygonInfo", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_class(getPolygonInfo(giotto_empty), "SpatVector")
})

x <- getSpatialLocations(giotto_object)
giotto_empty <- setSpatialLocations(giotto_empty,
    x = x
)

x <- getSpatialLocations(giotto_object,
    spat_unit = "aggregate"
)
giotto_empty <- setSpatialLocations(giotto_empty,
    spat_unit = "aggregate",
    x = x
)

test_that("Sets SpatialLocations", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_class(getSpatialLocations(giotto_empty), "spatLocsObj")
})

x <- getSpatialEnrichment(giotto_object,
    spat_unit = "aggregate",
    feat_type = "rna",
    name = "cluster_metagene"
)
giotto_empty <- setSpatialEnrichment(giotto_empty,
    spat_unit = "aggregate",
    feat_type = "rna",
    name = "cluster_metagene",
    x = x
)

test_that("Sets SpatialEnrichment", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_class(
        getSpatialEnrichment(giotto_empty,
            spat_unit = "aggregate",
            feat_type = "rna",
            name = "cluster_metagene"
        ),
        "spatEnrObj"
    )
})

x <- getSpatialNetwork(giotto_object,
    spat_unit = "aggregate"
)
giotto_empty <- setSpatialNetwork(giotto_empty,
    spat_unit = "aggregate",
    x = x
)

test_that("Sets SpatialNetwork", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_class(
        getSpatialNetwork(giotto_empty,
            spat_unit = "aggregate"
        ),
        "spatialNetworkObj"
    )
})


#### setting: expression ####


suppressWarnings(
    test <- giotto()
)

# Load subobjects
ex <- GiottoData::loadSubObjectMini("exprObj")
sl <- GiottoData::loadSubObjectMini("spatLocsObj")
cm <- GiottoData::loadSubObjectMini("cellMetaObj")
fm <- GiottoData::loadSubObjectMini("featMetaObj")
sn <- GiottoData::loadSubObjectMini("spatialNetworkObj")
enr <- GiottoData::loadSubObjectMini("spatEnrObj")
dr <- GiottoData::loadSubObjectMini("dimObj")
nn <- GiottoData::loadSubObjectMini("nnNetObj")
gpoly <- GiottoData::loadSubObjectMini("giottoPolygon")
gpoints <- GiottoData::loadSubObjectMini("giottoPoints")


ex1 <- ex2 <- ex
objName(ex1) <- "data1"
featType(ex1) <- "protein"
objName(ex2) <- "data2"
featType(ex2) <- "feat3"
spatUnit(ex2) <- "nucleus"


test_that("Single: exprObj can be set", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    test_ex <- setExpression(test, ex)

    avail_ex <- list_expression(test_ex)
    expect_s3_class(avail_ex, "data.table")

    expect_identical(avail_ex$spat_unit, "aggregate")
    expect_identical(avail_ex$feat_type, "rna")
    expect_identical(avail_ex$name, "normalized")
})


test_that("List: exprObj can be set", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    test_ex <- setExpression(test, list(ex, ex1, ex2))

    avail_ex <- list_expression(test_ex)
    expect_s3_class(avail_ex, "data.table")

    expect_identical(avail_ex$spat_unit, c("aggregate", "aggregate", "nucleus"))
    expect_identical(avail_ex$feat_type, c("rna", "protein", "feat3"))
    expect_identical(avail_ex$name, c("normalized", "data1", "data2"))
})

test_that("Non-native throws error", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    test_ex <- expect_error(setExpression(test, ex[]), regexp = "Only exprObj")
})

spatUnit(ex) <- "aggregate"
spatUnit(ex2) <- "nucleus"
test_ex <- setExpression(test, ex) # spat_unit aggregate
test_ex2 <- setExpression(test_ex, ex2) # spat_unit nucleus


## setting: cell metadata ####

test_that("Single: cellMetaObj can be set", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    test_ex <- setExpression(test, ex)
    test_ex <- setCellMetadata(test_ex, cm)

    avail_ex <- list_cell_metadata(test_ex)
    expect_s3_class(avail_ex, "data.table")

    expect_identical(avail_ex$spat_unit, spatUnit(cm))
    expect_identical(avail_ex$feat_type, "rna")
})

cm1 <- cm2 <- cm
featType(cm1) <- "protein"
spatUnit(cm2) <- "nucleus"
featType(cm2) <- "feat3"

test_that("List: cellMetaObj can be set", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    test_ex <- setExpression(test, list(ex, ex1, ex2))

    # tests
    test_ex <- setCellMetadata(test_ex, list(cm, cm1, cm2))


    avail_ex <- list_cell_metadata(test_ex)
    expect_s3_class(avail_ex, "data.table")

    expect_identical(avail_ex$spat_unit, c(
        "aggregate", "aggregate", "aggregate",
        "nucleus", "nucleus", "nucleus"
    ))
    expect_identical(avail_ex$feat_type, c(
        "rna", "protein", "feat3",
        "rna", "protein", "feat3"
    ))
})

## setting: feat metadata ####

test_that("Single: featMetaObj can be set", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    test_ex <- setExpression(test, ex)
    test_ex <- setFeatureMetadata(test_ex, fm)

    avail_ex <- list_feat_metadata(test_ex)
    expect_s3_class(avail_ex, "data.table")

    expect_identical(avail_ex$spat_unit, spatUnit(fm))
    expect_identical(avail_ex$feat_type, "rna")
})

fm1 <- fm2 <- fm
featType(fm1) <- "protein"
spatUnit(fm2) <- "nucleus"
featType(fm2) <- "feat3"

test_that("List: featMetaObj can be set", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    test_ex <- setExpression(test, list(ex, ex1, ex2))

    # tests
    test_ex <- setFeatureMetadata(test_ex, list(fm, fm1, fm2))


    avail_ex <- list_cell_metadata(test_ex)
    expect_s3_class(avail_ex, "data.table")

    expect_identical(avail_ex$spat_unit, c(
        "aggregate", "aggregate", "aggregate",
        "nucleus", "nucleus", "nucleus"
    ))
    expect_identical(avail_ex$feat_type, c(
        "rna", "protein", "feat3",
        "rna", "protein", "feat3"
    ))
})

## setting: spatial locations ####

sl1 <- sl2 <- sl
objName(sl1) <- "data1"
objName(sl2) <- "data2"
spatUnit(sl2) <- "nucleus"


test_that("Single: spatLocsObj can be set", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    test_ex <- setSpatialLocations(test_ex, sl)

    avail_ex <- list_spatial_locations(test_ex)
    expect_s3_class(avail_ex, "data.table")

    expect_identical(avail_ex$spat_unit, spatUnit(sl))
    expect_identical(avail_ex$name, objName(sl))
})

test_that("List: spatLocsObj can be set", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    # setup
    test_ex <- setExpression(test_ex, ex2)

    # tests
    test_ex <- setSpatialLocations(test_ex, list(sl, sl1, sl2))

    avail_ex <- list_spatial_locations(test_ex)
    expect_s3_class(avail_ex, "data.table")

    expect_identical(avail_ex$spat_unit, c("aggregate", "aggregate", "nucleus"))
    expect_identical(avail_ex$name, c("raw", "data1", "data2"))
})

test_that("Non-native throws error", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_error(setSpatialLocations(test_ex, sl[]), regexp = "Only spatLocsObj")
})

## setting: spatial networks ####

sn1 <- sn2 <- sn
objName(sn1) <- "data1"
objName(sn2) <- "data2"
spatUnit(sl) <- "aggregate"
spatUnit(sl2) <- "nucleus"
spatUnit(sn2) <- "nucleus"

test_that("Spatial network requires matching spatial locations", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_error(setSpatialNetwork(test_ex, sn2), regexp = "Add spatial location") # none
    test_ex <- setSpatialLocations(test_ex, sl)
    expect_error(setSpatialNetwork(test_ex, sn2), regexp = "Matching") # no match (nucleus vs aggregate)

    # test_ex2 contains info with spat_unit = 'nucleus'
    test_ex2 <- setSpatialLocations(test_ex2, sl2)
    expect_no_error(setSpatialNetwork(test_ex2, sn2)) # should now work with correct sl spat_unit
})

test_that("Single: spatialNetworkObj can be set", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    test_ex <- setSpatialLocations(test_ex, sl)
    test_ex <- setSpatialNetwork(test_ex, sn)

    avail_ex <- list_spatial_networks(test_ex)
    expect_s3_class(avail_ex, "data.table")

    expect_identical(avail_ex$spat_unit, spatUnit(sn))
    expect_identical(avail_ex$name, objName(sn))
})

test_that("List: spatialNetworkObj can be set", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    # setup
    test_ex <- setSpatialLocations(test_ex, sl)
    test_ex <- setExpression(test_ex, ex2)
    test_ex <- setSpatialLocations(test_ex, sl2)

    # tests
    test_ex <- setSpatialNetwork(test_ex, list(sn, sn1, sn2))

    avail_ex <- list_spatial_networks(test_ex)
    expect_s3_class(avail_ex, "data.table")

    expect_identical(avail_ex$spat_unit, c("aggregate", "aggregate", "nucleus"))
    expect_identical(avail_ex$name, c(objName(sn), "data1", "data2"))
})

test_that("Non-native throws error", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    test_ex <- setSpatialLocations(test_ex, sl)
    expect_error(setSpatialNetwork(test_ex, sn[]), regexp = "Only spatialNetworkObj")
})

## setting: spatial enrichment ####

enr1 <- enr2 <- enr
objName(enr1) <- "data1"
objName(enr2) <- "data2"
spatUnit(sl) <- "aggregate"
spatUnit(sl2) <- "nucleus"
spatUnit(enr2) <- "nucleus"

test_that("Spatial enrichment requires matching spatial locations", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_error(setSpatialEnrichment(test_ex, enr2), regexp = "Add spatial location") # none
    test_ex <- setSpatialLocations(test_ex, sl)
    expect_error(setSpatialEnrichment(test_ex, enr2), regexp = "Matching") # no match (nucleus vs aggregate)

    # test_ex2 contains info with spat_unit = 'nucleus'
    test_ex2 <- setSpatialLocations(test_ex2, sl2)
    expect_no_error(setSpatialEnrichment(test_ex2, enr2)) # should now work with correct sl spat_unit
})

test_that("Single: spatEnrObj can be set", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    test_ex <- setSpatialLocations(test_ex, sl)
    test_ex <- setSpatialEnrichment(test_ex, enr)

    avail_se <- list_spatial_enrichments(test_ex)
    expect_s3_class(avail_se, "data.table")

    expect_identical(avail_se$spat_unit, spatUnit(enr))
    expect_identical(avail_se$name, objName(enr))
})

test_that("List: spatEnrObj can be set", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    # setup
    test_ex <- setSpatialLocations(test_ex, sl)
    test_ex <- setExpression(test_ex, ex2)
    test_ex <- setSpatialLocations(test_ex, sl2)

    # tests
    test_ex <- setSpatialEnrichment(test_ex, list(enr, enr1, enr2))

    avail_se <- list_spatial_enrichments(test_ex)
    expect_s3_class(avail_se, "data.table")

    expect_identical(avail_se$spat_unit, c("aggregate", "aggregate", "nucleus"))
    expect_identical(avail_se$name, c(objName(enr), "data1", "data2"))
})

test_that("Non-native throws error", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    test_ex <- setSpatialLocations(test_ex, sl)
    expect_error(setSpatialEnrichment(test_ex, enr[]), regexp = "Only spatEnrObj")
})

## setting: dim reduction ####

dr1 <- dr2 <- dr
objName(dr1) <- "data1"
featType(dr1) <- "test_feat"
objName(dr2) <- "data2"
spatUnit(dr2) <- "nucleus"
featType(dr2) <- "test_feat"

test_that("Dim red requires matching expression", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_error(setDimReduction(test, dr2), regexp = "Add expression") # none
    expect_error(setDimReduction(test_ex, dr2), regexp = "Matching") # no match (nucleus vs aggregate)

    # test_ex2 contains info with spat_unit = 'nucleus'
    expect_error(setDimReduction(test_ex2, dr2), regexp = "Matching") # wrong feat
    featType(dr2) <- "feat3"
    expect_no_error(setDimReduction(test_ex2, dr2))
})

test_that("Single: dimObj can be set", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    test_ex <- setSpatialLocations(test_ex, sl)
    test_ex <- setDimReduction(test_ex, dr)

    avail_dr <- list_dim_reductions(test_ex)
    expect_s3_class(avail_dr, "data.table")

    expect_identical(avail_dr$spat_unit, spatUnit(dr))
    expect_identical(avail_dr$name, objName(dr))
})

test_that("List: dimObj can be set", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    # setup
    featType(ex2) <- "test_feat"
    test_ex <- setExpression(test_ex, ex2)
    featType(ex2) <- "rna"
    test_ex <- setExpression(test_ex, ex2)
    featType(ex) <- "test_feat"
    test_ex <- setExpression(test_ex, ex)

    # tests
    test_ex <- setDimReduction(test_ex, list(dr, dr1, dr2))

    avail_dr <- list_dim_reductions(test_ex)
    expect_s3_class(avail_dr, "data.table")

    expect_identical(avail_dr$spat_unit, c("aggregate", "aggregate", "nucleus"))
    expect_identical(avail_dr$feat_type, c("rna", "test_feat", "test_feat"))
    expect_identical(avail_dr$name, c(objName(dr), "data1", "data2"))
})

test_that("Non-native throws error", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    test_ex <- setSpatialLocations(test_ex, sl)
    expect_error(setDimReduction(test_ex, dr[]), regexp = "Only dimObj")
})

## setting: nearest networks ####

nn1 <- nn2 <- nn
objName(nn1) <- "data1"
featType(nn1) <- "test_feat"
objName(nn2) <- "data2"
spatUnit(nn2) <- "nucleus"
featType(nn2) <- "test_feat"

test_that("Nearest neighbors requires matching dimreduction", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_error(setNearestNetwork(test, nn2), regexp = "Add dimension reduction") # none
    test_ex <- setDimReduction(test_ex, dr) # no match (nucleus vs aggregate)

    # test_ex2 contains info with spat_unit = 'nucleus'
    expect_error(setNearestNetwork(test_ex, nn2), regexp = "Matching") # wrong feat
    featType(dr2) <- "test_feat"
    featType(ex2) <- "test_feat"

    test_ex <- setExpression(test_ex, ex2)
    test_ex <- setDimReduction(test_ex, dr2)
    expect_no_error(setNearestNetwork(test_ex, nn2))
})

test_that("Single: nnNetObj can be set", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    test_ex <- setSpatialLocations(test_ex, sl)
    test_ex <- setDimReduction(test_ex, dr)
    test_ex <- setNearestNetwork(test_ex, nn)

    avail_nn <- list_nearest_networks(test_ex)
    expect_s3_class(avail_nn, "data.table")

    expect_identical(avail_nn$spat_unit, spatUnit(nn))
    expect_identical(avail_nn$name, objName(nn))
})

test_that("List: nnNetObj can be set", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    # setup
    featType(ex2) <- "test_feat"
    test_ex <- setExpression(test_ex, ex2)
    featType(ex2) <- "rna"
    test_ex <- setExpression(test_ex, ex2)
    featType(ex) <- "test_feat"
    test_ex <- setExpression(test_ex, ex)

    # tests
    test_ex <- setDimReduction(test_ex, list(dr, dr1, dr2))
    test_ex <- setNearestNetwork(test_ex, list(nn, nn1, nn2))

    avail_nn <- list_nearest_networks(test_ex)
    expect_s3_class(avail_nn, "data.table")

    expect_identical(avail_nn$spat_unit, c("aggregate", "aggregate", "nucleus"))
    expect_identical(avail_nn$feat_type, c("rna", "test_feat", "test_feat"))
    expect_identical(avail_nn$name, c(objName(nn), "data1", "data2"))
})

## ------------------------------------------------------------------------ ##

test_that("Non-native throws error", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    test_ex <- setSpatialLocations(test_ex, sl)
    expect_error(setDimReduction(test_ex, nn[]), regexp = "Only dimObj")
})

## ------------------------------------------------------------------------ ##

test_that("Native feature info is set directly", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    test_fi <- expect_no_error(setFeatureInfo(test, gpoints))

    avail_fi <- list_feature_info(test_fi)
    expect_s3_class(avail_fi, "data.table")

    expect_identical(avail_fi$feat_info, "rna")
})

## ------------------------------------------------------------------------ ##

test_that("Native feature info is set with lists", { # issues currently happen with unnamed lists
    rlang::local_options(lifecycle_verbosity = "quiet")
    # assign names by list names - this now happens through read fxns only
    # test_fi = setFeatureInfo(test, x = list(rna = gpoints,
    #                                         protein = gpoints2))
    # avail_fi = list_feature_info(test_fi)
    # expect_identical(avail_fi$feat_info, c('rna', 'protein'))
    # expect_identical(test_fi@feat_info$rna@feat_type, 'rna')
    # expect_identical(test_fi@feat_info$protein@feat_type, 'protein')

    # assign names by feat_type tag
    gp_1 <- gp_2 <- gpoints
    featType(gp_1) <- "new1"
    featType(gp_2) <- "new2"
    test_fi <- setFeatureInfo(test, x = list(gp_1, gp_2))

    avail_fi <- list_feature_info(test_fi)
    expect_identical(avail_fi$feat_info, c("new1", "new2"))
    expect_identical(test_fi@feat_info$new1@feat_type, "new1")
    expect_identical(test_fi@feat_info$new2@feat_type, "new2") # passes, but the message is wrong
})

## ------------------------------------------------------------------------ ##

test_that("Native spatial info is set directly", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    test_si <- expect_no_error(setPolygonInfo(test, gpoly))

    avail_si <- list_spatial_info(test_si)
    expect_s3_class(avail_si, "data.table")

    expect_identical(avail_si$spat_info, "aggregate")
})

## ------------------------------------------------------------------------ ##

test_that("Spatlocs is also set if centroids are available", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    # spat_unit (polygon_name) not explicitly set
    test_si <- setPolygonInfo(test, gpoly, centroids_to_spatlocs = TRUE)

    avail_spatlocs <- list_spatial_locations(test_si)
    expect_s3_class(avail_spatlocs, "data.table")

    expect_identical(avail_spatlocs$spat_unit, "aggregate")
    expect_identical(avail_spatlocs$name, "raw")

    # spat_unit (polygon_name) explicitly set
    test_si <- setPolygonInfo(test, gpoly, name = "test_unit", centroids_to_spatlocs = TRUE)

    avail_spatlocs <- list_spatial_locations(test_si)
    expect_s3_class(avail_spatlocs, "data.table")

    expect_identical(avail_spatlocs$spat_unit, "test_unit")
    expect_identical(avail_spatlocs$name, "raw")
})

## ------------------------------------------------------------------------ ##

test_that("Spatlocs setting requires expression", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_error(setSpatialLocations(test, sl),
        regexp = "Add expression"
    )
})

# set expression first
test_sl <- setExpression(test, ex)
test_that("Native spatlocs is set", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    test_sl <- suppressWarnings(setSpatialLocations(test_sl, sl))

    avail_sl <- list_spatial_locations(test_sl)
    expect_s3_class(avail_sl, "data.table") # should now exist

    expect_identical(avail_sl$spat_unit, "aggregate")
    expect_identical(avail_sl$name, "raw")
})


test_that("Native spatLocsObj is set with user specified nesting", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    # add needed spat_unit in expression first
    test_sl <- setExpression(test_sl, ex, spat_unit = "new")

    # whether this will work with just spatial_info already tested if
    # test that centroids are set as spatlocs already passed

    test_sl <- suppressWarnings(setSpatialLocations(test_sl, sl, spat_unit = "new"))
    test_sl <- suppressWarnings(setSpatialLocations(test_sl, sl, name = "new"))

    avail_sl <- list_spatial_locations(test_sl)
    expect_equal(nrow(avail_sl), 2L)

    set_sl_a <- getSpatialLocations(test_sl, spat_unit = "new")
    set_sl_b <- getSpatialLocations(test_sl, name = "new")
})


test_that("Spatlocs missing spat_unit in expr and spatial_info throws error", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    # available spat unit in expression is only 'aggregate'
    test_sl <- expect_error(setSpatialLocations(test_sl, sl, spat_unit = "new"),
        regexp = "No expression"
    )
})


test_that("Spatlocs spatID mismatch throws error", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    test_sl <- setPolygonInfo(test_sl, gpoly, name = "new")
    # due to subset, expected that sl will have fewer IDs
    expect_error(setSpatialLocations(test_sl, sl[1:6], spat_unit = "new"),
        regexp = "between spatial and"
    )
})

test_that("Native spatLocsObj can be removed", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    test_sl <- setSpatialLocations(test_sl, sl)

    test_sl <- setSpatialLocations(test_sl,
        x = NULL,
        spat_unit = "aggregate",
        name = "raw"
    )

    expect_null(test_sl@spatial_locs)
})

rm(test_sl)




# ADD ####

test_that("addCellMetadata() - DT with IDs", {
    rlang::local_options(lifecycle_verbosity = "quiet")

    ids <- spatIDs(giotto_object)
    # get starting order
    original_order <- spatIDs(getCellMetadata(giotto_object))

    dt_id <- data.table::data.table(
        "id" = ids, # col to match IDs on (but alt. naming)
        "id_check" = ids, # use this col to check if ID ordering has changed
        "random" = rnorm(length(ids)) # dummy data
    )
    dt_cell_id <- data.table::data.table(
        "cell_ID" = ids, # col to match IDs on (default naming)
        "random2" = rnorm(length(ids)) # dummy data 2
    )

    # scramble ordering of metadata to add
    dt_id <- dt_id[sample(1:.N)]
    dt_cell_id <- dt_cell_id[sample(1:.N)]

    am_giotto <- addCellMetadata(
        giotto_object,
        new_metadata = dt_id,
        column_cell_ID = "id",
        by_column = TRUE
    )

    # guess cell_ID as ID col to match
    am_giotto <- addCellMetadata(
        am_giotto,
        new_metadata = dt_cell_id,
        by_column = TRUE
    )

    res <- pDataDT(am_giotto)

    # check both sets of metadata were added
    expect_true(all(c("random", "random2") %in% colnames(res)))
    # expect that the order of old and new metadata were different
    expect_false(identical(res$id_check, dt_id$id_check))
    # check that the addition matched ordering of IDs
    expect_identical(res$cell_ID, res$id_check)
    # simple checks that added meta cols look right
    expect_numeric(res$random)
    expect_numeric(res$random2)
    # check that start meta order is the same as end
    expect_identical(original_order, res$cell_ID)
})

test_that("addCellMetadata() - DT without IDs", {
    rlang::local_options(lifecycle_verbosity = "quiet")

    ids <- spatIDs(giotto_object)
    # get starting order
    original_order <- spatIDs(getCellMetadata(giotto_object))

    dt_no_id <- data.table::data.table(
        "random" = rnorm(length(ids)),
        "chars" = sample(LETTERS, size = length(ids), replace = TRUE)
    )

    am_giotto <- addCellMetadata(
        giotto_object,
        new_metadata = dt_no_id
    )

    res <- pDataDT(am_giotto)

    # expect meta is appended as-is
    expect_true(all(c("random", "chars") %in% colnames(res)))
    expect_identical(res$random, dt_no_id$random) # values are matched
    expect_identical(res$chars, dt_no_id$chars) # values are matched
    # check that start meta order is the same as end
    expect_identical(original_order, res$cell_ID)
})

test_that("addCellMetadata() - vector", {
    rlang::local_options(lifecycle_verbosity = "quiet")

    ids <- spatIDs(giotto_object)
    # get starting order
    original_order <- spatIDs(getCellMetadata(giotto_object))

    chars <- sample(LETTERS, size = length(ids), replace = TRUE)

    am_giotto <- addCellMetadata(
        giotto_object,
        new_metadata = chars
    )

    res <- pDataDT(am_giotto)

    # expect meta vector is appended as-is when no names provided
    expect_true(all(c("chars") %in% colnames(res)))
    expect_identical(res$chars, chars) # values are matched
    expect_vector(res$chars)
    # check that start meta order is the same as end
    expect_identical(original_order, res$cell_ID)


    chars2 <- chars
    names(chars2) <- ids
    chars2 <- sample(chars2) # scramble values

    am_giotto <- addCellMetadata(
        am_giotto,
        new_metadata = chars2,
        by_column = TRUE
    )

    res <- pDataDT(am_giotto)

    # expect meta vector has been added via merge
    expect_true(all(c("chars", "chars2") %in% colnames(res)))
    # values are appended with merge so that they are the same even after factor2
    # was scrambled.
    expect_identical(res$chars, res$chars2)
    expect_vector(res$chars2) # values retain type
    # check that start meta order is the same as end
    expect_identical(original_order, res$cell_ID)
})

test_that("addCellMetadata() - factor", {
    rlang::local_options(lifecycle_verbosity = "quiet")

    ids <- spatIDs(giotto_object)
    # get starting order
    original_order <- spatIDs(getCellMetadata(giotto_object))

    factors <- factor(sample(LETTERS, size = length(ids), replace = TRUE))

    am_giotto <- addCellMetadata(
        giotto_object,
        new_metadata = factors
    )

    res <- pDataDT(am_giotto)

    # expect meta vector is appended as-is
    expect_true(all(c("factors") %in% colnames(res)))
    expect_identical(res$factors, factors) # values are matched
    expect_factor(res$factors)
    # check that start meta order is the same as end
    expect_identical(original_order, res$cell_ID)


    factors2 <- factors
    names(factors2) <- ids
    factors2 <- sample(factors2) # scramble values

    am_giotto <- addCellMetadata(
        am_giotto,
        new_metadata = factors2,
        by_column = TRUE
    )

    res <- pDataDT(am_giotto)

    # expect meta vector has been added via merge
    expect_true(all(c("factors", "factors2") %in% colnames(res)))
    # values are appended with merge so that they are the same even after factor2
    # was scrambled.
    expect_identical(res$factors, res$factors2)
    expect_factor(res$factors2) # values retain type
    # check that start meta order is the same as end
    expect_identical(original_order, res$cell_ID)
})



test_that("addFeatMetadata() - DT with IDs", {
    rlang::local_options(lifecycle_verbosity = "quiet")

    ids <- featIDs(giotto_object)
    # get starting order
    original_order <- featIDs(getFeatureMetadata(giotto_object))

    dt_id <- data.table::data.table(
        "id" = ids, # col to match IDs on (but alt. naming)
        "id_check" = ids, # use this col to check if ID ordering has changed
        "random" = rnorm(length(ids)) # dummy data
    )
    dt_feat_id <- data.table::data.table(
        "feat_ID" = ids, # col to match IDs on (default naming)
        "random2" = rnorm(length(ids)) # dummy data 2
    )

    # scramble ordering of metadata to add
    dt_id <- dt_id[sample(1:.N)]
    dt_feat_id <- dt_feat_id[sample(1:.N)]

    am_giotto <- addFeatMetadata(
        giotto_object,
        new_metadata = dt_id,
        column_feat_ID = "id",
        by_column = TRUE
    )

    # guess feat_ID as ID col to match
    am_giotto <- addFeatMetadata(
        am_giotto,
        new_metadata = dt_feat_id,
        by_column = TRUE
    )

    res <- fDataDT(am_giotto)

    # check both sets of metadata were added
    expect_true(all(c("random", "random2") %in% colnames(res)))
    # expect that the order of old and new metadata were different
    expect_false(identical(res$id_check, dt_id$id_check))
    # check that the addition matched ordering of IDs
    expect_identical(res$feat_ID, res$id_check)
    # simple checks that added meta cols look right
    expect_numeric(res$random)
    expect_numeric(res$random2)
    # check that start meta order is the same as end
    expect_identical(original_order, res$feat_ID)
})

test_that("addFeatMetadata() - DT without IDs", {
    rlang::local_options(lifecycle_verbosity = "quiet")

    ids <- featIDs(giotto_object)
    # get starting order
    original_order <- featIDs(getFeatureMetadata(giotto_object))

    dt_no_id <- data.table::data.table(
        "random" = rnorm(length(ids)),
        "chars" = sample(LETTERS, size = length(ids), replace = TRUE)
    )

    am_giotto <- addFeatMetadata(
        giotto_object,
        new_metadata = dt_no_id
    )

    res <- fDataDT(am_giotto)

    # expect meta is appended as-is
    expect_true(all(c("random", "chars") %in% colnames(res)))
    expect_identical(res$random, dt_no_id$random) # values are matched
    expect_identical(res$chars, dt_no_id$chars) # values are matched
    # check that start meta order is the same as end
    expect_identical(original_order, res$feat_ID)
})

test_that("addFeatMetadata() - vector", {
    rlang::local_options(lifecycle_verbosity = "quiet")

    ids <- featIDs(giotto_object)
    # get starting order
    original_order <- featIDs(getFeatureMetadata(giotto_object))

    chars <- sample(LETTERS, size = length(ids), replace = TRUE)

    am_giotto <- addFeatMetadata(
        giotto_object,
        new_metadata = chars
    )

    res <- fDataDT(am_giotto)

    # expect meta vector is appended as-is
    expect_true(all(c("chars") %in% colnames(res)))
    expect_identical(res$chars, chars) # values are matched
    expect_vector(res$chars)
    # check that start meta order is the same as end
    expect_identical(original_order, res$feat_ID)


    chars2 <- chars
    names(chars2) <- ids
    chars2 <- sample(chars2) # scramble values

    am_giotto <- addFeatMetadata(
        am_giotto,
        new_metadata = chars2,
        by_column = TRUE
    )

    res <- fDataDT(am_giotto)

    # expect meta vector has been added via merge
    expect_true(all(c("chars", "chars2") %in% colnames(res)))
    # values are appended with merge so that they are the same even after factor2
    # was scrambled.
    expect_identical(res$chars, res$chars2)
    expect_vector(res$chars2) # values retain type
    # check that start meta order is the same as end
    expect_identical(original_order, res$feat_ID)
})

test_that("addFeatMetadata() - factor", {
    rlang::local_options(lifecycle_verbosity = "quiet")

    ids <- featIDs(giotto_object)
    # get starting order
    original_order <- featIDs(getFeatureMetadata(giotto_object))

    factors <- factor(sample(LETTERS, size = length(ids), replace = TRUE))

    am_giotto <- addFeatMetadata(
        giotto_object,
        new_metadata = factors
    )

    res <- fDataDT(am_giotto)

    # expect meta vector is appended as-is
    expect_true(all(c("factors") %in% colnames(res)))
    expect_identical(res$factors, factors) # values are appended with no change
    expect_factor(res$factors) # values retain type
    # check that start meta order is the same as end
    expect_identical(original_order, res$feat_ID)


    factors2 <- factors
    names(factors2) <- ids
    factors2 <- sample(factors2) # scramble values

    am_giotto <- addFeatMetadata(
        am_giotto,
        new_metadata = factors2,
        by_column = TRUE
    )

    res <- fDataDT(am_giotto)

    # expect meta vector has been added via merge
    expect_true(all(c("factors", "factors2") %in% colnames(res)))
    # values are appended with merge so that they are the same even after factor2
    # was scrambled.
    expect_identical(res$factors, res$factors2)
    expect_factor(res$factors2) # values retain type
    # check that start meta order is the same as end
    expect_identical(original_order, res$feat_ID)
})

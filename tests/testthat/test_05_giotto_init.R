describe("Giotto Object Setup With Aggregate Data", {

    test <- giotto()
    ex <- test_data$ex

    it("Expression initiates ID slots", {
        rlang::local_options(lifecycle_verbosity = "quiet")
        test_ex <- setExpression(test, ex, verbose = FALSE)

        expect_identical(spatIDs(test_ex), spatIDs(ex))
        expect_identical(featIDs(test_ex), featIDs(ex))

        expect_true(inherits(test_ex@cell_ID, "list"))
        expect_true(inherits(test_ex@cell_ID$aggregate, "character"))
        expect_true(inherits(test_ex@feat_ID, "list"))
        expect_true(inherits(test_ex@feat_ID$rna, "character"))
    })



    it("Expression initiates metadata slots", {
        rlang::local_options(lifecycle_verbosity = "quiet")
        test_ex <- setExpression(test, ex, verbose = FALSE)

        expect_identical(spatIDs(test_ex), pDataDT(test_ex)$cell_ID)
        expect_identical(featIDs(test_ex), fDataDT(test_ex)$feat_ID)

        expect_true(inherits(test_ex@cell_metadata$aggregate$rna, "cellMetaObj"))
        expect_true(inherits(test_ex@feat_metadata$aggregate$rna, "featMetaObj"))

        expect_true(inherits(test_ex@cell_metadata$aggregate$rna[], "data.table"))
        expect_true(inherits(test_ex@feat_metadata$aggregate$rna[], "data.table"))
    })


    it("Expression sets active spat_unit and feat_type", {
        rlang::local_options(lifecycle_verbosity = "quiet")
        test_ex <- setExpression(test, ex, verbose = FALSE)

        # check in instructions settings
        expect_identical(activeSpatUnit(test_ex), spatUnit(ex))
        expect_identical(activeFeatType(test_ex), featType(ex))

        # check output from default setting
        expect_identical(set_default_spat_unit(test_ex), spatUnit(ex))
        expect_identical(set_default_feat_type(test_ex), featType(ex))
    })


    it("expression_feats slot is set by expression", {
        rlang::local_options(lifecycle_verbosity = "quiet")
        # test single
        test_ex <- setExpression(test, ex, feat_type = "test_feat", verbose = FALSE)
        expect_identical(test_ex@expression_feat, "test_feat")
        # test multiple
        test_ex <- setExpression(test_ex, ex, verbose = FALSE) # default 'rna'
        expect_identical(test_ex@expression_feat, c("rna", "test_feat"))
    })
})

describe("Giotto Object Setup With Subcellular Data", {

    test <- giotto()
    gpoly <- test_data$gpoly
    gpoints <- test_data$gpoints

    it("Spatial info initiates spat_ID slot", {
        test_si <- setPolygonInfo(test, gpoly, verbose = FALSE)

        expect_identical(spatIDs(test_si), spatIDs(gpoly))

        expect_true(inherits(test_si@cell_ID, "list"))
        expect_true(inherits(test_si@cell_ID$aggregate, "character"))
    })

    it("Spatial info sets active spat_unit", {
        test_si <- setPolygonInfo(test, gpoly, verbose = FALSE)

        expect_identical(activeSpatUnit(test_si), "aggregate")
        expect_identical(set_default_spat_unit(test_si), "aggregate")
    })


    it("Feature info initiates feat_ID slot", {
        featType(gpoints) <- "test_feat"
        test_fi <- setFeatureInfo(test, gpoints, verbose = FALSE)

        expect_identical(activeFeatType(test_fi), "test_feat")
        expect_identical(set_default_feat_type(test_fi), "test_feat")
    })


    it("Spat and Feat info initiates cell_metadata slot", {
        test_sf <- setFeatureInfo(test, gpoints, verbose = FALSE)

        expect_null(list_cell_metadata(test_sf))
        expect_null(list_feat_metadata(test_sf))

        test_sf <- setPolygonInfo(test_sf, gpoly, verbose = FALSE)

        expect_s3_class(list_cell_metadata(test_sf), "data.table")
        expect_s3_class(list_feat_metadata(test_sf), "data.table")
    })


    it("expression_feats slot is set by feature_info", {
        # test single
        test_fi <- setFeatureInfo(test, gpoints, feat_type = "test_feat", verbose = FALSE)
        expect_identical(test_fi@expression_feat, "test_feat")
        # test multiple
        test_fi <- setFeatureInfo(test_fi, gpoints, verbose = FALSE) # default 'rna'
        expect_identical(test_fi@expression_feat, c("rna", "test_feat"))
    })

})

describe("Gobject ID interaction/edge cases", {

    test <- giotto()
    ex <- test_data$ex
    gpoly <- test_data$gpoly
    gpoints <- test_data$gpoints

    it("cell_ID from spatial_info is overwritten by expression", {
        expected_IDs <- spatIDs(ex)

        test_int <- setPolygonInfo(test, gpoly, verbose = FALSE)
        expect_false(identical(spatIDs(test_int), expected_IDs))

        test_int <- setExpression(test, ex, verbose = FALSE)
        expect_identical(spatIDs(test_int), expected_IDs)
    })


    it("feat_ID from feat_info is overwritten by expression", {
        expected_IDs <- featIDs(ex)

        test_int <- setFeatureInfo(test, gpoints, verbose = FALSE)
        test_IDs <- expect_warning(featIDs(test_int)) # no spat_unit info
        expect_false(identical(test_IDs, expected_IDs))

        test_int <- setExpression(test, ex, verbose = FALSE)
        expect_identical(featIDs(test_int), expected_IDs)
    })
})

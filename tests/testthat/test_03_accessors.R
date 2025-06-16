
# GETTERS ####

describe("Error handling: Gobject Getting Non-Existent Data", {

    g <- giotto()

    it("Not found exprObj returns error", {
        expect_error(
            getExpression(g,
                spat_unit = "none",
                feat_type = "none",
                values = "raw"
            )
        )
    })

    it("Not found CellMetadata returns error", {
        expect_error(
            getCellMetadata(g,
                spat_unit = "none",
                feat_type = "none"
            )
        )
    })

    it("Not found DimReduction returns error", {
        expect_error(
            getDimReduction(g,
                spat_unit = "none",
                feat_type = "none",
                name = "raw"
            )
        )
    })

    it("Not found FeatureInfo returns error", {
        expect_error(
            getFeatureInfo(g, feat_type = "none")
        )
    })

    it("Not found FeatureMetadata returns error", {
        expect_error(
            getFeatureMetadata(g,
                spat_unit = "none",
                feat_type = "none"
            )
        )
    })

    it("Not found GiottoImage returns error", {
        expect_error(
            getGiottoImage(g, name = "none")
        )
    })

    it("Not found Multiomics returns error", {
        expect_error(
            getMultiomics(g,
                spat_unit = "none",
                feat_type = "none",
                result_name = "use_this",
                integration_method = "wnn"
            )
        )
    })

    it("Not found NearestNetwork returns error", {
        expect_error(
            getNearestNetwork(g,
                spat_unit = "none",
                feat_type = "none",
                nn_type = "knn",
                name = "random"
            )
        )
    })

    it("Not found PolygonInfo returns error", {
        expect_error(
            getPolygonInfo(g, polygon_name = "none")
        )
    })

    it("Not found SpatialEnrichment returns error", {
        expect_error(
            getSpatialEnrichment(g,
                spat_unit = "none",
                feat_type = "none",
                name = "metafeat"
            )
        )
    })

    it("Not found SpatialGrid returns error", {
        expect_error(
            getSpatialGrid(g,
                spat_unit = "none",
                feat_type = "none",
                name = "raw"
            )
        )
    })

    it("Not found SpatialLocations returns error", {
        expect_error(
            getSpatialLocations(g,
                spat_unit = "none",
                name = "raw"
            )
        )
    })

    it("Not found SpatialNetwork returns error", {
        expect_error(
            getSpatialNetwork(g,
                spat_unit = "none",
                name = "raw"
            )
        )
    })
})

describe("Giotto Object Getters Work", {

    viz <- test_data$viz

    it("Finds exprObj", {
        checkmate::expect_class(getExpression(viz), "exprObj")
    })

    it("Finds CellMetadata", {
        checkmate::expect_class(getCellMetadata(viz), "cellMetaObj")
    })

    it("Finds DimReduction", {
        checkmate::expect_class(
            getDimReduction(viz,
                spat_unit = "aggregate",
                feat_type = "rna"
            ),
            "dimObj"
        )
    })

    it("Finds FeatureInfo", {
        checkmate::expect_class(getFeatureInfo(viz), "SpatVector")
    })

    it("Finds FeatureMetadata", {
        checkmate::expect_class(getFeatureMetadata(viz), "featMetaObj")
    })

    it("Finds NearestNetwork", {
        checkmate::expect_class(
            getNearestNetwork(viz,
                spat_unit = "aggregate",
                feat_type = "rna",
                nn_type = "sNN",
                name = "sNN.pca"
            ),
            "nnNetObj"
        )
    })

    it("Finds PolygonInfo", {
        checkmate::expect_class(
            getPolygonInfo(viz,
                polygon_name = "z0"
            ),
            "SpatVector"
        )
    })

    it("Finds SpatialEnrichment", {
        checkmate::expect_class(
            getSpatialEnrichment(viz,
                spat_unit = "aggregate",
                feat_type = "rna",
                name = "cluster_metagene"
            ),
            "spatEnrObj"
        )
    })

    it("Finds SpatialLocations", {
        checkmate::expect_class(getSpatialLocations(viz), "spatLocsObj")
    })

    it("Finds SpatialNetwork", {
        checkmate::expect_class(
            getSpatialNetwork(viz, spat_unit = "aggregate"),
            "spatialNetworkObj"
        )
    })

})

# SETTERS ####

describe("Giotto Object Setters Basic Functionalities", {

    viz <- test_data$viz # data source
    g <- giotto()

    # EXPRESSION

    g <- setExpression(g,
        spat_unit = "z0",
        feat_type = "rna",
        verbose = FALSE,
        x = getExpression(viz,
            spat_unit = "z0",
            feat_type = "rna"
        )
    )

    g <- setExpression(g,
        spat_unit = "z1",
        feat_type = "rna",
        verbose = FALSE,
        x = getExpression(viz,
            spat_unit = "z1",
            feat_type = "rna"
        )
    )

    g <- setExpression(g,
        spat_unit = "aggregate",
        feat_type = "rna",
        verbose = FALSE,
        x = getExpression(viz,
            spat_unit = "aggregate",
            feat_type = "rna"
        )
    )

    it("Sets exprObj", {
        checkmate::expect_class(getExpression(g, spat_unit = "z0"), "exprObj")
        checkmate::expect_class(getExpression(g, spat_unit = "z1"), "exprObj")
        checkmate::expect_class(
            getExpression(g, spat_unit = "aggregate"), "exprObj")
    })

    # CELLMETA

    g <- setCellMetadata(g, getCellMetadata(viz), verbose = FALSE)

    it("Sets CellMetadata", {
        checkmate::expect_class(getCellMetadata(g), "cellMetaObj")
    })

    # DIMRED

    g <- setDimReduction(g,
        spat_unit = "aggregate",
        feat_type = "rna",
        verbose = FALSE,
        x = getDimReduction(viz,
            spat_unit = "aggregate",
            feat_type = "rna"
        )
    )

    it("Sets DimReduction", {
        checkmate::expect_class(getDimReduction(g,
            spat_unit = "aggregate",
            feat_type = "rna"
        ),
        "dimObj")
    })

    # FEATINFO

    g <- setFeatureInfo(g, createGiottoPoints(getFeatureInfo(viz)),
                        verbose = FALSE)

    it("Sets FeatureInfo", {
        checkmate::expect_class(getFeatureInfo(g), "SpatVector")
        checkmate::expect_class(getFeatureInfo(g, return_giottoPoints = TRUE),
                                "giottoPoints")
    })

    # FEATMETA

    g <- setFeatureMetadata(g, getFeatureMetadata(viz), verbose = FALSE)

    it("Sets FeatureMetadata", {
        checkmate::expect_class(getFeatureMetadata(g), "featMetaObj")
    })

    # NNNET

    g <- setNearestNetwork(g,
        spat_unit = "aggregate",
        feat_type = "rna",
        verbose = FALSE,
        x = getNearestNetwork(viz,
            spat_unit = "aggregate",
            feat_type = "rna",
            nn_type = "sNN",
            name = "sNN.pca"
        )
    )

    it("Sets NearestNetwork", {
        checkmate::expect_class(
            getNearestNetwork(g,
                spat_unit = "aggregate",
                feat_type = "rna",
                nn_type = "sNN",
                name = "sNN.pca"
            ),
            "nnNetObj"
        )
    })

    # SPATINFO

    g <- setPolygonInfo(g,
        x = getPolygonInfo(viz, polygon_name = "z0",
            return_giottoPolygon = TRUE,
            verbose = FALSE
        )
    )

    it("Sets PolygonInfo", {
        checkmate::expect_class(getPolygonInfo(g), "SpatVector")
    })

    # SPATLOC

    g <- setSpatialLocations(g, verbose = FALSE,
        getSpatialLocations(viz, spat_unit = "z0"))

    g <- setSpatialLocations(g, verbose = FALSE,
        getSpatialLocations(viz, spat_unit = "z1"))

    g <- setSpatialLocations(g, verbose = FALSE,
        getSpatialLocations(viz, spat_unit = "aggregate"))

    it("Sets SpatialLocations", {
        checkmate::expect_class(getSpatialLocations(g, spat_unit = "aggregate"),
                     "spatLocsObj")
        checkmate::expect_class(getSpatialLocations(g, spat_unit = "z0"),
                     "spatLocsObj")
        checkmate::expect_class(getSpatialLocations(g, spat_unit = "z1"),
                     "spatLocsObj")
    })

    # SPATENR

    g <- setSpatialEnrichment(g,
        spat_unit = "aggregate",
        feat_type = "rna",
        name = "cluster_metagene",
        verbose = FALSE,
        getSpatialEnrichment(viz,
            spat_unit = "aggregate",
            feat_type = "rna",
            name = "cluster_metagene"
        )
    )

    it("Sets SpatialEnrichment", {
        checkmate::expect_class(
            getSpatialEnrichment(g,
                spat_unit = "aggregate",
                feat_type = "rna",
                name = "cluster_metagene"
            ),
            "spatEnrObj"
        )
    })

    # SPATNET

    g <- setSpatialNetwork(g,
        spat_unit = "aggregate",
        verbose = FALSE,
        getSpatialNetwork(viz, spat_unit = "aggregate")
    )

    it("Sets SpatialNetwork", {
        checkmate::expect_class(
            getSpatialNetwork(g, spat_unit = "aggregate"),
            "spatialNetworkObj"
        )
    })

})



# setter edge cases ####

describe("Giotto Object Setters Validation and Edge Cases", {

    test <- giotto()
    ex1 <- ex2 <- ex <- test_data$ex
    objName(ex1) <- "data1"
    featType(ex1) <- "protein"
    objName(ex2) <- "data2"
    featType(ex2) <- "feat3"
    spatUnit(ex2) <- "nucleus"

    describe("setExpression()", {

        it("can set single object", {
            test_ex <- setExpression(test, ex, verbose = FALSE)

            avail_ex <- list_expression(test_ex)
            expect_s3_class(avail_ex, "data.table")

            expect_identical(avail_ex$spat_unit, "aggregate")
            expect_identical(avail_ex$feat_type, "rna")
            expect_identical(avail_ex$name, "normalized")
        })

        it("can set list of objects", {
            test_ex <- setExpression(test, list(ex, ex1, ex2), verbose = FALSE)

            avail_ex <- list_expression(test_ex)
            expect_s3_class(avail_ex, "data.table")

            expect_identical(avail_ex$spat_unit, c("aggregate", "aggregate", "nucleus"))
            expect_identical(avail_ex$feat_type, c("rna", "protein", "feat3"))
            expect_identical(avail_ex$name, c("normalized", "data1", "data2"))
        })

        it("Non-native throws error", {
            test_ex <- expect_error(setExpression(test, ex[]), regexp = "Only exprObj")
        })

    })

    spatUnit(ex) <- "aggregate"
    spatUnit(ex2) <- "nucleus"
    test_ex <- setExpression(test, ex, verbose = FALSE) # spat_unit aggregate
    test_ex2 <- setExpression(test_ex, ex2, verbose = FALSE) # spat_unit nucleus

    describe("setCellMetadata()", {

        cm <- test_data$cm

        it("can set single object", {
            test_ex <- setExpression(test, ex, verbose = FALSE)
            test_ex <- setCellMetadata(test_ex, cm, verbose = FALSE)

            avail_ex <- list_cell_metadata(test_ex)
            expect_s3_class(avail_ex, "data.table")

            expect_identical(avail_ex$spat_unit, spatUnit(cm))
            expect_identical(avail_ex$feat_type, "rna")
        })

        cm1 <- cm2 <- cm
        featType(cm1) <- "protein"
        spatUnit(cm2) <- "nucleus"
        featType(cm2) <- "feat3"

        it("can set list of objects", {
            test_ex <- setExpression(test, list(ex, ex1, ex2), verbose = FALSE)

            # tests
            test_ex <- setCellMetadata(test_ex, list(cm, cm1, cm2), verbose = FALSE)


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

    })

    describe("setFeatureMetadata()", {

        fm <- test_data$fm

        it("can set single object", {
            test_ex <- setExpression(test, ex, verbose = FALSE)
            test_ex <- setFeatureMetadata(test_ex, fm, verbose = FALSE)

            avail_ex <- list_feat_metadata(test_ex)
            expect_s3_class(avail_ex, "data.table")

            expect_identical(avail_ex$spat_unit, spatUnit(fm))
            expect_identical(avail_ex$feat_type, "rna")
        })

        fm1 <- fm2 <- fm
        featType(fm1) <- "protein"
        spatUnit(fm2) <- "nucleus"
        featType(fm2) <- "feat3"

        it("can set list of objects", {
            test_ex <- setExpression(test, list(ex, ex1, ex2), verbose = FALSE)

            # tests
            test_ex <- setFeatureMetadata(test_ex, list(fm, fm1, fm2), verbose = FALSE)


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

    })

    sl <- test_data$sl

    sl1 <- sl2 <- sl
    objName(sl1) <- "data1"
    objName(sl2) <- "data2"
    spatUnit(sl2) <- "nucleus"

    describe("setSpatialLocations()", {

        it("can set single object", {
            test_ex <- setSpatialLocations(test_ex, sl, verbose = FALSE)

            avail_ex <- list_spatial_locations(test_ex)
            expect_s3_class(avail_ex, "data.table")

            expect_identical(avail_ex$spat_unit, spatUnit(sl))
            expect_identical(avail_ex$name, objName(sl))
        })

        it("can set list of objects", {
            # setup
            test_ex <- setExpression(test_ex, ex2, verbose = FALSE)

            # tests
            test_ex <- setSpatialLocations(test_ex, list(sl, sl1, sl2), verbose = FALSE)

            avail_ex <- list_spatial_locations(test_ex)
            expect_s3_class(avail_ex, "data.table")

            expect_identical(avail_ex$spat_unit, c("aggregate", "aggregate", "nucleus"))
            expect_identical(avail_ex$name, c("raw", "data1", "data2"))
        })

        it("Non-native throws error", {
            expect_error(setSpatialLocations(test_ex, sl[]), regexp = "Only spatLocsObj")
        })

        it("needs expression info to set spatlocs", {
            expect_error(setSpatialLocations(test, sl, verbose = FALSE),
                         regexp = "Add expression"
            )
        })

        it("can set native spatLocsObj with user specified nesting", {
            # add needed spat_unit in expression first
            test_sl <- setExpression(test, ex, spat_unit = "new", verbose = FALSE)

            test_sl <- setSpatialLocations(test_sl, sl, spat_unit = "new", verbose = FALSE)
            test_sl <- setSpatialLocations(test_sl, sl, spat_unit = "new", name = "new", verbose = FALSE)

            avail_sl <- list_spatial_locations(test_sl)
            expect_equal(nrow(avail_sl), 2L)

            set_sl_a <- expect_no_error(getSpatialLocations(test_sl, spat_unit = "new"))
            set_sl_b <- expect_no_error(getSpatialLocations(test_sl, name = "new"))
        })

        it("errors when setting Spatlocs for a spat_unit not backed by expr or spatial_info", {
            # available spat unit in expression is only 'aggregate'
            test_sl <- expect_error(setSpatialLocations(test_ex, sl, spat_unit = "new", verbose = FALSE),
                regexp = "No expression"
            )
        })

        it("errors when Spatlocs spatID is mismatched with expression info", {
            gpoly <- test_data$gpoly
            test_ex <- setPolygonInfo(test_ex, gpoly, name = "new", verbose = FALSE)
            # due to subset, expected that sl will have fewer IDs
            expect_error(setSpatialLocations(test_ex, sl[1:6], spat_unit = "new", verbose = FALSE),
                         regexp = "between spatial and"
            )
        })

        it("can remove spatlocs by setting NULL", {
            test_ex <- setSpatialLocations(test_ex, sl, verbose = FALSE)

            test_ex <- setSpatialLocations(test_ex,
                x = NULL,
                spat_unit = "aggregate",
                name = "raw",
                verbose = FALSE
            )

            expect_null(test_ex@spatial_locs)
        })

    })

    describe("setSpatialNetwork()", {

        sn <- test_data$sn

        sn1 <- sn2 <- sn
        objName(sn1) <- "data1"
        objName(sn2) <- "data2"
        spatUnit(sl) <- "aggregate"
        spatUnit(sl2) <- "nucleus"
        spatUnit(sn2) <- "nucleus"

        it("requires matching spatial locations data", {
            expect_error(setSpatialNetwork(test_ex, sn2, verbose = FALSE), regexp = "Add spatial location") # none
            test_ex <- setSpatialLocations(test_ex, sl, verbose = FALSE)
            expect_error(setSpatialNetwork(test_ex, sn2, verbose = FALSE), regexp = "Matching") # no match (nucleus vs aggregate)

            # test_ex2 contains info with spat_unit = 'nucleus'
            test_ex2 <- setSpatialLocations(test_ex2, sl2, verbose = FALSE)
            expect_no_error(setSpatialNetwork(test_ex2, sn2, verbose = FALSE)) # should now work with correct sl spat_unit
        })

        it("can set single object", {
            rlang::local_options(lifecycle_verbosity = "quiet")
            test_ex <- setSpatialLocations(test_ex, sl, verbose = FALSE)
            test_ex <- setSpatialNetwork(test_ex, sn, verbose = FALSE)

            avail_ex <- list_spatial_networks(test_ex)
            expect_s3_class(avail_ex, "data.table")

            expect_identical(avail_ex$spat_unit, spatUnit(sn))
            expect_identical(avail_ex$name, objName(sn))
        })

        it("it can set list of objects", {
            rlang::local_options(lifecycle_verbosity = "quiet")
            # setup
            test_ex <- setSpatialLocations(test_ex, sl, verbose = FALSE)
            test_ex <- setExpression(test_ex, ex2, verbose = FALSE)
            test_ex <- setSpatialLocations(test_ex, sl2, verbose = FALSE)

            # tests
            test_ex <- setSpatialNetwork(test_ex, list(sn, sn1, sn2), verbose = FALSE)

            avail_ex <- list_spatial_networks(test_ex)
            expect_s3_class(avail_ex, "data.table")

            expect_identical(avail_ex$spat_unit, c("aggregate", "aggregate", "nucleus"))
            expect_identical(avail_ex$name, c(objName(sn), "data1", "data2"))
        })

        it("Non-native throws error", {
            rlang::local_options(lifecycle_verbosity = "quiet")
            test_ex <- setSpatialLocations(test_ex, sl, verbose = FALSE)
            expect_error(setSpatialNetwork(test_ex, sn[], verbose = FALSE), regexp = "Only spatialNetworkObj")
        })
    })

    describe("setSpatialEnrichment()", {

        enr <- test_data$enr

        enr1 <- enr2 <- enr
        objName(enr1) <- "data1"
        objName(enr2) <- "data2"
        spatUnit(sl) <- "aggregate"
        spatUnit(sl2) <- "nucleus"
        spatUnit(enr2) <- "nucleus"

        it("requires matching spatial locations data", {
            expect_error(setSpatialEnrichment(test_ex, enr2, verbose = FALSE), regexp = "Add spatial location") # none
            test_ex <- setSpatialLocations(test_ex, sl, verbose = FALSE)
            expect_error(setSpatialEnrichment(test_ex, enr2, verbose = FALSE), regexp = "Matching") # no match (nucleus vs aggregate)

            # test_ex2 contains info with spat_unit = 'nucleus'
            test_ex2 <- setSpatialLocations(test_ex2, sl2, verbose = FALSE)
            expect_no_error(setSpatialEnrichment(test_ex2, enr2, verbose = FALSE)) # should now work with correct sl spat_unit
        })

        it("can set single object", {
            test_ex <- setSpatialLocations(test_ex, sl, verbose = FALSE)
            test_ex <- setSpatialEnrichment(test_ex, enr, verbose = FALSE)

            avail_se <- list_spatial_enrichments(test_ex)
            expect_s3_class(avail_se, "data.table")

            expect_identical(avail_se$spat_unit, spatUnit(enr))
            expect_identical(avail_se$name, objName(enr))
        })

        it("can set list of objects", {
            # setup
            test_ex <- setSpatialLocations(test_ex, sl, verbose = FALSE)
            test_ex <- setExpression(test_ex, ex2, verbose = FALSE)
            test_ex <- setSpatialLocations(test_ex, sl2, verbose = FALSE)

            # tests
            test_ex <- setSpatialEnrichment(test_ex, list(enr, enr1, enr2), verbose = FALSE)

            avail_se <- list_spatial_enrichments(test_ex)
            expect_s3_class(avail_se, "data.table")

            expect_identical(avail_se$spat_unit, c("aggregate", "aggregate", "nucleus"))
            expect_identical(avail_se$name, c(objName(enr), "data1", "data2"))
        })

        it("Non-native throws error", {
            test_ex <- setSpatialLocations(test_ex, sl, verbose = FALSE)
            expect_error(setSpatialEnrichment(test_ex, enr[], verbose = FALSE), regexp = "Only spatEnrObj")
        })

    })

    dr <- test_data$dr

    dr1 <- dr2 <- dr
    objName(dr1) <- "data1"
    featType(dr1) <- "test_feat"
    objName(dr2) <- "data2"
    spatUnit(dr2) <- "nucleus"
    featType(dr2) <- "test_feat"

    describe("setDimReduction()", {

        it("requires matching expression data", {
            expect_error(setDimReduction(test, dr2, verbose = FALSE), regexp = "Add expression") # none
            expect_error(setDimReduction(test_ex, dr2, verbose = FALSE), regexp = "Matching") # no match (nucleus vs aggregate)

            # test_ex2 contains info with spat_unit = 'nucleus'
            expect_error(setDimReduction(test_ex2, dr2, verbose = FALSE), regexp = "Matching") # wrong feat
            featType(dr2) <- "feat3"
            expect_no_error(setDimReduction(test_ex2, dr2, verbose = FALSE))
        })

        it("can set single object", {
            test_ex <- setSpatialLocations(test_ex, sl, verbose = FALSE)
            test_ex <- setDimReduction(test_ex, dr, verbose = FALSE)

            avail_dr <- list_dim_reductions(test_ex)
            expect_s3_class(avail_dr, "data.table")

            expect_identical(avail_dr$spat_unit, spatUnit(dr))
            expect_identical(avail_dr$name, objName(dr))
        })

        it("can set list of objects", {
            # setup
            featType(ex2) <- "test_feat"
            test_ex <- setExpression(test_ex, ex2, verbose = FALSE)
            featType(ex2) <- "rna"
            test_ex <- setExpression(test_ex, ex2, verbose = FALSE)
            featType(ex) <- "test_feat"
            test_ex <- setExpression(test_ex, ex, verbose = FALSE)

            # tests
            test_ex <- setDimReduction(test_ex, list(dr, dr1, dr2), verbose = FALSE)

            avail_dr <- list_dim_reductions(test_ex)
            expect_s3_class(avail_dr, "data.table")

            expect_identical(avail_dr$spat_unit, c("aggregate", "aggregate", "nucleus"))
            expect_identical(avail_dr$feat_type, c("rna", "test_feat", "test_feat"))
            expect_identical(avail_dr$name, c(objName(dr), "data1", "data2"))
        })

        it("Non-native throws error", {
            test_ex <- setSpatialLocations(test_ex, sl, verbose = FALSE)
            expect_error(setDimReduction(test_ex, dr[], verbose = FALSE), regexp = "Only dimObj")
        })
    })

    describe("setNearestNetwork()", {

        nn <- test_data$nn

        nn1 <- nn2 <- nn
        objName(nn1) <- "data1"
        featType(nn1) <- "test_feat"
        objName(nn2) <- "data2"
        spatUnit(nn2) <- "nucleus"
        featType(nn2) <- "test_feat"

        it("requires matching dimreduction data", {
            expect_error(setNearestNetwork(test, nn2, verbose = FALSE), regexp = "Add dimension reduction") # none
            test_ex <- setDimReduction(test_ex, dr, verbose = FALSE) # no match (nucleus vs aggregate)

            # test_ex2 contains info with spat_unit = 'nucleus'
            expect_error(setNearestNetwork(test_ex, nn2, verbose = FALSE), regexp = "Matching") # wrong feat
            featType(dr2) <- "test_feat"
            featType(ex2) <- "test_feat"

            test_ex <- setExpression(test_ex, ex2, verbose = FALSE)
            test_ex <- setDimReduction(test_ex, dr2, verbose = FALSE)
            expect_no_error(setNearestNetwork(test_ex, nn2, verbose = FALSE))
        })

        it("can set single object", {
            test_ex <- setSpatialLocations(test_ex, sl, verbose = FALSE)
            test_ex <- setDimReduction(test_ex, dr, verbose = FALSE)
            test_ex <- setNearestNetwork(test_ex, nn, verbose = FALSE)

            avail_nn <- list_nearest_networks(test_ex)
            expect_s3_class(avail_nn, "data.table")

            expect_identical(avail_nn$spat_unit, spatUnit(nn))
            expect_identical(avail_nn$name, objName(nn))
        })

        it("can set list of objects", {
            # setup
            featType(ex2) <- "test_feat"
            test_ex <- setExpression(test_ex, ex2, verbose = FALSE)
            featType(ex2) <- "rna"
            test_ex <- setExpression(test_ex, ex2, verbose = FALSE)
            featType(ex) <- "test_feat"
            test_ex <- setExpression(test_ex, ex, verbose = FALSE)

            # tests
            test_ex <- setDimReduction(test_ex, list(dr, dr1, dr2), verbose = FALSE)
            test_ex <- setNearestNetwork(test_ex, list(nn, nn1, nn2), verbose = FALSE)

            avail_nn <- list_nearest_networks(test_ex)
            expect_s3_class(avail_nn, "data.table")

            expect_identical(avail_nn$spat_unit, c("aggregate", "aggregate", "nucleus"))
            expect_identical(avail_nn$feat_type, c("rna", "test_feat", "test_feat"))
            expect_identical(avail_nn$name, c(objName(nn), "data1", "data2"))
        })

        it("Non-native throws error", {
            test_ex <- setDimReduction(test_ex, dr, verbose = FALSE)
            expect_error(setNearestNetwork(test_ex, nn[], verbose = FALSE), regexp = "Only nnNetObj")
        })
    })

    describe("setFeatureInfo()", {

        gpoints <- test_data$gpoints

        it("can set single object", {
            test_fi <- expect_no_error(setFeatureInfo(test, gpoints, verbose = FALSE))

            avail_fi <- list_feature_info(test_fi)
            expect_s3_class(avail_fi, "data.table")

            expect_identical(avail_fi$feat_info, "rna")
        })

        ## ------------------------------------------------------------------------ ##

        it("can set list of objects", { # issues currently happen with unnamed lists
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
            test_fi <- setFeatureInfo(test, x = list(gp_1, gp_2), verbose = FALSE)

            avail_fi <- list_feature_info(test_fi)
            expect_identical(avail_fi$feat_info, c("new1", "new2"))
            expect_identical(test_fi@feat_info$new1@feat_type, "new1")
            expect_identical(test_fi@feat_info$new2@feat_type, "new2") # passes, but the message is wrong
        })



    })

    describe("setPolygonInfo()", {

        gpoly <- test_data$gpoly

        it("can set single object", {
            test_si <- expect_no_error(setPolygonInfo(test, gpoly,
                verbose = FALSE)
            )

            avail_si <- list_spatial_info(test_si)
            expect_s3_class(avail_si, "data.table")

            expect_identical(avail_si$spat_info, "aggregate")
        })

        it("also sets spatlocs if centroids are available", {
            # spat_unit (polygon_name) not explicitly set
            test_si <- setPolygonInfo(test, gpoly,
                centroids_to_spatlocs = TRUE,
                verbose = FALSE
            )

            avail_spatlocs <- list_spatial_locations(test_si)
            expect_s3_class(avail_spatlocs, "data.table")

            expect_identical(avail_spatlocs$spat_unit, "aggregate")
            expect_identical(avail_spatlocs$name, "raw")

            # spat_unit (polygon_name) explicitly set
            test_si <- setPolygonInfo(test, gpoly,
                name = "test_unit",
                centroids_to_spatlocs = TRUE,
                verbose = FALSE
            )

            avail_spatlocs <- list_spatial_locations(test_si)
            expect_s3_class(avail_spatlocs, "data.table")

            expect_identical(avail_spatlocs$spat_unit, "test_unit")
            expect_identical(avail_spatlocs$name, "raw")
        })

    })



})

# addCellMetadata ####

describe("addCellMetadata()", {

    giotto_object <- test_data$viz

    it("works for DT with IDs input", {

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
        checkmate:: expect_numeric(res$random)
        checkmate:: expect_numeric(res$random2)
        # check that start meta order is the same as end
        expect_identical(original_order, res$cell_ID)
    })

    it("works for DT without IDs input", {

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

    it("works for vector input", {

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

    it("works for factor input", {

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
        checkmate::expect_factor(res$factors)
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
        checkmate:: expect_factor(res$factors2) # values retain type
        # check that start meta order is the same as end
        expect_identical(original_order, res$cell_ID)
    })


})

# addFeatureMetadata ####

describe("addFeatMetadata()", {

    giotto_object <- test_data$viz

    it("works for DT with IDs input", {

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
        checkmate::expect_numeric(res$random)
        checkmate::expect_numeric(res$random2)
        # check that start meta order is the same as end
        expect_identical(original_order, res$feat_ID)
    })

    it("works for DT without IDs input", {

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

    it("works for vector input", {

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

    it("works for factor input", {

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
        checkmate::expect_factor(res$factors) # values retain type
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
        checkmate::expect_factor(res$factors2) # values retain type
        # check that start meta order is the same as end
        expect_identical(original_order, res$feat_ID)
    })

})

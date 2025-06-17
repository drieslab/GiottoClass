describe("gobject [[ extraction", {

    # Test data setup
    g <- test_data$viz

    describe("Basic [[ extraction", {

        it("returns all subobjects as list with [[]]", {
            result <- g[[]]

            expect_true(is.list(result))
            expect_gt(length(result), 0)
            expect_true(all(sapply(result, function(x) inherits(x, "giottoSubobject"))))
        })

        it("returns giotto object with [[ drop = FALSE ]]", {
            result <- g[[, drop = FALSE]]

            expect_true(inherits(result, "giotto"))
            expect_no_error(validObject(result))
        })

        it("preserves original structure when drop = FALSE", {
            result <- g[[, drop = FALSE]]

            # Should have same spatial units and feature types
            expect_setequal(spatUnit(result), spatUnit(g))
            expect_setequal(featType(result), featType(g))
        })

    })

    describe("Slot-specific extraction", {

        it("can extract by slot name", {
            result <- g[["expression"]]

            expect_true(is.list(result))
            expect_true(all(sapply(result, function(x) inherits(x, "exprObj"))))
        })

        it("can extract multiple slots", {
            result <- g[[c("expression", "cell_metadata")]]

            expect_true(is.list(result))
            # Should contain both expression and metadata objects
            classes <- sapply(result, class)
            expect_true(any(grepl("exprObj", classes)))
            expect_true(any(grepl("cellMetaObj", classes)))
        })

        it("returns giotto object for slot extraction with drop = FALSE", {
            result <- g[["expression", drop = FALSE]]

            expect_true(inherits(result, "giotto"))
            # Should only contain expression data
            expr_objs <- getExpression(result, output = "exprObj")
            expect_gt(length(expr_objs), 0)
        })

    })

    describe("Name-specific extraction", {

        it("can extract by subobject name", {
            # Get available names first
            all_objs <- g[[]]
            if (length(all_objs) > 0) {
                obj_names <- objName(all_objs)
                if (length(obj_names) > 0) {
                    test_name <- obj_names[1]
                    result <- g[[, test_name]]

                    expect_true(is.list(result))
                    expect_true(length(result) <= length(all_objs))
                }
            }
        })

        it("returns empty list for non-existent name", {
            result <- g[[, "nonexistent_name"]]

            expect_true(is.list(result))
            expect_equal(length(result), 0)
        })

        it("can extract multiple names", {
            all_objs <- g[[]]
            if (length(all_objs) >= 2) {
                obj_names <- objName(all_objs)
                if (length(obj_names) >= 2) {
                    test_names <- obj_names[1:2]
                    result <- g[[, test_names]]

                    expect_true(is.list(result))
                    expect_true(length(result) <= 2)
                }
            }
        })

    })

    describe("Slot and name extraction", {

        it("can filter extraction by both slot and name", {
            # Get expression object names
            avail_dr <- list_dim_reductions(g)
            if (nrow(avail_dr) > 0L) {
                dr_name <- avail_dr$name
                result <- g[["dimension_reduction", dr_name[[1]]]]

                expect_true(is.list(result))
                expect_lt(length(result), length(g[["dimension_reduction"]]))
            }
        })

        it("returns giotto object with slot and name extraction when drop = FALSE", {
            expr_name <- "normalized"
            result <- g[["expression", expr_name, drop = FALSE]]

            expect_lt(length(result[["expression"]]), length(g[["expression"]]))
            expect_true(inherits(result, "giotto"))
            expect_no_error(validObject(result))
        })

    })

    describe("Spatial unit filtering", {

        it("can filter by spatial unit", {
            available_spat_units <- spatUnit(g)
            if (length(available_spat_units) > 0) {
                test_spat_unit <- available_spat_units[1]
                result <- g[[spat_unit = test_spat_unit]]

                expect_true(is.list(result))
                # All returned objects should have the specified spatial unit
                if (length(result) > 0) {
                    spat_units <- sapply(result, spatUnit)
                    expect_true(all(spat_units == test_spat_unit | is.na(spat_units)))
                }
            }
        })

        it("sets active spatial unit when drop = FALSE", {
            activeSpatUnit(g) <- "aggregate"
            result <- g[[spat_unit = "z0", drop = FALSE]]

            expect_true(inherits(result, "giotto"))
            expect_equal(activeSpatUnit(g), "aggregate")
            expect_equal(activeSpatUnit(result), "z0")
        })

        it("handles multiple spatial units", {
            test_spat_units <- c("z0", "z1")
            result <- g[[spat_unit = test_spat_units]]

            expect_true(is.list(result))
            # Should include objects from both spatial units
            if (length(result) > 0) {
                spat_units <- sapply(result, spatUnit)
                expect_true(any(spat_units %in% test_spat_units |
                                    is.na(spat_units)))
            }

        })

    })

    describe("Feature type filtering", {

        it("can filter by feature type", {
            test_feat_type <- "rna"
            result <- g[[feat_type = test_feat_type]]

            expect_true(is.list(result))
            # All returned objects should have the specified feature type
            if (length(result) > 0) {
                feat_types <- sapply(result, featType)
                expect_true(all(feat_types == test_feat_type | is.na(feat_types)))
            }
        })

        it("sets active feature type when drop = FALSE", {
            activeFeatType(g) <- "test" # doesn't actually exist

            test_feat_type <- "rna"
            result <- g[[feat_type = test_feat_type, drop = FALSE]]

            expect_true(inherits(result, "giotto"))
            expect_equal(activeFeatType(result), test_feat_type)
        })

        it("handles multiple feature types", {
            ex <- test_data$ex
            featType(ex) <- "protein"
            g <- setGiotto(g, ex, verbose = FALSE)
            available_feat_types <- featType(g)
            if (length(available_feat_types) >= 2) {
                test_feat_types <- available_feat_types[1:2]
                result <- g[[feat_type = test_feat_types]]

                expect_true(is.list(result))
                # Should include objects from both feature types
                if (length(result) > 0) {
                    feat_types <- sapply(result, featType)
                    expect_true(any(feat_types %in% test_feat_types | is.na(feat_types)))
                }
            }
        })

    })

    describe("Combined filtering", {

        it("can filter by both spatial unit and feature type", {
            test_spat_unit <- "aggregate"
            test_feat_type <- "rna"

            result <- g[[spat_unit = test_spat_unit, feat_type = test_feat_type]]

            expect_true(is.list(result))
            # All returned objects should match both filters
            spat_units <- sapply(result, spatUnit)
            feat_types <- sapply(result, featType)
            expect_true(all(spat_units == test_spat_unit | is.na(spat_units)))
            expect_true(all(feat_types == test_feat_type | is.na(feat_types)))


        })

        it("combines slot, name, and unit filtering", {
            test_spat_unit <- "z0"
            expr_name <- "raw"

            result <- g[["expression", expr_name, spat_unit = test_spat_unit]]

            expect_true(is.list(result))
            # Should be expression objects with correct spatial unit
            expect_true(all(sapply(result, function(x) inherits(x, "exprObj"))))
            spat_units <- sapply(result, spatUnit)
            expect_true(all(spat_units == test_spat_unit | is.na(spat_units)))
            expect_lt(length(result), length(g[["expression"]]))
        })

    })

    describe("Edge cases and error handling", {

        it("handles empty giotto object gracefully", {
            empty_g <- giotto()
            result <- empty_g[[]]

            expect_true(is.list(result))
            expect_equal(length(result), 0)
        })

        it("throws error for non-existent slot", {
            expect_error(g[["nonexistent_slot"]], regexp = "should be one of")
        })

        it("handles invalid spatial unit gracefully", {
            result <- g[[spat_unit = "invalid_spat_unit"]]

            expect_true(is.list(result))
            expect_equal(length(result), 0)
        })

        it("handles invalid feature type gracefully", {
            result <- g[[feat_type = "invalid_feat_type"]]

            expect_true(is.list(result))
            expect_equal(length(result), 0)
        })

        it("preserves object names in returned list", {
            result <- g[[]]

            expect_true(is.list(result))
            expect_true(!is.null(objName(result)))
            objname_nch <- nchar(objName(result))
            expect_true(all(objname_nch[!is.na(objname_nch)] > 0))
        })

        it("maintains consistent behavior between drop TRUE and FALSE", {
            # Test that drop=TRUE and drop=FALSE access the same underlying data
            list_result <- g[[]]
            obj_result <- g[[, drop = FALSE]]
            # Convert back to list and compare structure
            obj_as_list <- obj_result[[]]
            expect_equal(length(list_result), length(obj_as_list))
        })

    })

    describe("Return value validation", {

        it("returns valid giotto subobjects in list", {
            result <- g[[]]
            expect_true(all(sapply(result, function(x) {
                inherits(x, "giottoSubobject") && validObject(x)
            })))
        })

        it("returns valid giotto object when drop = FALSE", {
            result <- g[[, drop = FALSE]]

            expect_true(inherits(result, "giotto"))
            expect_no_error(validObject(result))
        })

        it("preserves instructions when drop = FALSE", {
            original_instructions <- instructions(g)
            result <- g[[, drop = FALSE]]
            result_instructions <- instructions(result)

            # Instructions should be preserved
            # won't be the case if active is changed
            expect_identical(original_instructions, result_instructions)
        })

    })

    describe("Integration with as.list method", {

        it("[[]] is equivalent to as.list() with same parameters", {
            # Test that [[ is using as.list internally correctly
            list_result1 <- g[[]]
            list_result2 <- as.list(g)

            expect_equal(length(list_result1), length(list_result2))
            expect_equal(objName(list_result1), objName(list_result2))
            expect_equal(class(list_result1), class(list_result2))
        })

        it("spatial unit filtering matches as.list behavior", {
            available_spat_units <- spatUnit(g)
            test_spat_unit <- available_spat_units[1]

            bracket_result <- g[[spat_unit = test_spat_unit]]
            aslist_result <- as.list(g, spat_unit = test_spat_unit)

            expect_equal(length(bracket_result), length(aslist_result))
            expect_equal(objName(bracket_result), objName(aslist_result))
        })

        it("feature type filtering matches as.list behavior", {
            available_feat_types <- featType(g)
            test_feat_type <- available_feat_types[1]

            bracket_result <- g[[feat_type = test_feat_type]]
            aslist_result <- as.list(g, feat_type = test_feat_type)

            expect_equal(length(bracket_result), length(aslist_result))
            expect_equal(objName(bracket_result), objName(aslist_result))
        })

    })

    describe("Slot and name interaction edge cases", {

        it("handles slot filtering when requested subobject name doesn't exist in slot", {
            # This should return empty but not error
            result <- g[["expression", "nonexistent_name"]]

            expect_true(is.list(result))
            expect_equal(length(result), 0)
        })

        it("handles name filtering when requested slot doesn't contain that name", {
            # Get a name from one slot and try to find it in another
            all_objs <- g[[]]
            obj_names <- objName(all_objs)
            if (length(obj_names) > 0) {
                test_name <- obj_names[1]
                # Try to find this name in a slot it likely doesn't belong to
                result <- g[["spatial_info", test_name]]

                expect_true(is.list(result))
                # May be empty or may contain matching objects
            }
        })

        it("preserves metadata slots when creating new giotto object", {
            original_params <- g@parameters
            original_instructions <- g@instructions

            result <- g[[, drop = FALSE]]

            expect_equal(result@parameters, original_params)
            expect_equal(result@instructions, original_instructions)
        })

    })

    describe("Parameter validation", {

        it("handles NULL spatial unit parameter", {
            result <- g[[spat_unit = NULL]]

            expect_true(is.list(result))
            # Should behave same as no spat_unit parameter
            result_no_param <- g[[]]
            expect_equal(length(result), length(result_no_param))
        })

        it("handles NULL feature type parameter", {
            result <- g[[feat_type = NULL]]

            expect_true(is.list(result))
            # Should behave same as no feat_type parameter
            result_no_param <- g[[]]
            expect_equal(length(result), length(result_no_param))
        })

        it("handles empty character vectors for filtering", {
            result_spat <- g[[spat_unit = character(0)]]
            result_feat <- g[[feat_type = character(0)]]

            expect_true(is.list(result_spat))
            expect_true(is.list(result_feat))
            expect_equal(length(result_spat), 0)
            expect_equal(length(result_feat), 0)
        })

        it("handles mixed valid and invalid spatial units", {
            available_spat_units <- spatUnit(g)
            mixed_units <- c(available_spat_units[1], "invalid_unit")
            result <- g[[spat_unit = mixed_units]]

            expect_true(is.list(result))
            # Should include objects from valid spatial unit only
            spat_units <- sapply(result, spatUnit)
            expect_true(all(spat_units == available_spat_units[1] | is.na(spat_units)))
        })

    })

    describe("Performance considerations", {

        it("handles large object extraction efficiently", {
            # Test with timing - should complete reasonably quickly
            start_time <- Sys.time()
            result <- g[[]]
            end_time <- Sys.time()

            expect_true(is.list(result))
            # Should complete within reasonable time (adjust threshold as needed)
            expect_lt(as.numeric(end_time - start_time), 10) # 10 seconds max
        })

        it("memory usage is reasonable for drop = FALSE", {
            # Basic check that we can create the object without memory errors
            expect_no_error({
                result <- g[[, drop = FALSE]]
                rm(result)
                gc()
            })
        })

        it("repeated extraction operations are stable", {
            # Test that repeated operations give consistent results
            result1 <- g[[]]
            result2 <- g[[]]
            result3 <- g[[]]

            expect_equal(length(result1), length(result2))
            expect_equal(length(result2), length(result3))
            expect_equal(objName(result1), objName(result2))
            expect_equal(objName(result2), objName(result3))
        })

    })

})
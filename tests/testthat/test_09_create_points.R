describe("giottoPoints", {

    # Test data setup ----
    test_df <- data.frame(
        feat_ID = c("gene_a", "gene_b", "gene_c", "gene_a", "gene_b", "gene_c"),
        x = c(1.5, 2.0, 3.5, 4.0, 5.5, 6.0),
        y = c(1.0, 2.5, 3.0, 4.5, 5.0, 6.5)
    )

    test_df_alt_names <- data.frame( # same data, different colnames
        ID = c("gene_a", "gene_b", "gene_c", "gene_a", "gene_b", "gene_c"),
        coord_x = c(1.5, 2.0, 3.5, 4.0, 5.5, 6.0),
        coord_y = c(1.0, 2.5, 3.0, 4.5, 5.0, 6.5),
        count = c(10, 20, 15, 25, 30, 35)
    )

    test_df_split <- data.frame( # split by feat_ID naming pattern
        feat_ID = c("transcript_a", "transcript_b", "protein_c", "protein_d",
                    "metabolite_e", "transcript_f"),
        x = c(1.5, 2.0, 3.5, 4.0, 5.5, 6.0),
        y = c(1.0, 2.5, 3.0, 4.5, 5.0, 6.5)
    )

    # Basic creation tests ----
    describe("data.frame Input", {

        it("can be created from data.frame with default column names", {
            gpoints <- createGiottoPoints(test_df, verbose = FALSE)

            expect_s4_class(gpoints, "giottoPoints")
            expect_equal(nrow(gpoints), 6)
            expect_equal(gpoints@feat_type, "rna")
            expect_equal(terra::geomtype(gpoints[]), "points")
            expect_no_error(validObject(gpoints))
        })

        it("can be created with manual column specification", {
            gpoints <- createGiottoPoints(test_df_alt_names,
                x_colname = "coord_x",
                y_colname = "coord_y",
                feat_ID_colname = "ID",
                verbose = FALSE
            )

            expect_s4_class(gpoints, "giottoPoints")
            expect_equal(nrow(gpoints), 6)
            expect_equal(gpoints@feat_type, "rna")
            expect_equal(terra::geomtype(gpoints[]), "points")
        })

        it("can be created from data.table", {
            test_dt <- data.table::as.data.table(test_df)
            gpoints <- createGiottoPoints(test_dt, verbose = FALSE)

            expect_s4_class(gpoints, "giottoPoints")
            expect_equal(nrow(gpoints), 6)
        })

        it("can set custom feat_type", {
            gpoints <- createGiottoPoints(test_df,
                feat_type = "protein", verbose = FALSE)

            expect_equal(gpoints@feat_type, "protein")
        })

        it("preserves coordinate values correctly", {
            gpoints <- createGiottoPoints(test_df, verbose = FALSE)
            coords_dt <- data.table::as.data.table(gpoints[], geom = "XY")

            expect_equal(coords_dt$x, test_df$x)
            expect_equal(coords_dt$y, test_df$y)
            expect_equal(coords_dt$feat_ID, test_df$feat_ID)
        })

        it("handles additional attributes", {
            gpoints <- createGiottoPoints(test_df_alt_names,
                x_colname = "coord_x",
                y_colname = "coord_y",
                feat_ID_colname = "ID",
                verbose = FALSE
            )

            # Should preserve the count column as an attribute
            spatvec_dt <- data.table::as.data.table(gpoints[], geom = "XY")
            expect_true("count" %in% names(spatvec_dt))
            expect_equal(spatvec_dt$count, test_df_alt_names$count)
        })
    })

    # SpatVector input tests ----
    describe("SpatVector Input", {

        it("can be created from SpatVector", {
            # Create SpatVector first
            sv <- terra::vect(test_df,
                geom = c("x", "y")
            )

            gpoints <- createGiottoPoints(sv, verbose = FALSE)

            expect_s4_class(gpoints, "giottoPoints")
            expect_equal(nrow(gpoints), 6)
            expect_equal(terra::geomtype(gpoints[]), "points")
        })

        it("validates SpatVector input", {
            # Test with non-SpatVector object
            expect_error(createGiottoPoints(list(x = 1, y = 2)),
                         "unable to find an inherited method")
        })
    })

    # Split functionality tests ----
    describe("Split Functionality", {

        it("can split based on keywords", {
            gpoints_list <- createGiottoPoints(test_df_split,
                feat_type = c("rna", "protein", "metabolite"),
                split_keyword = list(c("protein"), c("metabolite")),
                verbose = FALSE
            )

            expect_type(gpoints_list, "list")
            expect_length(gpoints_list, 3)
            expect_true(all(sapply(gpoints_list, inherits, "giottoPoints")))

            # Check feat_types are correctly assigned
            expect_equal(gpoints_list[[1]]@feat_type, "rna")
            expect_equal(gpoints_list[[2]]@feat_type, "protein")
            expect_equal(gpoints_list[[3]]@feat_type, "metabolite")
        })

        it("handles split with correct point distribution", {
            gpoints_list <- createGiottoPoints(test_df_split,
                feat_type = c("rna", "protein", "metabolite"),
                split_keyword = list(c("protein"), c("metabolite")),
                verbose = FALSE
            )

            # Check number of points in each split
            expect_equal(nrow(gpoints_list$rna), 3)     # transcript_a, transcript_b, transcript_f
            expect_equal(nrow(gpoints_list$protein), 2)  # protein_c, protein_d
            expect_equal(nrow(gpoints_list$metabolite), 1) # metabolite_e
        })

        it("validates split_keyword parameter", {
            expect_error(createGiottoPoints(test_df,
                feat_type = c("rna", "protein"),
                split_keyword = "not_a_list"
            ), "Assertion on 'split_keyword' failed")
        })

        it("returns single object when split_keyword is NULL", {
            gpoints <- createGiottoPoints(test_df_split,
                feat_type = "rna",
                split_keyword = NULL,
                verbose = FALSE
            )

            expect_s4_class(gpoints, "giottoPoints")
            expect_false(is.list(gpoints))
        })
    })

    # Object structure and validation tests ----
    describe("Object Structure", {

        it("has correct slot structure", {
            gpoints <- createGiottoPoints(test_df, verbose = FALSE)

            expect_true(isS4(gpoints))
            expect_equal(gpoints@feat_type, "rna")
            # these may change...
            expect_true(methods::is(gpoints@spatVector, "SpatVector"))
            expect_true(is.character(gpoints@unique_ID_cache))
        })

        it("caches unique IDs correctly", {
            gpoints <- createGiottoPoints(test_df, verbose = FALSE)
            unique_feats <- unique(test_df$feat_ID)

            expect_setequal(gpoints@unique_ID_cache, unique_feats)
        })

        it("accepts custom unique_IDs parameter", {
            custom_ids <- c("gene_a", "gene_b", "gene_c")
            gpoints <- createGiottoPoints(test_df,
                unique_IDs = custom_ids, verbose = FALSE)

            expect_equal(gpoints@unique_ID_cache, custom_ids)
        })

        it("has proper spatVector attributes", {
            gpoints <- createGiottoPoints(test_df, verbose = FALSE)
            spatvec_data <- data.table::as.data.table(gpoints[], geom = "XY")

            expect_true("feat_ID" %in% names(spatvec_data))
            expect_true("feat_ID_uniq" %in% names(spatvec_data))
            expect_true("x" %in% names(spatvec_data))
            expect_true("y" %in% names(spatvec_data))
        })
    })

    # Subsetting tests ----
    describe("Subsetting", {

        gpoints <- createGiottoPoints(test_df, verbose = FALSE)

        it("supports numerical subsetting", {
            sub_gpoints <- gpoints[c(1, 3, 5)]
            expect_equal(nrow(sub_gpoints), 3)
            expect_s4_class(sub_gpoints, "giottoPoints")
        })

        it("supports logical subsetting", {
            logical_vec <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
            sub_gpoints <- gpoints[logical_vec]
            expect_equal(nrow(sub_gpoints), 3)
        })

        it("supports character subsetting by feat_ID", {
            sub_gpoints <- gpoints[c("gene_a", "gene_c")]
            expect_equal(nrow(sub_gpoints), 4) # 2 gene_a + 2 gene_c points
        })

        it("can drop to SpatVector with []", {
            spatvec <- gpoints[]
            expect_s4_class(spatvec, "SpatVector")
            expect_false(inherits(spatvec, "giottoPoints"))
        })
    })

    # Error handling tests ----
    describe("Error Handling", {

        it("handles missing required columns gracefully", {
            bad_df <- data.frame(a = 1:3, b = 4:6) # no x, y, or feat_ID columns
            expect_error(createGiottoPoints(bad_df),
                         regex = "at least 3 expected")
        })

        it("validates feat_type parameter", {
            expect_error(createGiottoPoints(test_df, feat_type = 123),
                         "Assertion on 'feat_type' failed")
        })

        it("handles empty data.frame", {
            empty_df <- data.frame(
                feat_ID = character(0),
                x = numeric(0),
                y = numeric(0)
            )
            gpoints <- createGiottoPoints(empty_df, verbose = FALSE)
            expect_equal(nrow(gpoints), 0)
        })
    })

    # Edge cases ----
    describe("Edge Cases", {

        it("handles single point", {
            single_df <- data.frame(feat_ID = "gene_a", x = 1.0, y = 2.0)
            gpoints <- createGiottoPoints(single_df, verbose = FALSE)

            expect_equal(nrow(gpoints), 1)
            expect_s4_class(gpoints, "giottoPoints")
        })

        it("handles duplicate coordinates", {
            dup_df <- data.frame(
                feat_ID = c("gene_a", "gene_b"),
                x = c(1.0, 1.0),
                y = c(2.0, 2.0)
            )
            gpoints <- createGiottoPoints(dup_df, verbose = FALSE)

            expect_equal(nrow(gpoints), 2)
            expect_s4_class(gpoints, "giottoPoints")
        })

        it("handles special characters in feat_ID", {
            special_df <- data.frame(
                feat_ID = c("gene-1", "gene.2", "gene_3"),
                x = c(1.0, 2.0, 3.0),
                y = c(1.0, 2.0, 3.0)
            )
            gpoints <- createGiottoPoints(special_df, verbose = FALSE)

            expect_equal(nrow(gpoints), 3)
            coords_dt <- data.table::as.data.table(gpoints[], geom = "XY")
            expect_setequal(coords_dt$feat_ID, special_df$feat_ID)
        })

        it("handles large coordinate values", {
            large_df <- data.frame(
                feat_ID = c("gene_a", "gene_b"),
                x = c(1e6, 2e6),
                y = c(1e6, 2e6)
            )
            gpoints <- createGiottoPoints(large_df, verbose = FALSE)

            expect_equal(nrow(gpoints), 2)
            coords_dt <- data.table::as.data.table(gpoints[], geom = "XY")
            expect_equal(coords_dt$x, large_df$x)
        })
    })

    # Integration with other functionality ----
    describe("Integration", {

        it("works with featIDs accessor", {
            gpoints <- createGiottoPoints(test_df, verbose = FALSE)
            feat_ids <- featIDs(gpoints)

            expect_type(feat_ids, "character")
            expect_setequal(feat_ids, unique(test_df$feat_ID))
        })

        it("preserves object name from feat_type", {
            gpoints <- createGiottoPoints(test_df,
                feat_type = "protein", verbose = FALSE)
            expect_equal(objName(gpoints), "protein")
        })

        it("can be plotted without error", {
            gpoints <- createGiottoPoints(test_df, verbose = FALSE)
            expect_no_error(plot(gpoints, raster = FALSE, cex = 0.5))
        })
    })
})

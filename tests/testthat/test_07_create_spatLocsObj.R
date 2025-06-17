describe("spatLocsObj", {

    # Test data setup ----
    test_df_2d <- data.frame(
        cell_ID = c("cell_1", "cell_2", "cell_3", "cell_4"),
        sdimx = c(100.5, 200.0, 300.7, 400.2),
        sdimy = c(50.3, 150.8, 250.1, 350.9)
    )

    test_df_3d <- data.frame(
        cell_ID = c("cell_1", "cell_2", "cell_3"),
        sdimx = c(100.5, 200.0, 300.7),
        sdimy = c(50.3, 150.8, 250.1),
        sdimz = c(10.0, 20.5, 30.2)
    )

    test_df_alt_names <- data.frame(
        ID = c("cell_1", "cell_2", "cell_3"),
        x_coord = c(100.5, 200.0, 300.7),
        y_coord = c(50.3, 150.8, 250.1),
        extra_col = c("A", "B", "C")
    )

    test_matrix_2d <- matrix(c(100, 200, 300, 50, 150, 250), ncol = 2)
    rownames(test_matrix_2d) <- c("cell1", "cell2", "cell3")

    test_matrix_3d <- matrix(c(100, 200, 300, 50, 150, 250, 10, 20, 30), ncol = 3)
    rownames(test_matrix_3d) <- c("cell1", "cell2", "cell3")

    # Basic creation tests ----
    describe("data.frame Input", {

        it("can be created from data.frame with standard columns", {
            spatlocs <- createSpatLocsObj(test_df_2d, verbose = FALSE)

            expect_s4_class(spatlocs, "spatLocsObj")
            expect_equal(nrow(spatlocs), 4)
            expect_equal(spatlocs@name, "test")
            expect_equal(spatlocs@spat_unit, "cell")
            expect_no_error(validObject(spatlocs))
        })

        it("can be created from data.frame with alternative column names", {
            expect_message(
                createSpatLocsObj(test_df_alt_names, verbose = TRUE),
                regexp = "Input has multiple non numeric columns"
            )

            spatlocs <- createSpatLocsObj(test_df_alt_names, verbose = FALSE)

            expect_s4_class(spatlocs, "spatLocsObj")
            expect_equal(nrow(spatlocs), 3)

            # Check that coordinates are properly converted
            coords_dt <- spatlocs[]
            expect_true("sdimx" %in% names(coords_dt))
            expect_true("sdimy" %in% names(coords_dt))
            expect_true("cell_ID" %in% names(coords_dt))
        })

        it("can be created with 3D coordinates", {
            spatlocs <- createSpatLocsObj(test_df_3d, verbose = FALSE)

            expect_s4_class(spatlocs, "spatLocsObj")
            expect_equal(nrow(spatlocs), 3)

            coords_dt <- spatlocs[]
            expect_true("sdimz" %in% names(coords_dt))
            expect_equal(coords_dt$sdimz, test_df_3d$sdimz)
        })

        it("preserves coordinate values correctly", {
            spatlocs <- createSpatLocsObj(test_df_2d, verbose = FALSE)
            coords_dt <- spatlocs[]

            expect_equal(coords_dt$sdimx, test_df_2d$sdimx)
            expect_equal(coords_dt$sdimy, test_df_2d$sdimy)
            expect_equal(coords_dt$cell_ID, test_df_2d$cell_ID)
        })

        it("can be created from data.table", {
            test_dt <- data.table::as.data.table(test_df_2d)
            spatlocs <- createSpatLocsObj(test_dt, verbose = FALSE)

            expect_s4_class(spatlocs, "spatLocsObj")
            expect_equal(nrow(spatlocs), 4)
        })
    })

    # Matrix input tests ----
    describe("matrix Input", {

        it("can be created from 2D matrix", {
            spatlocs <- createSpatLocsObj(test_matrix_2d, verbose = FALSE)

            expect_s4_class(spatlocs, "spatLocsObj")
            expect_equal(nrow(spatlocs), 3)

            coords_dt <- spatlocs[]
            expect_equal(coords_dt$sdimx, unname(test_matrix_2d[, 1]))
            expect_equal(coords_dt$sdimy, unname(test_matrix_2d[, 2]))
            expect_equal(coords_dt$cell_ID, rownames(test_matrix_2d))
        })

        it("can be created from 3D matrix", {
            spatlocs <- createSpatLocsObj(test_matrix_3d, verbose = FALSE)

            expect_s4_class(spatlocs, "spatLocsObj")
            expect_equal(nrow(spatlocs), 3)

            coords_dt <- spatlocs[]
            expect_true("sdimz" %in% names(coords_dt))
            expect_equal(coords_dt$sdimz, unname(test_matrix_3d[, 3]))
        })

        it("handles matrix without rownames", {
            test_matrix_no_names <- test_matrix_2d
            rownames(test_matrix_no_names) <- NULL

            spatlocs <- createSpatLocsObj(test_matrix_no_names, verbose = FALSE)

            expect_s4_class(spatlocs, "spatLocsObj")
            expect_equal(nrow(spatlocs), 3)
            # Should have default cell_IDs
            expect_true(all(is.na(spatlocs$cell_ID)))
        })
    })

    # Numeric vector input tests ----
    describe("numeric Input", {

        it("can be created from numeric vector as pairs", {
            num_pairs <- c(100, 50, 200, 150, 300, 250)
            spatlocs <- createSpatLocsObj(num_pairs,
                numeric_format = "pair", verbose = FALSE)

            expect_s4_class(spatlocs, "spatLocsObj")
            expect_equal(nrow(spatlocs), 3)

            coords_dt <- spatlocs[]
            expect_equal(coords_dt$sdimx, c(100, 200, 300))
            expect_equal(coords_dt$sdimy, c(50, 150, 250))
        })

        it("can be created from numeric vector as triplets", {
            num_triplets <- c(100, 50, 10, 200, 150, 20)
            spatlocs <- createSpatLocsObj(num_triplets,
                numeric_format = "triplet", verbose = FALSE)

            expect_s4_class(spatlocs, "spatLocsObj")
            expect_equal(nrow(spatlocs), 2)

            coords_dt <- spatlocs[]
            expect_true("sdimz" %in% names(coords_dt))
            expect_equal(coords_dt$sdimx, c(100, 200))
            expect_equal(coords_dt$sdimy, c(50, 150))
            expect_equal(coords_dt$sdimz, c(10, 20))
        })

        it("validates numeric_format parameter", {
            num_vec <- c(1, 2, 3, 4)
            expect_error(createSpatLocsObj(num_vec, numeric_format = "invalid"),
                         "arg.*should be one of")
        })
    })

    # Parameter testing ----
    describe("Parameters", {

        it("can set custom name", {
            spatlocs <- createSpatLocsObj(test_df_2d,
                name = "custom_coords", verbose = FALSE)

            expect_equal(spatlocs@name, "custom_coords")
        })

        it("can set custom spat_unit", {
            spatlocs <- createSpatLocsObj(test_df_2d,
                spat_unit = "nucleus", verbose = FALSE)

            expect_equal(spatlocs@spat_unit, "nucleus")
        })

        it("can set provenance", {
            spatlocs <- createSpatLocsObj(test_df_2d,
                provenance = "manual_annotation", verbose = FALSE)

            expect_equal(spatlocs@provenance, "manual_annotation")
        })

        it("can set misc data", {
            misc_data <- list(method = "centroid_calculation", version = "1.0")
            spatlocs <- createSpatLocsObj(test_df_2d,
                misc = misc_data, verbose = FALSE)

            expect_equal(spatlocs@misc, misc_data)
        })
    })

    # Object structure tests ----
    describe("Object Structure", {

        it("has correct S4 slot structure", {
            spatlocs <- createSpatLocsObj(test_df_2d, verbose = FALSE)

            expect_true(isS4(spatlocs))
            expect_true(inherits(spatlocs@coordinates, "data.table"))
            expect_equal(spatlocs@name, "test")
            expect_equal(spatlocs@spat_unit, "cell")
        })

        it("has required columns in coordinates slot", {
            spatlocs <- createSpatLocsObj(test_df_2d, verbose = FALSE)
            coord_cols <- names(spatlocs@coordinates)

            expect_true("sdimx" %in% coord_cols)
            expect_true("sdimy" %in% coord_cols)
            expect_true("cell_ID" %in% coord_cols)
        })

        it("validates object structure", {
            spatlocs <- createSpatLocsObj(test_df_2d, verbose = FALSE)
            expect_no_error(validObject(spatlocs))
        })

        it("maintains data.table class for coordinates", {
            spatlocs <- createSpatLocsObj(test_df_2d, verbose = FALSE)
            expect_s3_class(spatlocs@coordinates, "data.table")
        })
    })

    # Accessor methods ----
    describe("Accessor Methods", {

        spatlocs <- createSpatLocsObj(test_df_2d, verbose = FALSE)

        it("supports [] operator for data.table extraction", {
            coords_dt <- spatlocs[]
            expect_s3_class(coords_dt, "data.table")
            expect_equal(nrow(coords_dt), 4)
        })

        it("supports XY coordinate extraction", {
            xy_matrix <- XY(spatlocs)
            expect_true(is.matrix(xy_matrix))
            expect_equal(ncol(xy_matrix), 2)
            expect_equal(colnames(xy_matrix), c("x", "y"))
            expect_equal(xy_matrix[, "x"], test_df_2d$sdimx)
            expect_equal(xy_matrix[, "y"], test_df_2d$sdimy)
        })

        it("supports XY coordinate replacement", {
            new_coords <- cbind(c(500, 600, 700, 800), c(50, 60, 70, 80))
            spatlocs_copy <- spatlocs
            XY(spatlocs_copy) <- new_coords

            updated_coords <- XY(spatlocs_copy)
            expect_equal(updated_coords[, 1], new_coords[, 1])
            expect_equal(updated_coords[, 2], new_coords[, 2])
        })

        it("supports spatIDs accessor", {
            cell_ids <- spatIDs(spatlocs)
            expect_equal(cell_ids, test_df_2d$cell_ID)
        })

        it("supports nrow method", {
            expect_equal(nrow(spatlocs), 4)
        })
    })

    # Coordinate range and statistics ----
    describe("Coordinate Analysis", {

        it("calculates coordinate ranges correctly", {
            spatlocs <- createSpatLocsObj(test_df_2d, verbose = FALSE)

            # Test via ext() method
            e <- ext(spatlocs)
            e <- unname(e[])
            expect_equal(e[1], min(test_df_2d$sdimx))  # xmin
            expect_equal(e[2], max(test_df_2d$sdimx))  # xmax
            expect_equal(e[3], min(test_df_2d$sdimy))  # ymin
            expect_equal(e[4], max(test_df_2d$sdimy))  # ymax
        })
    })

    # Error handling ----
    describe("Error Handling", {

        it("handles insufficient coordinate dimensions", {
            single_col_df <- data.frame(x = c(1, 2, 3))
            expect_error(createSpatLocsObj(single_col_df),
                         "at least 2 numeric columns")
        })

        it("handles invalid numeric vector lengths for pairs", {
            odd_length_vec <- c(1, 2, 3)  # odd length can't be pairs
            expect_error(
                createSpatLocsObj(odd_length_vec, numeric_format = "pair"),
                regexp = "inputs must be given as x,y pairs."
            )
        })

        it("handles invalid numeric vector lengths for triplets", {
            bad_length_vec <- c(1, 2, 3, 4, 5)  # not divisible by 3
            expect_error(
                createSpatLocsObj(bad_length_vec, numeric_format = "triplet"),
                regexp = "inputs must be given as x,y,z triplets."
            )
        })

        it("handles empty data.frame", {
            empty_df <- data.frame(
                cell_ID = character(0),
                sdimx = numeric(0),
                sdimy = numeric(0)
            )
            spatlocs <- createSpatLocsObj(empty_df, verbose = FALSE)
            expect_equal(nrow(spatlocs), 0)
        })

        it("validates matrix dimensions", {
            bad_matrix <- matrix(1:3, ncol = 1)  # only 1 column
            expect_error(createSpatLocsObj(bad_matrix),
                         "at least 2 numeric columns")
        })
    })

    # Edge cases ----
    describe("Edge Cases", {

        it("handles large coordinate values", {
            large_df <- data.frame(
                cell_ID = c("cell_1", "cell_2"),
                sdimx = c(1e6, 2e6),
                sdimy = c(1e6, 2e6)
            )
            spatlocs <- createSpatLocsObj(large_df, verbose = FALSE)

            expect_equal(nrow(spatlocs), 2)
            coords_dt <- spatlocs[]
            expect_equal(coords_dt$sdimx, large_df$sdimx)
        })

        it("handles negative coordinates", {
            neg_df <- data.frame(
                cell_ID = c("cell_1", "cell_2"),
                sdimx = c(-100.5, -200.0),
                sdimy = c(-50.3, -150.8)
            )
            spatlocs <- createSpatLocsObj(neg_df, verbose = FALSE)

            expect_equal(nrow(spatlocs), 2)
            coords_dt <- spatlocs[]
            expect_equal(coords_dt$sdimx, neg_df$sdimx)
        })

        it("handles special characters in cell_ID", {
            special_df <- data.frame(
                cell_ID = c("cell-1", "cell.2", "cell_3"),
                sdimx = c(100, 200, 300),
                sdimy = c(50, 150, 250)
            )
            spatlocs <- createSpatLocsObj(special_df, verbose = FALSE)

            expect_equal(nrow(spatlocs), 3)
            coords_dt <- spatlocs[]
            expect_equal(coords_dt$cell_ID, special_df$cell_ID)
        })

        it("handles decimal coordinates", {
            decimal_df <- data.frame(
                cell_ID = c("cell_1", "cell_2"),
                sdimx = c(100.123456, 200.987654),
                sdimy = c(50.555555, 150.111111)
            )
            spatlocs <- createSpatLocsObj(decimal_df, verbose = FALSE)

            coords_dt <- spatlocs[]
            expect_equal(coords_dt$sdimx, decimal_df$sdimx)
            expect_equal(coords_dt$sdimy, decimal_df$sdimy)
        })
    })

    # Integration tests ----
    describe("Integration", {

        it("can be plotted without error", {
            spatlocs <- createSpatLocsObj(test_df_2d, verbose = FALSE)
            expect_no_error(terra::plot(spatlocs))
        })

        it("works with affine transformations", {
            spatlocs <- createSpatLocsObj(test_df_2d, verbose = FALSE)
            # Create simple translation matrix
            trans_matrix <- matrix(c(1, 0, 0, 0, 1, 0, 100, 200, 1), nrow = 3)

            expect_no_error(affine(spatlocs, trans_matrix))
        })

        it("preserves object integrity after XY modification", {
            spatlocs <- createSpatLocsObj(test_df_2d, verbose = FALSE)

            # Extract and modify coordinates
            coords <- XY(spatlocs)
            modified_coords <- coords + 100
            XY(spatlocs) <- modified_coords

            # Verify object is still valid
            expect_no_error(validObject(spatlocs))
            expect_s4_class(spatlocs, "spatLocsObj")
        })

        it("maintains consistency with data.table operations", {
            spatlocs <- createSpatLocsObj(test_df_2d, verbose = FALSE)

            # Should be able to perform data.table operations on extracted data
            coords_dt <- spatlocs[]
            sdimx <- NULL # NSE
            subset_dt <- coords_dt[sdimx > 150]

            expect_s3_class(subset_dt, "data.table")
            expect_true(nrow(subset_dt) < nrow(coords_dt))
        })
    })

    # File I/O tests (if applicable) ----
    describe("File I/O", {

        it("can be created from file path", {
            # Create temporary file
            temp_file <- tempfile(fileext = ".csv")
            data.table::fwrite(test_df_2d, temp_file)

            spatlocs <- createSpatLocsObj(temp_file, verbose = FALSE)

            expect_s4_class(spatlocs, "spatLocsObj")
            expect_equal(nrow(spatlocs), 4)

            # Clean up
            unlink(temp_file)
        })

        it("handles non-existent file paths", {
            expect_error(createSpatLocsObj("non_existent_file.csv"),
                         "path.*does not exist")
        })
    })
})

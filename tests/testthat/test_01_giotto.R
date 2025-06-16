
describe("Core Giotto Object Creation and Validation", {

    it("creates empty Giotto object without errors", {
        g <- giotto()
        expect_s4_class(g, "giotto")
        expect_true(validObject(g))
    })

    g <- giotto()

    it("automatically generates giottoInstructions", {
        g <- giotto()
        expect_s3_class(instructions(g), "giottoInstructions")
    })

    test_that("Warning is thrown for defaults when no data or actives set", {
        g <- giotto()
        expect_warning(set_default_spat_unit(g),
            regexp = "No default for spat_unit could be set")
        expect_warning(set_default_feat_type(g),
            regexp = "No default for feat_type could be set")
    })

    it("handles active spatial unit and feature type setting", {
        g <- giotto()

        # Initially should be NULL
        expect_null(activeSpatUnit(g))
        expect_null(activeFeatType(g))

        # Set actives
        activeSpatUnit(g) <- "aggregate"
        activeFeatType(g) <- "rna"

        # should now have no warnings despite lack of expr or spat_info
        # and pull the assigned active settings.
        expect_identical(activeSpatUnit(g), "aggregate")
        expect_identical(activeFeatType(g), "rna")

        # setting a specific value (does not even need the gobject)
        test_that("specific input spat_unit returns unmodified", {
            expect_identical("test_value",
                set_default_spat_unit(spat_unit = "test_value"))
        })
        test_that("specific input feat_type returns unmodified", {
            expect_identical("test_value",
                set_default_feat_type(feat_type = "test_value"))
        })
    })

    it("creates Giotto object from expression matrix", {
        expr_matrix <- matrix(1:100, nrow = 10)
        colnames(expr_matrix) <- paste0("cell", 1:10)
        rownames(expr_matrix) <- paste0("gene", 1:10)

        g <- createGiottoObject(
            expression = expr_matrix,
            verbose = FALSE
        )

        expect_s4_class(g, "giotto")
        expect_true(validObject(g))
        expect_equal(ncol(getExpression(g)), 10)
        expect_equal(nrow(getExpression(g)), 10)
    })
})

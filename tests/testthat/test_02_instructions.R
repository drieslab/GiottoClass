### TESTS FUNCTIONS FOR CREATING/CHANGING GIOTTO INSTRUCTIONS
# -------------------------------------------------------------- #

# CREATE GIOTTO OBJECT FOR TESTING

describe("giottoInstructions", {

    describe("object creation", {
        it("can be created with createGiottoInstructions", {
            instrs <- createGiottoInstructions(
                show_plot = TRUE,
                return_plot = NULL,
                save_plot = FALSE,
                save_dir = NULL,
                plot_format = "png",
                dpi = 300,
                units = NULL,
                height = NULL,
                width = NULL,
                is_docker = FALSE,
                plot_count = 0,
                fiji_path = NULL
            )
            expect_true(inherits(instrs, "giottoInstructions"))
            expect_type(instrs, "list")
        })

        it("can be create with instructions()", {
            instrs <- instructions()
            expect_true(inherits(instrs, "giottoInstructions"))
            expect_type(instrs, "list")
        })

        it("is automatically created with a giotto object", {
            g <- giotto()
            instrs <- g@instructions
            expect_true(inherits(instrs, "giottoInstructions"))
            expect_type(instrs, "list")
            expect_identical(instrs, createGiottoInstructions())
        })
    })

    describe("reading values", {

        g <- giotto()

        it("can retrieve specific instructions with instructions()", {
            expect_type(instructions(g, param = "show_plot"), "logical")
            expect_type(instructions(g, param = "plot_format"), "character")
            expect_type(instructions(g, param = "dpi"), "double")
        })

        it("can be retrieved from a gobject with instructions()", {
            instrs <- instructions(g)
            expect_true(inherits(instrs, "giottoInstructions"))
            expect_type(instrs, "list")
        })

    })


    describe("setting values", {

        g <- giotto()

        it("can replace specific instructions values with instructions<-()", {
            # check defaults
            expect_true(instructions(g, "show_plot"))
            expect_false(instructions(g, "save_plot"))

            # change value
            instructions(g,
                         param = c("show_plot", "save_plot")
            ) <- list(FALSE, TRUE)

            expect_false(instructions(g, param = "show_plot"))
            expect_true(instructions(g, param = "save_plot"))
        })

        it("can be replaced in a gobject with instructions<-()", {
            # check defaults
            expect_true(instructions(g, "show_plot"))
            expect_false(instructions(g, "save_plot"))

            instrs <- createGiottoInstructions(
                show_plot = FALSE,
                save_plot = TRUE
            )

            instructions(g) <- instrs
            expect_false(instructions(g, param = "show_plot"))
            expect_true(instructions(g, param = "save_plot"))
        })

    })


})
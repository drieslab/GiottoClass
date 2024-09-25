### TESTS FUNCTIONS FOR CREATING/CHANGING GIOTTO INSTRUCTIONS
# -------------------------------------------------------------- #

# CREATE GIOTTO OBJECT FOR TESTING

# no need for python env in these tests
options("giotto.use_conda" = FALSE)
# silence deprecated internals
rlang::local_options(lifecycle_verbosity = "quiet")

suppressWarnings({
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
})


expression_matrix <- matrix(1:100, nrow = 10)
colnames(expression_matrix) <- paste0("cell", 1:10)
colnames(expression_matrix) <- paste0("feature", 1:10)

gobject <- createGiottoObject(
    expression = expression_matrix,
    instructions = instrs,
    verbose = FALSE
)

# createGiottoInstructions
test_that("Instructions are created", {
    expect_type(instrs, "list")
})

# read GiottoInstructions
test_that("instructions reads a few giotto object params correctly", {
    expect_type(instructions(gobject, param = "show_plot"), "logical")
    expect_type(instructions(gobject, param = "plot_format"), "character")
    expect_type(instructions(gobject, param = "dpi"), "double")
})

# show GiottoInstructions
test_that("instructions returns expected list", {
    expect_type(instructions(gobject), "list")
})

# change GiottoInstructions
instructions(gobject,
             param = c("show_plot", "save_plot")) <- list(FALSE, TRUE)

test_that("change GiottoInstructions changes instruction params in object", {
    expect_false(instructions(gobject, param = "show_plot"))
    expect_true(instructions(gobject, param = "save_plot"))
})

# replace GiottoInstructions
instructions(gobject) <- instrs

test_that("instructions returns object instructions to original", {
    expect_true(instructions(gobject, param = "show_plot"))
    expect_false(instructions(gobject, param = "save_plot"))
})

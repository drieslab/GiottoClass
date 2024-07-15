g <- GiottoData::loadGiottoMini("viz")

test <- tempdir()

test_that("gobject can be saved and loaded - qs", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    saveGiotto(g, dir = test, method = "qs", overwrite = TRUE, verbose = FALSE)
    g2 <<- loadGiotto(file.path(test, "saveGiottoDir"))

    expect_true(methods::validObject(g2))
})

test_that("gobject an be ovewritten and loaded - qs", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    saveGiotto(g2, dir = test, method = "qs", overwrite = TRUE, verbose = FALSE)
    g3 <- loadGiotto(file.path(test, "saveGiottoDir"))

    expect_true(methods::validObject(g3))
})

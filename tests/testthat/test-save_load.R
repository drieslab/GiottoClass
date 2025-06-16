
describe("Giotto Object Save/Load", {

    g <- test_data$viz
    empty <- giotto()
    test <- tempdir()

    describe("qs method", {

        # register cleanup
        on.exit(unlink(file.path(test, "saveGiottoDir"), recursive = TRUE),
                add = TRUE)

        it("it can be saved and loaded", {
            rlang::local_options(lifecycle_verbosity = "quiet")
            saveGiotto(g, dir = test, method = "qs", overwrite = TRUE,
                       verbose = FALSE)
            g2 <<- loadGiotto(file.path(test, "saveGiottoDir"))

            expect_true(methods::validObject(g2))
        })

        it("it can be overwritten and loaded", {
            rlang::local_options(lifecycle_verbosity = "quiet")
            saveGiotto(g2, dir = test, method = "qs", overwrite = TRUE,
                       verbose = FALSE)
            g3 <- loadGiotto(file.path(test, "saveGiottoDir"))

            expect_true(methods::validObject(g3))
        })

    })

    describe("RDS method", {

        # register cleanup
        on.exit(unlink(file.path(test, "saveGiottoDir"), recursive = TRUE),
                add = TRUE)

        it("it can be saved and loaded", {
            rlang::local_options(lifecycle_verbosity = "quiet")
            saveGiotto(g, dir = test, overwrite = TRUE, verbose = FALSE)
            g2 <<- loadGiotto(file.path(test, "saveGiottoDir"))

            expect_true(methods::validObject(g2))
        })

        it("it can be overwritten and loaded", {
            rlang::local_options(lifecycle_verbosity = "quiet")
            saveGiotto(g2, dir = test, overwrite = TRUE, verbose = FALSE)
            g3 <- loadGiotto(file.path(test, "saveGiottoDir"))

            expect_true(methods::validObject(g3))
        })

    })

})

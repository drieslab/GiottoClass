
describe("Giotto Object Save/Load", {

    g <- test_data$viz
    empty <- giotto()
    test <- tempdir()

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

    describe("zero attribute SpatVector", {

        # register cleanup
        on.exit(unlink(file.path(test, c("zeroattr_poly", "zeroattr_pts",
                                 "badnames")),
                       recursive = TRUE),
                add = TRUE)

        it("polygons round trip without attributes", {
            sv <- terra::vect("POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))")
            terra::values(sv) <- NULL
            g <- giotto()
            g@spatial_info <- list(cell = new("giottoPolygon",
                                              name = "cell", spatVector = sv))

            saveGiotto(g, dir = test, foldername = "zeroattr_poly",
                       overwrite = TRUE, verbose = FALSE)
            g2 <- loadGiotto(file.path(test, "zeroattr_poly"), verbose = FALSE)
            sv2 <- g2@spatial_info$cell@spatVector

            expect_equal(nrow(sv2), nrow(sv))
            # writing a shapefile with an empty attribute table adds a
            # placeholder "FID" field that must not be read back in
            expect_equal(terra::ncol(sv2), 0L)
            expect_length(names(sv2), 0L)
        })

        it("reports a names file that does not match the attributes", {
            gp <- test_data$gpoly
            g <- setGiotto(giotto(), gp, verbose = FALSE)

            saveGiotto(g, dir = test, foldername = "badnames",
                       overwrite = TRUE, verbose = FALSE)
            # corrupt the saved column names
            nfile <- file.path(test, "badnames", "SpatialInfo",
                               paste0(objName(gp), "_spatInfo_spatVector",
                                      "_names.txt"))
            write.table(c("poly_ID", "extra"), file = nfile,
                        col.names = FALSE, row.names = FALSE)

            expect_error(
                loadGiotto(file.path(test, "badnames"), verbose = FALSE),
                sprintf("2 saved column names for %d attributes",
                        terra::ncol(gp[]))
            )
        })

        it("points round trip without attributes", {
            pv <- terra::vect(cbind(c(1, 2, 3), c(1, 2, 3)), type = "points")
            terra::values(pv) <- NULL
            g <- giotto()
            g@feat_info <- list(rna = new("giottoPoints",
                                          feat_type = "rna", spatVector = pv))

            saveGiotto(g, dir = test, foldername = "zeroattr_pts",
                       overwrite = TRUE, verbose = FALSE)
            g2 <- loadGiotto(file.path(test, "zeroattr_pts"), verbose = FALSE)
            pv2 <- g2@feat_info$rna@spatVector

            expect_equal(nrow(pv2), nrow(pv))
            expect_equal(terra::ncol(pv2), 0L)
            expect_length(names(pv2), 0L)
        })

    })

})

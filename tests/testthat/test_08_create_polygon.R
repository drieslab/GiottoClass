
describe("giottoPolygon", {

    json <- system.file("extdata/toy_poly.geojson", package = "GiottoClass")
    df <- system.file("extdata/viz_interactive_select.csv", package = "GiottoClass")
    mask_multi <- system.file("extdata/toy_mask_multi.tif", package = "GiottoClass")
    mask_single <- system.file("extdata/toy_mask_single.tif", package = "GiottoClass")

    b <- data.table::data.table( # source coords for toy data
        sdimx = c(5, 10, 20, 10, 25, 22, 6),
        sdimy = c(5, 3, 8, 10, 3, 10, 8),
        cell_ID = letters[seq(7)]
    )

    # x <- createGiottoPolygon(polyStamp(a, b))[]
    # x$idx <- rev(4:10)
    # r <- terra::rast(ncol = 100, nrow = 100)
    # ext(r) <- c(0, 30, 0, 13)
    # mask_multi <- terra::rasterize(x, r, field = "idx")
    # terra::writeRaster(mask_multi,
    #                    filename = "inst/extdata/toy_mask_multi.tif",
    #                    gdal = "COG",
    #                    overwrite = TRUE)
    # mask_single <- terra::rasterize(x, r)
    # terra::writeRaster(mask_single,
    #                    filename = "inst/extdata/toy_mask_single.tif",
    #                    gdal = "COG",
    #                    overwrite = TRUE)
    # terra::writeVector(x,
    #                    filename = "inst/extdata/toy_poly.shp",
    #                    overwrite = TRUE)

    describe("GeoJSON Input", {

        it("can be created with generic", {
            p <- createGiottoPolygon(json,
                calc_centroids = TRUE, verbose = FALSE)
            expect_true(inherits(p, "giottoPolygon"))
            expect_equal(nrow(p), 7)
            expect_equal(terra::geomtype(p[]), "polygons")
        })

        it("can be created with specific function", {
            p <- createGiottoPolygonsFromGeoJSON(json,
                calc_centroids = TRUE, verbose = FALSE)
            expect_true(inherits(p, "giottoPolygon"))
            expect_equal(nrow(p), 7)
            expect_equal(terra::geomtype(p[]), "polygons")
        })

    })

    describe("data.frame Input", {

        it("can be created from data.frame", {
            poly_df <- data.table::fread(df)
            data.table::setDF(poly_df)
            # drop first V1 col
            poly_df <- poly_df[, c("x", "y", "name")]
            p <- createGiottoPolygon(poly_df,
                calc_centroids = TRUE, verbose = FALSE)
            expect_equal(nrow(p), 3)
            expect_equal(spatIDs(p), c("polygon1", "polygon2", "polygon3"))
            expect_equal(terra::geomtype(p[]), "polygons")
            expect_no_error(validObject(p))
            expect_s4_class(p, "giottoPolygon")
            expect_setequal(poly_df$name, spatIDs(p))
        })

        it("can be created from data.table", {
            poly_dt <- data.table::fread(df)
            # drop first V1 col
            poly_dt <- poly_dt[, c("x", "y", "name")]
            p <- createGiottoPolygon(poly_dt,
                                     calc_centroids = TRUE, verbose = FALSE)
            expect_equal(nrow(p), 3)
            expect_equal(spatIDs(p), c("polygon1", "polygon2", "polygon3"))
            expect_equal(terra::geomtype(p[]), "polygons")
            expect_no_error(validObject(p))
            expect_s4_class(p, "giottoPolygon")
            expect_setequal(poly_dt$name, spatIDs(p))
        })

        it("can be created from dataframe specific function", {
            poly_dt <- data.table::fread(df)
            # drop first V1 col
            poly_dt <- poly_dt[, c("x", "y", "name")]
            p <- createGiottoPolygonsFromDfr(poly_dt,
                calc_centroids = TRUE, verbose = FALSE)
            expect_equal(nrow(p), 3)
            expect_equal(spatIDs(p), c("polygon1", "polygon2", "polygon3"))
            expect_equal(terra::geomtype(p[]), "polygons")
            expect_no_error(validObject(p))
            expect_s4_class(p, "giottoPolygon")
            expect_setequal(poly_dt$name, spatIDs(p))
        })

        it("can be created from path to csv", {
            suppressWarnings(p <- createGiottoPolygon(df, verbose = FALSE))
            expect_equal(nrow(p), 3)
            expect_equal(spatIDs(p), c("polygon1", "polygon2", "polygon3"))
            expect_equal(terra::geomtype(p[]), "polygons")
        })

    })

    describe("Mask Image Input", {

        describe("raster value-derived naming", {
            it("can be created with generic", {
                p <- createGiottoPolygon(mask_multi,
                                         calc_centroids = TRUE, verbose = FALSE)
                expect_true(inherits(p, "giottoPolygon"))
                expect_equal(nrow(p), 7)
                expect_equal(terra::geomtype(p[]), "polygons")
            })

            it("can be created with specific function - multi values", {
                # expect 7 polys
                p <- createGiottoPolygonsFromMask(mask_multi,
                    flip_vertical = FALSE,
                    flip_horizontal = FALSE,
                    shift_horizontal_step = FALSE,
                    shift_vertical_step = FALSE,
                    ID_fmt = "id_test_%03d",
                    name = "multi_test",
                    verbose = FALSE,
                    calc_centroids = TRUE
                )
                expect_true(inherits(p, "giottoPolygon"))
                expect_equal(nrow(p), 7)
                expect_equal(terra::geomtype(p[]), "polygons")
                centroids_dt <- data.table::as.data.table(centroids(p), geom = "XY")
                expect_identical(centroids_dt$poly_ID, sprintf("id_test_%03d", 4:10))
                # compare against reversed values from spatlocs DT since values were applied
                # in reverse (from idx col)
                expect_identical(round(centroids_dt$x), rev(b$sdimx))
                expect_identical(round(centroids_dt$y), rev(b$sdimy))
            })

            it("can be created with specific function - single value", {
                # expect 5 polys
                p <- createGiottoPolygonsFromMask(mask_single,
                    calc_centroids = TRUE,
                    flip_vertical = FALSE,
                    flip_horizontal = FALSE,
                    shift_horizontal_step = FALSE,
                    shift_vertical_step = FALSE,
                    ID_fmt = "id_test_%03d",
                    name = "single_test",
                    verbose = FALSE
                )
                expect_true(inherits(p, "giottoPolygon"))
                expect_equal(nrow(p), 5)
                expect_equal(terra::geomtype(p[]), "polygons")
                centroids_dt <- data.table::as.data.table(centroids(p), geom = "XY")
                expect_identical(centroids_dt$poly_ID, sprintf("id_test_%03d", seq(1:5)))
                # ordering from readin for "single" is ordered first by row then col
                data.table::setkeyv(b, c("sdimy", "sdimx")) # note that y ordering is still inverted
                singles_x <- c(b$sdimx[6], mean(b$sdimx[c(7, 5)]), mean(b$sdimx[c(3, 4)]), b$sdimx[c(1, 2)])
                singles_y <- c(b$sdimy[6], mean(b$sdimy[c(7, 5)]), mean(b$sdimy[c(3, 4)]), b$sdimy[c(1, 2)])

                expect_identical(round(centroids_dt$x, digits = 1), singles_x)
                expect_identical(round(centroids_dt$y, digits = 1), singles_y)
            })
        })

        describe("specified naming", {
            it("can be created with specific function - multi values", {
                # expect 7 polys
                p <- createGiottoPolygonsFromMask(mask_multi,
                    flip_vertical = FALSE,
                    flip_horizontal = FALSE,
                    shift_horizontal_step = FALSE,
                    shift_vertical_step = FALSE,
                    poly_IDs = letters[1:7],
                    ID_fmt = "id_test_%03d", # ignored
                    name = "multi_test",
                    verbose = FALSE,
                    calc_centroids = TRUE
                )
                expect_true(inherits(p, "giottoPolygon"))
                expect_equal(nrow(p), 7)
                expect_equal(terra::geomtype(p[]), "polygons")
                expect_identical(p$poly_ID, letters[1:7])
                centroids_dt <- data.table::as.data.table(centroids(p), geom = "XY")
                data.table::setkey(b, cell_ID)
                expect_identical(round(centroids_dt$x), rev(b$sdimx))
                expect_identical(round(centroids_dt$y), rev(b$sdimy))
            })

            it("can be created with specific function - single value", {
                # expect 5 polys
                p <- createGiottoPolygonsFromMask(mask_single,
                    flip_vertical = FALSE,
                    flip_horizontal = FALSE,
                    shift_horizontal_step = FALSE,
                    shift_vertical_step = FALSE,
                    poly_IDs = LETTERS[1:5],
                    ID_fmt = "id_test_%03d", # ignored
                    name = "single_test",
                    verbose = FALSE,
                    calc_centroids = TRUE
                )
                expect_true(inherits(p, "giottoPolygon"))
                expect_equal(nrow(p), 5)
                expect_equal(terra::geomtype(p[]), "polygons")
                expect_identical(p$poly_ID, LETTERS[1:5])
                centroids_dt <- data.table::as.data.table(centroids(p), geom = "XY")
                data.table::setkeyv(b, c("sdimy", "sdimx")) # note that y ordering is still inverted
                singles_x <- c(b$sdimx[6], mean(b$sdimx[c(7, 5)]), mean(b$sdimx[c(3, 4)]), b$sdimx[c(1, 2)])
                singles_y <- c(b$sdimy[6], mean(b$sdimy[c(7, 5)]), mean(b$sdimy[c(3, 4)]), b$sdimy[c(1, 2)])

                expect_identical(round(centroids_dt$x, digits = 1), singles_x)
                expect_identical(round(centroids_dt$y, digits = 1), singles_y)
            })
        })

    })

    describe("SpatVector Input", {
        it("can be created from SpatVector", {
            sv <- test_data$gpoly[]

            p <- createGiottoPolygon(sv, verbose = FALSE)
            expect_no_error(validObject(p))
            expect_s4_class(p, "giottoPolygon")
            expect_setequal(sv$poly_ID, spatIDs(p))
        })
    })

})


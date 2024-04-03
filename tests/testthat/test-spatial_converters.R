# raster is needed for sp converter
if (!requireNamespace("raster", quietly = TRUE)) {
    install.packages("raster")
}

library(checkmate)

# create dummy data
points_dt <- data.table::data.table(
    feat_ID = letters[1:10],
    x = 1:10,
    y = 1:10
)

terra_gpoints <- createGiottoPoints(points_dt)
terra_gpolys <- GiottoData::loadSubObjectMini("giottoPolygon")[1:20]

# drop = TRUE is expected as default for each of these

# convert from terra ####
test_that("terra to sf works", {
    sf_out <- as.sf(terra_gpoints)
    expect_class(sf_out, "sf")

    gpoints_out <- as.sf(terra_gpoints, drop = FALSE)
    expect_class(gpoints_out, "giottoPoints")
    expect_class(gpoints_out@spatVector, "sf")

    gpoly_out <- as.sf(terra_gpolys, drop = FALSE)
    expect_class(gpoly_out, "giottoPolygon")
    expect_class(gpoly_out@spatVector, "sf")
    expect_class(gpoly_out@spatVectorCentroids, "sf")
    expect_class(gpoly_out@overlaps$rna, "sf")
})

test_that("terra to sp works", {
    sp_out <- as.sp(terra_gpoints)
    expect_class(sp_out, "Spatial")

    gpoints_out <- as.sp(terra_gpoints, drop = FALSE)
    expect_class(gpoints_out, "giottoPoints")
    expect_class(gpoints_out@spatVector, "Spatial")

    # will usually warn about few vertices in the overlaps
    suppressWarnings(gpoly_out <- as.sp(terra_gpolys, drop = FALSE))
    expect_class(gpoly_out, "giottoPolygon")
    expect_class(gpoly_out@spatVector, "Spatial")
    expect_class(gpoly_out@spatVectorCentroids, "Spatial")
    expect_class(gpoly_out@overlaps$rna, "Spatial") # TODO this should be points
})

test_that("terra to stars works", {
    stars_out <- as.stars(terra_gpoints)
    expect_class(stars_out, "stars")

    gpoints_out <- as.stars(terra_gpoints, drop = FALSE)
    expect_class(gpoints_out, "giottoPoints")
    expect_class(gpoints_out@spatVector, "stars")

    gpoly_out <- as.stars(terra_gpolys, drop = FALSE)
    expect_class(gpoly_out, "giottoPolygon")
    expect_class(gpoly_out@spatVector, "stars")
    expect_class(gpoly_out@spatVectorCentroids, "stars")
    expect_class(gpoly_out@overlaps$rna, "stars")
})


# convert from sf ####
sf_gpoints <- as.sf(terra_gpoints, drop = FALSE)
sf_gpolys <- as.sf(terra_gpolys, drop = FALSE)

test_that("sf to terra works", {
    sf_out <- as.terra(sf_gpoints)
    expect_class(sf_out, "SpatVector")

    gpoints_out <- as.terra(sf_gpoints, drop = FALSE)
    expect_class(gpoints_out, "giottoPoints")
    expect_class(gpoints_out@spatVector, "SpatVector")

    gpoly_out <- as.terra(sf_gpolys, drop = FALSE)
    expect_class(gpoly_out, "giottoPolygon")
    expect_class(gpoly_out@spatVector, "SpatVector")
    expect_class(gpoly_out@spatVectorCentroids, "SpatVector")
    expect_class(gpoly_out@overlaps$rna, "SpatVector")
})

test_that("sf to sp works", {
    sp_out <- as.sp(sf_gpoints)
    expect_class(sp_out, "Spatial")

    gpoints_out <- as.sp(sf_gpoints, drop = FALSE)
    expect_class(gpoints_out, "giottoPoints")
    expect_class(gpoints_out@spatVector, "Spatial")

    gpoly_out <- as.sp(sf_gpolys, drop = FALSE)
    expect_class(gpoly_out, "giottoPolygon")
    expect_class(gpoly_out@spatVector, "Spatial")
    expect_class(gpoly_out@spatVectorCentroids, "Spatial")
    expect_class(gpoly_out@overlaps$rna, "Spatial") # TODO this should be points
})

test_that("sf to stars works", {
    stars_out <- as.stars(sf_gpoints)
    expect_class(stars_out, "stars")

    gpoints_out <- as.stars(sf_gpoints, drop = FALSE)
    expect_class(gpoints_out, "giottoPoints")
    expect_class(gpoints_out@spatVector, "stars")

    gpoly_out <- as.stars(sf_gpolys, drop = FALSE)
    expect_class(gpoly_out, "giottoPolygon")
    expect_class(gpoly_out@spatVector, "stars")
    expect_class(gpoly_out@spatVectorCentroids, "stars")
    expect_class(gpoly_out@overlaps$rna, "stars")
})

# convert from stars ####
stars_gpoints <- as.stars(terra_gpoints, drop = FALSE)
stars_gpolys <- as.stars(terra_gpolys, drop = FALSE)

test_that("stars to sf works", {
    sf_out <- as.sf(stars_gpoints)
    expect_class(sf_out, "sf")

    gpoints_out <- as.sf(stars_gpoints, drop = FALSE)
    expect_class(gpoints_out, "giottoPoints")
    expect_class(gpoints_out@spatVector, "sf")

    gpoly_out <- as.sf(stars_gpolys, drop = FALSE)
    expect_class(gpoly_out, "giottoPolygon")
    expect_class(gpoly_out@spatVector, "sf")
    expect_class(gpoly_out@spatVectorCentroids, "sf")
    expect_class(gpoly_out@overlaps$rna, "sf")
})

test_that("stars to sp works", {
    sp_out <- as.sp(stars_gpoints)
    expect_class(sp_out, "Spatial")

    gpoints_out <- as.sp(stars_gpoints, drop = FALSE)
    expect_class(gpoints_out, "giottoPoints")
    expect_class(gpoints_out@spatVector, "Spatial")

    gpoly_out <- as.sp(stars_gpolys, drop = FALSE)
    expect_class(gpoly_out, "giottoPolygon")
    expect_class(gpoly_out@spatVector, "Spatial")
    expect_class(gpoly_out@spatVectorCentroids, "Spatial")
    expect_class(gpoly_out@overlaps$rna, "Spatial")
})

test_that("stars to terra works", {
    stars_out <- as.terra(stars_gpoints)
    expect_class(stars_out, "SpatVector")

    gpoints_out <- as.terra(stars_gpoints, drop = FALSE)
    expect_class(gpoints_out, "giottoPoints")
    expect_class(gpoints_out@spatVector, "SpatVector")

    gpoly_out <- as.terra(stars_gpolys, drop = FALSE)
    expect_class(gpoly_out, "giottoPolygon")
    expect_class(gpoly_out@spatVector, "SpatVector")
    expect_class(gpoly_out@spatVectorCentroids, "SpatVector")
    expect_class(gpoly_out@overlaps$rna, "SpatVector")
})



# convert from sp ####
sp_gpoints <- as.sp(terra_gpoints, drop = FALSE)
sp_gpolys <- as.sp(terra_gpolys, drop = FALSE)

test_that("sp to sf works", {
    sf_out <- as.sf(sp_gpoints)
    expect_class(sf_out, "sf")

    gpoints_out <- as.sf(sp_gpoints, drop = FALSE)
    expect_class(gpoints_out, "giottoPoints")
    expect_class(gpoints_out@spatVector, "sf")

    gpoly_out <- as.sf(sp_gpolys, drop = FALSE)
    expect_class(gpoly_out, "giottoPolygon")
    expect_class(gpoly_out@spatVector, "sf")
    expect_class(gpoly_out@spatVectorCentroids, "sf")
    expect_class(gpoly_out@overlaps$rna, "sf")
})

test_that("sp to terra works", {
    sp_out <- as.terra(sp_gpoints)
    expect_class(sp_out, "SpatVector")

    gpoints_out <- as.terra(sp_gpoints, drop = FALSE)
    expect_class(gpoints_out, "giottoPoints")
    expect_class(gpoints_out@spatVector, "SpatVector")

    # will usually warn about few vertices in the overlaps
    suppressWarnings(gpoly_out <- as.terra(sp_gpolys, drop = FALSE))
    expect_class(gpoly_out, "giottoPolygon")
    expect_class(gpoly_out@spatVector, "SpatVector")
    expect_class(gpoly_out@spatVectorCentroids, "SpatVector")
    expect_class(gpoly_out@overlaps$rna, "SpatVector") # TODO this should be points
})

test_that("sp to stars works", {
    stars_out <- as.stars(sp_gpoints)
    expect_class(stars_out, "stars")

    gpoints_out <- as.stars(sp_gpoints, drop = FALSE)
    expect_class(gpoints_out, "giottoPoints")
    expect_class(gpoints_out@spatVector, "stars")

    gpoly_out <- as.stars(sp_gpolys, drop = FALSE)
    expect_class(gpoly_out, "giottoPolygon")
    expect_class(gpoly_out@spatVector, "stars")
    expect_class(gpoly_out@spatVectorCentroids, "stars")
    expect_class(gpoly_out@overlaps$rna, "stars")
})

gpoints <- GiottoData::loadSubObjectMini("giottoPoints")
gpoly <- GiottoData::loadSubObjectMini("giottoPolygon")
sl <- GiottoData::loadSubObjectMini("spatLocsObj")
svpoints <- gpoints[]
svpolys <- gpoly[]

# get
test_that("XY works for giottoPoints", {
    checkmate::expect_matrix(XY(gpoints), ncols = 2L, nrows = nrow(gpoints))
    checkmate::expect_matrix(XY(gpoints[0]), ncols = 2L, nrows = nrow(gpoints[0]))
})

test_that("XY works for giottoPolygon", {
    checkmate::expect_matrix(XY(gpoly), ncols = 2L, nrows = nrow(terra::geom(gpoly[])))
    checkmate::expect_matrix(XY(gpoly[0]), ncols = 2L, nrows = nrow(terra::geom(gpoly[0][])))
})

test_that("XY works for spatLocsObj", {
    checkmate::expect_matrix(XY(sl), ncols = 2L, nrows = nrow(sl))
    checkmate::expect_matrix(XY(sl[0]), ncols = 2L, nrows = nrow(sl[0]))
})

test_that("XY works for SpatVector", {
    # points
    checkmate::expect_matrix(XY(svpoints), ncols = 2L, nrows = nrow(svpoints))
    checkmate::expect_matrix(XY(svpoints[0]), ncols = 2L, nrows = nrow(svpoints[0]))
    # polys
    checkmate::expect_matrix(XY(svpolys), ncols = 2L, nrows = nrow(terra::geom(svpolys)))
    checkmate::expect_matrix(XY(svpolys[0]), ncols = 2L, nrows = nrow(terra::geom(svpolys[0])))
})

# set
test_that("XY replaces for giottoPoints", {
    temp <- gpoints
    temp0 <- gpoints[0]
    xy <- XY(gpoints)
    xy0 <- XY(gpoints[0])

    xy <- xy * 2
    xy0 <- xy0 * 2

    XY(temp) <- xy
    XY(temp0) <- xy0

    expect_true(nrow(temp) == nrow(gpoints) && ncol(temp) == ncol(gpoints))
    expect_true(identical(max(ext(temp)), max(ext(gpoints)) * 2))
    expect_true(nrow(temp0) == nrow(gpoints[0]))
})

test_that("XY replaces for giottoPolygons", {
    temp <- gpoly
    temp0 <- gpoly[0]
    xy <- XY(gpoly)
    xy0 <- XY(gpoly[0])

    xy <- xy * 2
    xy0 <- xy0 * 2

    XY(temp) <- xy
    XY(temp0) <- xy0

    expect_true(nrow(temp) == nrow(gpoly) && ncol(temp) == ncol(gpoly))
    expect_true(identical(max(ext(temp)), max(ext(gpoly)) * 2))
    expect_true(nrow(temp0) == nrow(gpoly[0]))
})

test_that("XY replaces for SpatVector", {
    temp_points <- svpoints
    temp_points0 <- svpoints[0]
    temp_polys <- svpolys
    temp_polys0 <- svpolys[0]
    # points
    XY(temp_points, geomtype = "points") <- XY(svpoints) * 2
    XY(temp_points0, geomtype = "points") <- XY(svpoints[0]) * 2
    # polys
    XY(temp_polys, geomtype = "polygons") <- XY(svpolys) * 2
    XY(temp_polys0, geomtype = "polygons") <- XY(svpolys[0]) * 2

    expect_true(nrow(temp_points) == nrow(svpoints) && ncol(temp_points) == ncol(svpoints))
    expect_true(nrow(temp_polys) == nrow(svpolys) && ncol(temp_polys) == ncol(svpolys))
    expect_true(identical(max(ext(temp_points)), max(ext(svpoints)) * 2))
    expect_true(identical(max(ext(temp_polys)), max(ext(svpolys)) * 2))
    expect_true(nrow(temp_points0) == nrow(svpoints[0]))
    expect_true(nrow(temp_polys0) == nrow(svpolys[0]))
})

test_that("XY replaces for spatlocs", {
    temp <- sl
    temp0 <- sl[0]
    xy <- XY(sl)
    xy0 <- XY(sl[0])

    xy <- xy * 2
    xy0 <- xy0 * 2

    XY(temp) <- xy
    XY(temp0) <- xy0

    expect_true(nrow(temp) == nrow(sl) && ncol(temp) == ncol(sl))
    expect_true(identical(max(ext(temp)), max(ext(sl)) * 2))
    expect_true(nrow(temp0) == nrow(sl[0]))
})

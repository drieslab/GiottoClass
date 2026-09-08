# spatRelate: filter form on giottoSpatial.
#
# `spatRelate()` is the filter-form complement to `relate()` -- returns x
# narrowed to features satisfying the spatial predicate against any feature
# of y, rather than the relation matrix.

gpoly_full <- GiottoData::loadSubObjectMini("giottoPolygon")
gpoints_full <- GiottoData::loadSubObjectMini("giottoPoints")
# Trim to a small subset so tests are quick and the y reference is contained.
gpoly_small <- gpoly_full[1:10]
gpoints_small <- gpoints_full[1:200]


test_that("spatRelate(giottoSpatial, giottoSpatial) returns same class as x", {
    res <- spatRelate(gpoints_small, gpoly_small, relation = "intersects")
    expect_s4_class(res, "giottoPoints")
    res2 <- spatRelate(gpoly_small, gpoly_small, relation = "intersects")
    expect_s4_class(res2, "giottoPolygon")
})


test_that("spatRelate() narrows to features satisfying the predicate", {
    res <- spatRelate(gpoints_small, gpoly_small, relation = "intersects")
    expect_lte(nrow(res), nrow(gpoints_small))
    # The narrowed set should match the unique x indices from relate(pairs)
    pairs <- relate(gpoints_small, gpoly_small,
        relation = "intersects",
        pairs = TRUE, output = "data.table", use_names = FALSE
    )
    expect_equal(nrow(res), length(unique(pairs$x)))
})


test_that("spatRelate() default relation is 'intersects'", {
    res_default <- spatRelate(gpoints_small, gpoly_small)
    res_explicit <- spatRelate(gpoints_small, gpoly_small,
        relation = "intersects")
    expect_equal(nrow(res_default), nrow(res_explicit))
})


test_that("spatRelate() predicate choice affects the narrowing", {
    # `within` and `disjoint` should partition the input on the same y
    n_within <- nrow(spatRelate(gpoints_small, gpoly_small, "within"))
    n_disjoint <- nrow(spatRelate(gpoints_small, gpoly_small, "disjoint"))
    # NB: in general within + disjoint can exceed input n due to boundary
    # cases; assert each is bounded by the input count.
    expect_lte(n_within, nrow(gpoints_small))
    expect_lte(n_disjoint, nrow(gpoints_small))
    # They should report different counts for non-trivial inputs.
    expect_false(identical(n_within, n_disjoint))
})


test_that("spatRelate() with no matches returns an empty giottoSpatial", {
    # Build a small polygon far outside the data extent so nothing matches.
    far_poly <- createGiottoPolygon(
        terra::vect("POLYGON ((1e9 1e9, 1e9 1e9.1, 1e9.1 1e9.1, 1e9.1 1e9, 1e9 1e9))"),
        verbose = FALSE
    )
    res <- spatRelate(gpoints_small, far_poly, relation = "intersects")
    expect_s4_class(res, "giottoPoints")
    expect_equal(nrow(res), 0L)
})


# y-form cascade ####
#
# SpatVector is the canonical y in memory, because terra is the engine and
# consumes it directly; WKT and sf coerce into it. This is the mirror image
# of GiottoDisk's cascade, which canonicalizes to WKT because it embeds the
# string in SQL. Each side canonicalizes to what its engine consumes.

.ids_of <- function(x) {
    v <- terra::values(x[])
    v[[intersect(c("cell_ID", "feat_ID", "poly_ID"), names(v))[[1L]]]]
}

test_that("every y-form gives the same answer for the same geometry", {
    y_gpoly <- gpoly_small[1:3]
    y_sv <- y_gpoly[]
    y_wkt <- terra::geom(y_sv, wkt = TRUE)

    from_gpoly <- spatRelate(gpoints_small, y_gpoly)
    from_sv <- spatRelate(gpoints_small, y_sv)
    from_wkt <- spatRelate(gpoints_small, y_wkt)

    expect_identical(.ids_of(from_sv), .ids_of(from_gpoly))
    expect_identical(.ids_of(from_wkt), .ids_of(from_gpoly))
})

test_that("each y-form preserves x's carrier class", {
    y_sv <- gpoly_small[1:3][]
    y_wkt <- terra::geom(y_sv, wkt = TRUE)
    for (y in list(y_sv, y_wkt)) {
        expect_s4_class(spatRelate(gpoints_small, y), "giottoPoints")
        expect_s4_class(spatRelate(gpoly_small, y), "giottoPolygon")
    }
})

test_that("a WKT y must be parseable geometry", {
    expect_error(spatRelate(gpoints_small, "not wkt"))
})

test_that("engine is validated rather than silently ignored", {
    y_sv <- gpoly_small[1:3][]
    expect_no_error(spatRelate(gpoints_small, y_sv, engine = "terra"))
    expect_no_error(spatRelate(gpoints_small, y_sv, engine = NULL))
    # "auto" means "best available", and in memory that IS terra, so
    # engine-agnostic caller code passing auto must work on both sides.
    # GiottoDisk resolves auto as sedona > duckdb > terra.
    expect_no_error(spatRelate(gpoints_small, y_sv, engine = "auto"))
    # naming a SQL engine explicitly cannot be honoured here, so it fails
    # rather than silently returning a terra answer
    expect_error(spatRelate(gpoints_small, y_sv, engine = "sedona"),
        "not available")
    expect_error(spatRelate(gpoints_small, y_sv, engine = "duckdb"),
        "not available")
})


# relate() coercion ####

test_that("relate() works on a spatLocsObj x", {
    # Regression: the spatLocsObj as.points() coercion used to be
    # overwritten one line later by `x_use <- x[]`, which for a
    # spatLocsObj is a data.table, so this errored with
    # 'x = "data.table", y = "SpatVector"'.
    sl <- createSpatLocsObj(
        data.frame(cell_ID = c("a", "b"), sdimx = c(5, 5e6),
            sdimy = c(5, 5e6)),
        verbose = FALSE)
    sv <- terra::vect(
        rbind(c(0, 0), c(10, 0), c(10, 10), c(0, 10), c(0, 0)),
        type = "polygons")
    gp <- createGiottoPolygon(sv, verbose = FALSE)

    res <- relate(sl, gp, relation = "intersects")
    expect_s3_class(res, "data.table")
    expect_identical(res$x, "a")

    # and the filter form on the same input keeps the carrier
    expect_s4_class(spatRelate(sl, gp), "spatLocsObj")
})

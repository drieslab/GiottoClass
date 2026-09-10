
describe(".make_valid", {
    # one polygon per class of invalidity terra/GEOS can produce.
    # `bowtie` and `spike` are invalid but repairable; `twovert` through
    # `emptypoly` are repaired into non-polygonal geometries, which is what
    # bare `terra::makeValid()` drops without dropping their attribute rows.
    w <- c(
        good = "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
        bowtie = "POLYGON ((0 0, 2 2, 2 0, 0 2, 0 0))",
        spike = "POLYGON ((0 0, 4 0, 4 4, 0 4, 0 0, 6 0, 0 0))",
        dupnode = "POLYGON ((0 0, 1 0, 1 0, 1 1, 0 1, 0 0))",
        twovert = "POLYGON ((0 0, 1 1, 0 0))",
        collinear = "POLYGON ((0 0, 1 0, 2 0, 3 0, 0 0))",
        dupring = "POLYGON ((0 0, 1 1, 0 0, 0 0))",
        emptypoly = "POLYGON EMPTY"
    )
    keepers <- c("good", "bowtie", "spike", "dupnode")

    make_sv <- function(which = names(w)) {
        sv <- terra::vect(unname(w[which]))
        sv$poly_ID <- which
        terra::set.crs(sv, NULL)
        sv
    }

    it("keeps geometries and attributes aligned", {
        sv <- make_sv()
        bare <- terra::makeValid(sv)
        expect_true(nrow(bare) < nrow(terra::values(bare)))

        res <- .make_valid(sv, verbose = FALSE)
        expect_equal(nrow(res), nrow(terra::values(res)))
        expect_identical(res$poly_ID, keepers)
    })

    it("repairs rather than discards self-intersections", {
        # buffer(0) would return a single lobe with an area of 1
        res <- .make_valid(make_sv(c("good", "bowtie")), verbose = FALSE)
        expect_equal(nrow(res), 2)
        expect_equal(suppressWarnings(terra::expanse(res, transform = FALSE)), c(1, 2))
    })

    it("reports the dropped poly_IDs", {
        expect_message(
            .make_valid(make_sv(), verbose = TRUE),
            "twovert"
        )
    })

    it("handles vectors with nothing to drop", {
        sv <- make_sv(keepers)
        expect_silent(res <- .make_valid(sv, verbose = FALSE))
        expect_identical(res$poly_ID, keepers)
    })

    it("handles a vector where every geometry is degenerate", {
        # buffer(x, 0) returns an empty SpatVector here rather than one empty
        # geometry per input, so the 1:1 probe cannot be used directly
        sv <- make_sv(c("twovert", "collinear"))
        res <- suppressWarnings(.make_valid(sv, verbose = FALSE))
        expect_equal(nrow(res), 0)
        expect_equal(nrow(terra::values(res)), 0)
    })

    it("passes non-polygon geometries through", {
        pts <- terra::vect(cbind(seq_len(3), seq_len(3)))
        expect_identical(terra::geomtype(.make_valid(pts)), "points")
        expect_equal(nrow(.make_valid(pts)), 3)
    })
})


describe(".dt_to_spatvector_polygon degenerate ring warning", {
    dt <- data.table::data.table(
        poly_ID = c(rep("cellA", 4L), rep("cellB", 2L), rep("cellC", 4L)),
        geom = c(rep(1L, 4L), rep(2L, 2L), rep(3L, 4L)),
        part = 1L,
        hole = 0L,
        x = c(0, 1, 1, 0, 10, 11, 20, 21, 21, 20),
        y = c(0, 0, 1, 1, 0, 1, 0, 0, 1, 1)
    )

    it("names polygons that cannot form a closed ring", {
        expect_warning(
            sv <- .dt_to_spatvector_polygon(dt),
            "cellB"
        )
        # still created: the vertex count is a report, not the filter
        expect_equal(nrow(sv), 3)
    })

    it("stays quiet when every ring is closeable", {
        ok <- dt[dt$poly_ID != "cellB", ]
        ok$geom <- c(rep(1L, 4L), rep(2L, 4L))
        expect_silent(.dt_to_spatvector_polygon(ok))
    })
})

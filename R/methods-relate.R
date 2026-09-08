# docs ----------------------------------------------------------- #
#' @title Spatial relationships between geometries
#' @name relate
#' @description `relate()` returns a logical matrix indicating the presence or
#'  absence of a specific spatial relationships between the geometries in
#'  x and y.
#' @param x spatial object with records to test
#' @param y spatial object records to test relations against
#' @param ... additional args to pass
#' @param output character. `"data.table"` or `"matrix"`. `"data.table"` is
#' only possible when `pairs=TRUE`
#' @param use_names logical. If `TRUE`, `pairs=TRUE`, and `output="data.table"`
#' the IDs of the geometries will be used.
#' @returns `data.table` if `output="data.table"`. `matrix` if `output="matrix"`
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#' activeSpatUnit(g) <- "aggregate"
#' sl <- g[["spatial_locs"]][[1]]
#' gpoints <- g[["feat_info"]][[1]]
#' gpoly <- g[["spatial_info"]][[1]]
#'
#' res1 <- relate(gpoints, gpoly, relation = "intersects")
#' res2 <- relate(gpoints, gpoly, relation = "intersects", use_names = FALSE)
#'
#' selection <- system.file("extdata/viz_interactive_select.csv",
#'     package = "GiottoClass"
#' )
#' select_polys <- createGiottoPolygon(
#'     # we don't want the rownumber column.
#'     data.table::fread(selection)[, c("x", "y", "name")]
#' )
#' res <- relate(g, select_polys, relation = "intersects")
#' g[, res[y == "polygon1", x]]
#' g[, res[y == "polygon2", x]]
#' g[, res[y == "polygon3", x]]
NULL
# ---------------------------------------------------------------- #

# Coerce one side of a relate() call to the SpatVector terra expects.
#
# A `spatLocsObj` has to go through `as.points()` — its `[]` returns the
# coordinates data.table, which terra has no relate method for. Doing this
# in one place is what keeps that coercion from being clobbered: the
# previous form set `x_use <- as.points(x)` and then immediately overwrote
# it with `x_use <- x[]`, since a spatLocsObj is also a giottoSpatial.
#' @keywords internal
#' @noRd
.as_relate_geom <- function(x) {
    if (inherits(x, "spatLocsObj")) return(as.points(x))
    if (inherits(x, "giottoSpatial")) return(x[])
    x
}

#' @rdname relate
#' @inheritParams terra::relate
#' @export
setMethod(
    "relate", signature(x = "giottoSpatial", y = "giottoSpatial"),
    function(
        x, y, relation,
        pairs = TRUE,
        na.rm = TRUE,
        output = c("data.table", "matrix"),
        use_names = TRUE,
        ...) {
        output <- match.arg(output, choices = c("data.table", "matrix"))

        x_use <- .as_relate_geom(x)
        y_use <- .as_relate_geom(y)

        res <- relate(x_use, y_use, relation, pairs, na.rm, ...)

        if (pairs && output == "data.table") {
            res <- data.table::as.data.table(res)
            data.table::setnames(res, new = c("x", "y"))

            if (use_names) {
                x_ids <- .get_ids(x, res$x)
                y_ids <- .get_ids(y, res$y)
                res[, x := x_ids]
                res[, y := y_ids]
            }
        }

        return(res)
    }
)

#' @rdname relate
#' @param what character. Which type of spatial data in the `giotto` object to
#' relate. One of "polygon", "spatlocs", "points"
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param spat_locs_name name of spatlocs to use if what = "spatlocs"
#' @export
setMethod(
    "relate", signature(x = "giotto", y = "giottoSpatial"),
    function(
        x, y, ...,
        what = c("polygon", "spatlocs", "points"),
        spat_unit = NULL,
        feat_type = NULL,
        spat_locs_name = NULL) {
        what <- match.arg(what, c("polygon", "spatlocs", "points"))

        spat_unit <- set_default_spat_unit(x, spat_unit = spat_unit)
        feat_type <- set_default_feat_type(
            x,
            spat_unit = spat_unit, feat_type = feat_type
        )

        x <- switch(what,
            "polygon" = {
                getPolygonInfo(x,
                    name = spat_unit,
                    return_giottoPolygon = TRUE
                )
            },
            "points" = {
                getFeatureInfo(x,
                    feat_type = feat_type,
                    return_giottoPoints = TRUE
                )
            },
            "spatlocs" = {
                getSpatialLocations(x,
                    spat_unit = spat_unit,
                    output = "spatLocsObj",
                    name = spat_locs_name
                )
            }
        )

        res <- relate(x, y, ...)
        return(res)
    }
)






# spatRelate ####

# TODO: audit internal `relate()` call sites across the suite and swap to
# `spatRelate()` where the pattern is "narrow x by predicate against y"
# rather than "consume the relation table/matrix".

#' @title Spatial relationship as a filter
#' @name spatRelate
#' @description
#' Narrow `x` to features that satisfy a spatial predicate against any feature
#' of `y`. Returns an object of the same class as `x` rather than a relation
#' matrix -- the "filter form" complement to [relate()].
#'
#' @param x spatial object to be narrowed (rows kept where predicate holds
#'   against any feature of `y`)
#' @param y query geometry; the form depends on the method (giottoSpatial,
#'   SpatVector, sf, character WKT)
#' @param relation `character`. Spatial predicate. One of `"intersects"`,
#'   `"touches"`, `"crosses"`, `"overlaps"`, `"within"`, `"contains"`,
#'   `"covers"`, `"covered_by"`, `"disjoint"`. Default `"intersects"`.
#' @param ... additional args to pass
#' @returns an object of the same class as `x`, narrowed to features
#'   satisfying the predicate against any feature of `y`
#' @seealso [relate()] for the relation-matrix / pairs form;
#'   [spatQuery()] for the gobject-level multi-filter pipeline.
#' @examples
#' g <- GiottoData::loadGiottoMini("vizgen")
#' gpoly <- g[["spatial_info"]][[1]]
#' gpoints <- g[["feat_info"]][[1]]
#'
#' # narrow points to those that intersect at least one polygon
#' pts_in_polys <- spatRelate(gpoints, gpoly, relation = "intersects")
NULL

# y-form cascade ####
#
# `SpatVector` is the canonical `y` here, because terra is the engine and
# consumes it directly; every other accepted form coerces into it and
# recurses. This is the mirror image of {GiottoDisk}'s cascade, which
# canonicalizes to WKT `character` because it embeds the string in SQL
# (`ST_Within(geom, ...)`). Each side canonicalizes to what its own engine
# consumes -- the two are deliberately opposite, not drifting.
#
# In-memory has one engine, so `engine` is accepted for signature parity
# with the backed methods and rejected unless it names terra. Swallowing it
# in `...` would let engine-agnostic caller code silently get terra when it
# asked for sedona.

# Accepted engines for the in-memory methods.
#
# `"auto"` is accepted, not rejected: it means "pick the best available",
# and in memory the only engine is terra, so auto is satisfied. {GiottoDisk}
# resolves auto as sedona > duckdb > terra, so engine-agnostic caller code
# that passes auto has to work on both sides. Naming a SQL engine
# explicitly is what fails here, because that request cannot be honoured
# rather than merely being redundant.
#' @keywords internal
#' @noRd
.assert_relate_engine <- function(engine) {
    if (is.null(engine) || engine %in% c("auto", "terra")) {
        return(invisible(engine))
    }
    stop("[spatRelate] engine '", engine, "' is not available for an ",
        "in-memory object; terra is the only engine here. Use ",
        "engine = \"auto\" (or omit it), or move the data to a backed ",
        "store, where {GiottoDisk} offers sedona and duckdb.",
        call. = FALSE)
}

#' @rdname spatRelate
#' @param engine `character` or `NULL`. Predicate engine. In-memory objects
#'   support only `"terra"` (the default when `NULL`); backed stores in
#'   \pkg{GiottoDisk} additionally offer `"sedona"` and `"duckdb"`.
#' @export
setMethod(
    "spatRelate", signature(x = "giottoSpatial", y = "SpatVector"),
    function(x, y, relation = "intersects", engine = NULL, ...) {
        .assert_relate_engine(engine)
        # The single terra call site for the in-memory spatRelate family;
        # every other y-form coerces and recurses into here. `relate()` is
        # not reused because its own y-side is `giottoSpatial`-only, and
        # widening the relation-matrix API is a separate concern from
        # widening the filter form.
        pairs <- terra::relate(
            .as_relate_geom(x), y,
            relation = relation, pairs = TRUE, ...
        )
        if (nrow(pairs) == 0L) {
            return(x[integer(0L)])
        }
        x[sort(unique(pairs[, 1L]))]
    }
)

#' @rdname spatRelate
#' @export
setMethod(
    "spatRelate", signature(x = "giottoSpatial", y = "character"),
    function(x, y, relation = "intersects", ...) {
        # WKT. Geometry only -- attributes are not carried through.
        checkmate::assert_character(y, min.len = 1L, any.missing = FALSE)
        spatRelate(x, terra::vect(y), relation = relation, ...)
    }
)

#' @rdname spatRelate
#' @export
setMethod(
    "spatRelate", signature(x = "giottoSpatial", y = "sf"),
    function(x, y, relation = "intersects", ...) {
        package_check("sf", repository = "CRAN")
        spatRelate(x, terra::vect(y), relation = relation, ...)
    }
)

#' @rdname spatRelate
#' @export
setMethod(
    "spatRelate", signature(x = "giottoSpatial", y = "giottoSpatial"),
    function(x, y, relation = "intersects", ...) {
        # Unwrap to the canonical form and delegate, so there is exactly
        # one relate() call site for the in-memory family.
        spatRelate(x, .as_relate_geom(y), relation = relation, ...)
    }
)


# internals ####

.get_ids <- function(x, idx) {
    ids <- x[idx]$cell_ID
    ids <- ids %null% x[idx]$feat_ID
    ids <- ids %null% x[idx]$poly_ID
    if (is.null(ids)) {
        stop("no ids found for an object. `use_names` might not work",
            call. = FALSE
        )
    }
    return(ids)
}

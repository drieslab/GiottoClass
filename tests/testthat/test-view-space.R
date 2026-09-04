# Tests for giottoView + giottoSpace classes, the resolver engine, and
# the JIT view/space integration in getters.
#
# silence deprecated internal functions
rlang::local_options(lifecycle_verbosity = "quiet")
options("giotto.use_conda" = FALSE)

# fixture — visium mini with leiden clusters in metadata
.fixture_giotto <- function() {
    g <- GiottoData::loadGiottoMini("visium", verbose = FALSE)
    updateGiottoObject(g)
}

# fixture — two-sample giottoMulti with each child's cells under distinct
# local IDs (s2_* in the second child) so global IDs are unambiguous.
.fixture_gmulti <- function() {
    g1 <- .fixture_giotto()
    g2 <- .fixture_giotto()
    cm2 <- pDataDT(g2)
    new_ids <- paste0("s2_", cm2$cell_ID)
    g2@cell_metadata$cell$rna@metaDT$cell_ID <- new_ids
    sl <- g2@spatial_locs$cell$raw
    sl@coordinates$cell_ID <- new_ids
    g2@spatial_locs$cell$raw <- sl
    sv <- g2@spatial_info$cell@spatVector
    sv$poly_ID <- new_ids
    g2@spatial_info$cell@spatVector <- sv
    g2@spatial_info$cell@unique_ID_cache <- new_ids
    e <- g2@expression$cell$rna$raw
    colnames(e@exprMat) <- new_ids
    g2@expression$cell$rna$raw <- e
    g2@cell_ID$cell <- new_ids
    createGiottoMulti(list(a = g1, b = g2))
}


# --- giottoView class ------------------------------------------------------

test_that("giottoView() constructs empty view", {
    v <- giottoView()
    expect_s4_class(v, "giottoView")
    expect_length(v@steps, 0L)
    expect_true(is.na(v@name))
    expect_true(is.na(v@space))
})

test_that("giottoView(space = ...) records space reference", {
    v <- giottoView(space = "atlas")
    expect_identical(v@space, "atlas")
})

test_that("subset() records a filter step", {
    v <- giottoView() |> subset(cluster == "A")
    expect_length(v@steps, 1L)
    expect_identical(v@steps[[1L]]$type, "filter")
    expect_identical(v@steps[[1L]]$predicate, 'cluster == "A"')
})

test_that("subset() forwards scope_args", {
    v <- giottoView() |> subset(x > 0, spat_unit = "cell",
        feat_type = "rna", negate = TRUE)
    sa <- v@steps[[1L]]$scope_args
    expect_identical(sa$spat_unit, "cell")
    expect_identical(sa$feat_type, "rna")
    expect_true(sa$negate)
})

test_that("crop() records a crop step", {
    v <- giottoView() |> crop(c(0, 100, 0, 100))
    expect_length(v@steps, 1L)
    expect_identical(v@steps[[1L]]$type, "crop")
    # Q7: numeric extents are normalized to WKT at record time, so the
    # terra (xmin, xmax, ymin, ymax) convention is applied exactly once
    expect_type(v@steps[[1L]]$region, "character")
    expect_equal(terra::ext(terra::vect(v@steps[[1L]]$region))[],
        terra::ext(c(0, 100, 0, 100))[])
    expect_identical(v@steps[[1L]]$relation, "intersects")
})

test_that("crop() with custom relation records it", {
    v <- giottoView() |> crop(c(0, 100, 0, 100), relation = "within")
    expect_identical(v@steps[[1L]]$relation, "within")
})

test_that("crop() with polygon region works", {
    poly <- terra::vect(rbind(
        c(4000, -5000), c(5500, -5000),
        c(5500, -3500), c(4000, -3500),
        c(4000, -5000)
    ), type = "polygons")
    v <- giottoView() |> crop(poly)
    expect_identical(v@steps[[1L]]$type, "crop")
    # SpatVector polygon regions are normalized to WKT at ingest so the
    # recipe is serializable (no live C++ pointer). See methods-view.R
    # .normalize_crop_region.
    expect_type(v@steps[[1L]]$region, "character")
})

test_that("materialize with polygon crop narrows by region", {
    g <- .fixture_giotto()
    # build a polygon equivalent to a known extent
    poly <- terra::vect(rbind(
        c(4000, -5000), c(5500, -5000),
        c(5500, -3500), c(4000, -3500),
        c(4000, -5000)
    ), type = "polygons")
    # expected: cells whose centroid is within the polygon's AABB
    sl <- getSpatialLocations(g, output = "data.table")
    expected <- sum(sl$sdimx >= 4000 & sl$sdimx <= 5500 &
                    sl$sdimy >= -5000 & sl$sdimy <= -3500)

    giottoView(g, "poly_crop") <- giottoView() |> crop(poly)
    g2 <- materialize(g, "poly_crop")
    expect_equal(nrow(pDataDT(g2)), expected)
})

test_that("selectSamples() records a samples step", {
    v <- giottoView() |> selectSamples("a", "b")
    expect_length(v@steps, 1L)
    expect_identical(v@steps[[1L]]$type, "samples")
    expect_identical(v@steps[[1L]]$samples, c("a", "b"))
})

test_that("steps compose in order under pipe", {
    v <- giottoView() |>
        subset(x > 0) |>
        crop(c(0, 100, 0, 100)) |>
        selectSamples("a")
    expect_length(v@steps, 3L)
    expect_identical(v@steps[[1L]]$type, "filter")
    expect_identical(v@steps[[2L]]$type, "crop")
    expect_identical(v@steps[[3L]]$type, "samples")
})


# --- giottoSpace class -----------------------------------------------------

test_that("giottoSpace() constructs empty sample-anonymous space", {
    s <- giottoSpace()
    expect_s4_class(s, "giottoSpace")
    expect_named(s@samples, ":default:")
    expect_length(s@samples[[1L]], 0L)
})

test_that("giottoSpace(sample) constructs sample-bound space", {
    s <- giottoSpace("sample_a")
    expect_named(s@samples, "sample_a")
})

test_that("transform generics record on giottoSpace", {
    M <- diag(c(1, 1, 1))
    s <- giottoSpace() |> spin(30) |> affine(M) |> spatShift(dx = 10)
    steps <- s@samples[[1L]]
    expect_length(steps, 3L)
    expect_identical(vapply(steps, function(x) x$op, character(1L)),
        c("spin", "affine", "spatShift"))
})

test_that("spin/affine record (0,0) anchor by default", {
    s <- giottoSpace() |> spin(45)
    expect_equal(s@samples[[1L]][[1L]]$args$x0, 0)
    expect_equal(s@samples[[1L]][[1L]]$args$y0, 0)
})

test_that("user-supplied anchor overrides default", {
    s <- giottoSpace() |> spin(45, x0 = 100, y0 = 200)
    expect_equal(s@samples[[1L]][[1L]]$args$x0, 100)
    expect_equal(s@samples[[1L]][[1L]]$args$y0, 200)
})


# --- + composition on giottoSpace -----------------------------------------

test_that("+ on same-sample concatenates step lists", {
    s <- (giottoSpace("a") |> spin(30)) +
         (giottoSpace("a") |> spatShift(dx = 10))
    expect_length(s@samples, 1L)
    expect_length(s@samples[["a"]], 2L)
})

test_that("+ on different samples merges keyed", {
    s <- (giottoSpace("a") |> spin(30)) +
         (giottoSpace("b") |> spin(45))
    expect_named(s@samples, c("a", "b"))
    expect_length(s@samples[["a"]], 1L)
    expect_length(s@samples[["b"]], 1L)
})

test_that("+ giottoView+giottoView errors (not yet implemented)", {
    expect_error(giottoView() + giottoView(),
        "not yet implemented")
})


# --- Accessors -------------------------------------------------------------

test_that("giottoView<- slots in and giottoView() retrieves by name", {
    g <- giotto()
    v <- giottoView() |> subset(cluster == "A")
    giottoView(g, "tumor") <- v
    expect_identical(giottoViews(g), "tumor")
    out <- giottoView(g, "tumor")
    expect_s4_class(out, "giottoView")
    expect_identical(out@name, "tumor")
})

test_that("giottoView(g, name) <- NULL removes", {
    g <- giotto()
    giottoView(g, "a") <- giottoView()
    giottoView(g, "b") <- giottoView()
    giottoView(g, "a") <- NULL
    expect_identical(giottoViews(g), "b")
})

test_that("giottoSpace accessor and lookup", {
    g <- giotto()
    giottoSpace(g, "atlas") <- giottoSpace() |> spin(30)
    expect_identical(giottoSpaces(g), "atlas")
    out <- giottoSpace(g, "atlas")
    expect_s4_class(out, "giottoSpace")
    expect_identical(out@name, "atlas")
})

test_that("missing slotted name errors clearly", {
    g <- giotto()
    expect_error(giottoView(g, "missing"), "no slotted giottoView")
    expect_error(giottoSpace(g, "missing"), "no slotted giottoSpace")
})


# --- Migration on updateGiottoObject --------------------------------------

test_that("updateGiottoObject() adds @view and @spaces for pre-0.7.0", {
    g <- giotto()
    g@versions$gclass <- "0.6.0"
    g <- updateGiottoObject(g)
    expect_true(methods::.hasSlot(g, "view"))
    expect_true(methods::.hasSlot(g, "spaces"))
    expect_null(g@view)
    expect_null(g@spaces)
})

test_that("save/load round-trip preserves @view and @spaces", {
    g <- giotto()
    giottoView(g, "demo") <- giottoView() |> subset(x > 0)
    giottoSpace(g, "tilted") <- giottoSpace() |> spin(45)

    td <- tempfile("gv-")
    on.exit(unlink(file.path(dirname(td), basename(td)), recursive = TRUE),
        add = TRUE)

    saveGiotto(g, foldername = basename(td), dir = dirname(td),
        verbose = FALSE, overwrite = TRUE)
    g2 <- loadGiotto(file.path(dirname(td), basename(td)), verbose = FALSE)

    expect_identical(giottoViews(g2), "demo")
    expect_identical(giottoSpaces(g2), "tilted")
})


# --- Coordinator: dataTableCoordinator ---------------------------------------

test_that("dataTableCoordinator() constructs", {
    p <- dataTableCoordinator()
    expect_s4_class(p, "dataTableCoordinator")
    expect_s4_class(p, "viewCoordinator")
})

test_that(".default_view_coordinator returns dataTableCoordinator for in-memory", {
    g <- giotto()
    p <- GiottoClass:::.default_view_coordinator(g)
    expect_s4_class(p, "dataTableCoordinator")
})

test_that("prepareIds() for dataTableCoordinator is identity", {
    ids <- c("a", "b", "c")
    expect_identical(prepareIds(dataTableCoordinator(), ids), ids)
})

test_that("defaultViewCoordinator() defaults to dataTableCoordinator for any source", {
    # ANY signature method
    p <- defaultViewCoordinator(NULL)  # NULL is technically ANY
    # NULL source path goes through .default_view_coordinator's null check
    # before reaching the generic; here we just verify the ANY method
    # returns dataTableCoordinator for an unknown source class
    expect_s4_class(defaultViewCoordinator(structure(list(),
        class = "_unknown_source_class_")), "dataTableCoordinator")
})

test_that("defaultViewCoordinator() S4 dispatch is registrable from downstream", {
    # Simulate GiottoDisk-style registration: define a fake source class
    # and add a method returning a different coordinator. After cleanup,
    # the method is removed so other tests aren't affected.
    setClass("_test_fake_source_", representation = "list",
        where = globalenv())
    on.exit(removeClass("_test_fake_source_", where = globalenv()),
        add = TRUE)

    fake_src <- new("_test_fake_source_")
    # default (no method registered) returns dataTableCoordinator
    expect_s4_class(defaultViewCoordinator(fake_src), "dataTableCoordinator")

    # register a method
    setMethod("defaultViewCoordinator",
        signature(source = "_test_fake_source_"),
        function(source, ...) {
            # use the existing dataTableCoordinator subclass as a stand-in
            new("dataTableCoordinator", misc = list(marker = "downstream"))
        },
        where = globalenv())
    on.exit(removeMethod("defaultViewCoordinator", "_test_fake_source_",
        where = globalenv()), add = TRUE)

    p <- defaultViewCoordinator(fake_src)
    expect_s4_class(p, "dataTableCoordinator")
    expect_identical(p@misc$marker, "downstream")
})


# --- materialize() end-to-end on visium mini ------------------------------

test_that("materialize() with empty view returns equivalent gobject", {
    g <- .fixture_giotto()
    giottoView(g, "empty") <- giottoView()
    g2 <- materialize(g, "empty")
    expect_equal(nrow(pDataDT(g2)), nrow(pDataDT(g)))
    expect_equal(
        nrow(getSpatialLocations(g2, output = "data.table")),
        nrow(getSpatialLocations(g, output = "data.table"))
    )
})

test_that("materialize() narrows tabular slots by subset predicate", {
    g <- .fixture_giotto()
    n_total <- length(spatIDs(g))
    n_target <- sum(pDataDT(g)$leiden_clus == "1")

    giottoView(g, "c1") <- giottoView() |> subset(leiden_clus == "1")
    g2 <- materialize(g, "c1")

    expect_lt(nrow(pDataDT(g2)), n_total)
    expect_equal(nrow(pDataDT(g2)), n_target)
    expect_equal(ncol(getExpression(g2, output = "matrix")), n_target)
})

test_that("materialize() narrows spatial slots via cell_ID cascade", {
    g <- .fixture_giotto()
    giottoView(g, "c1") <- giottoView() |> subset(leiden_clus == "1")
    g2 <- materialize(g, "c1")

    n_filter <- nrow(pDataDT(g2))
    expect_equal(
        nrow(getSpatialLocations(g2, output = "data.table")), n_filter)
    expect_equal(
        length(spatIDs(getPolygonInfo(g2, return_giottoPolygon = TRUE))),
        n_filter)
})

test_that("materialize() with %in% and env-resident value works (NSE)", {
    g <- .fixture_giotto()
    targets <- c("1", "2")
    giottoView(g, "c12") <- giottoView() |> subset(leiden_clus %in% targets)
    g2 <- materialize(g, "c12")
    expected <- sum(pDataDT(g)$leiden_clus %in% targets)
    expect_equal(nrow(pDataDT(g2)), expected)
})

test_that("materialize() with expression-column predicate routes via spatValues", {
    g <- .fixture_giotto()
    # pick a gene known to be in the panel by literal name to avoid NSE
    gene <- "Gfap"
    skip_if_not(gene %in% rownames(getExpression(g, output = "matrix")),
        sprintf("gene %s not in panel", gene))

    expected <- sum(getExpression(g, output = "matrix")[gene, ] > 0)
    giottoView(g, "gfap_pos") <- giottoView() |> subset(Gfap > 0)
    g2 <- materialize(g, "gfap_pos")
    expect_equal(nrow(pDataDT(g2)), expected)
})

test_that("materialize() with crop narrows via spatLocs extent", {
    g <- .fixture_giotto()
    sl <- getSpatialLocations(g, output = "data.table")
    ext <- c(4000, 5500, -5000, -3500)
    expected <- sum(sl$sdimx >= ext[1L] & sl$sdimx <= ext[2L] &
                    sl$sdimy >= ext[3L] & sl$sdimy <= ext[4L])

    giottoView(g, "ext_crop") <- giottoView() |> crop(ext)
    g2 <- materialize(g, "ext_crop")
    expect_equal(nrow(pDataDT(g2)), expected)
    expect_equal(
        nrow(getSpatialLocations(g2, output = "data.table")), expected)
})

test_that("materialize() with space transforms spatial coords only", {
    g <- .fixture_giotto()
    giottoSpace(g, "tilted") <- giottoSpace() |> spin(30)
    giottoView(g, "empty") <- giottoView()

    g2 <- materialize(g, "empty", space = "tilted")
    sl_native <- getSpatialLocations(g, output = "data.table")
    sl_tilted <- getSpatialLocations(g2, output = "data.table")

    expect_equal(nrow(sl_tilted), nrow(sl_native))
    expect_false(isTRUE(all.equal(sl_tilted$sdimx, sl_native$sdimx)))
    # tabular slots unchanged in row count
    expect_equal(nrow(pDataDT(g2)), nrow(pDataDT(g)))
})

test_that("materialize() with filter + space combines both", {
    g <- .fixture_giotto()
    giottoSpace(g, "tilted") <- giottoSpace() |> spin(30)
    giottoView(g, "c12") <- giottoView() |> subset(leiden_clus %in% c("1", "2"))

    g2 <- materialize(g, "c12", space = "tilted")

    expected <- sum(pDataDT(g)$leiden_clus %in% c("1", "2"))
    sl_tilted <- getSpatialLocations(g2, output = "data.table")
    sl_native <- getSpatialLocations(g, output = "data.table")
    expect_equal(nrow(sl_tilted), expected)
    expect_false(isTRUE(all.equal(sl_tilted$sdimx, sl_native$sdimx[1:expected])))
})


# --- JIT getter integration -----------------------------------------------

test_that("getCellMetadata respects view = name", {
    g <- .fixture_giotto()
    giottoView(g, "x") <- giottoView() |> subset(leiden_clus == "1")
    n_base <- nrow(getCellMetadata(g, output = "data.table"))
    n_view <- nrow(getCellMetadata(g, view = "x", output = "data.table"))
    expect_lt(n_view, n_base)
    expect_equal(n_view, sum(pDataDT(g)$leiden_clus == "1"))
})

test_that("getCellMetadata rejects ad-hoc view objects (character-only contract)", {
    g <- .fixture_giotto()
    v <- giottoView() |> subset(leiden_clus == "2")
    expect_error(
        getCellMetadata(g, view = v, output = "data.table"),
        "Must be of type 'string'"
    )
})

test_that("getExpression view narrows columns", {
    g <- .fixture_giotto()
    giottoView(g, "x") <- giottoView() |> subset(leiden_clus == "1")
    n_base <- ncol(getExpression(g, output = "matrix"))
    n_view <- ncol(getExpression(g, view = "x", output = "matrix"))
    expect_lt(n_view, n_base)
})

test_that("getSpatialLocations view + space combined", {
    g <- .fixture_giotto()
    giottoView(g, "x") <- giottoView() |> subset(leiden_clus == "1")
    giottoSpace(g, "tilted") <- giottoSpace() |> spin(30)

    sl_base <- getSpatialLocations(g, output = "data.table")
    sl_v <- getSpatialLocations(g, view = "x", output = "data.table")
    sl_s <- getSpatialLocations(g, space = "tilted", output = "data.table")
    sl_vs <- getSpatialLocations(g, view = "x", space = "tilted",
        output = "data.table")

    expect_equal(nrow(sl_v), sum(pDataDT(g)$leiden_clus == "1"))
    expect_false(isTRUE(all.equal(sl_s$sdimx, sl_base$sdimx)))
    expect_equal(nrow(sl_vs), nrow(sl_v))
    # rotated x for the view+space combo differs from the unrotated view
    expect_false(isTRUE(all.equal(sl_vs$sdimx, sl_v$sdimx)))
})

test_that("getPolygonInfo view narrows; both output forms agree", {
    g <- .fixture_giotto()
    giottoView(g, "x") <- giottoView() |> subset(leiden_clus == "1")
    gp_full <- getPolygonInfo(g, view = "x", return_giottoPolygon = TRUE)
    sv_full <- getPolygonInfo(g, view = "x")  # SpatVector default
    expect_equal(length(spatIDs(gp_full)), nrow(sv_full))
})

test_that("getFeatureMetadata view is no-op (feat-keyed)", {
    g <- .fixture_giotto()
    giottoView(g, "x") <- giottoView() |> subset(leiden_clus == "1")
    n_base <- nrow(getFeatureMetadata(g, output = "data.table"))
    n_view <- nrow(getFeatureMetadata(g, view = "x", output = "data.table"))
    expect_equal(n_view, n_base)
})

test_that("getter without view/space returns unchanged baseline", {
    g <- .fixture_giotto()
    giottoView(g, "x") <- giottoView() |> subset(leiden_clus == "1")
    expect_equal(
        nrow(getCellMetadata(g, output = "data.table")),
        length(spatIDs(g))
    )
})


# --- Resolver cache --------------------------------------------------------

test_that(".cached_surviving_cell_ids memoises within a cache env", {
    g <- .fixture_giotto()
    v <- giottoView() |> subset(leiden_clus == "1")
    cache <- GiottoClass:::.new_resolver_cache()

    a <- GiottoClass:::.cached_surviving_cell_ids(g, v,
        dataTableCoordinator(), cache)
    expect_true(exists("surviving_ids", envir = cache))
    b <- GiottoClass:::.cached_surviving_cell_ids(g, v,
        dataTableCoordinator(), cache)
    expect_identical(a, b)
})

test_that(".cached_surviving_cell_ids with NULL cache works", {
    g <- .fixture_giotto()
    v <- giottoView() |> subset(leiden_clus == "1")
    ids <- GiottoClass:::.cached_surviving_cell_ids(g, v,
        dataTableCoordinator(), NULL)
    expect_type(ids, "character")
    expect_equal(length(ids), sum(pDataDT(g)$leiden_clus == "1"))
})


# --- spatValues with view = ----------------------------------------------

test_that("spatValues view = name narrows returned rows", {
    g <- .fixture_giotto()
    giottoView(g, "tumor") <- giottoView() |> subset(leiden_clus == "1")
    sv <- spatValues(g, feats = "leiden_clus", view = "tumor")
    expect_equal(nrow(sv), sum(pDataDT(g)$leiden_clus == "1"))
    expect_true(all(sv$leiden_clus == "1"))
})

test_that("spatValues view = rejects ad-hoc giottoView (character-only contract)", {
    g <- .fixture_giotto()
    v <- giottoView() |> subset(leiden_clus %in% c("2", "3"))
    expect_error(
        spatValues(g, feats = "leiden_clus", view = v),
        "Must be of type 'string'"
    )
})

test_that("spatValues view = NULL is identity (matches raw)", {
    g <- .fixture_giotto()
    raw <- spatValues(g, feats = "leiden_clus")
    same <- spatValues(g, feats = "leiden_clus", view = NULL)
    expect_identical(raw, same)
})

test_that("spatValues empty view recipe (slotted) returns same as raw", {
    g <- .fixture_giotto()
    giottoView(g, "empty") <- giottoView()
    raw <- spatValues(g, feats = "leiden_clus")
    via_empty <- spatValues(g, feats = "leiden_clus", view = "empty")
    expect_equal(nrow(via_empty), nrow(raw))
})

test_that("spatValues view = ... matches getCellMetadata view = ... narrowing", {
    g <- .fixture_giotto()
    giottoView(g, "x") <- giottoView() |> subset(leiden_clus == "1")
    sv <- spatValues(g, feats = "leiden_clus", view = "x")
    cm <- getCellMetadata(g, view = "x", output = "data.table")
    # both paths produce the same cell_ID set
    expect_setequal(sv$cell_ID, cm$cell_ID)
})

test_that("spatValues view = ... is consistent with materialize -> spatValues raw", {
    g <- .fixture_giotto()
    giottoView(g, "c1") <- giottoView() |> subset(leiden_clus == "1")
    direct <- spatValues(g, feats = "leiden_clus", view = "c1")
    g_m <- materialize(g, "c1")
    via_materialize <- spatValues(g_m, feats = "leiden_clus")
    # direct narrowing should match the materialized-then-raw path
    expect_setequal(direct$cell_ID, via_materialize$cell_ID)
})

test_that("spatValues view that filters to zero cells returns empty data.table", {
    g <- .fixture_giotto()
    giottoView(g, "none") <- giottoView() |>
        subset(leiden_clus == "_nonexistent_cluster_")
    sv <- spatValues(g, feats = "leiden_clus", view = "none")
    expect_equal(nrow(sv), 0L)
    expect_true("cell_ID" %in% colnames(sv))
})

test_that("spatValues view = composed predicate AND-narrows correctly", {
    g <- .fixture_giotto()
    # two subset steps chained — both should apply (intersection semantics)
    giottoView(g, "composed") <- giottoView() |>
        subset(leiden_clus %in% c("1", "2")) |>
        subset(total_expr > median(total_expr))
    sv <- spatValues(g, feats = "leiden_clus", view = "composed")
    cm <- pDataDT(g)
    n_expected <- sum(
        cm$leiden_clus %in% c("1", "2") &
            cm$total_expr > median(cm$total_expr))
    expect_equal(nrow(sv), n_expected)
})

test_that("spatValues view = on giottoMulti narrows joint output", {
    mg <- .fixture_gmulti()
    sv_raw <- spatValues(mg, feats = "leiden_clus")
    giottoView(mg, "c1") <- giottoView() |> subset(leiden_clus == "1")
    sv_v <- spatValues(mg, feats = "leiden_clus", view = "c1")
    expect_lt(nrow(sv_v), nrow(sv_raw))
    expect_true(all(sv_v$leiden_clus == "1"))
    # global cell_ID format still present
    expect_true(any(grepl("^a::", sv_v$cell_ID)) ||
                any(grepl("^b::", sv_v$cell_ID)))
})

test_that("spatValues view re-entry guard prevents recursion", {
    # Set the option as if we're mid-resolution; an outer spatValues
    # call with view should drop the view arg rather than recurse.
    g <- .fixture_giotto()
    giottoView(g, "x") <- giottoView() |> subset(leiden_clus == "1")
    options(giotto.spatValues_view_active = TRUE)
    on.exit(options(giotto.spatValues_view_active = FALSE), add = TRUE)
    sv_v <- spatValues(g, feats = "leiden_clus", view = "x")
    sv_raw <- spatValues(g, feats = "leiden_clus")
    # with guard active, view= is dropped; result matches raw
    expect_equal(nrow(sv_v), nrow(sv_raw))
})

test_that("spatValues space = NULL is no-op (currently accepted but not value-transforming)", {
    g <- .fixture_giotto()
    giottoSpace(g, "tilted") <- giottoSpace() |> spin(30)
    sv_native <- spatValues(g, feats = "leiden_clus")
    sv_space <- spatValues(g, feats = "leiden_clus", space = "tilted")
    # space does NOT transform value columns (leiden_clus is a label);
    # rows and values are unchanged
    expect_equal(nrow(sv_native), nrow(sv_space))
    expect_setequal(sv_native$leiden_clus, sv_space$leiden_clus)
})


# --- gmulti dispatch ------------------------------------------------------

test_that("giottoView accessors work on giottoMulti via gAny", {
    mg <- .fixture_gmulti()
    v <- giottoView() |> subset(leiden_clus == "1")
    giottoView(mg, "tumor") <- v
    expect_identical(giottoViews(mg), "tumor")
    out <- giottoView(mg, "tumor")
    expect_s4_class(out, "giottoView")
})

test_that("giottoSpace accessors work on giottoMulti via gAny", {
    mg <- .fixture_gmulti()
    s <- (giottoSpace("a") |> spin(30)) + (giottoSpace("b") |> spin(45))
    giottoSpace(mg, "atlas") <- s
    expect_identical(giottoSpaces(mg), "atlas")
    out <- giottoSpace(mg, "atlas")
    expect_named(out@samples, c("a", "b"))
})

test_that(".scope_space_to_sample picks the right key for a child", {
    s <- (giottoSpace("a") |> spin(30)) + (giottoSpace("b") |> spin(45))
    sa <- GiottoClass:::.scope_space_to_sample(s, "a")
    expect_named(sa@samples, GiottoClass:::.space_default_sample)
    expect_length(sa@samples[[1L]], 1L)
    expect_equal(sa@samples[[1L]][[1L]]$op, "spin")
    expect_equal(sa@samples[[1L]][[1L]]$args$angle, 30)

    sb <- GiottoClass:::.scope_space_to_sample(s, "b")
    expect_equal(sb@samples[[1L]][[1L]]$args$angle, 45)
})

test_that(".scope_space_to_sample falls back to :default: key", {
    s <- giottoSpace() |> spin(15)
    out <- GiottoClass:::.scope_space_to_sample(s, "any_sample_name")
    expect_equal(out@samples[[1L]][[1L]]$args$angle, 15)
})

test_that(".scope_space_to_sample returns NULL when no matching key", {
    s <- giottoSpace("only_x") |> spin(15)
    out <- GiottoClass:::.scope_space_to_sample(s, "missing")
    expect_null(out)
})

test_that("materialize on giottoMulti narrows children via selectSamples", {
    mg <- .fixture_gmulti()
    giottoView(mg, "only_a") <- giottoView() |> selectSamples("a")
    out <- materialize(mg, "only_a")
    expect_identical(names(out@objects), "a")
})

test_that("materialize on giottoMulti applies view per-child", {
    mg <- .fixture_gmulti()
    giottoView(mg, "c1") <- giottoView() |> subset(leiden_clus == "1")
    out <- materialize(mg, "c1")
    expect_named(out@objects, c("a", "b"))
    # each child has been narrowed
    n_a <- nrow(pDataDT(out@objects$a))
    n_b <- nrow(pDataDT(out@objects$b))
    expect_lt(n_a, length(spatIDs(mg@objects$a)))
    expect_lt(n_b, length(spatIDs(mg@objects$b)))
})

test_that("materialize on giottoMulti scopes space per-child", {
    mg <- .fixture_gmulti()
    s <- (giottoSpace("a") |> spin(30)) + (giottoSpace("b") |> spin(45))
    giottoSpace(mg, "atlas") <- s
    giottoView(mg, "empty") <- giottoView()
    out <- materialize(mg, "empty", space = "atlas")

    sl_a_native <- getSpatialLocations(mg@objects$a, output = "data.table")
    sl_b_native <- getSpatialLocations(mg@objects$b, output = "data.table")
    sl_a_post <- getSpatialLocations(out@objects$a, output = "data.table")
    sl_b_post <- getSpatialLocations(out@objects$b, output = "data.table")

    # both children transformed — but by different angles
    expect_false(isTRUE(all.equal(sl_a_post$sdimx, sl_a_native$sdimx)))
    expect_false(isTRUE(all.equal(sl_b_post$sdimx, sl_b_native$sdimx)))
    # different angles → ratios of transformed-to-native should differ
    # (we don't compute the exact rotation expectation here, just that
    # the two children's transforms are not identical)
    expect_false(isTRUE(all.equal(
        sl_a_post$sdimx - sl_a_native$sdimx,
        sl_b_post$sdimx - sl_b_native$sdimx)))
})

test_that("spatValues on giottoMulti finds features in joint cell_metadata", {
    mg <- .fixture_gmulti()
    sv <- spatValues(mg, feats = "leiden_clus")
    expect_equal(nrow(sv), sum(lengths(lapply(mg@objects, spatIDs))))
    expect_true(all(c("cell_ID", "leiden_clus") %in% colnames(sv)))
    # global cell_IDs use the sample::local_id format
    expect_true(any(grepl("^a::", sv$cell_ID)))
    expect_true(any(grepl("^b::", sv$cell_ID)))
})

test_that("spatValues on giottoMulti finds features in joint expression", {
    mg <- .fixture_gmulti()
    gene <- rownames(getExpression(mg, output = "matrix"))[1L]
    sv <- spatValues(mg, feats = gene)
    expect_equal(nrow(sv), sum(lengths(lapply(mg@objects, spatIDs))))
    expect_true(gene %in% colnames(sv))
})

test_that("materialize on giottoMulti narrows joint shared slots", {
    mg <- .fixture_gmulti()
    giottoView(mg, "c1") <- giottoView() |> subset(leiden_clus == "1")
    n_total <- nrow(pDataDT(mg))
    n_target <- sum(pDataDT(mg)$leiden_clus == "1")

    out <- materialize(mg, "c1")

    # joint cell_metadata narrowed
    expect_equal(nrow(pDataDT(out)), n_target)
    expect_lt(nrow(pDataDT(out)), n_total)
    # joint expression narrowed in column count
    expect_equal(ncol(getExpression(out, output = "matrix")), n_target)
})

test_that("materialize via slotted view name dispatches on multi", {
    mg <- .fixture_gmulti()
    giottoView(mg, "x") <- giottoView() |> selectSamples("a")
    out <- materialize(mg, "x")
    expect_identical(names(out@objects), "a")
})


# Q7 — recipes are plain data and survive serialization ####
#
# The whole reason steps are tagged lists rather than S4: a recipe has to
# survive saveRDS and reach a parallel worker. Before Q7 that claim was
# false — viewFilter carried an environment and viewCrop could hold a
# terra pointer.

test_that("steps are plain tagged lists, not S4", {
    v <- giottoView() |> subset(cluster == "A") |> crop(c(0, 10, 0, 10)) |>
        selectSamples("a")
    for (s in v@steps) {
        expect_type(s, "list")
        expect_false(isS4(s))
        expect_true(is.character(s$type))
    }
    expect_identical(
        vapply(v@steps, function(s) s$type, character(1L)),
        c("filter", "crop", "samples"))

    s <- giottoSpace() |> spin(30)
    step <- s@samples[[1L]][[1L]]
    expect_type(step, "list")
    expect_false(isS4(step))
    expect_identical(step$type, "transform")
})

test_that("a filter step carries no environment; the predicate is a string", {
    target <- "A"
    v <- giottoView() |> subset(cluster == target)
    step <- v@steps[[1L]]
    expect_null(step$env)
    expect_type(step$predicate, "character")
    # the VALUE of target was substituted in at record time, so the recipe
    # does not change when the binding does
    expect_identical(step$predicate, 'cluster == "A"')
    target <- "B"
    expect_identical(v@steps[[1L]]$predicate, 'cluster == "A"')
})

test_that("a view recipe round-trips through saveRDS and still resolves", {
    g <- .fixture_giotto()
    v <- giottoView() |> subset(leiden_clus == 1)
    f <- tempfile(fileext = ".rds")
    on.exit(unlink(f), add = TRUE)
    saveRDS(v, f)
    v2 <- readRDS(f)
    expect_identical(v@steps, v2@steps)

    # and the deserialized recipe resolves to the same cells
    giottoView(g, "a") <- v
    giottoView(g, "b") <- v2
    expect_identical(
        pDataDT(materialize(g, "a"))$cell_ID,
        pDataDT(materialize(g, "b"))$cell_ID)
})

test_that("a space recipe round-trips through saveRDS and still resolves", {
    g <- .fixture_giotto()
    s <- giottoSpace() |> spatShift(dx = 100)
    f <- tempfile(fileext = ".rds")
    on.exit(unlink(f), add = TRUE)
    saveRDS(s, f)
    s2 <- readRDS(f)
    expect_identical(s@samples, s2@samples)

    giottoSpace(g, "s2") <- s2
    sl <- getSpatialLocations(g, output = "data.table")
    sl_shift <- getSpatialLocations(g, space = "s2", output = "data.table")
    expect_equal(sl_shift$sdimx, sl$sdimx + 100)
})

test_that("a recorded crop region is WKT, and terra objects do not leak in", {
    poly <- terra::vect(rbind(
        c(0, 0), c(10, 0), c(10, 10), c(0, 10), c(0, 0)
    ), type = "polygons")
    v <- giottoView() |> crop(poly)
    region <- v@steps[[1L]]$region
    expect_type(region, "character")
    expect_match(region, "^POLYGON")
    # serializable: a SpatVector would not survive this
    f <- tempfile(fileext = ".rds")
    on.exit(unlink(f), add = TRUE)
    saveRDS(v, f)
    expect_identical(readRDS(f)@steps[[1L]]$region, region)
})

test_that("WKT round-trip does not shift a crop boundary", {
    # PLAN Q7 flagged emitted-WKT precision as unverified. Check that a
    # numeric extent survives numeric -> WKT -> SpatVector -> extent.
    exts <- list(
        c(0, 100, 0, 100),
        c(-5000.5, -3499.25, 1234.125, 5678.0625),
        c(1e-8, 2e-8, 3e-8, 4e-8),
        c(1e7, 1e7 + 0.001, -1e7, -1e7 + 0.001)
    )
    for (e in exts) {
        wkt <- GiottoClass:::.normalize_crop_region(e)
        got <- terra::ext(terra::vect(wkt))[]
        expect_equal(unname(got), e, tolerance = 0,
            info = paste("extent:", paste(e, collapse = ", ")))
    }
})

test_that("crop accepts WKT directly as the canonical entry", {
    wkt <- "POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))"
    v <- giottoView() |> crop(wkt)
    expect_identical(v@steps[[1L]]$region, wkt)
    expect_error(giottoView() |> crop("not wkt at all"), "not valid WKT")
})

test_that("multi-feature crop regions are unioned into one geometry", {
    p1 <- terra::vect(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)),
        type = "polygons")
    p2 <- terra::vect(rbind(c(5, 5), c(6, 5), c(6, 6), c(5, 6), c(5, 5)),
        type = "polygons")
    both <- rbind(p1, p2)
    expect_equal(nrow(both), 2)
    v <- giottoView() |> crop(both)
    expect_length(v@steps[[1L]]$region, 1L)
    # the union's extent spans both parts
    expect_equal(unname(terra::ext(terra::vect(v@steps[[1L]]$region))[]),
        c(0, 6, 0, 6))
})

test_that("the inline cap rejects an oversized query set", {
    withr::local_options(list(giotto.view_crop_inline_max = 2L))
    wkts <- rep("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))", 3L)
    expect_error(giottoView() |> crop(wkts), "inline cap")
})

test_that("transform args are whitelisted to serializable types", {
    # a terra object as a transform arg is exactly what breaks saveRDS
    sv <- terra::vect(rbind(c(0, 0), c(1, 1)), type = "points")
    expect_error(giottoSpace() |> spatShift(dx = sv), "cannot be recorded")
    # atomic vectors, numeric matrices and affine2d are accepted
    expect_no_error(giottoSpace() |> spatShift(dx = 1, dy = 2))
    expect_no_error(giottoSpace() |> affine(matrix(c(1, 0, 0, 1), nrow = 2)))
    expect_no_error(giottoSpace() |> flip(direction = "vertical"))
})

test_that("an unknown step type is rejected at record and validate time", {
    expect_error(GiottoClass:::.validate_view_step(list(type = "nope")),
        "unknown type")
    expect_error(GiottoClass:::.validate_space_step(
        list(type = "transform", op = "teleport", args = list())),
        "unknown transform")
    # a malformed hand-built recipe is caught by the container validator
    v <- giottoView()
    v@steps <- list(list(type = "filter", predicate = "cluster ==",
        scope_args = list()))
    expect_error(validObject(v), "does not parse")
})

test_that("a rectangular region takes the AABB path; a polygon does not", {
    rect <- terra::vect(terra::ext(c(0, 10, 0, 10)))
    expect_true(GiottoClass:::.region_is_rect(rect))
    tri <- terra::vect(rbind(c(0, 0), c(10, 0), c(5, 10), c(0, 0)),
        type = "polygons")
    expect_false(GiottoClass:::.region_is_rect(tri))
    # both give the same answer for points, which is what matters
    sl <- data.table::data.table(
        cell_ID = c("a", "b", "c"),
        sdimx = c(1, 9, 5), sdimy = c(1, 9, 1))
    expect_setequal(
        GiottoClass:::.cells_in_region(sl, rect, "intersects"),
        c("a", "b", "c"))
    expect_setequal(
        GiottoClass:::.cells_in_region(sl, tri, "intersects"),
        c("a", "c"))
})


# A7 — crop routing is decided by relation, not by storage kind ####

test_that("cropRelationNeedsGeom splits centroid- from geometry-relations", {
    expect_false(cropRelationNeedsGeom("intersects"))
    expect_false(cropRelationNeedsGeom("disjoint"))
    for (r in c("within", "covered_by", "contains", "covers",
                "overlaps", "touches", "crosses")) {
        expect_true(cropRelationNeedsGeom(r), info = r)
    }
    # vectorized
    expect_identical(cropRelationNeedsGeom(c("intersects", "within")),
        c(FALSE, TRUE))
    expect_error(cropRelationNeedsGeom(NA_character_), "missing")
})

test_that("intersects and disjoint partition the cell set", {
    g <- .fixture_giotto()
    sl <- getSpatialLocations(g, output = "data.table")
    box <- c(mean(range(sl$sdimx)) - 1500, mean(range(sl$sdimx)) + 1500,
             mean(range(sl$sdimy)) - 1500, mean(range(sl$sdimy)) + 1500)
    giottoView(g, "i") <- giottoView() |> crop(box, relation = "intersects")
    giottoView(g, "d") <- giottoView() |> crop(box, relation = "disjoint")
    n_i <- length(pDataDT(materialize(g, "i"))$cell_ID)
    n_d <- length(pDataDT(materialize(g, "d"))$cell_ID)
    expect_identical(n_i + n_d, nrow(sl))
    expect_gt(n_i, 0L)
    expect_gt(n_d, 0L)
})

test_that("a geometry relation is stricter than the centroid approximation", {
    # This is the bug A7 fixes: before routing, `within` went down the
    # centroid path and returned the `intersects` answer — silently
    # over-inclusive by the cells straddling the region boundary.
    g <- .fixture_giotto()
    sl <- getSpatialLocations(g, output = "data.table")
    box <- c(mean(range(sl$sdimx)) - 1500, mean(range(sl$sdimx)) + 1500,
             mean(range(sl$sdimy)) - 1500, mean(range(sl$sdimy)) + 1500)
    giottoView(g, "i") <- giottoView() |> crop(box, relation = "intersects")
    giottoView(g, "w") <- giottoView() |> crop(box, relation = "within")
    cells_i <- pDataDT(materialize(g, "i"))$cell_ID
    cells_w <- pDataDT(materialize(g, "w"))$cell_ID
    expect_lt(length(cells_w), length(cells_i))
    # and the within-set is a subset, not merely smaller
    expect_true(all(cells_w %in% cells_i))
})

test_that("a geometry relation without a polygon source errors loudly", {
    m <- matrix(0, nrow = 2, ncol = 3,
        dimnames = list(c("f1", "f2"), c("c1", "c2", "c3")))
    g <- createGiottoObject(expression = m, verbose = FALSE,
        spatial_locs = data.frame(cell_ID = c("c1", "c2", "c3"),
            sdimx = 1:3, sdimy = 1:3))
    expect_null(g@spatial_info)

    giottoView(g, "w") <- giottoView() |> crop(c(0, 10, 0, 10),
        relation = "within")
    expect_error(materialize(g, "w"), "no polygon source")
    # the message names the remedy
    expect_error(materialize(g, "w"), "intersects")

    # the centroid relations still work on the same object
    giottoView(g, "i") <- giottoView() |> crop(c(0, 10, 0, 10))
    expect_length(pDataDT(materialize(g, "i"))$cell_ID, 3L)
})

test_that("routing does not depend on which slot the view is read through", {
    # one usage layer per predicate: a geometry relation must narrow
    # cell metadata, expression and spatial locations identically
    g <- .fixture_giotto()
    sl <- getSpatialLocations(g, output = "data.table")
    box <- c(mean(range(sl$sdimx)) - 1500, mean(range(sl$sdimx)) + 1500,
             mean(range(sl$sdimy)) - 1500, mean(range(sl$sdimy)) + 1500)
    giottoView(g, "w") <- giottoView() |> crop(box, relation = "within")

    from_meta <- sort(getCellMetadata(g, view = "w",
        output = "data.table")$cell_ID)
    from_expr <- sort(colnames(getExpression(g, view = "w",
        output = "matrix")))
    from_locs <- sort(getSpatialLocations(g, view = "w",
        output = "data.table")$cell_ID)
    expect_identical(from_meta, from_expr)
    expect_identical(from_meta, from_locs)
})

test_that("the resolver cache is memoization only, never routing", {
    # A7 requires that passing or omitting a cache cannot change the
    # answer — only how many times it is computed.
    g <- .fixture_giotto()
    sl <- getSpatialLocations(g, output = "data.table")
    box <- c(mean(range(sl$sdimx)) - 1500, mean(range(sl$sdimx)) + 1500,
             mean(range(sl$sdimy)) - 1500, mean(range(sl$sdimy)) + 1500)
    v <- giottoView() |> crop(box, relation = "within")
    co <- dataTableCoordinator()

    no_cache <- GiottoClass:::.cached_surviving_cell_ids(g, v, co, NULL)
    cache <- GiottoClass:::.new_resolver_cache()
    with_cache <- GiottoClass:::.cached_surviving_cell_ids(g, v, co, cache)
    again <- GiottoClass:::.cached_surviving_cell_ids(g, v, co, cache)
    expect_identical(no_cache, with_cache)
    expect_identical(with_cache, again)
})

test_that("a space-piped transform broadcasts over the current key set", {
    # A8: `+` and a pipe do not commute, and that is the documented rule
    both <- (giottoSpace("a") + giottoSpace("b")) |> spin(30)
    expect_length(both@samples$a, 1L)
    expect_length(both@samples$b, 1L)

    a_only <- (giottoSpace("a") |> spin(30)) + giottoSpace("b")
    expect_length(a_only@samples$a, 1L)
    expect_length(a_only@samples$b, 0L)

    # a key with no steps still participates, at identity
    expect_true("b" %in% names(a_only@samples))
})

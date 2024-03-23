# silence deprecated internal functions
rlang::local_options(lifecycle_verbosity = "quiet")
# skip conda checks
options("giotto.use_conda" = FALSE)

g <- GiottoData::loadGiottoMini("viz")
sub_cell_ids <- spatIDs(g)[1:6]
sub_feat_ids <- featIDs(g)[1:6]


# subset :all: tests ####


test_that("subsetGiotto handles no params", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    expect_no_error({
        sub_g <- subsetGiotto(g)
    })
})


# test spat_unit = :all: subsets all spatial units
test_that("subsetGiotto can subset all spat_units", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    sub_g <- subsetGiotto(
        gobject = g,
        cell_ids = sub_cell_ids
    )

    # get availability tables
    avail_ex <- list_expression(g)
    avail_cm <- list_cell_metadata(g)
    avail_sl <- list_spatial_locations(g)

    # expression
    for (ex_i in seq(nrow(avail_ex))) {
        ex <- getExpression(
            gobject = sub_g,
            spat_unit = avail_ex[ex_i]$spat_unit,
            feat_type = avail_ex[ex_i]$feat_type,
            values = avail_ex[ex_i]$name,
            output = "exprObj"
        )

        expect_true(all(spatIDs(ex) %in% sub_cell_ids))
    }

    # cell meta
    for (cm_i in seq(nrow(avail_cm))) {
        cm <- getCellMetadata(
            gobject = sub_g,
            spat_unit = avail_cm[cm_i]$spat_unit,
            output = "cellMetaObj"
        )

        expect_true(all(spatIDs(cm) %in% sub_cell_ids))
    }

    # spatlocs
    for (sl_i in seq(nrow(avail_sl))) {
        sl <- getSpatialLocations(
            gobject = sub_g,
            spat_unit = avail_sl[sl_i]$spat_unit,
            name = avail_sl[sl_i]$name,
            output = "spatLocsObj"
        )

        expect_true(all(spatIDs(sl) %in% sub_cell_ids))
    }
})





test_that("subsetGiotto can subset feat_type", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    sub_g <- subsetGiotto(
        gobject = g,
        feat_ids = sub_feat_ids
    )

    # get availability tables
    avail_ex <- list_expression(g)
    avail_fm <- list_feat_metadata(g)

    # expression
    for (ex_i in seq(nrow(avail_ex))) {
        ex <- getExpression(
            gobject = sub_g,
            spat_unit = avail_ex[ex_i]$spat_unit,
            feat_type = avail_ex[ex_i]$feat_type,
            values = avail_ex[ex_i]$name,
            output = "exprObj"
        )

        expect_true(all(featIDs(ex) %in% sub_feat_ids))
    }

    # spatlocs
    for (fm_i in seq(nrow(avail_fm))) {
        fm <- getFeatureMetadata(
            gobject = sub_g,
            spat_unit = avail_fm[fm_i]$spat_unit,
            feat_type = avail_fm[fm_i]$feat_type,
            output = "featMetaObj"
        )

        expect_true(all(featIDs(fm) %in% sub_feat_ids))
    }
})




# subset !:all: tests ####






# subset ssub and fsub tests ####






# subsetGiottoLocs ####

test_that("subsetGiottoLocs works on one spat_unit", {
    rlang::local_options(lifecycle_verbosity = "quiet")
    g_sub_locs <- subsetGiottoLocs(
        gobject = g,
        x_min = 6600
    )

    spat_unit <- activeSpatUnit(g)
    avail_sl <- list_spatial_locations(g, spat_unit = spat_unit)
    avail_ex <- list_expression(g, spat_unit = spat_unit)
    avail_enr <- list_spatial_enrichments(g, spat_unit = spat_unit)

    for (sl_i in seq(nrow(avail_sl))) {
        sl <- getSpatialLocations(
            gobject = g_sub_locs,
            spat_unit = spat_unit,
            name = avail_sl[sl_i]$name,
            output = "data.table"
        )
        # expect no values to be outside of crop bound
        expect_true(nrow(sl[sdimx <= 6600]) == 0L)
    }

    subset_ids <- sl$cell_ID

    for (ex_i in seq(nrow(avail_ex))) {
        ex <- getExpression(
            gobject = g_sub_locs,
            spat_unit = spat_unit,
            feat_type = avail_ex[ex_i]$feat_type,
            values = avail_ex[ex_i]$name,
            output = "exprObj"
        )
        # expect all spatIDs to be identical to spatial locations IDs
        expect_true(setequal(subset_ids, spatIDs(ex)))
    }

    for (enr_i in seq(nrow(avail_enr))) {
        enr <- getSpatialEnrichment(
            gobject = g_sub_locs,
            spat_unit = spat_unit,
            feat_type = avail_enr[enr_i]$feat_type,
            name = avail_enr[enr_i]$name,
            output = "spatEnrObj"
        )
        # expect all spatIDs to be identical to spatial locations IDs
        expect_true(setequal(subset_ids, spatIDs(enr)))
    }
})





# subsetGiottoLocs - multi ####

test_that("Subsetting multiple spatial units works", {
    rlang::local_options(lifecycle_verbosity = "quiet")

    g_out <- subsetGiottoLocs(
        gobject = g,
        spat_unit = c("z0", "z1"), x_max = 6600
    )

    # polys are cropped
    z0_poly <- getPolygonInfo(g_out, polygon_name = "z0")
    z1_poly <- getPolygonInfo(g_out, polygon_name = "z1")
    agg_poly <- getPolygonInfo(g_out, polygon_name = "aggregate")

    expect_true({
        terra::xmax(ext(centroids(z0_poly))) <= 6600 &&
            terra::xmax(ext(centroids(z1_poly))) <= 6600 &&
            !terra::xmax(ext(centroids(agg_poly))) <= 6600 # not cropped
    })


    # spatlocs are cropped
    z0_sl <- getSpatialLocations(g_out, spat_unit = "z0")
    z1_sl <- getSpatialLocations(g_out, spat_unit = "z1")
    agg_sl <- getSpatialLocations(g_out, spat_unit = "aggregate")

    expect_true({
        ext(z0_sl)$xmax <= 6600 &&
            ext(z1_sl)$xmax <= 6600 &&
            !ext(agg_sl)$xmax <= 6600 # not cropped
    })
})





# subsetGiottoSubcellular ####

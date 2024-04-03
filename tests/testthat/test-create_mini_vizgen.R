# Test that GiottoClass is able to perform all steps needed in order to assemble
# a subset of the vizgen mouse brain receptor map data release as a giotto
# object.
# This object should include multiple z stack spat_units and finally a combined
# 'aggregate' spat_unit that combines the detected TX content of all z layers
# included.

library(checkmate)

# Ignore internal usage of deprecated accessors
lifecycle_opt <- getOption("lifecycle_verbosity")
options("lifecycle_verbosity" = "quiet")



## provide path to vizgen folder
GiottoUtils::package_check("GiottoData")
data_path <- system.file("/Mini_datasets/Vizgen/Raw/", package = "GiottoData")

## 0.1 path to images ####
# ---------------------- #

# vizgen creates data from 7 z-stack slices (z0, z1, ..., z6)
## - each z-slice has a DAPI image, polyT image
## - they also have a combined composite image, created from their segmentation kit (ab stainings)
DAPI_z0_image_path <- paste0(data_path, "/", "images/mini_dataset_dapi_z0.jpg")
DAPI_z1_image_path <- paste0(data_path, "/", "images/mini_dataset_dapi_z1.jpg")

polyT_z0_image_path <- paste0(data_path, "/", "images/mini_dataset_polyT_z0.jpg")
polyT_z1_image_path <- paste0(data_path, "/", "images/mini_dataset_polyT_z1.jpg")



## 0.2 path to transcripts ####
# --------------------------- #

## each transcript has x, y and z coordinate
tx_path <- paste0(data_path, "/", "vizgen_transcripts.gz")
tx_dt <- data.table::fread(tx_path)



## 0.3 path to cell boundaries folder ####
# -------------------------------------- #

## vizgen already provides segmentation information in .hdf5 files
## the hdf5 files are organized in different tiles
## Here I have already converted the hdf5 files to a simple data.table format

boundary_path <- paste0(data_path, "/cell_boundaries/")

z0_polygon_DT <- data.table::fread(paste0(boundary_path, "/", "z0_polygons.gz"))
z1_polygon_DT <- data.table::fread(paste0(boundary_path, "/", "z1_polygons.gz"))

z0_polygons <- createGiottoPolygonsFromDfr(
    name = "z0",
    segmdfr = z0_polygon_DT
)
z1_polygons <- createGiottoPolygonsFromDfr(
    name = "z1",
    segmdfr = z1_polygon_DT
)


test_that("gpolys are created from dfr", {
    expect_identical(objName(z0_polygons), "z0")
    expect_identical(objName(z1_polygons), "z1")

    expect_class(z0_polygons, "giottoPolygon")
    expect_class(z1_polygons, "giottoPolygon")
})


# 1. create subcellular dataset with transcript and polygon information ####
# ------------------------------------------------------------------------ #
suppressWarnings({
    options("giotto.use_conda" = FALSE) # skip python checks
    vizsubc <- createGiottoObjectSubcellular(
        gpoints = list("rna" = tx_dt[, .(global_x, -global_y, gene, global_z)]),
        gpolygons = list(
            "z0" = z0_polygons,
            "z1" = z1_polygons
        )
    )
})

test_that("gobject is created from tx and polys", {
    rlang::local_options(lifecycle_verbosity = "quiet")

    expect_class(vizsubc, "giotto")
    expect_identical(list_spatial_info_names(vizsubc), c("z0", "z1"))
    expect_identical(list_feature_info_names(vizsubc), "rna")
    expect_class(getFeatureInfo(vizsubc, return_giottoPoints = TRUE), "giottoPoints")
})


# calculate centroid for each polygon
# this can/will be used when aggregating for example counts to cells
vizsubc <- addSpatialCentroidLocations(
    gobject = vizsubc,
    poly_info = paste0("z", 0:1),
    provenance = list("z0", "z1"),
    return_gobject = TRUE
)

test_that("gobject centroids can be calculated", {
    rlang::local_options(lifecycle_verbosity = "quiet")

    expect_identical(list_spatial_locations_names(vizsubc, spat_unit = "z0"), c("raw"))
    expect_identical(list_spatial_locations_names(vizsubc, spat_unit = "z1"), c("raw"))
    expect_class(getSpatialLocations(vizsubc, spat_unit = "z0"), "spatLocsObj")
    expect_class(getSpatialLocations(vizsubc, spat_unit = "z1"), "spatLocsObj")
})


# 2. add image ####
# --------------- #

# x and y information from original script
ultra_mini_extent <- terra::ext(c(6400.029, 6900.037, -5150.007, -4699.967))

image_paths <- c(
    DAPI_z0_image_path, DAPI_z1_image_path,
    polyT_z0_image_path, polyT_z1_image_path
)
image_names <- c(
    "dapi_z0", "dapi_z1",
    "polyT_z0", "polyT_z1"
)

imagelist <- createGiottoLargeImageList(
    raster_objects = image_paths,
    names = image_names,
    negative_y = TRUE,
    extent = ultra_mini_extent
)

test_that("giottoLargeImages are created", {
    expect_list(imagelist)
})

vizsubc <- addGiottoImage(
    gobject = vizsubc,
    largeImages = imagelist
)

test_that("images were added", {
    expect_identical(
        list_images_names(vizsubc, "largeImage"),
        c("dapi_z0", "dapi_z1", "polyT_z0", "polyT_z1")
    )
})


# 3. aggregate information to matrix: polygons and transcripts ####
# --------------------------------------------------------------- #

# we will use the z1 polygon information
# we can set a global option or specify this for each command
# options('giotto.spat_unit' = 'z1') # now you don't need to think about setting spat_unit each time

vizsubc <- calculateOverlapRaster(
    vizsubc,
    spatial_info = "z0",
    feat_info = "rna",
    feat_subset_column = "global_z",
    feat_subset_ids = 0
)

vizsubc <- calculateOverlapRaster(
    vizsubc,
    spatial_info = "z1",
    feat_info = "rna",
    feat_subset_column = "global_z",
    feat_subset_ids = 1
)


# get values to test
z0_gpoly <- getPolygonInfo(vizsubc, polygon_name = "z0", return_giottoPolygon = TRUE)
z1_gpoly <- getPolygonInfo(vizsubc, polygon_name = "z1", return_giottoPolygon = TRUE)

rna_pnts <- getFeatureInfo(vizsubc, feat_type = "rna", return_giottoPoints = TRUE)


z0_ids <- spatIDs(z0_gpoly)
z1_ids <- spatIDs(z1_gpoly)
z0_nids <- length(z0_ids)
z1_nids <- length(z1_ids)

feats <- featIDs(rna_pnts)
nfeats <- length(feats)



test_that("overlaps are calculated", {
    expect_class(overlaps(z0_gpoly)$rna, "SpatVector")
    expect_class(overlaps(z0_gpoly)$rna, "SpatVector")
    expect_identical(terra::geomtype(overlaps(z0_gpoly)$rna), "points")
    expect_identical(terra::geomtype(overlaps(z0_gpoly)$rna), "points")
})

vizsubc <- overlapToMatrix(
    vizsubc,
    poly_info = "z0",
    feat_info = "rna",
    name = "raw"
)

vizsubc <- overlapToMatrix(
    vizsubc,
    poly_info = "z1",
    feat_info = "rna",
    name = "raw"
)

test_that("expression matrix is created from overlaps", {
    rlang::local_options(lifecycle_verbosity = "quiet")

    z0_exp <- getExpression(vizsubc, spat_unit = "z0")
    z1_exp <- getExpression(vizsubc, spat_unit = "z1")

    expect_setequal(colnames(z0_exp), z0_ids)
    expect_setequal(colnames(z1_exp), z1_ids)
    expect_setequal(rownames(z0_exp), feats)
    expect_setequal(rownames(z1_exp), feats)

    expect_identical(nrow(z0_exp), nfeats)
    expect_identical(nrow(z1_exp), nfeats)
    expect_identical(ncol(z0_exp), z0_nids)
    expect_identical(ncol(z1_exp), z1_nids)

    # most stringent check
    # Alter if object creation pipeline is altered and a different result is expected.
    expect_identical(dim(z0_exp), c(559L, 498L))
    expect_identical(dim(z1_exp), c(559L, 504L))
})


vizsubc <- aggregateStacks(
    gobject = vizsubc,
    spat_units = c("z0", "z1"),
    feat_type = "rna",
    values = "raw",
    summarize_expression = "sum",
    summarize_locations = "mean",
    new_spat_unit = "aggregate"
)


test_that("aggregateStacks works", {
    rlang::local_options(lifecycle_verbosity = "quiet")

    agg_exp <- getExpression(vizsubc, spat_unit = "aggregate")

    comb_ids <- unique(c(z0_ids, z1_ids))

    expect_setequal(colnames(agg_exp), comb_ids)
    expect_setequal(rownames(agg_exp), feats)

    expect_identical(ncol(agg_exp), length(comb_ids))
    expect_identical(nrow(agg_exp), nfeats)

    # most stringent check
    # Alter if object creation pipeline is altered and a different result is expected.
    expect_identical(dim(agg_exp), c(559L, 505L))
})




# Following steps are more analysis focused and belong (currently) in Giotto
# umbrella package.

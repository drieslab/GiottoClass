library(testthat)
library(Giotto)
library(Seurat)

giotto_obj <- GiottoData::loadGiottoMini("visium")
seurat_obj <- giottoToSeuratV5(giotto_obj)
spe_obj <- giottoToSpatialExperiment(giotto_obj)

Seurat_obj2 <- SeuratData::LoadData("stxBrain", type = "anterior1" )
giotto_obj2 <- seuratToGiottoV5(Seurat_obj2)

giotto_obj_roundtrip <- seuratToGiottoV5(seurat_obj, "rna")

test_that("giottotoseurat function handles basic conversion", {

# Basic checks
expect_s4_class(seurat_obj, "Seurat")
expect_true(!is.null(seurat_obj@meta.data))
expect_equal(nrow(seurat_obj@meta.data),
             nrow(giotto_obj@cell_metadata$cell$rna@metaDT))

# Check if image slots are handled correctly
if ("images" %in% slotNames(giotto_obj)) {
    expect_true(length(seurat_obj@images) > 0)
    expect_equal(length(seurat_obj@images), length(giotto_obj@images))
    expect_s4_class(giotto_obj_roundtrip@images$alignment, "giottoLargeImage")
    expect_s4_class(seurat_obj@images$alignment, "VisiumV1")
}
})

test_that("Giotto to Seurat Conversion Works",{
    expect_s4_class(seurat_obj, "Seurat")
})

test_that("Seurat to Giotto Conversion Works",{
  expect_s4_class(giotto_obj, "giotto")
})

test_that("Assay names are converted correctly (no spaces)", {
  # Check for spaces in Seurat assay names
  assay_names <- names(seurat_obj@assays)
  expect_false(any(grepl(" ", assay_names)))  # No spaces should be present
})

test_that("Data is consistent after roundtrip conversion", {
  
  identical(giotto_obj_roundtrip@expression, giotto_obj@expression)
  # Check consistency of expression data
  expect_equal(dim(giotto_obj@expression$cell$rna$raw),
               dim(giotto_obj_roundtrip@expression$cell$rna$raw))
  
  # Check consistency of cell metadata
  expect_equal(nrow(giotto_obj@cell_metadata$cell$rna@metaDT),
               nrow(giotto_obj_roundtrip@cell_metadata$cell$rna@metaDT))
})

test_that("Feature metadata is transferred correctly", {
  # Check if feature metadata (e.g., gene names) is correctly transferred
  expect_equal(rownames(seurat_obj@assays$rna),
               rownames(giotto_obj@expression$cell$rna$raw))
})

test_that("Giotto to SpatialExperiment Conversion Works",{
  expect_s4_class(spe_obj[[1]], "SpatialExperiment")
})

#TODO
#spe
#annData
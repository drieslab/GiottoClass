
# Ignore internal usage of deprecated accessors
lifecycle_opt = getOption('lifecycle_verbosity')
options('lifecycle_verbosity' = 'quiet')

# ignore conda
options('giotto.has_conda' = FALSE)


# load data to test
g = GiottoData::loadGiottoMini('viz')
sn = GiottoData::loadSubObjectMini('spatialNetworkObj')


test_that('full spatial network can be created and reverted', {

  # reduced to full
  n_edges = nrow(sn)
  full = convert_to_full_spatial_network(sn[])
  expect_equal(n_edges * 2, nrow(full))

  # revert from full to reduced
  reduced = convert_to_reduced_spatial_network(full)
  expect_equal(n_edges, nrow(reduced))
})

test_that('spatial weight matrix can be created', {
  rlang::local_options(lifecycle_verbosity = "quiet")
  test = createSpatialWeightMatrix(g, spat_unit = 'aggregate', return_gobject = TRUE)
  mat = getSpatialNetwork(test, spat_unit = 'aggregate', name = 'kNN_network')@misc$weight_matrix$spat_weights

  expect_true(inherits(mat, c('matrix', 'Matrix')))
})

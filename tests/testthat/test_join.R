options("giotto.use_conda" = FALSE)
g <- GiottoData::loadGiottoMini("vis")
nobs <- nrow(pDataDT(g))
nvar <- nrow(fDataDT(g))

test_that("visium can join", {
    # remove to avoid warning about incorrect networks after spatloc edits
    g@spatial_network <- NULL

    j <- joinGiottoObjects(
        gobject_list = list(g, g, g),
        gobject_names = letters[seq_len(3)]
    )

    checkmate::expect_class(j, "giotto")
    expect_equal(nrow(pDataDT(j)), nobs * 3)
    expect_equal(nrow(fDataDT(j)), nvar)

    # TODO check spatial
})

test_that("expression and spatloc join", {
    gtest <- suppressWarnings(giotto())
    gtest <- setGiotto(gtest, getExpression(g), verbose = FALSE)
    gtest <- setGiotto(gtest, getSpatialLocations(g), verbose = FALSE)

    j <- joinGiottoObjects(
        gobject_list = list(gtest, gtest, gtest),
        gobject_names = letters[seq_len(3)]
    )

    checkmate::expect_class(j, "giotto")
    expect_equal(nrow(pDataDT(j)), nobs * 3)
    expect_equal(nrow(fDataDT(j)), nvar)

    # TODO check spatial
})

# test_that("poly and spatloc join", {
#     gtest <- suppressWarnings(giotto())
#     gtest <- setGiotto(gtest, getPolygonInfo(g,
#         return_giottoPolygon = TRUE),
#         verbose = FALSE
#     )
#     gtest <- setGiotto(gtest, getSpatialLocations(g), verbose = FALSE)
#
#     j <- joinGiottoObjects(
#         gobject_list = list(gtest, gtest, gtest),
#         gobject_names = letters[seq_len(3)]
#     )
#
#     checkmate::expect_class(j, "giotto")
#     expect_equal(nrow(pDataDT(j)), nobs * 3)
#     expect_equal(nrow(fDataDT(j)), nvar)
#
#     # TODO check spatial
# })

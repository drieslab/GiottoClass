# GETTERS ####

## missing cases ####

### create test object
giotto_object = giotto()

test_that('Not found exprObj returns error', {
    expect_error(
        getExpression(giotto_object, spat_unit = 'none', 
                      feat_type = 'none', values = 'raw')
    )
})

test_that('Not found CellMetadata returns error', {
    expect_error(
        getCellMetadata(giotto_object, spat_unit = 'none', 
                        feat_type = 'none', values = 'raw')
    )
})

test_that('Not found DimReduction returns error', {
    expect_error(
        getDimReduction(giotto_object, spat_unit = 'none', 
                        feat_type = 'none', values = 'raw')
    )
})

test_that('Not found FeatureInfo returns error', {
    expect_error(
        getFeatureInfo(giotto_object, spat_unit = 'none', 
                       feat_type = 'none', values = 'raw')
    )
})

test_that('Not found FeatureMetadata returns error', {
    expect_error(
        getFeatureMetadata(giotto_object, spat_unit = 'none', 
                           feat_type = 'none', values = 'raw')
    )
})

test_that('Not found GiottoImage returns error', {
    expect_error(
        getGiottoImage(giotto_object, spat_unit = 'none', 
                       feat_type = 'none', values = 'raw')
    )
})

test_that('Not found Multiomics returns error', {
    expect_error(
        getMultiomics(giotto_object, spat_unit = 'none', 
                      feat_type = 'none', values = 'raw')
    )
})

test_that('Not found NearestNetwork returns error', {
    expect_error(
        getNearestNetwork(giotto_object, spat_unit = 'none', 
                          feat_type = 'none', values = 'raw')
    )
})

test_that('Not found PolygonInfo returns error', {
    expect_error(
        getPolygonInfo(giotto_object, spat_unit = 'none', 
                       feat_type = 'none', values = 'raw')
    )
})

test_that('Not found SpatialEnrichment returns error', {
    expect_error(
        getSpatialEnrichment(giotto_object, spat_unit = 'none', 
                             feat_type = 'none', values = 'raw')
    )
})

test_that('Not found SpatialGrid returns error', {
    expect_error(
        getSpatialGrid(giotto_object, spat_unit = 'none', 
                       feat_type = 'none', values = 'raw')
    )
})

test_that('Not found SpatialLocations returns error', {
    expect_error(
        getSpatialLocations(giotto_object, spat_unit = 'none', 
                            feat_type = 'none', values = 'raw')
    )
})

test_that('Not found SpatialNetwork returns error', {
    expect_error(
        getSpatialNetwork(giotto_object, spat_unit = 'none', 
                          feat_type = 'none', values = 'raw')
    )
})

## expect information ####

### download pre-processed Giotto object
giotto_object = GiottoData::loadGiottoMini('vizgen')

test_that('Finds exprObj', {
    expect_class(getExpression(giotto_object), 'exprObj')
})

test_that('Finds CellMetadata', {
    expect_class(getCellMetadata(giotto_object), 'cellMetaObj')
})

test_that('Finds DimReduction', {
    expect_class(getDimReduction(giotto_object,
                                 spat_unit = 'aggregate',
                                 feat_type = 'rna'), 
                 'dimObj')
})

test_that('Finds FeatureInfo', {
    expect_class(getFeatureInfo(giotto_object), 'SpatVector')
})

test_that('Finds FeatureMetadata', {
    expect_class(getFeatureMetadata(giotto_object), 'featMetaObj')
})

test_that('Finds NearestNetwork', {
    expect_class(getNearestNetwork(giotto_object,
                                   spat_unit = 'aggregate', 
                                   feat_type = 'rna'), 
                 'nnNetObj')
})

test_that('Finds PolygonInfo', {
    expect_class(getPolygonInfo(giotto_object), 'SpatVector')
})

test_that('Finds SpatialEnrichment', {
    expect_class(getSpatialEnrichment(giotto_object,
                                      spat_unit = 'aggregate', 
                                      feat_type = 'rna', 
                                      name = 'cluster_metagene'),
                 'spatEnrObj')
})

test_that('Finds SpatialLocations', {
    expect_class(getSpatialLocations(giotto_object), 'spatLocsObj')
})

test_that('Finds SpatialNetwork', {
    expect_class(getSpatialNetwork(giotto_object,
                                   spat_unit = 'aggregate'), 
                 'spatialNetworkObj')
})


# SETTERS ####

### create empty test object
giotto_empty = giotto()

x = getExpression(giotto_object,
                  spat_unit = 'z0', 
                  feat_type = 'rna')
giotto_empty = setExpression(giotto_empty,
                             spat_unit = 'z0', 
                             feat_type = 'rna',
                             x = x)

x = getExpression(giotto_object,
                  spat_unit = 'z1', 
                  feat_type = 'rna')
giotto_empty = setExpression(giotto_empty,
                             spat_unit = 'z1', 
                             feat_type = 'rna',
                             x = x)

x = getExpression(giotto_object,
                  spat_unit = 'aggregate', 
                  feat_type = 'rna')
giotto_empty = setExpression(giotto_empty, 
                             spat_unit = 'aggregate', 
                             feat_type = 'rna',
                             x = x)

test_that('Sets exprObj', {
    expect_class(getExpression(giotto_empty), 'exprObj')
})

x = getCellMetadata(giotto_object)
giotto_empty = setCellMetadata(giotto_empty,
                               x = x)

test_that('Sets CellMetadata', {
    expect_class(getCellMetadata(giotto_empty), 'cellMetaObj')
})

x = getDimReduction(giotto_object,
                    spat_unit = 'aggregate',
                    feat_type = 'rna')
giotto_empty = setDimReduction(giotto_empty,
                               spat_unit = 'aggregate',
                               feat_type = 'rna',
                               x = x)

test_that('Sets DimReduction', {
    expect_class(getDimReduction(giotto_empty,
                                 spat_unit = 'aggregate',
                                 feat_type = 'rna'), 
                 'dimObj')
})

x = getFeatureInfo(giotto_object)
giotto_empty = setFeatureInfo(giotto_empty,
                              x = createGiottoPoints(x))

test_that('Sets FeatureInfo', {
    expect_class(getFeatureInfo(giotto_empty), 'SpatVector')
})

x = getFeatureMetadata(giotto_object)
giotto_empty = setFeatureMetadata(giotto_empty,
                                  x = x)

test_that('Sets FeatureMetadata', {
    expect_class(getFeatureMetadata(giotto_empty), 'featMetaObj')
})

x = getNearestNetwork(giotto_object,
                      spat_unit = 'aggregate',
                      feat_type = 'rna')
giotto_empty = setNearestNetwork(giotto_empty,
                                 spat_unit = 'aggregate',
                                 feat_type = 'rna',
                                 x = x)

test_that('Sets NearestNetwork', {
    expect_class(getNearestNetwork(giotto_empty,
                                   spat_unit = 'aggregate', 
                                   feat_type = 'rna'), 
                 'nnNetObj')
})

x = getPolygonInfo(giotto_object)
x_polygon = GiottoClass:::create_giotto_polygon_object(name = 'z0',
                                                       spatVector = x)
giotto_empty = setPolygonInfo(giotto_empty,
                              x = x_polygon,
                              name = 'z0')

test_that('Sets PolygonInfo', {
    expect_class(getPolygonInfo(giotto_empty), 'SpatVector')
})

x = getSpatialLocations(giotto_object)
giotto_empty = setSpatialLocations(giotto_empty,
                                   x = x)

x = getSpatialLocations(giotto_object,
                        spat_unit = 'aggregate')
giotto_empty = setSpatialLocations(giotto_empty,
                                   spat_unit = 'aggregate',
                                   x = x)

test_that('Sets SpatialLocations', {
    expect_class(getSpatialLocations(giotto_empty), 'spatLocsObj')
})

x = getSpatialEnrichment(giotto_object,
                         spat_unit = 'aggregate',
                         feat_type = 'rna',
                         name = 'cluster_metagene')
giotto_empty = setSpatialEnrichment(giotto_empty,
                                    spat_unit = 'aggregate',
                                    feat_type = 'rna',
                                    name = 'cluster_metagene',
                                    x = x)

test_that('Sets SpatialEnrichment', {
    expect_class(getSpatialEnrichment(giotto_empty,
                                      spat_unit = 'aggregate',
                                      feat_type = 'rna',
                                      name = 'cluster_metagene'),
                 'spatEnrObj')
})

x = getSpatialNetwork(giotto_object,
                      spat_unit = 'aggregate')
giotto_empty = setSpatialNetwork(giotto_empty,
                                 spat_unit = 'aggregate',
                                 x = x)

test_that('Sets SpatialNetwork', {
    expect_class(getSpatialNetwork(giotto_empty,
                                   spat_unit = 'aggregate'),
                 'spatialNetworkObj')
})


g = GiottoData::loadGiottoMini('viz')
sub_cell_ids = spatIDs(g)[1:6]
sub_feat_ids = featIDs(g)[1:6]


# subset :all: tests ####

# test spat_unit = :all: subsets all spatial units
test_that('subsetGiotto can subset all spat_units', {
  sub_g = subsetGiotto(
    gobject = g,
    cell_ids = sub_cell_ids
  )
  
  # get availability tables
  avail_ex = list_expression(g)
  avail_cm = list_cell_metadata(g)
  avail_sl = list_spatial_locations(g)
  
  # expression
  for(ex_i in seq(nrow(avail_ex))) {
    ex = getExpression(
      gobject = sub_g,
      spat_unit = avail_ex[ex_i]$spat_unit,
      feat_type = avail_ex[ex_i]$feat_type,
      values = avail_ex[ex_i]$name,
      output = "exprObj"
    )
    
    expect_true(all(spatIDs(ex) %in% sub_cell_ids))
  }
  
  # cell meta
  for(cm_i in seq(nrow(avail_cm))) {
    cm = getCellMetadata(
      gobject = sub_g,
      spat_unit = avail_cm[cm_i]$spat_unit,
      output = "cellMetaObj"
    )
    
    expect_true(all(spatIDs(cm) %in% sub_cell_ids))
  }
  
  # spatlocs
  for(sl_i in seq(nrow(avail_sl))) {
    sl = getSpatialLocations(
      gobject = sub_g,
      spat_unit = avail_sl[sl_i]$spat_unit,
      name = avail_sl[sl_i]$name,
      output = "spatLocsObj"
    )
    
    expect_true(all(spatIDs(sl) %in% sub_cell_ids))
  }
  
})





test_that('subsetGiotto can subset feat_type', {
  sub_g = subsetGiotto(
    gobject = g,
    feat_ids = sub_feat_ids
  )
  
  # get availability tables
  avail_ex = list_expression(g)
  avail_fm = list_feat_metadata(g)
  
  # expression
  for(ex_i in seq(nrow(avail_ex))) {
    ex = getExpression(
      gobject = sub_g,
      spat_unit = avail_ex[ex_i]$spat_unit,
      feat_type = avail_ex[ex_i]$feat_type,
      values = avail_ex[ex_i]$name,
      output = "exprObj"
    )
    
    expect_true(all(featIDs(ex) %in% sub_feat_ids))
  }
  
  # spatlocs
  for(fm_i in seq(nrow(avail_fm))) {
    fm = getFeatureMetadata(
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

test_that('subsetGiottoLocs works on one spat_unit', {
  g_sub_locs = subsetGiottoLocs(
    gobject = g,
    x_min = 6600
  )
})





# subsetGiottoLocsMulti ####







# subsetGiottoSubcellular ####


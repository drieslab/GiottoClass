### aggregate stacks ####



#' @title combine_matrices
#' @keywords internal
combine_matrices = function(mat_list,
                            summarize = 'sum') {

  # data.table vars
  i = j = x = NULL

  DT_list = list()
  feats_list = list()
  samples_list = list()

  # loop through all matrices
  # create a triplet data.table (i, j, x)
  for(mat_i in 1:length(mat_list)) {

    mat = mat_list[[mat_i]]

    if(!inherits(mat, c('matrix', 'dgCMatrix'))) {
      stop('Matrix needs to be a base or sparse matrix from the Matrix package')
    }

    if(inherits(mat, 'matrix')) mat = methods::as(mat, 'dgCMatrix')

    mat_feats = mat@Dimnames[[1]]
    names(mat_feats) = 1:mat@Dim[[1]]
    feats_list[[mat_i]] = mat_feats

    mat_samples = mat@Dimnames[[2]]
    names(mat_samples) = 1:mat@Dim[[2]]
    samples_list[[mat_i]] = mat_samples

    matDT = data.table::as.data.table(Matrix::summary(mat))
    matDT[, c('i','j') := list(mat_feats[i], mat_samples[j])]
    DT_list[[mat_i]] = matDT
  }


  # combine matrices in data.table format and aggregate (sum by default)
  new_dt = data.table::rbindlist(l = DT_list)

  if(summarize == 'sum') {
    test = new_dt[, sum(x), by = .(i,j)]
  } else {
    'not implemented yet'
  }

  # feature list
  all_features = unique(unlist(feats_list))
  featnames = 1:length(all_features)
  names(featnames) = all_features

  # sample list
  all_samples = unique(unlist(samples_list))
  samplenames = 1:length(all_samples)
  names(samplenames) = all_samples

  # convert i and j to numericals for dgCmatrix
  test[, i2 := featnames[i]]
  test[, j2 := samplenames[j]]

  # convert triplet data.table to sparseMatrix
  featnames_rev = names(featnames)
  names(featnames_rev) = featnames

  samplenames_rev = names(samplenames)
  names(samplenames_rev) = samplenames

  combined_matrix = Matrix::sparseMatrix(i = test$i2,
                                         j = test$j2,
                                         x = test$V1,
                                         dims = c(length(featnames), length(samplenames)),
                                         dimnames = list(featnames_rev, samplenames_rev))


  return(combined_matrix)

}


#' @title aggregateStacksExpression
#' @name aggregateStacksExpression
#' @description aggregate expression matrices from different z-stacks
#' @param gobject giotto object
#' @param spat_units spatial units to aggregate
#' @param feat_type feature type
#' @param values values to use
#' @param summarize method to summarize expression information
#' @param new_spat_unit new name for aggregated spatial unit
#' @param verbose verbosity
#' @return giotto object
#' @family aggregate stacks
#' @export
#'
aggregateStacksExpression = function(gobject,
                                     spat_units,
                                     feat_type,
                                     values = 'raw',
                                     summarize = 'sum',
                                     new_spat_unit = 'aggregate',
                                     verbose = TRUE) {

  # aggregate matrices
  matrix_list = list()
  for(spat_unit in spat_units) {
    mat = get_expression_values(gobject,
                                spat_unit = spat_unit,
                                feat_type = feat_type,
                                values = values,
                                output = 'matrix')
    matrix_list[[spat_unit]] = mat
  }
  combined_matrix = combine_matrices(matrix_list,
                                     summarize = summarize)

  new_expr_obj = create_expr_obj(name = values,
                                 exprMat = combined_matrix,
                                 spat_unit = new_spat_unit,
                                 feat_type = feat_type,
                                 provenance = spat_units,
                                 misc = NULL)

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  gobject = set_expression_values(gobject = gobject, values = new_expr_obj)
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

  # set new cell IDs
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  gobject = set_cell_id(gobject = gobject,
                        spat_unit = new_spat_unit,
                        cell_IDs = colnames(combined_matrix))
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

  # set new cell metadata
  cell_meta_S4 = create_cell_meta_obj(metaDT = data.table::data.table('cell_ID' = colnames(combined_matrix)),
                                      spat_unit = new_spat_unit,
                                      feat_type = feat_type,
                                      provenance = spat_units)

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  gobject = set_cell_metadata(gobject = gobject, cell_meta_S4, verbose = verbose)
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

  # set new feat metadata
  feat_meta_S4 = create_feat_meta_obj(metaDT = data.table::data.table('feat_ID' = rownames(combined_matrix)),
                                      spat_unit = new_spat_unit,
                                      feat_type = feat_type,
                                      provenance = spat_units)

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  gobject = set_feature_metadata(gobject = gobject, feat_meta_S4, verbose = verbose)
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

  return(gobject)

}


#' @title combine_spatlocs
#' @keywords internal
combine_spatlocs = function(spatlocs_list,
                            summarize = 'mean') {

  # data.table vars
  sdimx = sdimy = sdimz = NULL

  newlocs = data.table::rbindlist(spatlocs_list)

  if(summarize == 'mean') {
    if('sdimz' %in% colnames(newlocs)) {
      newlocs = unique(newlocs[, c('sdimx', 'sdimy', 'sdimz') := list(mean(sdimx), mean(sdimy), mean(sdimz)), by = 'cell_ID'])
    } else {
      newlocs = unique(newlocs[, c('sdimx', 'sdimy') := list(mean(sdimx), mean(sdimy)), by = 'cell_ID'])
    }
  }

  return(newlocs)

}



#' @title aggregateStacksLocations
#' @name aggregateStacksLocations
#' @description aggregate expression matrices from different z-stacks
#' @param gobject giotto object
#' @param spat_units spatial units to aggregate
#' @param values values to use
#' @param summarize method to summarize spatial location information
#' @param new_spat_unit new name for aggregated spatial unit
#' @return giotto object
#' @family aggregate stacks
#' @export
#'
aggregateStacksLocations = function(gobject,
                                    spat_units,
                                    values = 'raw',
                                    summarize = 'mean',
                                    new_spat_unit = 'aggregate') {

  # aggregate locations
  locs_list = list()
  for(spat_unit in spat_units) {
    locDT = get_spatial_locations(gobject = gobject,
                                  spat_unit = spat_unit,
                                  spat_loc_name = values,
                                  output = 'data.table')
    locs_list[[spat_unit]] = locDT
  }
  combined_locs = combine_spatlocs(spatlocs_list = locs_list,
                                   summarize = summarize)


  new_spatlocs_obj = create_spat_locs_obj(name = values,
                                          coordinates = combined_locs,
                                          spat_unit = new_spat_unit,
                                          provenance = spat_units,
                                          misc = NULL)

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  gobject = set_spatial_locations(gobject = gobject,
                                  spatlocs = new_spatlocs_obj,
                                  set_defaults = FALSE)
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

  return(gobject)

}


#' @title combine_polygons
#' @keywords internal
combine_polygons = function(polygon_list) {

  polygon_DT = data.table::rbindlist(polygon_list)

  polygon = dt_to_spatVector_polygon(polygon_DT)

  # TODO: maybe replace step 1 and 2 with polygon_to_raster()
  # TODO: how to define number of columns and rows?

  # step 1: convert polygon into detailed raster
  pol_xmax = terra::xmax(polygon)
  pol_xmin = terra::xmin(polygon)
  ncols = abs(pol_xmax-pol_xmin)
  ncols = ncols * 2

  pol_ymax = terra::ymax(polygon)
  pol_ymin = terra::ymin(polygon)
  nrows = abs(pol_ymax-pol_ymin)
  nrows = nrows * 2

  myraster = terra::rast(polygon, ncols = ncols, nrows = nrows)

  # step 2: transfer vector data to a raster based on
  poly_rast = terra::rasterize(x = polygon, y = myraster, field = 'poly_ID')

  # create new combined polygon
  aggr_polygon = terra::as.polygons(poly_rast)

  return(aggr_polygon)

}



#' @title aggregateStacksPolygonsOLD
#' @name aggregateStacksPolygonsOLD
#' @description aggregate polygons from different z-stacks
#' @param gobject giotto object
#' @param spat_units spatial units to aggregate
#' @param new_spat_unit new name for aggregated spatial unit
#' @return giotto object
#' @family aggregate stacks
#' @export
#'
aggregateStacksPolygonsOLD = function(gobject,
                                      spat_units,
                                      new_spat_unit = 'aggregate') {


  # aggregate spatvectors
  polygon_list = list()

  for(i in 1:length(spat_units)) {

    spat_unit = spat_units[i]
    vecDT = gobject@spatial_info[[spat_unit]]@spatVector
    vecDT = spatVector_to_dt(vecDT)
    vecDT = vecDT[, c('geom', 'part', 'x', 'y', 'hole', 'poly_ID'), with = FALSE]
    vecDT[, 'stack' :=  i]
    polygon_list[[spat_unit]] = vecDT
  }

  combined_polygons = combine_polygons(polygon_list = polygon_list)

  gpolygon = create_giotto_polygon_object(name = new_spat_unit,
                                          spatVector = combined_polygons,
                                          spatVectorCentroids = NULL,
                                          overlaps = NULL)

  gobject = set_polygon_info(gobject = gobject,
                             polygon_name = new_spat_unit,
                             gpolygon = gpolygon,
                             verbose = F)

  return(gobject)

}


#' @title combine_stack_spatVectors
#' @description combines/aggregates polygons with the same cell ID from different z-stacks
#' @keywords internal
combine_stack_spatVectors = function(gobject,
                                     spat_units,
                                     for_loop = FALSE,
                                     for_loop_group_size = 100) {


  # 1. combine all spatVectors across all stacks
  stack_list = list()
  for(spat_i in 1:length(spat_units)) {
    spat = spat_units[[spat_i]]
    stackspatvector = get_polygon_info(gobject = gobject,
                                       polygon_name = spat,
                                       polygon_overlap = NULL,
                                       return_giottoPolygon = FALSE)
    #stackspatvector = gobject@spatial_info[[spat]]@spatVector
    stackspatvector[['stack']] = spat
    stack_list[[spat_i]] = stackspatvector
  }
  stack_spatvector = do.call('rbind', stack_list)

  # TODO: check if all stackspatvectors are identical
  # skip polygon aggregation step and simply keep one spatvector
  # dt_z0 = spatVector_to_dt(stackspatvector_z0)
  # dt_z1 = spatVector_to_dt(stackspatvector_z1)
  # identical(dt_z0[,.(x,y)], dt_z1[,.(x,y)])



  # 2. make sure spatvectors are valid
  stack_spatvector = terra::makeValid(stack_spatvector)

  # 3. aggregate individual cells/polys
  all_poly_ids = sort(unique(stack_spatvector$poly_ID))

  # run in for loop if data is very very big
  if(isTRUE(for_loop)) {
    poly_list = list()
    poly_id_groups = split(all_poly_ids, ceiling(seq_along(all_poly_ids)/for_loop_group_size))

    for(group_i in 1:length(poly_id_groups)) {
      selected_poly_ids = poly_id_groups[[group_i]]
      selected_poly = stack_spatvector[stack_spatvector$poly_ID %in% selected_poly_ids]
      selected_poly_aggr = terra::aggregate(selected_poly, by = 'poly_ID', dissolve = TRUE)
      poly_list[[group_i]] = selected_poly_aggr
    }
    aggr_spatvectors = do.call('rbind', poly_list)

  } else {
    aggr_spatvectors = terra::aggregate(stack_spatvector, by = 'poly_ID', dissolve = TRUE)
  }


  # 4. add valid information to aggregated spatvector
  aggr_spatvectors[['valid']] = terra::is.valid(aggr_spatvectors)

  return(aggr_spatvectors)

}



#' @title aggregateStacksPolygons
#' @name aggregateStacksPolygons
#' @description aggregate polygons from different z-stacks
#' @param gobject giotto object
#' @param spat_units spatial units to aggregate
#' @param new_spat_unit new name for aggregated spatial unit
#' @param for_loop aggregate polygons in for loop (default = FALSE)
#' @param for_loop_group_size size of polygon groups to aggregate in each loop
#' @return giotto object
#' @family aggregate stacks
#' @export
#'
aggregateStacksPolygons = function(gobject,
                                   spat_units,
                                   new_spat_unit = 'aggregate',
                                   for_loop = FALSE,
                                   for_loop_group_size = 100) {


  # aggregate spatvectors
  aggregated_spatVec = combine_stack_spatVectors(gobject = gobject,
                                                 spat_units = spat_units,
                                                 for_loop = for_loop,
                                                 for_loop_group_size = for_loop_group_size)

  gpolygon = create_giotto_polygon_object(name = new_spat_unit,
                                          spatVector = aggregated_spatVec,
                                          spatVectorCentroids = NULL,
                                          overlaps = NULL)

  gobject = set_polygon_info(gobject = gobject,
                             polygon_name = new_spat_unit,
                             gpolygon = gpolygon,
                             verbose = F)

  return(gobject)

}



#' @title aggregateStacksPolygonOverlaps
#' @name aggregateStacksPolygonOverlaps
#' @description aggregate polygons overlap information from different z-stacks
#' @param gobject giotto object
#' @param spat_units spatial units to aggregate
#' @param feat_type feature type used for overlap calculations
#' @param new_spat_unit new name for aggregated spatial unit
#' @return giotto object
#' @family aggregate stacks
#' @export
#'
aggregateStacksPolygonOverlaps = function(gobject,
                                          spat_units,
                                          feat_type,
                                          new_spat_unit = 'aggregate') {

  # aggregate spatvectors
  polygon_list = list()

  for(i in 1:length(spat_units)) {
    spat_unit = spat_units[i]
    vecDT = gobject@spatial_info[[spat_unit]]@overlaps[[feat_type]]

    if(!is.null(vecDT)) {
      vecDT = spatVector_to_dt(vecDT)
      vecDT[, 'stack' :=  i]
      polygon_list[[spat_unit]] = vecDT
    }

  }

  if(length(polygon_list) == 0) {
    wrap_msg('No feature overlaps found for stack aggregation \n')
  } else {
    polygon_DT = data.table::rbindlist(polygon_list)
    polygon = dt_to_spatVector_polygon(dt = polygon_DT,
                                       include_values = TRUE)
    gobject@spatial_info[[new_spat_unit]]@overlaps[[feat_type]] = polygon
  }

  return(gobject)

}

#' @title aggregateStacks
#' @name aggregateStacks
#' @description aggregate expression matrices from different z-stacks
#' @param gobject giotto object
#' @param spat_units spatial units to aggregate
#' @param feat_type feature type
#' @param values values to use
#' @param summarize_expression method to summarize expression information
#' @param summarize_locations method to summarize spatial location information
#' @param for_loop aggregate polygons in for loop (default = FALSE)
#' @param for_loop_group_size size of polygon groups to aggregate in each loop
#' @param new_spat_unit new name for aggregated spatial unit
#' @param verbose verbosity
#' @return giotto object
#' @details Combines both \code{\link{aggregateStacksExpression}} and \code{\link{aggregateStacksLocations}}
#' @family aggregate stacks
#' @export
#'
aggregateStacks = function(gobject,
                           spat_units,
                           feat_type,
                           values,
                           summarize_expression = 'sum',
                           summarize_locations = 'mean',
                           for_loop = FALSE,
                           for_loop_group_size = 100,
                           new_spat_unit = 'aggregate',
                           verbose = TRUE) {

  if(isTRUE(verbose)) {
    wrap_msg('1. Start aggregating expression data')
  }
  gobject = aggregateStacksExpression(gobject = gobject,
                                      spat_units = spat_units,
                                      feat_type = feat_type,
                                      values = values,
                                      summarize = summarize_expression,
                                      new_spat_unit = new_spat_unit)
  if(isTRUE(verbose)) {
    wrap_msg('1. Aggregating expression data completed')
  }

  # gobject = aggregateStacksLocations(gobject = gobject,
  #                                    spat_units = spat_units,
  #                                    values = values,
  #                                    summarize = summarize_locations,
  #                                    new_spat_unit = new_spat_unit)


  if(isTRUE(verbose)) {
    wrap_msg('2. Start aggregating Polygon data')
  }
  gobject = aggregateStacksPolygons(gobject = gobject,
                                    spat_units = spat_units,
                                    new_spat_unit = new_spat_unit,
                                    for_loop = for_loop,
                                    for_loop_group_size = for_loop_group_size)
  if(isTRUE(verbose)) {
    wrap_msg('2. Aggregating polygon data completed')
  }


  if(isTRUE(verbose)) {
    wrap_msg('3. Start aggregating centroid location data')
  }
  gobject = addSpatialCentroidLocations(gobject = gobject,
                                        feat_type = feat_type,
                                        poly_info = new_spat_unit,
                                        provenance = spat_units,
                                        spat_loc_name = 'raw',
                                        return_gobject = TRUE)

  if(isTRUE(verbose)) {
    wrap_msg('3. Aggregating centroid location data completed')
  }



  if(isTRUE(verbose)) {
    wrap_msg('4. Start aggregating polygon overlap data')
  }
  gobject = aggregateStacksPolygonOverlaps(gobject,
                                           spat_units = spat_units,
                                           feat_type = feat_type,
                                           new_spat_unit = new_spat_unit)
  if(isTRUE(verbose)) {
    wrap_msg('4. Aggregating polygon overlap data completed')
  }

  return(gobject)

}

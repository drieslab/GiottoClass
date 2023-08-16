

# Combine metadata functions combine information from other slots with metadata
# information to create a single report.




# Combine metadata ####

#' @title combineMetadata
#' @name combineMetadata
#' @description This function combines the cell metadata with spatial locations and
#' enrichment results from \code{\link[Giotto]{runSpatialEnrich}}
#' @param gobject Giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param spat_loc_name name of spatial locations to include
#' @param spat_enr_names names of spatial enrichment results to include
#' @param verbose verbosity
#' @return Extended cell metadata in data.table format.
#' @export
combineMetadata = function(gobject,
                           spat_unit = NULL,
                           feat_type = NULL,
                           spat_loc_name = 'raw',
                           spat_enr_names = NULL,
                           verbose = TRUE) {

  # Set feat_type and spat_unit
  spat_unit = set_default_spat_unit(gobject = gobject,
                                    spat_unit = spat_unit)
  feat_type = set_default_feat_type(gobject = gobject,
                                    spat_unit = spat_unit,
                                    feat_type = feat_type)

  # cell metadata
  metadata = get_cell_metadata(gobject,
                               spat_unit = spat_unit,
                               feat_type = feat_type,
                               output = 'data.table')

  # spatial locations
  if(!is.null(spat_loc_name)) {
    spatial_locs = get_spatial_locations(gobject = gobject,
                                         spat_unit = spat_unit,
                                         spat_loc_name = spat_loc_name,
                                         output = 'data.table',
                                         copy_obj = TRUE,
                                         verbose = verbose)
  } else {
    spatial_locs = NULL
  }

  # data.table variables
  cell_ID = NULL

  if(!is.null(spatial_locs)) {
    metadata = cbind(metadata, spatial_locs[, cell_ID := NULL])
  }


  # cell/spot enrichment data
  available_enr = list_spatial_enrichments_names(gobject = gobject,
                                                 spat_unit = spat_unit,
                                                 feat_type = feat_type)

  # output warning if not found
  not_available = spat_enr_names[!spat_enr_names %in% available_enr]
  if(length(not_available) > 0) {
    cat('These spatial enrichment results have not been found: \n',
        not_available)
  }

  spat_enr_names = spat_enr_names[spat_enr_names %in% available_enr]

  if(!is.null(spat_enr_names) & length(spat_enr_names) > 0) {

    result_list = list()
    for(spatenr in 1:length(spat_enr_names)) {

      spatenr_name = spat_enr_names[spatenr]

      temp_spat = get_spatial_enrichment(gobject = gobject,
                                         spat_unit = spat_unit,
                                         feat_type = feat_type,
                                         enrichm_name = spatenr_name,
                                         output = 'data.table',
                                         copy_obj = TRUE)

      temp_spat[, 'cell_ID' := NULL]

      result_list[[spatenr]] = temp_spat
    }
    final_meta = do.call('cbind', c(list(metadata), result_list))

    duplicates = sum(duplicated(colnames(final_meta)))
    if(duplicates > 0) cat('Some column names are not unique.
                           If you add results from multiple enrichments,
                           consider giving the signatures unique names')

  } else {

    final_meta = metadata

  }

  return(final_meta)

}





#' @title combineSpatialCellFeatureInfo
#' @name combineSpatialCellFeatureInfo
#' @description Combine spatial cell information (e.g. polygon)
#' and spatial feature information (e.g. transcript locations)
#' @param gobject Giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type(s)
#' @param selected_features select set of features
#' @return list of data.table(s)
#' @details
#' The returned data.table has the following columns: \cr
#' \itemize{
#'   \item{sdimx: spatial feature location on the x-axis}
#'   \item{sdimy: spatial feature location on the y-axis}
#'   \item{feat_ID: unique feature ID}
#'   \item{cell_ID: unique cell ID}
#'   \item{used: how often was the feature used/assigned to a cell}
#'   \item{feat: selected feature(s)}
#' }
#' @export
combineSpatialCellFeatureInfo = function(gobject,
                                         spat_unit = NULL,
                                         feat_type = NULL,
                                         selected_features = NULL) {

  # define for data.table
  feat_ID = NULL

  # combine
  # 1. spatial morphology information ( = polygon)
  # 2. spatial transcript location information

  # Set feat_type and spat_unit
  spat_unit = set_default_spat_unit(gobject = gobject,
                                    spat_unit = spat_unit)
  feat_type = set_default_feat_type(gobject = gobject,
                                    spat_unit = spat_unit,
                                    feat_type = feat_type)

  spatial_cell_info = gobject@spatial_info

  if(is.null(spatial_cell_info)) {
    stop('There is no available spatial segmentation/location information')
  }


  res_list = list()
  for(feat in unique(feat_type)) {

    spatial_feat_locs = gobject@feat_info[[feat]]

    if(!is.null(selected_features)) {
      spatial_feat_locs = spatial_feat_locs[feat_ID %in% selected_features]
    }

    if(is.null(spatial_feat_locs)) {
      stop('There is no available spatial feature location information for ', feat, '\n')
    }

    output = merge_spatial_locs_feat_info(spatial_info = spatial_cell_info,
                                          feature_info = spatial_feat_locs)
    output[, 'feat' := feat]

    res_list[[feat]] = output
  }

  return(res_list)

}



#' @title combineSpatialCellMetadataInfo
#' @name combineSpatialCellMetadataInfo
#' @description Combine cell metadata with spatial cell information (e.g. polygon)
#' @param gobject Giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type(s)
#' @return list of data.table(s)
#' @details
#' The returned data.table has the following columns: \cr
#' \itemize{
#'   \item{sdimx: spatial feature location on the x-axis}
#'   \item{sdimy: spatial feature location on the y-axis}
#'   \item{cell_ID: unique cell ID}
#'   \item{feat: selected feature(s)}
#'   \item{other columns that are part of the cell metadata}
#' }
#' @export
combineSpatialCellMetadataInfo = function(gobject,
                                          spat_unit = NULL,
                                          feat_type = NULL) {


  # combine
  # 1. spatial morphology information ( = polygon)
  # 2. cell metadata

  # Set feat_type and spat_unit
  spat_unit = set_default_spat_unit(gobject = gobject,
                                    spat_unit = spat_unit)
  feat_type = set_default_feat_type(gobject = gobject,
                                    spat_unit = spat_unit,
                                    feat_type = feat_type)

  # get spatial cell information
  spatial_cell_info = gobject@spatial_info

  res_list = list()
  for(feat in unique(feat_type)) {

    # get spatial cell metadata
    cell_meta = pDataDT(gobject,
                        spat_unit = spat_unit,
                        feat_type = feat)

    # merge
    spatial_cell_info_meta = merge.data.table(spatial_cell_info, cell_meta, by = 'cell_ID')

    spatial_cell_info_meta[, 'feat' := feat]

    res_list[[feat]] = spatial_cell_info_meta

  }

  return(res_list)

}







#' @title combineCellData
#' @name combineCellData
#' @description combine cell data information
#' @param gobject giotto object
#' @param feat_type feature type
#' @param include_spat_locs include information about spatial locations
#' @param spat_loc_name spatial location name
#' @param include_poly_info include information about polygon
#' @param poly_info polygon information name
#' @return data.table with combined spatial information
#' @concept combine cell metadata
#' @export
combineCellData = function(gobject,
                           feat_type = 'rna',
                           include_spat_locs = TRUE,
                           spat_loc_name = 'raw',
                           include_poly_info = TRUE,
                           poly_info = 'cell') {

  # combine
  # 1. spatial morphology information ( = polygon)
  # 2. cell metadata

  # specify feat_type
  # Set feat_type and spat_unit
  poly_info = set_default_spat_unit(gobject = gobject,
                                    spat_unit = poly_info)
  feat_type = set_default_feat_type(gobject = gobject,
                                    spat_unit = poly_info,
                                    feat_type = feat_type)


  # get spatial locations
  if(include_spat_locs == TRUE) {
    spat_locs_dt = get_spatial_locations(gobject = gobject,
                                         spat_unit = poly_info,
                                         spat_loc_name = spat_loc_name,
                                         output = 'data.table',
                                         copy_obj = TRUE)
  } else {
    spat_locs_dt = NULL
  }


  # get spatial cell information
  if(include_poly_info == TRUE) {
    # get spatial cell information
    spatial_cell_info_spatvec = get_polygon_info(gobject = gobject,
                                                 polygon_name = poly_info,
                                                 return_giottoPolygon = FALSE)
    spatial_cell_info_dt = spatVector_to_dt(spatial_cell_info_spatvec,
                                            include_values = TRUE)
    data.table::setnames(spatial_cell_info_dt, old = 'poly_ID', new = 'cell_ID')
  } else {
    spatial_cell_info_dt = NULL
  }




  # combine prior information if wanted
  if(!is.null(spat_locs_dt) & !is.null(spatial_cell_info_dt)) {
    comb_dt = data.table::merge.data.table(spat_locs_dt,
                                           spatial_cell_info_dt,
                                           by = 'cell_ID')
  } else if(!is.null(spat_locs_dt)) {
    comb_dt = spat_locs_dt
  } else if(!is.null(spatial_cell_info_dt)) {
    comb_dt = spatial_cell_info_dt
  } else {
    comb_dt = NULL
  }


  res_list = list()
  for(feat in unique(feat_type)) {


    # get spatial cell metadata
    cell_meta = get_cell_metadata(gobject = gobject,
                                  spat_unit = poly_info,
                                  feat_type = feat,
                                  output = 'data.table')

    # merge
    if(!is.null(comb_dt)) {
      spatial_cell_info_meta = data.table::merge.data.table(comb_dt, cell_meta, by = 'cell_ID')
    } else {
      spatial_cell_info_meta = cell_meta
    }

    spatial_cell_info_meta[, 'feat' := feat]

    res_list[[feat]] = spatial_cell_info_meta

  }

  return(res_list)



}


#' @title combineFeatureData
#' @name combineFeatureData
#' @description combine feature data information
#' @param gobject giotto object
#' @param feat_type feature type
#' @param spat_unit spatial unit
#' @param sel_feats selected features (default: NULL or no selection)
#' @return data.table with combined spatial feature information
#' @concept combine feature metadata
#' @export
combineFeatureData = function(gobject,
                              feat_type = NULL,
                              spat_unit = NULL,
                              sel_feats = NULL) {

  # data.table variables
  feat_ID = NULL

  spat_unit = set_default_spat_unit(gobject = gobject,
                                    spat_unit = spat_unit)
  feat_type = set_default_feat_type(gobject = gobject,
                                    spat_unit = spat_unit,
                                    feat_type = feat_type)

  res_list = list()
  for(feat in unique(feat_type)) {
    for(spat in unique(spat_unit)) {

      # feature meta
      # feat_meta = gobject@feat_metadata[[spat_unit]][[feat]]

      feat_meta = get_feature_metadata(gobject = gobject,
                                       spat_unit = spat_unit,
                                       feat_type = feat,
                                       output = 'data.table')

      if(!is.null(sel_feats[[feat_type]])) {
        selected_features = sel_feats[[feat_type]]
        feat_meta = feat_meta[feat_ID %in% selected_features]
      }


      # feature info
      feat_info_spatvec = get_feature_info(gobject = gobject,
                                           feat_type = feat,
                                           return_giottoPoints = FALSE)
      feat_info = spatVector_to_dt(feat_info_spatvec)
      if(!is.null(sel_feats[[feat_type]])) {
        selected_features = sel_feats[[feat_type]]
        feat_info = feat_info[feat_ID %in% selected_features]
      }

      comb_dt = data.table::merge.data.table(x = feat_meta,
                                             y = feat_info,
                                             by = 'feat_ID')

      comb_dt[, 'feat' := feat]
      comb_dt[, 'spat_unit' := spat]

    }

    res_list[[feat]] = comb_dt

  }

  return(res_list)

}





#' @title combineFeatureOverlapData
#' @name combineFeatureOverlapData
#' @description combine feature data information
#' @param gobject giotto object
#' @param feat_type feature type
#' @param sel_feats selected features (default: NULL or no selection)
#' @param poly_info polygon information name
#' @return data.table with combined spatial polygon information
#' @concept combine feature metadata
#' @export
combineFeatureOverlapData = function(gobject,
                                     feat_type = 'rna',
                                     sel_feats = NULL,
                                     poly_info = c('cell')) {

  # data.table vars
  feat_ID = NULL

  poly_info = set_default_spat_unit(gobject = gobject,
                                    spat_unit = poly_info)
  feat_type = set_default_feat_type(gobject = gobject,
                                    spat_unit = poly_info,
                                    feat_type = feat_type)


  res_list = list()
  for(feat in unique(feat_type)) {

    for(spat in unique(poly_info)) {

      # feature meta
      # feat_meta = gobject@feat_metadata[[feat]][[spat]]

      feat_meta = get_feature_metadata(gobject = gobject,
                                       spat_unit = spat,
                                       feat_type = feat,
                                       output = 'data.table')

      if(!is.null(sel_feats[[feat_type]])) {
        selected_features = sel_feats[[feat_type]]
        feat_meta = feat_meta[feat_ID %in% selected_features]
      }

      # overlap poly and feat info
      poly_list = list()
      for(poly in poly_info) {
        feat_overlap_info_spatvec = get_polygon_info(gobject = gobject,
                                                     polygon_name = poly,
                                                     polygon_overlap = feat)
        feat_overlap_info = spatVector_to_dt(feat_overlap_info_spatvec)

        if(!is.null(sel_feats[[feat_type]])) {
          selected_features = sel_feats[[feat_type]]
          feat_overlap_info = feat_overlap_info[feat_ID %in% selected_features]
        }

        feat_overlap_info[, poly_info := poly]
        poly_list[[poly]] = feat_overlap_info
      }

      poly_list_res = do.call('rbind', poly_list)

      comb_dt = data.table::merge.data.table(x = feat_meta,
                                             y = poly_list_res,
                                             by = 'feat_ID')

    }


    comb_dt[, 'feat' := feat]
    res_list[[feat]] = comb_dt

  }

  return(res_list)

}





# internals ####

#' @title merge_spatial_locs_feat_info
#' @name merge_spatial_locs_feat_info
#' @description merge spatial cell and feature location information
#' @keywords internal
merge_spatial_locs_feat_info = function(spatial_info,
                                        feature_info) {

  # data.table variables
  cell_ID = used = NULL

  reslist = list()
  for(i in 1:length(unique(spatial_info$cell_ID))) {

    cell_i = unique(spatial_info$cell_ID)[i]

    temp = sp::point.in.polygon(point.x = feature_info$sdimx,
                                point.y = feature_info$sdimy,
                                pol.x = spatial_info[cell_ID == cell_i]$sdimx,
                                pol.y = spatial_info[cell_ID == cell_i]$sdimy)

    detected_feats = feature_info[temp == 1]
    detected_feats[, cell_ID := cell_i]

    reslist[[i]] = detected_feats

  }

  reslistfinal = do.call('rbind', reslist)

  # calculate how often a single transcript is used
  # > 1 means that a transcript was assigned to more than 1 cell
  reslistfinal[, used := .N, by = c('sdimx', 'sdimy', 'feat_ID')]

  return(reslistfinal)

}




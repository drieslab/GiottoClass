### subset Giotto object ####


#' @title Subset expression data
#' @name subset_expression_data
#' @description Subset expression data from giotto object
#' @keywords internal
#' @noRd
subset_expression_data = function(gobject,
                                  cell_ids,
                                  feat_ids,
                                  feat_type,
                                  spat_unit,
                                  all_spat_units,
                                  all_feat_types) {



  # get cell/spatial ids and feature ids
  g_cell_IDs = get_cell_id(gobject = gobject, spat_unit = spat_unit)
  g_feat_IDs = get_feat_id(gobject = gobject, feat_type = feat_type)

  # get expression information from giotto object
  output_table = list_expression(gobject)

  if(!is.null(output_table)) {

    # loop through expression objects and update accordingly
    for(row in 1:nrow(output_table)) {

      spat_unit_name = output_table[row][['spat_unit']]
      feat_type_name = output_table[row][['feat_type']]
      expression_name = output_table[row][['name']]


      if(feat_type_name == feat_type & spat_unit_name == spat_unit) {

        S4_expr = get_expression_values(gobject = gobject,
                                        spat_unit = spat_unit_name,
                                        feat_type = feat_type_name,
                                        values = expression_name,
                                        output = 'exprObj')

        if(!is.null(slot(gobject, 'h5_file'))) {
          expr_dimnames = HDF5Array::h5readDimnames(filepath = slot(gobject, 'h5_file'),
                                                    name = paste0('/expression/',
                                                                  feat_type_name,'/',
                                                                  expression_name))
          g_cell_IDs = expr_dimnames[[2]]
          g_feat_IDs = expr_dimnames[[1]]

          filter_bool_feats = match(feat_ids, g_feat_IDs)
          filter_bool_cells = match(cell_ids, g_cell_IDs)
        } else {
          ## filter index
          g_cell_IDs = colnames(S4_expr@exprMat)
          g_feat_IDs = rownames(S4_expr@exprMat)

          if(!is.null(cell_ids)) {
            filter_bool_cells = g_cell_IDs %in% cell_ids
          } else filter_bool_cells = g_cell_IDs %in% g_cell_IDs
          if(!is.null(feat_ids)) {
            filter_bool_feats = g_feat_IDs %in% feat_ids
          } else filter_bool_feats = g_feat_IDs %in% g_feat_IDs
        }

        # for HDF5Array
        if(!is.null(slot(gobject, 'h5_file'))) {
          x = HDF5Array::h5mread(filepath = slot(gobject, 'h5_file'),
                                 name = paste0('/expression/',
                                               feat_type_name, '/',
                                               expression_name),
                                 starts = list(filter_bool_feats, filter_bool_cells))
          colnames(x) = cell_ids
          rownames(x) = feat_ids

          HDF5Array::writeHDF5Array(x,
                                    filepath = slot(gobject, 'h5_file'),
                                    name = paste0('/expression/',feat_type_name, '/filtered'),
                                    with.dimnames = TRUE)
          S4_expr@exprMat = paste0('/expression/',feat_type_name, '/filtered')

        } else if(methods::is(S4_expr@exprMat, 'HDF5Array')) {
          S4_expr@exprMat = DelayedArray::realize(S4_expr@exprMat[filter_bool_feats, filter_bool_cells], "HDF5Array")
        } else {
          S4_expr@exprMat = S4_expr@exprMat[filter_bool_feats, filter_bool_cells]
        }

        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        gobject = set_expression_values(gobject = gobject,
                                        values = S4_expr,
                                        verbose = FALSE)
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

      } else if(feat_type_name == feat_type & spat_unit_name != spat_unit) {

        if(all_spat_units == TRUE) {

          # filter only features, but NOT cells
          S4_expr = get_expression_values(gobject = gobject,
                                          spat_unit = spat_unit_name,
                                          feat_type = feat_type_name,
                                          values = expression_name,
                                          output = 'exprObj')
          ## filter index
          g_feat_IDs = rownames(S4_expr@exprMat)
          if(!is.null(feat_ids)) {
            filter_bool_feats = g_feat_IDs %in% feat_ids
          } else filter_bool_feats = g_feat_IDs %in% g_feat_IDs

          # for HDF5Array
          if(methods::is(S4_expr@exprMat, 'HDF5Array')) {
            S4_expr@exprMat = DelayedArray::realize(S4_expr@exprMat[filter_bool_feats, ], "HDF5Array")
          } else {
            S4_expr@exprMat = S4_expr@exprMat[filter_bool_feats, ]
          }

          ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
          gobject = set_expression_values(gobject = gobject,
                                          values = S4_expr,
                                          verbose = FALSE)
          ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

        }




      } else if(feat_type_name != feat_type & spat_unit_name == spat_unit) {

        if(all_feat_types == TRUE) {

          # filter only cells, but NOT features
          S4_expr = get_expression_values(gobject = gobject,
                                          spat_unit = spat_unit_name,
                                          feat_type = feat_type_name,
                                          values = expression_name,
                                          output = 'exprObj')
          ## filter index
          g_cell_IDs = colnames(S4_expr@exprMat)

          if(!is.null(cell_ids)) {
            filter_bool_cells = g_cell_IDs %in% cell_ids
          } else filter_bool_cells = g_cell_IDs %in% g_cell_IDs

          # for HDF5Array
          if(methods::is(S4_expr@exprMat, 'HDF5Array')) {
            S4_expr@exprMat = DelayedArray::realize(S4_expr@exprMat[, filter_bool_cells], "HDF5Array")
          } else {
            S4_expr@exprMat = S4_expr@exprMat[, filter_bool_cells]
          }

          ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
          gobject = set_expression_values(gobject = gobject,
                                          values = S4_expr,
                                          verbose = FALSE)
          ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

        }

      }

    }

  }

  return(gobject)

}

#' @title Subset spatial locations
#' @name subset_spatial_locations
#' @description Subset location data from giotto object
#' @keywords internal
#' @noRd
subset_spatial_locations = function(gobject,
                                    cell_ids,
                                    spat_unit) {

  avail_locs = list_spatial_locations_names(gobject, spat_unit = spat_unit)

  # only subset cell_ID if the spatial unit is the same (e.g. cell)

  if(!is.null(avail_locs)) {
    for(spatlocname in avail_locs) {

      spatObj = get_spatial_locations(gobject,
                                      spat_unit = spat_unit,
                                      spat_loc_name = spatlocname,
                                      output = 'spatLocsObj',
                                      copy_obj = FALSE)

      ## filter index
      g_cell_IDs = spatObj@coordinates[['cell_ID']]

      if(!is.null(cell_ids)) {
        filter_bool_cells = g_cell_IDs %in% cell_ids
      } else filter_bool_cells = g_cell_IDs %in% g_cell_IDs

      spatObj[] = spatObj[][filter_bool_cells]

      ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
      gobject = set_spatial_locations(gobject, spatlocs = spatObj, verbose = FALSE)
      ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
      # not yet possible to row subset data.tables by reference. Must be set back in.

    }
  }

  return(gobject)
}


#' @title Subset cell metadata
#' @name subset_cell_metadata
#' @description Subset cell metadata from giotto object
#' @inheritParams data_access_params
#' @param cell_ids cell ids to keep
#' @param all_feat_types (boolean) applies subset operation across the whole gobject
#' (ALL feature types), ignoring the \code{feat_type} input param. Defaults to TRUE.
#' @keywords internal
subset_cell_metadata = function(gobject,
                                feat_type = NULL,
                                cell_ids,
                                spat_unit,
                                all_feat_types = TRUE) {

  if(isTRUE(all_feat_types)) {
    avail_cm = list_cell_metadata(gobject,
                                  spat_unit = spat_unit)
  } else {
    avail_cm = list_cell_metadata(gobject,
                                  spat_unit = spat_unit,
                                  feat_type = feat_type)
  }

  if(!is.null(avail_cm)) {

    for(cm_i in seq(nrow(avail_cm))) {

      cm = get_cell_metadata(gobject,
                             spat_unit = avail_cm$spat_unit[[cm_i]],
                             feat_type = avail_cm$feat_type[[cm_i]],
                             output = 'cellMetaObj')

      ## filter index
      g_cell_IDs = cm@metaDT[['cell_ID']]

      if(!is.null(cell_ids)) {
        filter_bool_cells = g_cell_IDs %in% cell_ids
      } else filter_bool_cells = g_cell_IDs %in% g_cell_IDs

      cm[] = cm[][filter_bool_cells]

      ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
      gobject = set_cell_metadata(gobject, metadata = cm, verbose = FALSE)
      ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

    }

  }

  # if(!is.null(gobject@cell_metadata)) {
  #   # only subset cell_ID for selected spatial unit
  #   for(feat_type in names(gobject@cell_metadata[[spat_unit]])) {
  #     gobject@cell_metadata[[spat_unit]][[feat_type]] = gobject@cell_metadata[[spat_unit]][[feat_type]][filter_bool_cells,]
  #   }
  # }

  return(gobject)
}


#' @title Subset feature metadata
#' @name subset_feature_metadata
#' @description Subset feature metadata from giotto object
#' @inheritParams data_access_params
#' @param feat_ids feature ids to keep
#' @param all_spat_units (boolean) applies subset operation across the whole gobject
#' (ALL spat_units), ignoring the \code{spat_unit} input param. Defaults to TRUE.
#' @keywords internal
subset_feature_metadata = function(gobject,
                                   feat_type,
                                   spat_unit = NULL,
                                   feat_ids,
                                   all_spat_units = TRUE) {

  if(isTRUE(all_spat_units)) {
    avail_fm = list_feat_metadata(gobject,
                                  feat_type = feat_type)
  } else {
    avail_fm = list_feat_metadata(gobject,
                                  spat_unit = spat_unit,
                                  feat_type = feat_type)
  }


  if(!is.null(avail_fm)) {

    for(fm_i in seq(nrow(avail_fm))) {

      fm = get_feature_metadata(gobject,
                                spat_unit = avail_fm$spat_unit[[fm_i]],
                                feat_type = avail_fm$feat_type[[fm_i]],
                                output = 'featMetaObj')

      ## filter index
      g_feat_IDs = fm@metaDT[['feat_ID']]

      if(!is.null(feat_ids)) {
        filter_bool_feats = g_feat_IDs %in% feat_ids
      } else filter_bool_feats = g_feat_IDs %in% feat_ids

      fm[] = fm[][filter_bool_feats]

      ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
      gobject = set_feature_metadata(gobject, metadata = fm, verbose = FALSE)
      ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

    }

    # for(spat_unit in names(gobject@feat_metadata)) {
    #   # only subset features of the feat type, but do it for all spatial regions
    #   gobject@feat_metadata[[spat_unit]][[feat_type]] = gobject@feat_metadata[[spat_unit]][[feat_type]][filter_bool_feats,]
    # }

  }

  return(gobject)
}



#' @title Subset spatial network
#' @name subset_spatial_network
#' @description subset ALL spatial networks from giotto object of the given
#' spat_unit
#' @keywords internal
#' @noRd
subset_spatial_network = function(gobject,
                                  spat_unit,
                                  cell_ids) {

  # define for data.table
  to = from = NULL

  # cell spatial network
  if(!is.null(slot(gobject, 'spatial_network'))) {
    # Find existing networks for given spatial unit
    existing_networks = list_spatial_networks_names(gobject = gobject,
                                                    spat_unit = spat_unit)
    # Iterate through all networks of this spatial unit...
    for(network in existing_networks) {
      spatNetObj = get_spatialNetwork(gobject = gobject,
                                      spat_unit = spat_unit,
                                      name = network,
                                      output = 'spatialNetworkObj')

      # Within each spatialNetworkObj, subset only the cells_to_keep
      spatNetObj[] = spatNetObj[][to %in% cell_ids & from %in% cell_ids]

      # Set the spatialNetworkObj back into the gobject
      ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
      gobject = set_spatialNetwork(gobject = gobject,
                                   spatial_network = spatNetObj,
                                   verbose = FALSE)
      ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    }
  }

  return(gobject)
}



#' @title Subset dimension reduction
#' @name subset_dimension_reduction
#' @description Subset dimension reduction results from giotto object
#' @keywords internal
#' @noRd
subset_dimension_reduction = function(gobject,
                                      spat_unit,
                                      feat_type,
                                      cell_ids) {

  # find available dim reductions
  avail_dim = list_dim_reductions(gobject = gobject,
                                  data_type = 'cells',
                                  spat_unit = spat_unit,
                                  feat_type = feat_type)

  if(!is.null(avail_dim)) {

    for(data_i in seq(avail_dim[, .N])) {
      dimObj = get_dimReduction(gobject = gobject,
                                spat_unit = avail_dim$spat_unit[[data_i]],
                                feat_type = avail_dim$feat_type[[data_i]],
                                reduction = 'cells',
                                reduction_method = avail_dim$dim_type[[data_i]],
                                name = avail_dim$name[[data_i]],
                                output = 'dimObj')

      dimObj[] = dimObj[][rownames(dimObj[]) %in% cell_ids,]

      ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
      gobject = set_dimReduction(gobject = gobject, dimObject = dimObj, verbose = FALSE)
      ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

    }

  }

  return(gobject)
}




#' @title Subset nearest network
#' @name subset_nearest_network
#' @description Subset nearest network results from giotto object
#' @keywords internal
#' @noRd
subset_nearest_network = function(gobject,
                                  spat_unit,
                                  feat_type,
                                  cell_ids) {

  avail_kNN = list_nearest_networks(gobject,
                                    spat_unit = spat_unit,
                                    feat_type = feat_type,
                                    nn_type = 'kNN')
  avail_sNN = list_nearest_networks(gobject,
                                    spat_unit = spat_unit,
                                    feat_type = feat_type,
                                    nn_type = 'sNN')

  if(!is.null(avail_kNN)) {

    for(nn_i in seq(avail_kNN[, .N])) {
      nnObj = get_NearestNetwork(gobject = gobject,
                                 spat_unit = avail_kNN$spat_unit[[nn_i]],
                                 feat_type = avail_kNN$feat_type[[nn_i]],
                                 nn_network_to_use = 'kNN',
                                 network_name = avail_kNN$name[[nn_i]],
                                 output = 'nnNetObj')

      #vertices_to_keep = igraph::V(nnObj[])[filter_bool_cells]
      nnObj[] = igraph::induced_subgraph(graph = nnObj[], vids = cell_ids)

      ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
      gobject = set_NearestNetwork(gobject, nn_network = nnObj, verbose = FALSE)
      ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    }

  }
  if(!is.null(avail_sNN)) {

    for(nn_i in seq(avail_sNN[, .N])) {
      nnObj = get_NearestNetwork(gobject,
                                 spat_unit = avail_sNN$spat_unit[[nn_i]],
                                 feat_type = avail_sNN$feat_type[[nn_i]],
                                 nn_network_to_use = 'sNN',
                                 network_name = avail_sNN$name[[nn_i]],
                                 output = 'nnNetObj')

      #vertices_to_keep = igraph::V(nnObj[])[filter_bool_cells]
      nnObj[] = igraph::induced_subgraph(graph = nnObj[], vids = cell_ids)

      ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
      gobject = set_NearestNetwork(gobject, nn_network = nnObj, verbose = FALSE)
      ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    }

  }

  # ## nn network ##
  # if(!is.null(gobject@nn_network[['cells']])) {
  #
  #   for(spat_unit_name in names(gobject@nn_network[['cells']])) {
  #
  #     if(spat_unit_name == spat_unit) {
  #
  #       for(knn_name in names(gobject@nn_network[['cells']][[spat_unit_name]][['kNN']])) {
  #
  #         # # layout
  #         # old_layout = gobject@nn_network[['cells']][[spat_unit_name]][['kNN']][[knn_name]][['layout']]
  #         #
  #         # if(!is.null(old_layout)) {
  #         #   new_layout = old_layout[filter_bool_cells,]
  #         #   gobject@nn_network[['cells']][[spat_unit]][['kNN']][[knn_name]][['layout']] = new_layout
  #         # }
  #
  #         # igraph object
  #         old_graph = gobject@nn_network[['cells']][[spat_unit_name]][['kNN']][[knn_name]][['igraph']]
  #         vertices_to_keep = igraph::V(old_graph)[filter_bool_cells]
  #         new_subgraph = igraph::subgraph(graph = old_graph, v = vertices_to_keep)
  #         gobject@nn_network[['cells']][[spat_unit_name]][['kNN']][[knn_name]][['igraph']] = new_subgraph
  #       }
  #
  #       for(snn_name in names(gobject@nn_network[['cells']][[spat_unit_name]][['sNN']])) {
  #
  #         # # layout
  #         # old_layout = gobject@nn_network[['cells']][[spat_unit_name]][['sNN']][[snn_name]][['layout']]
  #         #
  #         # if(!is.null(old_layout)) {
  #         #   new_layout = old_layout[filter_bool_cells,]
  #         #   gobject@nn_network[['cells']][[spat_unit_name]][['sNN']][[snn_name]][['layout']] = new_layout
  #         # }
  #
  #         # igraph object
  #         old_graph = gobject@nn_network[['cells']][[spat_unit_name]][['sNN']][[snn_name]][['igraph']]
  #         vertices_to_keep = igraph::V(old_graph)[filter_bool_cells]
  #         new_subgraph = igraph::subgraph(graph = old_graph, v = vertices_to_keep)
  #         gobject@nn_network[['cells']][[spat_unit_name]][['sNN']][[snn_name]][['igraph']] = new_subgraph
  #       }
  #
  #     }
  #   }
  #
  # }

  return(gobject)
}



#' @title Subset spatial enrichment
#' @name subset_spatial_enrichment
#' @description Subset spatial enrichment results from giotto object
#' @keywords internal
#' @noRd
subset_spatial_enrichment = function(gobject,
                                     spat_unit,
                                     feat_type,
                                     cell_ids) {

  avail_enr = list_spatial_enrichments(gobject,
                                       spat_unit = spat_unit,
                                       feat_type = feat_type)

  if(!is.null(avail_enr)) {
    for(enr_i in seq(avail_enr[, .N])) {

      spatEnrObj = get_spatial_enrichment(gobject,
                                          spat_unit = avail_enr$spat_unit[[enr_i]],
                                          feat_type = avail_enr$feat_type[[enr_i]],
                                          enrichm_name = avail_enr$name[[enr_i]],
                                          output = 'spatEnrObj')

      spatEnrObj[] = spatEnrObj[][get('cell_ID') %in% cell_ids]

      ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
      gobject = set_spatial_enrichment(gobject, spatenrichment = spatEnrObj, verbose = FALSE)
      ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

    }
  }

  # if(!is.null(gobject@spatial_enrichment)) {
  #   for(spat_unit_name in names(gobject@spatial_enrichment)) {
  #
  #     for(feat_type_name in names(gobject@spatial_enrichment[[spat_unit_name]])) {
  #
  #       if(spat_unit_name == spat_unit & feat_type_name == feat_type) {
  #         for(spat_enrich_name in names(gobject@spatial_enrichment[[spat_unit_name]][[feat_type_name]])) {
  #
  #           gobject@spatial_enrichment[[spat_unit_name]][[feat_type_name]][[spat_enrich_name]] = gobject@spatial_enrichment[[spat_unit_name]][[feat_type_name]][[spat_enrich_name]][filter_bool_cells]
  #
  #         }
  #       }
  #
  #     }
  #
  #
  #   }
  # }

  return(gobject)
}






#' @title Subset giotto polygon object
#' @name subset_giotto_polygon_object
#' @description Subset a single giotto polygon object
#' @keywords internal
#' @noRd
subset_giotto_polygon_object = function(gpolygon,
                                        cell_ids,
                                        feat_ids,
                                        feat_type) {


  if(!is.null(gpolygon@spatVector)) {
    poly_IDs = gpolygon@spatVector$poly_ID
    cell_id_bool = poly_IDs %in% cell_ids
    gpolygon@spatVector = gpolygon@spatVector[cell_id_bool]
    gpolygon@unique_ID_cache = unique(poly_IDs[cell_id_bool]) # update cache
  }

  if(!is.null(gpolygon@spatVectorCentroids)) {
    cell_id_bool = gpolygon@spatVectorCentroids$poly_ID %in% cell_ids
    gpolygon@spatVectorCentroids = gpolygon@spatVectorCentroids[cell_id_bool]
  }

  if(!is.null(gpolygon@overlaps)) {

    for(feat in names(gpolygon@overlaps)) {
      cell_id_bool = gpolygon@overlaps[[feat]]$poly_ID %in% cell_ids
      gpolygon@overlaps[[feat]] = gpolygon@overlaps[[feat]][cell_id_bool]

      if(feat == feat_type) {
        feat_id_bool = gpolygon@overlaps[[feat]]$feat_ID %in% feat_ids
        gpolygon@overlaps[[feat]] = gpolygon@overlaps[[feat]][feat_id_bool]
      }

    }


  }

  return(gpolygon)

}



#' @title Subset spatial info data
#' @name subset_spatial_info_data
#' @description Subset  all spatial info (polygon) data
#' @keywords internal
#' @noRd
subset_spatial_info_data = function(spatial_info,
                                    cell_ids,
                                    poly_info = 'cell',
                                    feat_ids,
                                    feat_type = NULL,
                                    verbose = TRUE) {


  # set feat type
  if(is.null(feat_type)) {
    feat_type = 'rna'
  }

  res_list = list()
  for(spat_info in names(spatial_info)) {

    if(verbose) cat('for ', spat_info, '\n')

    if(spat_info %in% poly_info) {

      if(verbose) cat('--> ', spat_info, ' found back in polygon layer: ', poly_info, '\n')

      spat_subset = subset_giotto_polygon_object(spatial_info[[spat_info]],
                                                 cell_ids = cell_ids,
                                                 feat_ids = feat_ids,
                                                 feat_type = feat_type)

      res_list[[spat_info]] = spat_subset

    } else {

      if(!is.null(spatial_info[[spat_info]]@overlaps)) {

        for(feat in names(spatial_info[[spat_info]]@overlaps)) {

          if(feat == feat_type) {

            feat_id_bool = spatial_info[[spat_info]]@overlaps[[feat]]$feat_ID %in% feat_ids

            spatial_info[[spat_info]]@overlaps[[feat]] = spatial_info[[spat_info]]@overlaps[[feat]][feat_id_bool]
          }
        }
      }

      res_list[[spat_info]] = spatial_info[[spat_info]]
    }


  }
  return(res_list)
}


# subset giotto points

#' @title Subset giotto points object
#' @name subset_giotto_points_object
#' @description Subset a single giotto points object
#' @details Subset on feature ids and on x,y coordinates
#' @keywords internal
#' @noRd
subset_giotto_points_object = function(gpoints,
                                       feat_ids = NULL,
                                       x_min = NULL,
                                       x_max = NULL,
                                       y_min = NULL,
                                       y_max = NULL,
                                       verbose = FALSE) {

  # data.table vars
  x = y = feat_ID = NULL

  if(!is.null(gpoints@spatVector)) {

    if(!is.null(feat_ids)) {
      feat_id_bool = gpoints@spatVector$feat_ID %in% feat_ids
      gpoints@spatVector = gpoints@spatVector[feat_id_bool]
    }

    # spatial subset specific
    if(!any(is.null(c(x_min, x_max, y_min, y_max)))) {

      if(verbose) print('im1')

      myspatvector = gpoints@spatVector
      spatDT = spatVector_to_dt(myspatvector)

      if(verbose) print('im2')

      spatDT_subset = spatDT[x >= x_min & x <= x_max & y >= y_min & y <= y_max]
      myspatvector_subset = dt_to_spatVector_points(dt = spatDT_subset)

      if(verbose) print('im3')

      gpoints@spatVector = myspatvector_subset
      gpoints@unique_ID_cache = spatDT_subset[, unique(feat_ID)] # update cache
      return(gpoints)
    }

    # for when no spatial subsetting happens
    gpoints@unique_ID_cache = unique(terra::values(gpoints@spatVector)$feat_ID)

  }

  return(gpoints)

}



#' @title Subset feature info data
#' @name subset_feature_info_data
#' @description Subset all spatial feature (points) data
#' @keywords internal
#' @noRd
subset_feature_info_data = function(feat_info,
                                    feat_ids,
                                    feat_type = 'rna',
                                    x_min = NULL,
                                    x_max = NULL,
                                    y_min = NULL,
                                    y_max = NULL,
                                    verbose = FALSE) {

  res_list = list()
  for(feat in names(feat_info)) {

    if(verbose) print(feat)

    if(feat == feat_type) {

      feat_subset = subset_giotto_points_object(feat_info[[feat]],
                                                feat_ids = feat_ids,
                                                x_min = x_min,
                                                x_max = x_max,
                                                y_min = y_min,
                                                y_max = y_max,
                                                verbose = verbose)

      res_list[[feat]] = feat_subset

    } else {
      res_list[[feat]] = feat_info[[feat]]
    }


  }
  return(res_list)
}











#' @title subsetGiotto
#' @description Subsets Giotto object including previous analyses.
#' @inheritParams data_access_params
#' @param cell_ids cell IDs to keep
#' @param feat_ids feature IDs to keep
#' @param gene_ids deprecated. Use \code{feat_ids}
#' @param poly_info polygon information to use
#' @param all_spat_units subset all spatial units with selected feature ids
#' @param all_feat_types subset all feature type data with selected cell ids
#' @param x_max,x_min,y_max,y_min minimum and maximum x and y coordinates to keep for feature coordinates
#' @param verbose be verbose
#' @param toplevel_params parameters to extract
#' @return giotto object
#' @details Subsets a Giotto object for a specific spatial unit and feature type
#' @export
subsetGiotto <- function(gobject,
                         spat_unit = NULL,
                         feat_type = NULL,
                         cell_ids = NULL,
                         feat_ids = NULL,
                         gene_ids = NULL,
                         poly_info = NULL,
                         all_spat_units = TRUE,
                         all_feat_types = TRUE,
                         x_max = NULL,
                         x_min = NULL,
                         y_max = NULL,
                         y_min = NULL,
                         verbose = FALSE,
                         toplevel_params = 2) {

  # Set feat_type and spat_unit
  spat_unit = set_default_spat_unit(gobject = gobject,
                                    spat_unit = spat_unit)
  feat_type = set_default_feat_type(gobject = gobject,
                                    spat_unit = spat_unit,
                                    feat_type = feat_type)

  # set poly_info
  if(is.null(poly_info)) {
    poly_info = spat_unit
  }

  ## deprecated arguments
  if(!is.null(gene_ids)) {
    feat_ids = gene_ids
    warning('gene_ids argument is deprecated, use feat_ids argument in the future \n')
  }

  if(!is.null(slot(gobject, 'h5_file'))) {
    g_dimnames = HDF5Array::h5readDimnames(filepath = slot(gobject, 'h5_file'),
                                           name =  paste0("/expression/",feat_type,"/raw"))
    g_cell_IDs = g_dimnames[[2]]
    g_feat_IDs = g_dimnames[[1]]
  } else {
    # filter cell_ID and gene_ID
    g_cell_IDs = get_cell_id(gobject, spat_unit = spat_unit)
    g_feat_IDs = get_feat_id(gobject, feat_type = feat_type)

  }

  ## filter index
  if(!is.null(cell_ids)) {
    filter_bool_cells = g_cell_IDs %in% cell_ids
    cell_ids = g_cell_IDs[filter_bool_cells]
  } else {
    # set cell ids to all if not provided
    filter_bool_cells = g_cell_IDs %in% g_cell_IDs
    cell_ids = g_cell_IDs[filter_bool_cells]
  }

  if(!is.null(feat_ids)) {
    filter_bool_feats = g_feat_IDs %in% feat_ids
    feat_ids = g_feat_IDs[filter_bool_feats]
  } else {
    # set feat ids to all if not provided
    filter_bool_feats = g_feat_IDs %in% g_feat_IDs
    feat_ids = g_feat_IDs[filter_bool_feats]
  }







  if(verbose) cat('completed 1: preparation \n')


  ## FILTER ##
  # filter expression data
  gobject = subset_expression_data(gobject = gobject,
                                   cell_ids = cell_ids,
                                   feat_ids = feat_ids,
                                   feat_type = feat_type,
                                   spat_unit = spat_unit,
                                   all_spat_units = all_spat_units,
                                   all_feat_types = all_feat_types)

  if(verbose) cat('completed 2: subset expression data \n')


  # filter spatial locations

  gobject = subset_spatial_locations(gobject = gobject,
                                     cell_ids = cell_ids,
                                     spat_unit = spat_unit)

  if(verbose) cat('completed 3: subset spatial locations \n')

  # update ID slots now performed by intialization


  # gobject@cell_ID[[spat_unit]] = gobject@cell_ID[[spat_unit]][filter_bool_cells]
  # gobject@feat_ID[[feat_type]] = gobject@feat_ID[[feat_type]][filter_bool_feats]


  if(verbose) cat('completed 4: subset cell (spatial units) and feature IDs \n')


  ## cell & feature metadata ##
  # cell metadata
  gobject = subset_cell_metadata(gobject = gobject,
                                 feat_type = feat_type,
                                 cell_ids = cell_ids,
                                 spat_unit = spat_unit,
                                 all_feat_types = all_feat_types)

  if(verbose) cat('completed 5: subset cell metadata \n')

  # feature metadata
  gobject = subset_feature_metadata(gobject = gobject,
                                    feat_type = feat_type,
                                    spat_unit = spat_unit,
                                    feat_ids = feat_ids,
                                    all_spat_units = all_spat_units)

  if(verbose) cat('completed 6: subset feature metadata \n')


  ## spatial network & grid ##
  # cell spatial network
  gobject = subset_spatial_network(gobject = gobject,
                                   spat_unit = spat_unit,
                                   cell_ids = cell_ids)


  if(verbose) cat('completed 7: subset spatial network(s) \n')

  # spatial grid
  # need to be recomputed


  ## dimension reduction ##
  # cell dim reduction
  gobject = subset_dimension_reduction(gobject = gobject,
                                       spat_unit = spat_unit,
                                       feat_type = feat_type,
                                       cell_ids = cell_ids)

  if(verbose) cat('completed 8: subsetted dimension reductions \n')


  ## nn network ##
  gobject = subset_nearest_network(gobject = gobject,
                                   spat_unit = spat_unit,
                                   feat_type = feat_type,
                                   cell_ids =  cell_ids)

  if(verbose) cat('completed 9: subsetted nearest network(s) \n')


  ## spatial enrichment ##
  gobject = subset_spatial_enrichment(gobject = gobject,
                                      spat_unit = spat_unit,
                                      feat_type = feat_type,
                                      cell_ids = cell_ids)

  if(verbose) cat('completed 10: subsetted spatial enrichment results \n')

  ## spatial info
  if(!is.null(gobject@spatial_info)) {

    for(select_poly_info in poly_info) {

      gobject@spatial_info = subset_spatial_info_data(spatial_info = gobject@spatial_info,
                                                      feat_type = feat_type,
                                                      cell_ids = cell_ids,
                                                      feat_ids = feat_ids,
                                                      poly_info = select_poly_info,
                                                      verbose = verbose)

    }

    if(verbose) cat('completed 11: subsetted spatial information data \n')
  }


  ## feature info
  if(!is.null(gobject@feat_info)) {

    gobject@feat_info = subset_feature_info_data(feat_info = gobject@feat_info,
                                                 feat_ids = feat_ids,
                                                 feat_type = feat_type,
                                                 x_max = x_max,
                                                 x_min = x_min,
                                                 y_max = y_max,
                                                 y_min = y_min,
                                                 verbose = verbose)

    if(verbose) cat('completed 12: subsetted spatial feature data \n')
  }



  ## update parameters used ##

  # nframes = sys.nframe()
  # if(verbose) cat('number of frames: ', nframes, '\n')
  #
  # parent = sys.parent()
  # if(verbose) cat('sys parent: ', parent, '\n')

  parameters_info = update_giotto_params(gobject,
                                         description = '_subset',
                                         return_gobject = FALSE,
                                         toplevel = toplevel_params)

  # extra parameters to include
  cells_removed = length(filter_bool_cells[filter_bool_cells==FALSE])
  feats_removed = length(filter_bool_feats[filter_bool_feats==FALSE])

  parameters_list = parameters_info[['plist']]
  update_name = parameters_info[['newname']]

  parameters_list[[update_name]] = c(parameters_list[[update_name]],
                                     'cells removed' = cells_removed,
                                     'feats removed' = feats_removed)
  gobject@parameters = parameters_list

  # if(verbose){
  #   print(gobject@spatial_info)
  #   print(gobject@spatial_locs)
  # }

  return(initialize(gobject))

}




#' @title Subset by spatial locations
#' @name subsetGiottoLocs
#' @description Subsets Giotto object based on spatial locations
#' @inheritParams data_access_params
#' @param spat_loc_name name of spatial locations to use
#' @param x_max,x_min,y_max,y_min,z_max,z_min minimum and maximum x, y, and z coordinates
#'   to subset to
#' @param poly_info polygon information to use
#' @param return_gobject return Giotto object
#' @param verbose be verbose
#' @return giotto object
#' @details Subsets a Giotto based on spatial locations and for one provided spatial unit
#' if return_gobject = FALSE, then a filtered combined metadata data.table will be returned
#' @export
subsetGiottoLocs = function(gobject,
                            spat_unit = NULL,
                            feat_type = NULL,
                            spat_loc_name = NULL,
                            x_max = NULL,
                            x_min = NULL,
                            y_max = NULL,
                            y_min = NULL,
                            z_max = NULL,
                            z_min = NULL,
                            poly_info = 'cell',
                            return_gobject = TRUE,
                            verbose = FALSE) {


  # Set feat_type and spat_unit
  spat_unit = set_default_spat_unit(gobject = gobject,
                                    spat_unit = spat_unit)
  feat_type = set_default_feat_type(gobject = gobject,
                                    spat_unit = spat_unit,
                                    feat_type = feat_type)

  # Check spatial params
  spatError = NULL
  if(!is.null(x_min) && !is.null(x_max)) if(x_min > x_max) spatError = append(spatError, 'x_max must be larger than x_min \n')
  if(!is.null(y_min) && !is.null(y_max)) if(y_min > y_max) spatError = append(spatError, 'y_max must be larger than y_min \n')
  if(!is.null(z_min) && !is.null(z_max)) if(z_min > z_max) spatError = append(spatError, 'z_max must be larger than z_min \n')
  if(!is.null(spatError)) stop(spatError)


  # function requires spat_loc_name
  if(is.null(spat_loc_name)) {

    if(!is.null(slot(gobject, 'spatial_locs'))) {
      spat_loc_name = names(slot(gobject, 'spatial_locs')[[spat_unit]])[[1]]
      # cat('No spatial locations have been selected, the first one -',spat_loc_name, '- will be used \n')

    } else if(!is.null(slot(gobject, 'spatial_info'))) {
      # EXCEPTION: if no spatlocs found but polys exist, find cell_IDs from polys
      polys_list = slot(gobject, 'spatial_info')
      cropped_IDs = lapply(polys_list, function(x) {
        sv = slot(x, 'spatVector')
        sv = terra::crop(sv, terra::ext(x_min, x_max, y_min, y_max))
        sv$poly_ID
        # TODO add cropping for z values as well
      })

      cropped_IDs = unique(unlist(cropped_IDs))

      subset_object = subsetGiotto(gobject = gobject,
                                   spat_unit = spat_unit,
                                   feat_type = feat_type,
                                   cell_ids = cropped_IDs,
                                   poly_info = poly_info,
                                   x_max = x_max,
                                   x_min = x_min,
                                   y_max = y_max,
                                   y_min = y_min,
                                   verbose = verbose)

      return(subset_object)

    } else {
      spat_loc_name = NULL
      wrap_msg('No spatial locations or spatial info have been found \n')
      return(NULL)
    }
  }

  comb_metadata = combineMetadata(gobject = gobject,
                                  spat_unit = spat_unit,
                                  feat_type = feat_type,
                                  spat_loc_name = spat_loc_name)
  comb_colnames =  colnames(comb_metadata)

  # x spatial dimension
  if('sdimx' %in% comb_colnames) {
    if(is.null(x_max)) x_max = max(comb_metadata[['sdimx']])
    if(is.null(x_min)) x_min = min(comb_metadata[['sdimx']])

    comb_metadata = comb_metadata[get('sdimx') < x_max & get('sdimx') > x_min]
  }

  # y spatial dimension
  if('sdimy' %in% comb_colnames) {
    if(is.null(y_max)) y_max = max(comb_metadata[['sdimy']])
    if(is.null(y_min)) y_min = min(comb_metadata[['sdimy']])

    comb_metadata = comb_metadata[get('sdimy') < y_max & get('sdimy') > y_min]
  }

  # z spatial dimension
  if('sdimz' %in% comb_colnames) {
    if(is.null(z_max)) z_max = max(comb_metadata[['sdimz']])
    if(is.null(z_min)) z_min = min(comb_metadata[['sdimz']])

    comb_metadata = comb_metadata[get('sdimz') < z_max & get('sdimz') > z_min]
  }

  if(return_gobject == TRUE) {

    filtered_cell_IDs = comb_metadata[['cell_ID']]

    subset_object = subsetGiotto(gobject = gobject,
                                 spat_unit = spat_unit,
                                 feat_type = feat_type,
                                 cell_ids = filtered_cell_IDs,
                                 poly_info = poly_info,
                                 x_max = x_max,
                                 x_min = x_min,
                                 y_max = y_max,
                                 y_min = y_min,
                                 verbose = verbose)

    return(subset_object)

  } else {
    return(comb_metadata)
  }

}




#' @title Subset by spatial locations -- multi
#' @name subsetGiottoLocsMulti
#' @description Subsets Giotto object based on spatial locations
#' @inheritParams subsetGiottoLocs
#' @return giotto object
#' @details Subsets a Giotto based on spatial locations for multiple spatial units
#' if return_gobject = FALSE, then a filtered combined metadata data.table will be returned
#' @export
subsetGiottoLocsMulti = function(gobject,
                                 spat_unit = NULL,
                                 feat_type = NULL,
                                 spat_loc_name = NULL,
                                 x_max = NULL,
                                 x_min = NULL,
                                 y_max = NULL,
                                 y_min = NULL,
                                 z_max = NULL,
                                 z_min = NULL,
                                 poly_info = NULL,
                                 return_gobject = TRUE,
                                 verbose = TRUE) {




  res_list = list()

  for(spat_unit_selected in spat_unit) {

    poly_info_selected = poly_info[[spat_unit_selected]]

    cat('\n \n')

    if(verbose) wrap_msg('Start subset on location for spatial unit: ', spat_unit_selected,
                         'and polygon information layers: ', poly_info_selected, '\n')


    if(return_gobject == TRUE) {
      gobject = subsetGiottoLocs(gobject = gobject,
                                 spat_unit = spat_unit_selected,
                                 feat_type = feat_type,
                                 spat_loc_name = spat_loc_name,
                                 x_max = x_max,
                                 x_min = x_min,
                                 y_max = y_max,
                                 y_min = y_min,
                                 z_max = z_max,
                                 z_min = z_min,
                                 poly_info = poly_info_selected,
                                 return_gobject = return_gobject,
                                 verbose = verbose)
    } else {

      res_list[[spat_unit_selected]] = subsetGiottoLocs(gobject = gobject,
                                                        spat_unit = spat_unit_selected,
                                                        feat_type = feat_type,
                                                        spat_loc_name = spat_loc_name,
                                                        x_max = x_max,
                                                        x_min = x_min,
                                                        y_max = y_max,
                                                        y_min = y_min,
                                                        z_max = z_max,
                                                        z_min = z_min,
                                                        poly_info = poly_info_selected,
                                                        return_gobject = return_gobject,
                                                        verbose = verbose)

    }
  }

  if(return_gobject == TRUE) {
    return(gobject)
  } else {
    return(res_list)
  }

}





#' @title Subset raw subcellular information by location
#' @name subsetGiottoLocsSubcellular
#' @description Subsets Giotto object based on spatial coordinates
#' @inheritParams subsetGiottoLocs
#' @return giotto object
#' @details Subsets a Giotto Subcellular object - without aggregated information - based on spatial coordinates
#' @export
subsetGiottoLocsSubcellular = function(gobject,
                                       poly_info,
                                       feat_type = NULL,
                                       x_min = NULL,
                                       x_max = NULL,
                                       y_min = NULL,
                                       y_max = NULL,
                                       z_max = NULL,
                                       z_min = NULL,
                                       verbose = FALSE) {

  # only to be used if there is no aggregated information #
  if(!is.null(gobject@expression)) {
    stop('Aggregated information was found, use subsetGiottoLocs \n')
  }

  # Check spatial params
  spatError = NULL
  if(!is.null(x_min) && !is.null(x_max)) if(x_min > x_max) spatError = append(spatError, 'x_max must be larger than x_min \n')
  if(!is.null(y_min) && !is.null(y_max)) if(y_min > y_max) spatError = append(spatError, 'y_max must be larger than y_min \n')
  if(!is.null(z_min) && !is.null(z_max)) if(z_min > z_max) spatError = append(spatError, 'z_max must be larger than z_min \n')
  if(!is.null(spatError)) stop(spatError)


  # first subset feature ids based on location
  # this information could be needed for spatial_info if overlaps were calculated


  ## 1. feature info ##
  ## --------------- ##
  if(!is.null(gobject@feat_info)) {

    # TODO: make it possible for multiple feature types

    feats_list = slot(gobject, 'feat_info')
    cropped_feats = lapply(feats_list[[feat_type]], function(x) {
      sv = slot(x, 'spatVector')
      sv = terra::crop(sv, terra::ext(x_min, x_max, y_min, y_max))
      sv$feat_ID
      # TODO add cropping for z values as well
    })

    cropped_feats = unique(unlist(cropped_feats))

    gobject@feat_info = subset_feature_info_data(feat_info = gobject@feat_info,
                                                 feat_ids = cropped_feats,
                                                 feat_type = feat_type,
                                                 x_max = x_max,
                                                 x_min = x_min,
                                                 y_max = y_max,
                                                 y_min = y_min,
                                                 verbose = verbose)

    if(verbose == TRUE) cat('subsetted spatial feature data \n')
  } else {

    cropped_feats = NULL

  }


  ## 2. spatial info ###
  ## ---------------- ##
  if(!is.null(gobject@spatial_info)) {

    # get the associated poly_IDs
    polys_list = slot(gobject, 'spatial_info')
    cropped_IDs = lapply(polys_list, function(x) {
      sv = slot(x, 'spatVector')
      sv = terra::crop(sv, terra::ext(x_min, x_max, y_min, y_max))
      sv$poly_ID
      # TODO add cropping for z values as well
    })

    cropped_IDs = unique(unlist(cropped_IDs))

    for(select_poly_info in poly_info) {

      gobject@spatial_info = subset_spatial_info_data(spatial_info = gobject@spatial_info,
                                                      feat_type = feat_type,
                                                      cell_ids = cropped_IDs,
                                                      feat_ids = cropped_feats,
                                                      poly_info = select_poly_info)

    }

    if(verbose == TRUE) cat('subsetted spatial information data \n')
  }


  # TODO: update parameters

  return(initialize(gobject))

}

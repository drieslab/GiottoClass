### subset Giotto object slots ####


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

  if (!is.null(output_table)) {

    # loop through expression objects and update accordingly
    for(row in 1:nrow(output_table)) {

      spat_unit_name = output_table[row][['spat_unit']]
      feat_type_name = output_table[row][['feat_type']]
      expression_name = output_table[row][['name']]


      if(feat_type_name == feat_type && spat_unit_name == spat_unit) {

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

      } else if(feat_type_name == feat_type && spat_unit_name != spat_unit) {

        if(all_spat_units) {

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
            # m = mirai::mirai({
            #     DelayedArray::realize(expression_info[, filter_bool_cells], "HDF5Array")
            #   },
            #   expression_info = S4_expr@exprMat,
            #   filter_bool_cells = filter_bool_cells
            # )
            S4_expr@exprMat = DelayedArray::realize(S4_expr@exprMat[, filter_bool_cells], "HDF5Array")
          } else {
            # m = mirai::mirai({
            #     expression_info[, filter_bool_cells]
            #   },
            #   expression_info = S4_expr@exprMat,
            #   filter_bool_cells = filter_bool_cells
            # )
            S4_expr@exprMat = S4_expr@exprMat[, filter_bool_cells]
          }

          # S4_expr@exprMat = m

          ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
          ### set dummy
          gobject = set_expression_values(gobject = gobject,
                                          values = S4_expr,
                                          verbose = FALSE)
          # actual value
          # gobject = set_mirai(gobject, S4_expr)
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
#' @param all_spat_units logical. Applies subset operation across the whole gobject
#' (ALL spat_units), ignoring the \code{spat_unit} input param. Defaults to TRUE.
#' @keywords internal
#' @noRd
subset_spatial_locations = function(gobject,
                                    cell_ids,
                                    spat_unit,
                                    all_spat_units = TRUE) {

  # DT vars
  name = NULL

  if(all_spat_units) {
    avail_locs = list_spatial_locations(gobject)
  } else {
    avail_locs = list_spatial_locations(gobject, spat_unit = spat_unit)
  }

  # 3. get, subset, and set back a single spatLocsObj
  # returns a gobject
  do_subset = function(su, sname) {
    spatObj = get_spatial_locations(gobject,
                                    spat_unit = su,
                                    spat_loc_name = sname,
                                    output = 'spatLocsObj',
                                    copy_obj = FALSE)

    ## filter index
    g_cell_IDs = spatObj@coordinates[['cell_ID']]

    if(!is.null(cell_ids)) {
      filter_bool_cells = g_cell_IDs %in% cell_ids
    } else filter_bool_cells = g_cell_IDs %in% g_cell_IDs

    spatObj[] = spatObj[][filter_bool_cells]

    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    gobject <<- set_spatial_locations(gobject, spatlocs = spatObj, verbose = FALSE)
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    # not yet possible to row subset data.tables by reference. Must be set back in.
  }

  # 2. for each spat unit, subset all contained spatLocsObjs
  per_spat_unit = function(su) {
    spat_names = avail_locs[spat_unit == su, name]
    for(sname in spat_names) {
      do_subset(su, sname)
    }
  }

  # 1. for each spatial unit requested to be subset...
  for(su in unique(avail_locs$spat_unit)) {
    per_spat_unit(su = su)
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
                                  cell_ids,
                                  all_spat_units = TRUE) {

  # DT vars
  to = from = name = NULL

  # if no spatial networks available, return directly
  if(is.null(slot(gobject, 'spatial_network'))) {
    return(gobject)
  }

  # Find existing networks and return as DT
  if(all_spat_units) {
    existing_networks = list_spatial_networks(gobject = gobject)
  } else {
    existing_networks = list_spatial_networks(gobject = gobject,
                                              spat_unit = spat_unit)
  }

  do_subset = function(su, nname) {
    spatNetObj = get_spatialNetwork(gobject = gobject,
                                    spat_unit = su,
                                    name = nname,
                                    output = 'spatialNetworkObj')

    # Within each spatialNetworkObj, subset only the cells_to_keep
    spatNetObj[] = spatNetObj[][to %in% cell_ids & from %in% cell_ids]

    # Set the spatialNetworkObj back into the gobject
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    gobject <<- set_spatialNetwork(gobject = gobject,
                                   spatial_network = spatNetObj,
                                   verbose = FALSE)
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  }

  per_spat_unit = function(su) {
    net_names = existing_networks[spat_unit == su, name]
    for(nname in net_names) {
      do_subset(su, nname)
    }
  }

  for(su in unique(existing_networks$spat_unit)) {
    per_spat_unit(su = su)
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
                                      cell_ids,
                                      all_feat_types = TRUE,
                                      all_spat_units = TRUE) {

  # find available dim reductions
  avail_dim = list_dim_reductions(
    gobject = gobject,
    data_type = 'cells',
    spat_unit = if(all_spat_units) NULL else spat_unit,
    feat_type = if(all_feat_types) NULL else feat_type
  )

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
                                  cell_ids,
                                  all_spat_units = TRUE,
                                  all_feat_types = TRUE) {

  avail_kNN = list_nearest_networks(
    gobject,
    spat_unit = if(all_spat_units) NULL else spat_unit,
    feat_type = if(all_feat_types) NULL else feat_type,
    nn_type = 'kNN'
  )
  avail_sNN = list_nearest_networks(
    gobject,
    spat_unit = if(all_spat_units) NULL else spat_unit,
    feat_type = if(all_feat_types) NULL else feat_type,
    nn_type = 'sNN'
  )

  if(!is.null(avail_kNN)) {

    for(nn_i in seq(avail_kNN[, .N])) {
      nnObj = get_NearestNetwork(gobject = gobject,
                                 spat_unit = avail_kNN$spat_unit[[nn_i]],
                                 feat_type = avail_kNN$feat_type[[nn_i]],
                                 nn_network_to_use = 'kNN',
                                 network_name = avail_kNN$name[[nn_i]],
                                 output = 'nnNetObj')

      #vertices_to_keep = igraph::V(nnObj[])[filter_bool_cells]
      vids = which(cell_ids %in% igraph::V(nnObj[])$name)
      nnObj[] = igraph::induced_subgraph(graph = nnObj[], vids = vids)

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
      vids = which(cell_ids %in% igraph::V(nnObj[])$name)
      nnObj[] = igraph::induced_subgraph(graph = nnObj[], vids = vids)

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
                                     cell_ids,
                                     all_spat_units = TRUE,
                                     all_feat_types = TRUE) {

  avail_enr = list_spatial_enrichments(
    gobject,
    spat_unit = if(all_spat_units) NULL else spat_unit,
    feat_type = if(all_feat_types) NULL else feat_type
  )

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
#' @description Subset a single giotto polygon object for cell_ids and feat_ids
#' @param gpolygon giottoPolygon object to subset
#' @param cell_ids character. cell_ids to keep
#' @param feat_ids character. feat_ids to keep
#' @param feat_type character. feature type to subset feat_ids from if overlaps
#' are present within the giottoPolygon object
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
#' @description Subset all spatial info (polygon) data.
#' @param spatial_info contents of the Giotto spatial_info slot
#' @param cell_ids character. cell ids to keep
#' @param poly_info character. polygon(s) to subset
#' @param feat_type feature type of overlaps to subset if they exist within the
#' giottoPolygon
#' @param feat_ids character. feat ids to keep
#' @keywords internal
#' @noRd
subset_spatial_info_data = function(spatial_info,
                                    cell_ids,
                                    poly_info = 'cell',
                                    feat_type = NULL,
                                    feat_ids,
                                    verbose = TRUE) {


  # set feat type
  if(is.null(feat_type)) {
    feat_type = 'rna'
  }

  res_list = list()
  # iterate through all spatial info entries...
  for(spat_info in names(spatial_info)) {

    if(verbose) cat('for ', spat_info, '\n')

    # if the spatial info is one selected in poly_info...
    if(spat_info %in% poly_info) {

      if(verbose) cat('--> ', spat_info, ' found back in polygon layer: ', poly_info, '\n')

      # subset the giottoPolygon object for the cell_ids and the specified
      # feat_type overlap information (if existing) for the feat_ids
      spat_subset = subset_giotto_polygon_object(spatial_info[[spat_info]],
                                                 cell_ids = cell_ids,
                                                 feat_ids = feat_ids,
                                                 feat_type = feat_type)

      res_list[[spat_info]] = spat_subset

    } else {
      # even if the spatial info is not one selected directly through poly_info,
      # still subset subset any existing feature overlaps matching the feat_type
      # for the feat_ids
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

  # 0. check if spatial feature information exists
  if(is.null(gpoints@spatVector)) {
    return(gpoints) # return without change since there is no points info
  }


  # 1. ID based subsetting #
  # ---------------------- #
  if(!is.null(feat_ids)) {
    feat_id_bool = gpoints@spatVector$feat_ID %in% feat_ids
    gpoints@spatVector = gpoints@spatVector[feat_id_bool]
  }

  # 2. Spatial subsetting #
  # --------------------- #
  # 2.1 if NO spatial subset information available,
  # ie: if all spat subset params are NULL, return directly because there are
  # no following steps
  if(all(sapply(list(x_min, x_max, y_min, y_max), is.null))) {
    # even if no spatial subsetting happened, if ID subsetting occurred, the
    # unique_ID_cache needs to be updated
    gpoints@unique_ID_cache = unique(terra::values(gpoints@spatVector)$feat_ID)
    return(gpoints)
  }

  # 2.2 otherwise use DT crop method
  gpoints = crop(gpoints,
                 xmin = x_min, xmax = x_max,
                 ymin = y_min, ymax = y_max,
                 DT = TRUE)

  return(gpoints)
}



#' @title Subset feature info data
#' @name subset_feature_info_data
#' @description Subset all spatial feature (points) data
#' @param feat_info contents of giotto object feat_info slot
#' @param feat_ids character. feat ids to keep
#' @param feat_type character vector. feature type(s) to subset
#' @param x_min,x_max,y_min,y_max spatial bounds to subset by
#' @param verbose be verbose
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






# Exported subset functions ####


# Overview of subset functions #
#
# subsetGiotto
#   Main subsetting pipeline that subsets based on the aggregated information
#   available. It determines a set of IDs (cell_ids or feat_ids) to subset with
#   and then walks through each of the slots, performing the subset.
# subsetGiottoLocs
#   Pulls cell_IDs information and spatial locations and performs a spatial
#   subset. The cell_IDs that are selected are then fed back into subsetGiotto()
#   This operation is performed only for a single spat_unit
# subsetGiottoLocsMulti
#   Performs more than one subset operation using subsetGiottoLocs
# subsetGiottoLocsSubcellular
#   Performs the spatial subset only on the subcellular spatial info (polygons)
#   and feature info (points). This is useful for situations in which aggregate
#   information has not been created but the subcellular data is present.


# all_spat_units and all_feat_types - may need name changes



# TODO
# Consider an `across_spat_units` and `across_feat_types` for finer control
# with a special ':all:' input that will apply to all spat_units/feat_types

# Hierarchical subsetting?


#' @title subsetGiotto
#' @description Subsets Giotto object including previous analyses. For subsetting
#' the subcellular information only without editing the aggregate information,
#' use [subsetGiottoLocsSubcellular]
#' @inheritParams data_access_params
#' @param cell_ids character. cell IDs to keep
#' @param feat_ids character. feature IDs to keep
#' @param poly_info character. polygon info(s) to subset if present. (defaults
#' to be the same as the spat_unit)
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
                         poly_info = NULL,
                         all_spat_units = TRUE,
                         all_feat_types = TRUE,
                         x_max = NULL,
                         x_min = NULL,
                         y_max = NULL,
                         y_min = NULL,
                         verbose = FALSE,
                         toplevel_params = 2) {

  # mirai::daemons(n = GiottoUtils::determine_cores())
  #
  # # mirai cleanup
  # on.exit({
  #   mirai::daemons(0) # reset
  # })


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


  # spatial subsetting #
  # pass to subsetGiottoLocs if not all spatial subset params are NULL
  if(!all(sapply(list(x_max, x_min, y_min, y_max), is.null))) {
    comb_metadata = subsetGiottoLocs(
      gobject = gobject,
      spat_unit = spat_unit,
      feat_type = feat_type,
      x_min = x_min, x_max = x_max,
      y_min = y_min, y_max = y_max,
      return_gobject = FALSE # returned the combined and spatially subset
      # metadata instead
    )

    # get spatially filtered cell_IDs
    # filter current IDs to keep (cell_ids) by selecting only those that are
    # also within the spat_filtered_cell_ids
    spat_filtered_cell_ids = comb_metadata[['cell_ID']]
    if (is.null(cell_ids)) cell_ids = spat_filtered_cell_ids
    else cell_ids = cell_ids[cell_ids %in% spat_filtered_cell_ids]
  }


  # all subsetting operations below here should rely on cell_ID or feat_ID #
  # ---------------------------------------------------------------------- #




  if(!is.null(slot(gobject, 'h5_file'))) {
    g_dimnames = HDF5Array::h5readDimnames(filepath = slot(gobject, 'h5_file'),
                                           name =  paste0("/expression/",feat_type,"/raw"))
    g_cell_IDs = g_dimnames[[2]]
    g_feat_IDs = g_dimnames[[1]]
  } else {
    # filter cell_ID and feat_ID
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
                                     spat_unit = spat_unit,
                                     all_spat_units = all_spat_units)

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
                                   cell_ids = cell_ids,
                                   all_spat_units = all_spat_units)


  if(verbose) cat('completed 7: subset spatial network(s) \n')

  # spatial grid
  # need to be recomputed


  ## dimension reduction ##
  # cell dim reduction
  gobject = subset_dimension_reduction(gobject = gobject,
                                       spat_unit = spat_unit,
                                       feat_type = feat_type,
                                       cell_ids = cell_ids,
                                       all_feat_types = all_feat_types,
                                       all_spat_units = all_spat_units)

  if(verbose) cat('completed 8: subsetted dimension reductions \n')


  ## nn network ##
  gobject = subset_nearest_network(gobject = gobject,
                                   spat_unit = spat_unit,
                                   feat_type = feat_type,
                                   cell_ids = cell_ids,
                                   all_spat_units = all_spat_units,
                                   all_feat_types = all_feat_types)

  if(verbose) cat('completed 9: subsetted nearest network(s) \n')


  ## spatial enrichment ##
  gobject = subset_spatial_enrichment(gobject = gobject,
                                      spat_unit = spat_unit,
                                      feat_type = feat_type,
                                      cell_ids = cell_ids,
                                      all_spat_units = all_spat_units,
                                      all_feat_types = all_feat_types)

  if(verbose) cat('completed 10: subsetted spatial enrichment results \n')

  ## spatial info
  if(!is.null(gobject@spatial_info)) {

    for(select_poly_info in poly_info) {

      # for each entry entry in poly_info, subset using cell_ids
      # note that even if no poly_info is selected, the overlaps slots that match
      # the feat_type param will be subset using feat_ids
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



# browser()
#   # mirai
#   mirai_res = get_mirai_list(gobject)
#   if (length(mirai_res > 0)) {
#
#     lapply(mirai_res, function(res) {
#       # wait until everything is done evaluating
#       mirai::call_mirai(res)
#
#       # handle errors
#       if(mirai::is_error_value(res)) stop(wrap_txt('mirai', res$data))
#
#       # set value
#       gobject <<- setGiotto(gobject, x = res$data, verbose = FALSE)
#
#       return(NULL) # lapply itself should not return anything
#     })
#
#     # TODO clear results
#     gobject@mirai <- list()
#     gobject <<- gobject
#
#   }



  return(initialize(gobject))

}




#' @title Subset by spatial locations
#' @name subsetGiottoLocs
#' @description Subsets Giotto object spatially by defining a set of
#' cropping bounds. The information to be subset is preferred to be from spatial
#' locations. If no `spat_loc_name` is given, the first available set of
#' spatial locations for the `spat_unit` will be picked.
#' If no spatial locations are available, the polygon information will be subset.
#' Spatial IDs surviving the crop are then applied to the rest of the Giotto object
#' using [subsetGiotto].
#'
#' The spatial subset is only performed on one spatial unit at a time.\cr
#' For subsetting more than one spatial unit, see [subsetGiottoLocsMulti]\cr
#' This function
#' @inheritParams data_access_params
#' @param spat_loc_name name of spatial locations to use within spat_unit
#' @param spat_unit spatial unit to subset
#' @param feat_type
#' @param x_max,x_min,y_max,y_min,z_max,z_min minimum and maximum x, y, and z coordinates
#'   to subset to
#' @param poly_info character. polygon information to subset
#' @param return_gobject return Giotto object
#' @param verbose be verbose
#' @return giotto object
#' @details If `return_gobject = FALSE`, then a filtered combined metadata
#' data.table will be returned
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

  # FOR AGGREGATE DATA #
  # Spatial subsetting is performed on the spatial locations information #
  # The IDs after this operation are then used to subset the rest of the #
  # Giotto object using subsetGiotto()                                   #

  # 0. Set feat_type and spat_unit
  spat_unit = set_default_spat_unit(gobject = gobject,
                                    spat_unit = spat_unit)
  feat_type = set_default_feat_type(gobject = gobject,
                                    spat_unit = spat_unit,
                                    feat_type = feat_type)

  checkmate::assert_character(spat_unit)
  if (length(spat_unit) > 1) stop(wrap_txt(
    'subsetGiottoLocs: Length of spat_unit > 1
    Use subsetGiottoLocsMulti() for spatial subsets across multiple spat_units'
  ))


  # 1. Check spatial params
  valid_spat_subset_params(x_min = x_min, x_max = x_max,
                           y_min = y_min, y_max = y_max,
                           z_min = z_min, z_max = z_max)


  # function requires spat_loc_name
  if(is.null(spat_loc_name)) {

    # first check spatial locations
    if (!is.null(slot(gobject, 'spatial_locs'))) {
      # get name of first available spatial unit if none provided
      spat_loc_name = list_spatial_locations_names(
        gobject = gobject,
        spat_unit = spat_unit
      )[[1]]
      # cat('No spatial locations have been selected, the first one -',spat_loc_name, '- will be used \n')

      # if spatlocs missing, check spatial_info
    } else if(!is.null(slot(gobject, 'spatial_info'))) {

      if (!return_gobject) {
        stop(wrap_txt(
          'subsetGiottoLocs:
          No spatial locations for spat_unit', paste0('\'', spat_unit, '\''),
          'have been found.
          return_gobject = FALSE is not possible when defaulting to polygon info
          subsetting.',
          errWidth = TRUE
        ))
      }

      # EXCEPTION: if no spatlocs found but polys exist, find cell_IDs from polys
      # by spatially scanning across all of them. The resulting cell_ID list
      # can be expected not be specific for the spatial unit, but this should
      # be fine in most cases since subsetGiotto() cell_ID input is which cells
      # to keep as opposed to which to exclude.
      polys_list = slot(gobject, 'spatial_info')
      cropped_IDs = lapply(polys_list, function(x) {
        terra::crop(x, terra::ext(x_min, x_max, y_min, y_max))@unique_ID_cache
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

  if(return_gobject) {

    filtered_cell_IDs = comb_metadata[['cell_ID']]

    # assumes that all spatlocs within a spat unit contain the same cell_IDs
    subset_object = subsetGiotto(gobject = gobject,
                                 spat_unit = spat_unit,
                                 feat_type = feat_type,
                                 cell_ids = filtered_cell_IDs,
                                 poly_info = poly_info,
                                 x_max = x_max,
                                 x_min = x_min,
                                 y_max = y_max,
                                 y_min = y_min,
                                 all_spat_units = FALSE,
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
#' @param spat_unit character vector of multiple spatial units
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

    if(verbose) wrap_msg('Start subset on locations for spatial unit: ', spat_unit_selected,
                         'and polygon information layers: ', poly_info_selected, '\n')


    if(return_gobject) {
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

  if(return_gobject) {
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
    stop(wrap_txt('Aggregated information was found in gobject.
                  Use subsetGiottoLocs() instead'))
  }

  # Check spatial params
  valid_spat_subset_params(x_min = x_min, x_max = x_max,
                           y_min = y_min, y_max = y_max,
                           z_min = z_min, z_max = z_max)


  # first subset feature ids based on location
  # this information could be needed for spatial_info if overlaps were calculated


  ## 1. feature info ##
  ## --------------- ##
  if(!is.null(gobject@feat_info)) {

    # perform crop and return to gobject
    gpoints_list = get_feature_info_list(gobject, return_giottoPoints = TRUE)[feat_type]
    cropped_gpoints = lapply(gpoints_list, function(x) {
      crop(gpoints_list[[x]],
           DT = TRUE,
           xmin = x_min, xmax = x_max,
           ymin = y_min, ymax = y_max)
      # TODO add cropping for z values as well
    })
    gobject@feat_info = cropped_gpoints

    # extract the cropped feature IDs for use with spatial info overlaps
    cropped_feats = lapply(cropped_gpoints, function(cropped_x) {
      cropped_x@unique_ID_cache
    })
    cropped_feats = unique(unlist(cropped_feats))

    if(verbose) wrap_msg('subsetted spatial feature data')
  } else {

    cropped_feats = NULL

  }


  ## 2. spatial info ###
  ## ---------------- ##
  if(!is.null(gobject@spatial_info)) {

    # get the associated poly_IDs
    polys_list = get_polygon_info_list(gobject, return_giottoPolygon = TRUE)
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

    if(verbose) wrap_msg('subsetted spatial information data')
  }


  # TODO: update parameters

  return(initialize(gobject))

}




# helpers ####

valid_spat_subset_params = function(
    x_min = NULL,
    x_max = NULL,
    y_min = NULL,
    y_max = NULL,
    z_min = NULL,
    z_max = NULL
) {
  # Check spatial params
  spatError = NULL
  if(!is.null(x_min) && !is.null(x_max)) {
    if(x_min > x_max) {
      spatError = append(spatError, 'x max must be larger than x min \n')
    }
  }
  if(!is.null(y_min) && !is.null(y_max)) {
    if(y_min > y_max) {
      spatError = append(spatError, 'y max must be larger than y min \n')
    }
  }
  if(!is.null(z_min) && !is.null(z_max)) {
    if(z_min > z_max) {
      spatError = append(spatError, 'z max must be larger than z min \n')
    }
  }

  if(!is.null(spatError)) {
    stop('Invalid spatial subset params:\n',
         spatError,
         call. = FALSE)
  }
}








#### slot checks ####

#' @keywords internal
#' @noRd
.check_cell_metadata <- function(gobject,
    verbose = TRUE) {
    # data.table vars
    cell_ID <- spat_unit <- NULL

    # find available cell metadata
    avail_cm <- list_cell_metadata(gobject)
    g_su <- list_cell_id_names(gobject)
    used_su <- unique(avail_cm$spat_unit)


    # [check hierarchical]
    # metadata is only allowed to exist when the associated spat_unit exists in
    # polygon and/or expression data
    missing_su <- !used_su %in% g_su
    if (any(missing_su)) {
        stop(wrap_txt("No expression or polygon information discovered for
            spat_unit:", used_su[missing_su],
            "Please add expression or polygon information for this spatial",
            "unit first",
            errWidth = TRUE
        ))
    }

    for (su_i in used_su) {
        IDs <- spatIDs(gobject, spat_unit = su_i)
        search_IDs <- c(head(IDs, 10L), tail(IDs, 10L))

        su_cm <- avail_cm[spat_unit == su_i, ]
        lapply(seq(nrow(su_cm)), function(obj_i) {
            # get metadata
            meta <- getCellMetadata(
                gobject = gobject,
                spat_unit = su_i,
                feat_type = su_cm$feat_type[[obj_i]],
                output = "cellMetaObj",
                copy_obj = FALSE,
                set_defaults = FALSE
            )

            # no cell_IDs
            if (any(meta[][, is.na(cell_ID)])) {
                # denotes missing or need to repair IDs

                ID_col_guess <- which.max(vapply(
                    meta[],
                    function(x) sum(search_IDs %in% x),
                    FUN.VALUE = integer(1L)
                ))

                if (ID_col_guess == 0L) {
                    # likely that cell_ID col does not exist yet
                    if (length(IDs) == nrow(meta[])) {
                        if (isTRUE(verbose)) {
                            wrap_msg("No cell_ID info found within cell metadata
                                Directly assigning based on gobject cell_ID")
                        }
                        meta[][, cell_ID := IDs] # set by reference
                    } else {
                        stop(wrap_txt("No cell_ID info found within cell
                            metadata and unable",
                            "to guess IDs based on gobject cell_ID",
                            errWidth = TRUE
                        ))
                    }
                } else { # otherwise, cell_ID col found
                    ID_col_name <- names(ID_col_guess)
                    meta[][, cell_ID := NULL] # remove older column
                    data.table::setnames(
                        meta[],
                        old = ID_col_name, new = "cell_ID"
                    )
                    if (isTRUE(verbose)) {
                        wrap_msg(
                            "Cell metadata: guessing", ID_col_name,
                            "as cell_ID column."
                        )
                    }
                }

                # cell ID guessing and assignment done #
            }


            # duplicated IDs
            if (any(meta[][, duplicated(cell_ID)])) {
                stop(wrap_txt(
                    "Cell metadata: duplicates found in cell_ID column.",
                    errWidth = TRUE
                ))
            }

            # length mismatch
            if (nrow(meta[]) > length(IDs)) {
                m_IDs <- meta[][["cell_ID"]]
                filter_bool_cells <- m_IDs %in% IDs

                meta[] <- meta[][filter_bool_cells]
            }

            if (nrow(meta[]) < length(IDs)) {
                ID_dt <- data.table::data.table(cell_ID = IDs)
                meta[] <- merge(ID_dt, meta[], all.x = TRUE)
            }

            if (nrow(meta[]) != length(IDs)) {
                stop(wrap_txt(
                    "Cell metadata: number of entries does not match",
                    "number of gobject IDs for this spat_unit (",
                    length(IDs), ")",
                    errWidth = TRUE
                ))
            }

            # cell_ID  contents mismatch
            if (!meta[][, setequal(cell_ID, IDs)]) {
                stop(wrap_txt(
                    "Cell_metadata: IDs do not match between metadata and
                    cell_ID slot for this spat_unit"
                ))
            }

            # ensure ID col first
            setcolorder(meta[], "cell_ID")
        })
    }
}


#' @keywords internal
#' @noRd
.check_feat_metadata <- function(gobject,
    verbose = TRUE) {
    # data.table vars
    feat_ID <- spat_unit <- feat_type <- NULL

    # find available feat metadata
    avail_fm <- list_feat_metadata(gobject)
    avail_ex <- list_expression(gobject)
    g_ft <- list_feat_id_names(gobject)
    used_ft <- unique(avail_fm$feat_type)


    # check hierarchical
    missing_ft <- !used_ft %in% g_ft
    if (any(missing_ft)) {
        stop(wrap_txt(
            "No expression or polygon information discovered for feat_type:",
            used_ft[missing_ft],
            "Please add expression or polygon information for this feature",
            "type first",
            errWidth = TRUE
        ))
    }

    for (ft_i in used_ft) {
        ft_fm <- avail_fm[feat_type == ft_i, ]
        lapply(seq(nrow(ft_fm)), function(obj_i) {
            su_i <- ft_fm$spat_unit[[obj_i]]

            # get metadata
            meta <- getFeatureMetadata(
                gobject = gobject,
                spat_unit = su_i,
                feat_type = ft_i,
                output = "featMetaObj",
                copy_obj = FALSE,
                set_defaults = FALSE
            )

            # Start checking values when specific expression is added
            if (is.null(avail_ex)) {
                return()
            }

            if (!nrow(avail_ex[spat_unit == su_i & feat_type == ft_i]) == 0L) {
                IDs <- featIDs(getExpression(
                    gobject = gobject,
                    spat_unit = su_i,
                    feat_type = ft_i,
                    output = "exprObj"
                ))
            } else {
                return() # skip checks if no expression found
            }



            # check metadata
            search_IDs <- c(head(IDs, 10L), tail(IDs, 10L))

            # no feat_IDs
            if (any(meta[][, is.na(feat_ID)])) {
                # denotes missing or need to repair IDs

                ID_col_guess <- which.max(vapply(
                    meta[],
                    function(x) sum(search_IDs %in% x),
                    FUN.VALUE = integer(1L)
                ))

                if (ID_col_guess == 0L) {
                    # likely that feat_ID col does not exist yet
                    if (length(IDs) == nrow(meta[])) {
                        if (isTRUE(verbose)) {
                            wrap_msg("No feat_ID info found within feat metadata
                                Directly assigning based on gobject feat_ID")
                        }
                        meta[][, feat_ID := IDs] # set by reference
                    } else {
                        stop(wrap_txt(
                            "No feat_ID info found within feat metadata and
                            unable",
                            "to guess IDs based on gobject feat_ID",
                            errWidth = TRUE
                        ))
                    }
                } else { # otherwise, feat_ID col found
                    ID_col_name <- names(ID_col_guess)
                    meta[][, feat_ID := NULL] # remove older column
                    data.table::setnames(
                        meta[],
                        old = ID_col_name, new = "feat_ID"
                    )
                    if (isTRUE(verbose)) {
                        wrap_msg(
                            "Feature metadata: guessing", ID_col_name,
                            "as feat_ID column."
                        )
                    }
                }

                # feat ID guessing and assignment done #
            }


            # duplicated IDs
            if (any(meta[][, duplicated(feat_ID)])) {
                warning(wrap_txt(
                    "Feature metadata: duplicates found in feat_ID column.",
                    errWidth = TRUE
                ))
            }

            # length mismatch
            if (nrow(meta[]) > length(IDs)) {
                m_IDs <- meta[][["feat_ID"]]
                filter_bool_feats <- m_IDs %in% IDs

                meta[] <- meta[][filter_bool_feats]
            }

            if (nrow(meta[]) < length(IDs)) {
                ID_dt <- data.table::data.table(feat_ID = IDs)
                meta[] <- merge(ID_dt, meta[], all.x = TRUE)
            }

            if (nrow(meta[]) != length(IDs)) {
                stop(wrap_txt(
                    "Feature metadata: number of entries does not match",
                    "number of gobject IDs for this spat_unit (",
                    length(IDs), ")",
                    errWidth = TRUE
                ))
            }

            # feat_ID  contents mismatch
            if (!meta[][, setequal(feat_ID, IDs)]) {
                stop(wrap_txt(
                    "Feature metadata: IDs do not match between metadata and
                    feat_ID slot for this spat_unit"
                ))
            }

            # ensure ID col first
            setcolorder(meta[], "feat_ID")
        })
    }
}








#' @title Check spatial location data
#' @name .check_spatial_location_data
#' @description check cell ID (spatial unit) names between spatial location
#' and expression data. It will look for identical IDs after sorting.
#' @keywords internal
#' @returns character or NULL
.check_spatial_location_data <- function(gobject) {
    # define for data.table
    cell_ID <- spat_unit <- name <- NULL

    # find available spatial locations
    avail_sl <- list_spatial_locations(gobject)
    avail_ex <- list_expression(gobject)
    avail_si <- list_spatial_info(gobject)

    # check hierarchical
    missing_unit <- !(avail_sl$spat_unit) %in%
        c(avail_ex$spat_unit, avail_si$spat_info)
    if (any(missing_unit)) {
        stop(wrap_txt(
            "No expression or polygon information discovered for spat_unit:",
            avail_sl$spat_unit[missing_unit],
            "Please add expression or polygon information for this spatial",
            "unit first"
        ))
    }

    for (spat_unit_i in avail_sl[["spat_unit"]]) {
        expected_cell_ID_names <- get_cell_id(
            gobject = gobject,
            spat_unit = spat_unit_i
        )

        for (coord_i in avail_sl[spat_unit == spat_unit_i, name]) {
            # 1. get colnames
            spatlocsDT <- get_spatial_locations(gobject,
                spat_unit = spat_unit_i,
                spat_loc_name = coord_i,
                output = "data.table",
                copy_obj = FALSE
            )
            missing_cell_IDs <- spatlocsDT[, all(is.na(cell_ID))]

            # if cell_ID column is provided then compare with expected cell_IDs
            if (!isTRUE(missing_cell_IDs)) {
                spatial_cell_id_names <- spatlocsDT[["cell_ID"]]

                if (!setequal(spatial_cell_id_names, expected_cell_ID_names)) {
                    stop(
                        "cell_IDs between spatial and expression information
                        are not the same for: \n spatial unit: ",
                        spat_unit_i, " and coordinates: ",
                        coord_i, " \n"
                    )
                }
            } else {
                # if cell_ID column is not provided then add expected cell_IDs

                ## error if spatlocs and cell_ID do not match in length
                if (spatlocsDT[, .N] != length(expected_cell_ID_names)) {
                    stop(
                        "Number of rows of spatial locations do not match
                        with cell IDs for: \n spatial unit: ",
                        spat_unit_i, " and coordinates: ", coord_i, " \n"
                    )
                }

                ## ! modify coords within gobject by reference
                spatlocsDT <- spatlocsDT[, cell_ID := expected_cell_ID_names]
            }
        }
    }

    return(invisible())
}






#' @keywords internal
#' @noRd
.check_spatial_networks <- function(gobject) {
    # DT vars
    spat_unit <- NULL

    avail_sn <- list_spatial_networks(gobject = gobject)
    avail_sl <- list_spatial_locations(gobject = gobject)
    used_su <- unique(avail_sn$spat_unit)

    # check hierarchical
    missing_su <- !used_su %in% avail_sl$spat_unit
    if (sum(missing_su != 0L)) {
        stop(wrap_txt("Matching spatial locations in spat_unit(s)",
            used_su[missing_su],
            "must be added before the respective spatial networks",
            errWidth = TRUE
        ))
    }

    if (!is.null(used_su)) {
        for (su_i in used_su) {
            IDs <- spatIDs(gobject, spat_unit = su_i)

            su_sn <- avail_sn[spat_unit == su_i, ]
            lapply(seq(nrow(su_sn)), function(obj_i) {
                sn_obj <- get_spatialNetwork(
                    gobject = gobject,
                    spat_unit = su_i,
                    name = su_sn$name[[obj_i]],
                    output = "spatialNetworkObj",
                    set_defaults = FALSE,
                    copy_obj = FALSE,
                    verbose = FALSE
                )
                if (!all(spatIDs(sn_obj) %in% IDs)) {
                    warning(wrap_txt(
                        "spat_unit:", su_i,
                        "name:", su_sn$name[[obj_i]], "\n",
                        "Spatial network vertex names are not all found in
                        gobject IDs"
                    ))
                }
            })
        }
    }
}






#' @name .check_spatial_enrichment
#' @description check the spatial enrichment information within the gobject
#' @keywords internal
#' @noRd
.check_spatial_enrichment <- function(gobject) {
    # DT vars
    spat_unit <- NULL

    avail_se <- list_spatial_enrichments(gobject = gobject)
    avail_sl <- list_spatial_locations(gobject = gobject)
    used_su <- unique(avail_se$spat_unit)

    # check hierarchical
    missing_su <- !used_su %in% avail_sl$spat_unit
    if (sum(missing_su != 0L)) {
        stop(wrap_txt("Matching spatial locations in spat_unit(s)",
            used_su[missing_su],
            "must be added before the respective spatial enrichments",
            errWidth = TRUE
        ))
    }

    if (!is.null(used_su)) {
        for (su_i in used_su) {
            IDs <- spatIDs(gobject, spat_unit = su_i)

            su_se <- avail_se[spat_unit == su_i, ]
            lapply(seq(nrow(su_se)), function(obj_i) {
                se_obj <- get_spatial_enrichment(
                    gobject = gobject,
                    spat_unit = su_i,
                    feat_type = su_se$feat_type[[obj_i]],
                    enrichm_name = su_se$name[[obj_i]],
                    output = "spatEnrObj",
                    copy_obj = FALSE,
                    set_defaults = FALSE
                )
                if (!setequal(spatIDs(se_obj), IDs)) {
                    warning(wrap_txt(
                        "spat_unit:", su_i,
                        "feat_type:", su_se$feat_type[[obj_i]],
                        "name:", su_se$name[[obj_i]], "\n",
                        "Spatial enrichment IDs are not all found in gobject
                        IDs"
                    ))
                }
            })
        }
    }
}










#' @name .check_dimension_reduction
#' @keywords internal
#' @noRd
.check_dimension_reduction <- function(gobject) {
    # DT vars
    spat_unit <- feat_type <- NULL

    # check that all spatIDs of coordinates setequals with gobject cell_ID
    # for the particular spat_unit
    avail_dr <- list_dim_reductions(gobject = gobject)
    avail_ex <- list_expression(gobject = gobject)
    used_su <- unique(avail_dr$spat_unit)
    used_su_ft <- unique(avail_dr[
        ,
        paste0("[", spat_unit, "][", feat_type, "]")
    ])
    ex_su_ft <- unique(avail_ex[
        ,
        paste0("[", spat_unit, "][", feat_type, "]")
    ])

    # check hierarchical
    missing_su_ft <- !used_su_ft %in% ex_su_ft
    if (sum(missing_su_ft != 0L)) {
        stop(wrap_txt("Matching expression values [spat_unit][feat_type]:\n",
            used_su_ft[missing_su_ft],
            "\nmust be added before the respective dimension reductions",
            errWidth = TRUE
        ))
    }

    if (!is.null(used_su)) {
        for (su_i in used_su) {
            IDs <- spatIDs(gobject, spat_unit = su_i)

            su_dr <- avail_dr[spat_unit == su_i, ]
            lapply(seq(nrow(su_dr)), function(obj_i) {
                dr_obj <- get_dimReduction(
                    gobject = gobject,
                    spat_unit = su_i,
                    feat_type = su_dr$feat_type[[obj_i]],
                    reduction = su_dr$data_type[[obj_i]],
                    reduction_method = su_dr$dim_type[[obj_i]],
                    name = su_dr$name[[obj_i]],
                    output = "dimObj",
                    set_defaults = FALSE
                )

                # if matrix has no IDs, regenerate from gobject IDs
                if (is.null(spatIDs(dr_obj))) {
                    if (nrow(dr_obj[]) == length(IDs)) {
                        # if nrow of matrix and number of gobject cell_IDs for
                        # the spat unit
                        # match, then try guessing then set data back to replace
                        warning(wrap_txt(
                            "data_type:", su_dr$data_type[[obj_i]],
                            "spat_unit:", su_i,
                            "feat_type:", su_dr$feat_type[[obj_i]],
                            "dim_type:", su_dr$dim_type[[obj_i]],
                            "name:", su_dr$name[[obj_i]], "\n",
                            "Dimension reduction has no cell_IDs.
                             Guessing based on existing expression cell_IDs"
                        ))
                        rownames(dr_obj[]) <- IDs
                        ### ### ### ### ### ### ### ### ### ### ### ### ### ###
                        gobject <- set_dimReduction(
                            gobject = gobject,
                            dimObject = dr_obj,
                            set_defaults = FALSE,
                            initialize = TRUE,
                            verbose = FALSE
                        )
                        ### ### ### ### ### ### ### ### ### ### ### ### ### ###
                    } else {
                        # if number of values do NOT match, throw error
                        stop(wrap_txt(
                            "data_type:", su_dr$data_type[[obj_i]],
                            "spat_unit:", su_i,
                            "feat_type:", su_dr$feat_type[[obj_i]],
                            "dim_type:", su_dr$dim_type[[obj_i]],
                            "name:", su_dr$name[[obj_i]], "\n",
                            "Dimension reduction has no cell_IDs.
                            Number of rows also does not match expression
                            columns"
                        ))
                    }
                }

                # if does not have all the IDS seen in the gobject
                if (!all(spatIDs(dr_obj) %in% IDs)) {
                    warning(wrap_txt(
                        "data_type:", su_dr$data_type[[obj_i]],
                        "spat_unit:", su_i,
                        "feat_type:", su_dr$feat_type[[obj_i]],
                        "dim_type:", su_dr$dim_type[[obj_i]],
                        "name:", su_dr$name[[obj_i]], "\n",
                        "Dimension reduction coord names are not all found in
                        gobject IDs"
                    ))
                }
            })
        }
    }
    return(gobject)
}







#' @keywords internal
#' @noRd
.check_nearest_networks <- function(gobject) {
    # DT vars
    spat_unit <- feat_type <- NULL

    avail_nn <- list_nearest_networks(gobject = gobject)
    avail_dr <- list_dim_reductions(gobject = gobject)
    used_su <- unique(avail_nn$spat_unit)
    used_su_ft <- unique(avail_nn[
        ,
        paste0("[", spat_unit, "][", feat_type, "]")
    ])
    dr_su_ft <- unique(avail_dr[, paste0("[", spat_unit, "][", feat_type, "]")])

    # check hierarchical
    missing_su_ft <- !used_su_ft %in% dr_su_ft
    if (sum(missing_su_ft != 0L)) {
        stop(wrap_txt("Matching dimension reductions [spat_unit][feat_type]:\n",
            used_su_ft[missing_su_ft],
            "\nmust be added before the respective nearest neighbor networks",
            errWidth = TRUE
        ))
    }

    if (!is.null(used_su)) {
        for (su_i in used_su) {
            IDs <- spatIDs(gobject, spat_unit = su_i)

            su_nn <- avail_nn[spat_unit == su_i, ]
            lapply(seq(nrow(su_nn)), function(obj_i) {
                nn_obj <- get_NearestNetwork(
                    gobject = gobject,
                    spat_unit = su_i,
                    feat_type = su_nn$feat_type[[obj_i]],
                    nn_network_to_use = su_nn$nn_type[[obj_i]],
                    network_name = su_nn$name[[obj_i]],
                    output = "nnNetObj",
                    set_defaults = FALSE
                )
                if (!all(spatIDs(nn_obj) %in% IDs)) {
                    warning(wrap_txt(
                        "spat_unit:", su_i,
                        "feat_type:", su_nn$feat_type[[obj_i]],
                        "nn_type:", su_nn$nn_type[[obj_i]],
                        "name:", su_nn$name[[obj_i]], "\n",
                        "Nearest network vertex names are not all found in
                        gobject IDs"
                    ))
                }
            })
        }
    }
}








#' @name .check_spatial_info
#' @keywords internal
#' @noRd
.check_spatial_info <- function(gobject) {
    # DT vars
    spat_unit <- NULL

    avail_sinfo <- list_spatial_info(gobject)
    if (is.null(avail_sinfo)) {
        return(gobject)
    } # quit early if none available

    avail_slocs <- list_spatial_locations(gobject)

    common_su <- intersect(avail_sinfo$spat_info, avail_slocs$spat_unit)

    # If there are any shared spatial units, match IDs
    if (length(common_su) != 0) {
        for (su_i in common_su) {
            # get spat_info
            sinfo <- get_polygon_info(
                gobject = gobject,
                polygon_name = su_i,
                return_giottoPolygon = TRUE
            )

            # get spatlocs
            su_sloc <- avail_slocs[spat_unit == su_i]
            lapply(seq(nrow(su_sloc)), function(obj_i) {
                spatlocs <- get_spatial_locations(
                    gobject = gobject,
                    spat_unit = su_i,
                    spat_loc_name = su_sloc$name[[obj_i]],
                    output = "spatLocsObj",
                    set_defaults = FALSE,
                    copy_obj = FALSE
                )
                if (!all(spatIDs(spatlocs) %in% spatIDs(sinfo))) {
                    warning(wrap_txt(
                        "spat_unit:", su_i,
                        "spatloc name:", su_sloc$name[[obj_i]], "\n",
                        "cell IDs in spatial locations are missing from spatial
                        polygon info"
                    ))
                }
            })
        }
    }
}





#' @name .check_feature_info
#' @keywords internal
#' @noRd
.check_feature_info <- function(gobject) {
    # TODO ... expr info or meta info w/ IDs not in feature info
}

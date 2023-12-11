## Giotto auxiliary functions ####





#' @title pDataDT
#' @name pDataDT
#' @description show cell metadata
#' @inheritParams data_access_params
#' @param ... additional params to pass
#' @return data.table with cell metadata
#' @export
pDataDT <- function(gobject,
                    spat_unit = NULL,
                    feat_type = NULL,
                    ...) {
  # Set feat_type and spat_unit
  spat_unit <- set_default_spat_unit(
    gobject = gobject,
    spat_unit = spat_unit
  )
  feat_type <- set_default_feat_type(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type
  )


  if (!inherits(gobject, c("ExpressionSet", "SCESet", "seurat", "giotto"))) {
    stop("only works with ExpressionSet (-like) objects")
  }

  if (inherits(gobject, c("ExpressionSet", "SCESet"))) {
    return(data.table::as.data.table(Biobase::pData(gobject)))
  } else if (inherits(gobject, "giotto")) {
    if (is.null(match.call(expand.dots = TRUE)$output)) {
      output <- "data.table"
    } else {
      output <- match.call(expand.dots = TRUE)$output
    }

    return(get_cell_metadata(
      gobject = gobject,
      spat_unit = spat_unit,
      feat_type = feat_type,
      output = output
    ))
  } else if (inherits(gobject, "seurat")) {
    return(data.table::as.data.table(gobject@meta.data))
  }
}


#' @title fDataDT
#' @name fDataDT
#' @description show feature metadata
#' @inheritParams data_access_params
#' @param ... additional params to pass
#' @return data.table with feature metadata
#' @export
fDataDT <- function(gobject,
                    spat_unit = NULL,
                    feat_type = NULL,
                    ...) {
  # Set feat_type and spat_unit
  spat_unit <- set_default_spat_unit(
    gobject = gobject,
    spat_unit = spat_unit
  )
  feat_type <- set_default_feat_type(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type
  )

  if (!inherits(gobject, c("ExpressionSet", "SCESet", "giotto"))) {
    stop("only works with ExpressionSet (-like) objects")
  } else if (inherits(gobject, "giotto")) {
    if (is.null(match.call(expand.dots = TRUE)$output)) {
      output <- "data.table"
    } else {
      output <- match.call(expand.dots = TRUE)$output
    }

    return(get_feature_metadata(
      gobject = gobject,
      spat_unit = spat_unit,
      feat_type = feat_type,
      output = output
    ))
  }
  return(data.table::as.data.table(Biobase::fData(gobject)))
}








## Feature & Cell metadata functions ####


#' @title Annotate giotto clustering
#' @name annotateGiotto
#' @description Converts cluster results into a user provided annotation.
#' @param gobject giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param annotation_vector named annotation vector (names = cluster ids)
#' @param cluster_column cluster column to convert to annotation names
#' @param name new name for annotation column
#' @return giotto object
#' @details You need to specifify which (cluster) column you want to annotate
#' and you need to provide an annotation vector like this:
#' \itemize{
#'   \item{1. identify the cell type of each cluster}
#'   \item{2. create a vector of these cell types, e.g. cell_types =  c('T-cell', 'B-cell', 'Stromal')}
#'   \item{3. provide original cluster names to previous vector, e.g. names(cell_types) = c(2, 1, 3)}
#' }
#' @export
annotateGiotto <- function(gobject,
                           spat_unit = NULL,
                           feat_type = NULL,
                           annotation_vector = NULL,
                           cluster_column = NULL,
                           name = "cell_types") {
  # Set feat_type and spat_unit
  spat_unit <- set_default_spat_unit(
    gobject = gobject,
    spat_unit = spat_unit
  )
  feat_type <- set_default_feat_type(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type
  )

  # data.table: set global variable
  temp_cluster_name <- NULL

  if (is.null(annotation_vector) | is.null(cluster_column)) {
    stop("\n You need to provide both a named annotation vector and the corresponding cluster column  \n")
  }

  cell_metadata <- get_cell_metadata(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type,
    output = "cellMetaObj",
    copy_obj = TRUE
  )

  # 1. verify if cluster column exist
  if (!cluster_column %in% colnames(cell_metadata[])) {
    stop("\n Cluster column is not found in cell metadata \n")
  }

  # 2. verify if each cluster has an annotation
  uniq_names <- names(annotation_vector)
  uniq_clusters <- unique(cell_metadata[][[cluster_column]])
  missing_annotations <- uniq_clusters[!uniq_clusters %in% uniq_names]
  no_matching_annotations <- uniq_names[!uniq_names %in% uniq_clusters]

  if (length(missing_annotations) > 0) {
    cat(
      "Not all clusters have an accompanying annotation in the annotation_vector: \n",
      "These names are missing: ", as.character(missing_annotations), "\n",
      "These annotations have no match: ", as.character(no_matching_annotations), "\n"
    )
    stop("Annotation interrupted \n")
  }


  # 3. remove previous annotation name if it's the same
  # but only if new name is not the same as cluster to be used
  if (name %in% colnames(cell_metadata[])) {
    wrap_msg('annotation name "', name, '" was already used and will be overwritten',
      sep = ""
    )

    cell_metadata[][, temp_cluster_name := annotation_vector[[as.character(get(cluster_column))]], by = 1:nrow(cell_metadata[])]
    cell_metadata[][, (name) := NULL]
  } else {
    cell_metadata[][, temp_cluster_name := annotation_vector[[as.character(get(cluster_column))]], by = 1:nrow(cell_metadata[])]
  }

  data.table::setnames(cell_metadata[], old = "temp_cluster_name", new = name)
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  gobject <- set_cell_metadata(
    gobject = gobject,
    metadata = cell_metadata,
    verbose = FALSE
  )
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

  return(gobject)
}



#' @title Remove cell annotation
#' @name removeCellAnnotation
#' @description Removes cell annotation from a Giotto object for a specific feature modality (default = 'rna')
#' @param gobject giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param columns names of columns to remove
#' @param return_gobject boolean: return giotto object (default = TRUE)
#' @return giotto object
#' @details if \code{return_gobject = FALSE}, it will return the cell metadata
#' @export
removeCellAnnotation <- function(gobject,
                                 spat_unit = NULL,
                                 feat_type = NULL,
                                 columns = NULL,
                                 return_gobject = TRUE) {
  # Set feat_type and spat_unit
  spat_unit <- set_default_spat_unit(
    gobject = gobject,
    spat_unit = spat_unit
  )
  feat_type <- set_default_feat_type(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type
  )


  if (is.null(columns)) {
    stop("\t You need to provide a vector of metadata column names to remove \t")
  }


  # get cell metadata
  cell_metadata <- get_cell_metadata(gobject,
    spat_unit = spat_unit,
    feat_type = feat_type,
    output = "cellMetaObj",
    copy_obj = TRUE
  )
  # remove columns
  cell_metadata[] <- cell_metadata[][, (columns) := NULL]

  # return giotto object or cell metadata
  if (return_gobject == TRUE) {
    gobject <- set_cell_metadata(gobject,
      metadata = cell_metadata,
      verbose = FALSE
    )
    return(gobject)
  } else {
    cell_metadata[]
  }
}


#' @title Remove feature annotation
#' @name removeFeatAnnotation
#' @description Removes feature annotation from a Giotto object for a specific feature modality
#' @param gobject giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param columns names of columns to remove
#' @param return_gobject boolean: return giotto object (default = TRUE)
#' @return giotto object
#' @details if \code{return_gobject = FALSE}, it will return the gene metadata
#' @export
removeFeatAnnotation <- function(gobject,
                                 spat_unit = NULL,
                                 feat_type = NULL,
                                 columns = NULL,
                                 return_gobject = TRUE) {
  # Set feat_type and spat_unit
  spat_unit <- set_default_spat_unit(
    gobject = gobject,
    spat_unit = spat_unit
  )
  feat_type <- set_default_feat_type(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type
  )


  if (is.null(columns)) {
    stop("\t You need to provide a vector of metadata column names to remove \t")
  }

  # get feat metadata
  feat_metadata <- get_feature_metadata(gobject,
    spat_unit = spat_unit,
    feat_type = feat_type,
    output = "featMetaObj",
    copy_obj = TRUE
  )
  # remove columns
  feat_metadata[] <- feat_metadata[][, (columns) := NULL]

  # return giotto object or cell metadata
  if (return_gobject == TRUE) {
    gobject <- set_feature_metadata(gobject,
      metadata = feat_metadata,
      verbose = FALSE
    )
    return(gobject)
  } else {
    feat_metadata[]
  }
}







#' @title Add cell metadata
#' @name addCellMetadata
#' @description Adds cell metadata to the giotto object
#' @param gobject giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param new_metadata new cell metadata to use (data.table, data.frame, ...)
#' @param vector_name (optional) custom name if you provide a single vector
#' @param by_column merge metadata based on \emph{cell_ID} column in \code{\link{pDataDT}} (default = FALSE)
#' @param column_cell_ID column name of new metadata to use if by_column = TRUE
#' @return giotto object
#' @details You can add additional cell metadata in two manners:
#' \itemize{
#'   \item{1. Provide a data.table or data.frame with cell annotations in the same order as the \emph{cell_ID} column in pDataDT(gobject) }
#'   \item{2. Provide a data.table or data.frame with cell annotations and specify which column contains the cell IDs, these cell IDs need to match with the \emph{cell_ID} column in pDataDT(gobject)}
#' }
#' @export
addCellMetadata <- function(gobject,
                            spat_unit = NULL,
                            feat_type = NULL,
                            new_metadata,
                            vector_name = NULL,
                            by_column = FALSE,
                            column_cell_ID = NULL) {
  # Set feat_type and spat_unit
  spat_unit <- set_default_spat_unit(
    gobject = gobject,
    spat_unit = spat_unit
  )
  feat_type <- set_default_feat_type(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type
  )


  # check hierarchical slots
  avail_ex <- list_expression(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type
  )
  if (is.null(avail_ex)) {
    stop(wrap_txt(
      "No matching expression information discovered for:
                  spat_unit:", spat_unit,
      "\nfeature type:", feat_type,
      "\nPlease add expression information first"
    ))
  }


  cell_metadata <- get_cell_metadata(gobject,
    spat_unit = spat_unit,
    feat_type = feat_type,
    output = "cellMetaObj",
    copy_obj = TRUE
  )

  ordered_cell_IDs <- get_cell_id(gobject, spat_unit = spat_unit)

  if (is.vector(new_metadata) | is.factor(new_metadata)) {
    original_name <- deparse(substitute(new_metadata))
    new_metadata <- data.table::as.data.table(new_metadata)

    if (!is.null(vector_name) & is.character(vector_name)) {
      colnames(new_metadata) <- vector_name
    } else {
      colnames(new_metadata) <- original_name
    }
  } else {
    new_metadata <- data.table::as.data.table(new_metadata)
  }

  if (is.null(column_cell_ID)) {
    column_cell_ID <- "cell_ID"
  }

  # overwrite columns with same name
  new_col_names <- colnames(new_metadata)
  new_col_names <- new_col_names[new_col_names != column_cell_ID]
  old_col_names <- colnames(cell_metadata[])
  old_col_names <- old_col_names[old_col_names != "cell_ID"]
  same_col_names <- new_col_names[new_col_names %in% old_col_names]


  if (length(same_col_names) >= 1) {
    wrap_msg(
      "\nThese column names were already used: ", same_col_names, "\n",
      "and will be overwritten \n"
    )
    cell_metadata[][, (same_col_names) := NULL]
  }



  if (by_column == FALSE) {
    cell_metadata[] <- cbind(cell_metadata[], new_metadata)
  } else {
    if (is.null(column_cell_ID)) stop("You need to provide cell_ID column")
    cell_metadata[] <- data.table::merge.data.table(cell_metadata[],
      by.x = "cell_ID",
      new_metadata,
      by.y = column_cell_ID,
      all.x = TRUE
    )
  }

  # data.table variables
  cell_ID <- NULL

  # reorder
  cell_metadata[] <- cell_metadata[][match(ordered_cell_IDs, cell_ID)]


  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  gobject <- set_cell_metadata(gobject,
    metadata = cell_metadata,
    verbose = FALSE
  )
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

  return(gobject)
}


#' @title Add feature metadata
#' @name addFeatMetadata
#' @description Adds feature metadata to the giotto object
#' @param gobject giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param new_metadata new metadata to use)
#' @param vector_name (optional) custom name if you provide a single vector
#' @param by_column merge metadata based on \emph{feat_ID} column in \code{\link{fDataDT}}
#' @param column_feat_ID column name of new metadata to use if by_column = TRUE
#' @return giotto object
#' @details You can add additional feature metadata in two manners: \cr
#' 1. Provide a data.table or data.frame with feature annotations in the same order as the \emph{feat_ID} column in fDataDT(gobject) \cr
#' 2. Provide a data.table or data.frame with feature annotations and specify which column contains the feature IDs,
#' these feature IDs need to match with the \emph{feat_ID} column in fDataDT(gobject)
#' @export
addFeatMetadata <- function(gobject,
                            feat_type = NULL,
                            spat_unit = NULL,
                            new_metadata,
                            by_column = F,
                            column_feat_ID = NULL,
                            vector_name = NULL) {
  # data.table variables
  feat_ID <- NULL

  # Set feat_type and spat_unit
  spat_unit <- set_default_spat_unit(
    gobject = gobject,
    spat_unit = spat_unit
  )
  feat_type <- set_default_feat_type(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type
  )


  # check hierarchical slots
  avail_ex <- list_expression(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type
  )
  if (is.null(avail_ex)) {
    stop(wrap_txt(
      "No matching expression information discovered for:
                  spat_unit:", spat_unit,
      "\nfeature type:", feat_type,
      "\nPlease add expression information first"
    ))
  }


  feat_metadata <- get_feature_metadata(gobject,
    spat_unit = spat_unit,
    feat_type = feat_type,
    output = "featMetaObj",
    copy_obj = TRUE
  )

  ordered_feat_IDs <- get_feat_id(gobject, feat_type = feat_type)

  if (is.vector(new_metadata) | is.factor(new_metadata)) {
    original_name <- deparse(substitute(new_metadata))
    new_metadata <- data.table::as.data.table(new_metadata)
    if (!is.null(vector_name) & is.character(vector_name)) {
      colnames(new_metadata) <- vector_name
    } else {
      colnames(new_metadata) <- original_name
    }
  } else {
    new_metadata <- data.table::as.data.table(new_metadata)
  }

  if (is.null(column_feat_ID)) {
    column_feat_ID <- "feat_ID"
  }

  # overwrite columns with same name
  new_col_names <- colnames(new_metadata)
  new_col_names <- new_col_names[new_col_names != column_feat_ID]
  old_col_names <- colnames(feat_metadata[])
  old_col_names <- old_col_names[old_col_names != "feat_ID"]
  same_col_names <- new_col_names[new_col_names %in% old_col_names]

  if (length(same_col_names) >= 1) {
    wrap_msg(
      "\nThese column names were already used: ", same_col_names, "\n",
      "and will be overwritten \n"
    )
    feat_metadata[][, (same_col_names) := NULL]
  }


  if (by_column == FALSE) {
    feat_metadata[] <- cbind(feat_metadata[], new_metadata)
  } else {
    if (is.null(column_feat_ID)) stop("You need to provide feat ID column")
    feat_metadata[] <- data.table::merge.data.table(feat_metadata[],
      by.x = "feat_ID",
      new_metadata,
      by.y = column_feat_ID,
      all.x = T
    )
  }

  # reorder
  feat_metadata[] <- feat_metadata[][match(ordered_feat_IDs, feat_ID)]

  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  gobject <- set_feature_metadata(gobject,
    metadata = feat_metadata,
    verbose = FALSE
  )
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

  return(gobject)
}





# expression ####


#' @title create_average_DT
#' @description calculates average gene expression for a cell metadata factor (e.g. cluster)
#' @param gobject giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param meta_data_name name of metadata column to use
#' @param expression_values which expression values to use
#' @return data.table with average gene epression values for each factor
#' @keywords internal
#' @export
create_average_DT <- function(gobject,
                              spat_unit = NULL,
                              feat_type = NULL,
                              meta_data_name,
                              expression_values = c("normalized", "scaled", "custom")) {
  # Set feat_type and spat_unit
  spat_unit <- set_default_spat_unit(
    gobject = gobject,
    spat_unit = spat_unit
  )
  feat_type <- set_default_feat_type(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type
  )

  # expression values to be used
  values <- match.arg(expression_values, unique(c("normalized", "scaled", "custom", expression_values)))
  expr_data <- get_expression_values(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type,
    values = values,
    output = "matrix"
  )

  # metadata
  cell_metadata <- get_cell_metadata(gobject,
    spat_unit = spat_unit,
    feat_type = feat_type,
    output = "data.table",
    copy_obj = TRUE
  )
  myrownames <- rownames(expr_data)

  savelist <- list()
  for (group in unique(cell_metadata[[meta_data_name]])) {
    name <- paste0("cluster_", group)

    temp <- expr_data[, cell_metadata[[meta_data_name]] == group]
    temp_DT <- rowMeans_flex(temp)

    savelist[[name]] <- temp_DT
  }

  finalDF <- do.call("cbind", savelist)
  rownames(finalDF) <- myrownames

  return(as.data.frame(finalDF))
}

#' @title create_average_detection_DT
#' @description calculates average gene detection for a cell metadata factor (e.g. cluster)
#' @param gobject giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param meta_data_name name of metadata column to use
#' @param expression_values which expression values to use
#' @param detection_threshold detection threshold to consider a gene detected
#' @return data.table with average gene epression values for each factor
#' @keywords internal
#' @export
create_average_detection_DT <- function(gobject,
                                        feat_type = NULL,
                                        spat_unit = NULL,
                                        meta_data_name,
                                        expression_values = c("normalized", "scaled", "custom"),
                                        detection_threshold = 0) {
  # Set feat_type and spat_unit
  spat_unit <- set_default_spat_unit(
    gobject = gobject,
    spat_unit = spat_unit
  )
  feat_type <- set_default_feat_type(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type
  )

  # expression values to be used
  values <- match.arg(expression_values, unique(c("normalized", "scaled", "custom", expression_values)))
  expr_data <- get_expression_values(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type,
    values = values,
    output = "matrix"
  )

  # metadata
  cell_metadata <- get_cell_metadata(gobject,
    spat_unit = spat_unit,
    feat_type = feat_type,
    output = "data.table",
    copy_obj = TRUE
  )
  myrownames <- rownames(expr_data)

  savelist <- list()
  for (group in unique(cell_metadata[[meta_data_name]])) {
    name <- paste0("cluster_", group)

    temp <- expr_data[, cell_metadata[[meta_data_name]] == group]
    temp <- as.matrix(temp)

    if (is.matrix(temp)) {
      temp_DT <- rowSums_flex(temp > detection_threshold) / ncol(temp)
    } else {
      temp_DT <- as.numeric(temp > detection_threshold)
    }

    savelist[[name]] <- temp_DT
  }

  finalDF <- do.call("cbind", savelist)
  rownames(finalDF) <- myrownames

  return(as.data.frame(finalDF))
}






#' @title create_cluster_matrix
#' @name create_cluster_matrix
#' @description creates aggregated matrix for a given clustering column
#' @inheritParams data_access_params
#' @param expression_values name of expression values to use
#' @param cluster_column name of cluster column to use,
#' @param feat_subset subset of features to use
#' @param gene_subset deprecated do not use.
#' @keywords internal
#' @export
create_cluster_matrix <- function(gobject,
                                  spat_unit = NULL,
                                  feat_type = NULL,
                                  expression_values = c("normalized", "scaled", "custom"),
                                  cluster_column,
                                  feat_subset = NULL,
                                  gene_subset = NULL) {
  # data.table variables
  feats <- NULL

  ## deprecated arguments
  if (!is.null(gene_subset)) {
    feat_subset <- gene_subset
  }

  # Set feat_type and spat_unit
  spat_unit <- set_default_spat_unit(
    gobject = gobject,
    spat_unit = spat_unit
  )
  feat_type <- set_default_feat_type(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type
  )

  values <- match.arg(expression_values, unique(c("normalized", "scaled", "custom", expression_values)))

  # average expression per cluster
  aggr_sc_clusters <- create_average_DT(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type,
    meta_data_name = cluster_column,
    expression_values = values
  )
  aggr_sc_clusters_DT <- data.table::as.data.table(aggr_sc_clusters)
  aggr_sc_clusters_DT[, feats := rownames(aggr_sc_clusters)]
  aggr_sc_clusters_DT_melt <- data.table::melt.data.table(aggr_sc_clusters_DT,
    variable.name = "cluster",
    id.vars = "feats",
    value.name = "expression"
  )

  # create matrix
  testmat <- data.table::dcast.data.table(aggr_sc_clusters_DT_melt,
    formula = feats ~ cluster,
    value.var = "expression"
  )
  testmatrix <- dt_to_matrix(testmat)

  # create subset if required
  if (!is.null(feat_subset)) {
    feat_subset_detected <- feat_subset[feat_subset %in% rownames(testmatrix)]
    testmatrix <- testmatrix[rownames(testmatrix) %in% feat_subset_detected, ]
  }

  return(testmatrix)
}













#' @title calculateMetaTable
#' @name calculateMetaTable
#' @description calculates the average gene expression for one or more (combined) annotation columns.
#' @param gobject giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param expression_values expression values to use
#' @param metadata_cols annotation columns found in \code{pDataDT(gobject)}
#' @param selected_feats subset of features to use
#' @param selected_genes subset of genes to use (deprecated)
#' @return data.table with average expression values for each gene per (combined) annotation
#' @export
calculateMetaTable <- function(gobject,
                               spat_unit = NULL,
                               feat_type = NULL,
                               expression_values = c("normalized", "scaled", "custom"),
                               metadata_cols = NULL,
                               selected_feats = NULL,
                               selected_genes = NULL) {
  if (is.null(metadata_cols)) stop("\n You need to select one or more valid column names from pDataDT() \n")

  # Set feat_type and spat_unit
  spat_unit <- set_default_spat_unit(
    gobject = gobject,
    spat_unit = spat_unit
  )
  feat_type <- set_default_feat_type(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type
  )

  ## deprecated arguments
  if (!is.null(selected_genes)) {
    selected_feats <- selected_genes
    warning("selected_genes is deprecated, use selected_feats in the future \n")
  }

  # data.table variables
  uniq_ID <- NULL

  ## get metadata and create unique groups
  metadata <- data.table::copy(pDataDT(gobject, feat_type = feat_type, spat_unit = spat_unit))
  if (length(metadata_cols) > 1) {
    metadata[, uniq_ID := paste(.SD, collapse = "-"), by = 1:nrow(metadata), .SDcols = metadata_cols]
  } else {
    metadata[, uniq_ID := get(metadata_cols)]
  }

  ## possible groups
  possible_groups <- unique(metadata[, metadata_cols, with = F])
  if (length(metadata_cols) > 1) {
    possible_groups[, uniq_ID := paste(.SD, collapse = "-"), by = 1:nrow(possible_groups), .SDcols = metadata_cols]
  } else {
    possible_groups[, uniq_ID := get(metadata_cols)]
  }

  ## get expression data
  values <- match.arg(expression_values, unique(c("normalized", "scaled", "custom", expression_values)))
  expr_values <- get_expression_values(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type,
    values = values,
    output = "matrix"
  )
  if (!is.null(selected_feats)) {
    expr_values <- expr_values[rownames(expr_values) %in% selected_feats, ]
  }

  ## summarize unique groups (average)
  result_list <- list()

  for (row in 1:nrow(possible_groups)) {
    uniq_identifiier <- possible_groups[row][["uniq_ID"]]
    selected_cell_IDs <- metadata[uniq_ID == uniq_identifiier][["cell_ID"]]
    sub_expr_values <- expr_values[, colnames(expr_values) %in% selected_cell_IDs]

    if (is.vector(sub_expr_values) == FALSE) {
      subvec <- rowMeans_flex(sub_expr_values)
    } else {
      subvec <- sub_expr_values
    }
    result_list[[row]] <- subvec
  }
  finaldt <- data.table::as.data.table(do.call("rbind", result_list))
  possible_groups_res <- cbind(possible_groups, finaldt)
  possible_groups_res_melt <- data.table::melt.data.table(possible_groups_res, id.vars = c(metadata_cols, "uniq_ID"))

  return(possible_groups_res_melt)
}


#' @title calculateMetaTableCells
#' @name calculateMetaTableCells
#' @description calculates the average metadata values for one or more (combined) annotation columns.
#' @param gobject giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param value_cols metadata or enrichment value columns to use
#' @param metadata_cols annotation columns found in \code{pDataDT(gobject)}
#' @param spat_enr_names which spatial enrichment results to include
#' @return data.table with average metadata values per (combined) annotation
#' @export
calculateMetaTableCells <- function(gobject,
                                    spat_unit = NULL,
                                    feat_type = NULL,
                                    value_cols = NULL,
                                    metadata_cols = NULL,
                                    spat_enr_names = NULL) {
  # Set feat_type and spat_unit
  spat_unit <- set_default_spat_unit(
    gobject = gobject,
    spat_unit = spat_unit
  )
  feat_type <- set_default_feat_type(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type
  )

  if (is.null(metadata_cols)) stop("\n You need to select one or more valid column names from pDataDT() \n")
  if (is.null(value_cols)) stop("\n You need to select one or more valid value column names from pDataDT() \n")

  cell_metadata <- combineMetadata(
    gobject = gobject,
    feat_type = feat_type,
    spat_unit = spat_unit,
    spat_enr_names = spat_enr_names
  )

  ## only keep columns that exist
  cell_metadata_cols <- colnames(cell_metadata)

  if (!all(value_cols %in% cell_metadata_cols)) {
    missing_value_cols <- value_cols[!value_cols %in% cell_metadata_cols]
    cat("These value columns were not found: ", missing_value_cols)
  }
  value_cols <- value_cols[value_cols %in% cell_metadata_cols]

  if (!all(metadata_cols %in% cell_metadata_cols)) {
    missing_metadata_cols <- metadata_cols[!metadata_cols %in% cell_metadata_cols]
    cat("These metadata columns were not found: ", missing_metadata_cols)
  }
  metadata_cols <- metadata_cols[metadata_cols %in% cell_metadata_cols]

  if (!length(metadata_cols) > 0 | !length(value_cols) > 0) {
    stop("\n missing sufficient metadata or value columns \n")
  }

  workdt <- cell_metadata[, lapply(.SD, mean), by = metadata_cols, .SDcols = value_cols]
  workdtmelt <- data.table::melt.data.table(workdt, measure.vars = value_cols)

  return(workdtmelt)
}






#' @title createMetafeats
#' @name createMetafeats
#' @description This function creates an average metafeat/metagene/module for clusters.
#' @param gobject Giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param expression_values expression values to use
#' @param feat_clusters numerical vector with features as names
#' @param name name of the metagene results
#' @param return_gobject return giotto object
#' @return giotto object
#' @details An example for the 'gene_clusters' could be like this:
#' cluster_vector = c(1, 1, 2, 2); names(cluster_vector) = c('geneA', 'geneB', 'geneC', 'geneD')
#' @examples
#' \dontrun{
#' # load a dataset
#' g <- GiottoData::loadGiottoMini("viz")
#' # set a spat unit to use
#' activeSpatUnit(g) <- "aggregate"
#'
#' # create the metafeats
#' # We do this by making an annotation vector which is a numerical vector
#' # of cluster assignments where each number is named by the feature it describes.
#' #
#' # Here we create an example annotation vector by arbitrarily using the first 6
#' # features and putting 3 in cluster 1 and the other 3 in cluster 2.
#' feats_to_use <- featIDs(g)[1:6]
#' clust_to_use <- c(1, 1, 1, 2, 2, 2)
#' names(clust_to_use) <- feats_to_use
#'
#' # show
#' clust_to_use
#'
#' g <- createMetafeats(
#'   gobject = g,
#'   feat_clusters = clust_to_use,
#'   name = "new_metagene"
#' )
#'
#' GiottoVisuals::spatCellPlot(
#'   gobject = g,
#'   spat_enr_names = "new_metagene",
#'   cell_annotation_values = c("1", "2"),
#'   point_size = 3.5,
#'   cow_n_col = 2
#' )
#' }
#' @seealso [GiottoVisuals::spatCellPlot()]
#' @export
createMetafeats <- function(gobject,
                            spat_unit = NULL,
                            feat_type = NULL,
                            expression_values = c("normalized", "scaled", "custom"),
                            feat_clusters,
                            name = "metafeat",
                            return_gobject = TRUE) {
  # Set feat_type and spat_unit
  spat_unit <- set_default_spat_unit(
    gobject = gobject,
    spat_unit = spat_unit
  )
  feat_type <- set_default_feat_type(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type
  )

  # expression values to be used
  values <- match.arg(expression_values, unique(c("normalized", "scaled", "custom", expression_values)))
  expr_values <- get_expression_values(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type,
    values = values,
    output = "exprObj"
  )


  ## calculate metafeat ##
  res_list <- list()

  for (id in sort(unique(feat_clusters))) {
    clus_id <- id

    selected_feats <- names(feat_clusters[feat_clusters == clus_id])
    sub_mat <- expr_values[][rownames(expr_values[]) %in% selected_feats, ]

    # calculate mean
    if (length(selected_feats) == 1) {
      mean_score <- sub_mat
    } else {
      mean_score <- colMeans_flex(sub_mat)
    }

    res_list[[id]] <- mean_score
  }

  res_final <- data.table::as.data.table(t(do.call("rbind", res_list)))
  colnames(res_final) <- as.character(sort(unique(feat_clusters)))

  # data.table variables
  cell_ID <- NULL

  res_final[, cell_ID := colnames(expr_values[])]

  # create spatial enrichment object
  enrObj <- create_spat_enr_obj(
    name = name,
    method = "metafeat",
    enrichDT = res_final,
    spat_unit = spat_unit,
    feat_type = feat_type,
    provenance = expr_values@provenance,
    misc = list(expr_values_used = expression_values)
  )

  if (return_gobject == TRUE) {
    ## enrichment scores
    spenr_names <- list_spatial_enrichments_names(gobject = gobject, spat_unit = spat_unit, feat_type = feat_type)

    if (name %in% spenr_names) {
      cat("\n ", name, " has already been used, will be overwritten \n")
    }

    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    gobject <- set_spatial_enrichment(gobject = gobject, spatenrichment = enrObj)
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

    ## update parameters used ##
    gobject <- update_giotto_params(gobject, description = "_create_metafeat")
    return(gobject)
  } else {
    return(enrObj)
  }
}





# DT overallocation ####


#' @title Over-allocation for giotto DT-based info
#' @description Finds DT based objects, overallocates the data.tables, then sets
#' the objects back in the giotto object
#' @param gobject giotto object
#' @keywords internal
.giotto_alloc_dt <- function(gobject) {
  # data.table vars
  spat_unit <- feat_type <- name <- NULL

  # metadata
  avail_cm <- list_cell_metadata(gobject)
  if (!is.null(avail_cm)) {
    for (cm_i in seq(nrow(avail_cm))) {
      cm <- get_cell_metadata(
        gobject = gobject,
        spat_unit = avail_cm[cm_i, spat_unit],
        feat_type = avail_cm[cm_i, feat_type],
        output = "cellMetaObj",
        copy_obj = FALSE
      )
      if (!is.null(cm[])) {
        cm[] <- data.table::setalloccol(cm[])
        gobject <- set_cell_metadata(
          gobject = gobject,
          metadata = cm,
          set_defaults = FALSE,
          verbose = FALSE
        )
      }
    }
  }

  avail_fm <- list_feat_metadata(gobject)
  if (!is.null(avail_fm)) {
    for (fm_i in seq(nrow(avail_fm))) {
      fm <- get_feature_metadata(
        gobject = gobject,
        spat_unit = avail_fm[fm_i, spat_unit],
        feat_type = avail_fm[fm_i, feat_type],
        output = "featMetaObj",
        copy_obj = FALSE
      )
      if (!is.null(fm[])) {
        fm[] <- data.table::setalloccol(fm[])
        gobject <- set_feature_metadata(
          gobject = gobject,
          metadata = fm,
          set_defaults = FALSE,
          verbose = FALSE
        )
      }
    }
  }

  # spatlocs
  avail_sl <- list_spatial_locations(gobject)
  if (!is.null(avail_sl)) {
    for (sl_i in seq(nrow(avail_sl))) {
      sl <- get_spatial_locations(
        gobject = gobject,
        spat_unit = avail_sl[sl_i, spat_unit],
        spat_loc_name = avail_sl[sl_i, name],
        output = "spatLocsObj",
        copy_obj = FALSE
      )
      if (!is.null(sl[])) {
        sl[] <- data.table::setalloccol(sl[])
        gobject <- set_spatial_locations(
          gobject = gobject,
          spatlocs = sl,
          set_defaults = FALSE,
          verbose = FALSE
        )
      }
    }
  }

  # spatial enrichment
  avail_se <- list_spatial_enrichments(gobject)
  if (!is.null(avail_se)) {
    for (se_i in seq(nrow(avail_se))) {
      se <- get_spatial_enrichment(
        gobject = gobject,
        spat_unit = avail_se[se_i, spat_unit],
        feat_type = avail_se[se_i, feat_type],
        enrichm_name = avail_se[se_i, name],
        output = "spatEnrObj",
        copy_obj = FALSE
      )
      if (!is.null(se[])) {
        se[] <- data.table::setalloccol(se[])
        gobject <- set_spatial_enrichment(
          gobject = gobject,
          spatenrichment = se,
          set_defaults = FALSE,
          verbose = FALSE
        )
      }
    }
  }

  # spatial network
  avail_sn <- list_spatial_networks(gobject)
  if (!is.null(avail_sn)) {
    for (sn_i in seq(nrow(avail_sn))) {
      sn <- get_spatialNetwork(
        gobject = gobject,
        spat_unit = avail_sn[sn_i, spat_unit],
        name = avail_sn[sn_i, name],
        output = "spatialNetworkObj"
      )
      if (!is.null(slot(sn, "networkDT_before_filter"))) {
        slot(sn, "networkDT_before_filter") <- data.table::setalloccol(slot(sn, "networkDT_before_filter"))
      }
      if (!is.null(sn[])) {
        sn[] <- data.table::setalloccol(sn[])
        gobject <- set_spatialNetwork(
          gobject = gobject,
          spatial_network = sn,
          verbose = FALSE,
          set_defaults = FALSE
        )
      }
    }
  }

  # spatial grid
  avail_sg <- list_spatial_grids(gobject)
  if (!is.null(avail_sg)) {
    for (sg_i in seq(nrow(avail_sg))) {
      sg <- get_spatialGrid(
        gobject = gobject,
        spat_unit = avail_sg[sg_i, spat_unit],
        feat_type = avail_sg[sg_i, feat_type],
        name = avail_sg[sg_i, name],
        return_grid_Obj = TRUE
      )
      if (!is.null(sg[])) {
        sg[] <- data.table::setalloccol(sg[])
        gobject <- set_spatialGrid(
          gobject = gobject,
          spatial_grid = sg,
          verbose = FALSE,
          set_defaults = FALSE
        )
      }
    }
  }
  return(gobject)
}

## Giotto auxiliary functions ####





#' @title pDataDT
#' @name pDataDT
#' @description show cell metadata
#' @inheritParams data_access_params
#' @param ... additional params to pass
#' @returns data.table with cell metadata
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' pDataDT(g)
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

        return(getCellMetadata(
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
#' @returns data.table with feature metadata
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' fDataDT(g)
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

        return(getFeatureMetadata(
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
#' @returns giotto object
#' @details You need to specify which (cluster) column you want to annotate
#' and you need to provide an annotation vector like this:
#' \itemize{
#'   \item{1. identify the cell type of each cluster}
#'   \item{2. create a vector of these cell types, e.g.
#'   cell_types =  c('T-cell', 'B-cell', 'Stromal')}
#'   \item{3. provide original cluster names to previous vector,
#'   e.g. names(cell_types) = c(2, 1, 3)}
#' }
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' annotation <- c(
#'     "1" = "cell_type_1",
#'     "2" = "cell_type_2",
#'     "3" = "cell_type_3",
#'     "4" = "cell_type_4",
#'     "5" = "cell_type_5",
#'     "6" = "cell_type_6",
#'     "7" = "cell_type_7",
#'     "8" = "cell_type_8"
#' )
#'
#' g <- annotateGiotto(g,
#'     annotation_vector = annotation,
#'     cluster_column = "leiden_clus"
#' )
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
        stop("\n You need to provide both a named annotation vector and
            the corresponding cluster column  \n")
    }

    cell_metadata <- getCellMetadata(
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
        wrap_msg(
            "Not all clusters have an accompanying annotation in the
          annotation_vector: \n", "These names are missing: ",
            as.character(missing_annotations), "\n",
            "These annotations have no match: ",
            as.character(no_matching_annotations)
        )
        stop("Annotation interrupted \n")
    }


    # 3. remove previous annotation name if it's the same
    # but only if new name is not the same as cluster to be used
    if (name %in% colnames(cell_metadata[])) {
        wrap_msg('annotation name "', name,
            '" was already used and will be overwritten',
            sep = ""
        )

        cell_metadata[][, temp_cluster_name := annotation_vector[[
            as.character(get(cluster_column))
        ]], by = seq_len(nrow(cell_metadata[]))]
        cell_metadata[][, (name) := NULL]
    } else {
        cell_metadata[][, temp_cluster_name := annotation_vector[[
            as.character(get(cluster_column))
        ]], by = seq_len(nrow(cell_metadata[]))]
    }

    data.table::setnames(cell_metadata[], old = "temp_cluster_name", new = name)
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    gobject <- setCellMetadata(
        gobject = gobject,
        x = cell_metadata,
        verbose = FALSE,
        initialize = FALSE
    )
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

    return(gobject)
}



#' @title Remove cell annotation
#' @name removeCellAnnotation
#' @description Removes cell annotation from a Giotto object for a specific
#' feature modality (default = 'rna')
#' @param gobject giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param columns names of columns to remove
#' @param return_gobject boolean: return giotto object (default = TRUE)
#' @returns giotto object
#' @details if \code{return_gobject = FALSE}, it will return the cell metadata
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' annotation <- c(
#'     "1" = "cell_type_1",
#'     "2" = "cell_type_2",
#'     "3" = "cell_type_3",
#'     "4" = "cell_type_4",
#'     "5" = "cell_type_5",
#'     "6" = "cell_type_6",
#'     "7" = "cell_type_7",
#'     "8" = "cell_type_8"
#' )
#'
#' g <- annotateGiotto(g,
#'     annotation_vector = annotation,
#'     cluster_column = "leiden_clus"
#' )
#'
#' g <- removeCellAnnotation(g, columns = "cell_types")
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
        stop("\t You need to provide a vector of metadata column names to
            remove \t")
    }


    # get cell metadata
    cell_metadata <- getCellMetadata(gobject,
        spat_unit = spat_unit,
        feat_type = feat_type,
        output = "cellMetaObj",
        copy_obj = TRUE
    )
    # remove columns
    cell_metadata[] <- cell_metadata[][, (columns) := NULL]

    # return giotto object or cell metadata
    if (return_gobject == TRUE) {
        gobject <- setCellMetadata(gobject,
            x = cell_metadata,
            verbose = FALSE,
            initialize = FALSE
        )
        return(gobject)
    } else {
        cell_metadata[]
    }
}


#' @title Remove feature annotation
#' @name removeFeatAnnotation
#' @description Removes feature annotation from a Giotto object for a
#' specific feature modality
#' @param gobject giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param columns names of columns to remove
#' @param return_gobject boolean: return giotto object (default = TRUE)
#' @returns giotto object
#' @details if \code{return_gobject = FALSE}, it will return the gene metadata
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' g <- removeFeatAnnotation(g, columns = "hvf")
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
        stop("\t You need to provide a vector of metadata column names
            to remove \t")
    }

    # get feat metadata
    feat_metadata <- getFeatureMetadata(gobject,
        spat_unit = spat_unit,
        feat_type = feat_type,
        output = "featMetaObj",
        copy_obj = TRUE
    )
    # remove columns
    feat_metadata[] <- feat_metadata[][, (columns) := NULL]

    # return giotto object or cell metadata
    if (return_gobject == TRUE) {
        gobject <- setFeatureMetadata(gobject,
            x = feat_metadata,
            verbose = FALSE,
            initialize = FALSE
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
#' @param new_metadata new cell metadata to
#' use (data.table, data.frame, vector, factor, ...)
#' @param vector_name (optional) custom name for new metadata column if single
#' vector or factor is provided
#' @param by_column merge metadata based on \emph{cell_ID} column in
#' \code{\link{pDataDT}} (default = FALSE)
#' @param column_cell_ID column name of new metadata to use if
#' \code{by_column = TRUE}
#' @details You can add additional cell metadata in several manners:
#' \itemize{
#'   \item{1. Provide a data.frame-like object, vector, or factor with cell
#'   annotations in the same order as the \emph{cell_ID} column in
#'   pDataDT(gobject). This is a bit risky and not the most recommended.}
#'   \item{2. Provide a data.frame-like object with cell annotations and
#'   specify which column contains the cell IDs, these cell IDs need to match
#'   with the \emph{cell_ID} column in pDataDT(gobject)}
#'   \item{3. Provide a vector or factor that is named with the cell IDs they
#'   correspond to. These names will be matched against the \emph{cell_ID}
#'   column in pDataDT(gobject).}
#' }
#'
#' @returns giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' m <- pDataDT(g)
#' m <- m[, c("cell_ID", "leiden_clus")]
#' m$cell_type <- paste0("cell_type_", m$leiden_clus)
#' m <- m[, c("cell_ID", "cell_type")]
#'
#' g <- addCellMetadata(
#'     g,
#'     new_metadata = m,
#'     by_column = TRUE,
#'     column_cell_ID = "cell_ID"
#' )
#'
#' pDataDT(g)
#' @export
addCellMetadata <- function(gobject,
    spat_unit = NULL,
    feat_type = NULL,
    new_metadata,
    vector_name = NULL,
    by_column = FALSE,
    column_cell_ID = NULL) {
    # NSE variables
    cell_ID <- NULL

    # 0. set feat_type and spat_unit
    spat_unit <- set_default_spat_unit(
        gobject = gobject,
        spat_unit = spat_unit
    )
    feat_type <- set_default_feat_type(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type
    )


    # 1. check hierarchical slots
    # Expression information must first exist in the gobject for the
    # corresponding metadata information to be added.
    avail_ex <- list_expression(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type
    )
    if (is.null(avail_ex)) {
        .gstop(
            "No matching expression information discovered for:
      spat_unit:", spat_unit, "\nfeature type:", feat_type,
            "\nPlease add expression information first"
        )
    }


    # 2. get the cell metadata to add to
    cell_metadata <- getCellMetadata(
        gobject,
        spat_unit = spat_unit,
        feat_type = feat_type,
        output = "cellMetaObj",
        copy_obj = TRUE
    )

    # record initial order
    ordered_cell_IDs <- spatIDs(cell_metadata)


    # 3. format input metadata
    # [vector/factor input]
    # Values are assumed to be in the same order as the existing metadata info.
    # Convert vector or factor into a single-column data.table
    # Colname is the variable name of the vector or factor.
    # [all other inputs]
    # Coerce to data.table
    if (is.vector(new_metadata) || is.factor(new_metadata)) {
        original_name <- deparse(substitute(new_metadata))
        new_metadata <- data.table::as.data.table(
            new_metadata,
            keep.rownames = TRUE
        )
        if ("rn" %in% colnames(new_metadata) && ncol(new_metadata) > 1L) {
            # should only be TRUE when an "rn" col for rownames was added based
            # on the vector or factor's names
            data.table::setnames(new_metadata, old = "rn", new = "cell_ID")
        }

        # add column name for new meta info.
        # if a cell_ID col was added via rownames, it should be in the first
        # position.
        # ncol(new_metadata) should be the col index of the new information.
        if (!is.null(vector_name) && is.character(vector_name)) {
            colnames(new_metadata)[ncol(new_metadata)] <- vector_name
        } else {
            colnames(new_metadata)[ncol(new_metadata)] <- original_name
        }
    } else {
        # [DF or DT-like input]
        new_metadata <- data.table::as.data.table(new_metadata)
    }

    # If no specific column_cell_ID is provided, assume "cell_ID"
    if (is.null(column_cell_ID)) {
        column_cell_ID <- "cell_ID"
    }


    # 4. combine with existing metadata
    # get old and new meta colnames that are not the ID col
    new_col_names <- colnames(new_metadata)
    new_col_names <- new_col_names[new_col_names != column_cell_ID]
    old_col_names <- colnames(cell_metadata)
    old_col_names <- old_col_names[old_col_names != "cell_ID"]

    # overwrite columns with same name
    same_col_names <- new_col_names[new_col_names %in% old_col_names]
    if (length(same_col_names) >= 1) {
        wrap_msg(
            "\nThese column names were already used: ", same_col_names, "\n",
            "and will be overwritten \n"
        )
        cell_metadata[][, (same_col_names) := NULL]
    }


    if (!isTRUE(by_column)) {
        cell_metadata[] <- cbind(cell_metadata[], new_metadata)
    } else {
        if (!column_cell_ID %in% colnames(new_metadata)) {
            stop("'by_column' is TRUE and 'column_cell_ID' not found
                in new_metadata")
        }
        cell_metadata[] <- data.table::merge.data.table(
            x = cell_metadata[],
            by.x = "cell_ID",
            y = new_metadata,
            by.y = column_cell_ID,
            all.x = TRUE
        )
    }


    # 5. ensure data is in same order as start and set data
    cell_metadata[] <- cell_metadata[][match(ordered_cell_IDs, cell_ID)]


    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    gobject <- setCellMetadata(gobject,
        x = cell_metadata,
        verbose = FALSE,
        initialize = FALSE
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
#' @param by_column merge metadata based on \emph{feat_ID} column
#' in \code{\link{fDataDT}}
#' @param column_feat_ID column name of new metadata to use if by_column = TRUE
#' @returns giotto object
#' @details You can add additional feature metadata in several manners:
#' \itemize{
#'   \item{1. Provide a data.table or data.frame with feature annotations in
#'   the same order as the \emph{feat_ID} column in fDataDT(gobject) This is
#'   a bit risky and not the most recommended.}
#'   \item{2. Provide a data.table or data.frame with feature annotations and
#'   specify which column contains the feature IDs, these feature IDs need to
#'   match with the \emph{feat_ID} column in fDataDT(gobject)}
#'   \item{3. Provide a vector or factor that is named with the feature IDs
#'   they correspond to. These names will be matched against
#'   the \emph{feat_ID} column in fDataDT(gobject).}
#' }
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' m <- fDataDT(g)
#' m <- m[, "feat_ID"]
#' m$new_feat_ID <- paste0("gene_", m$feat_ID)
#'
#' g <- addFeatMetadata(
#'     g,
#'     new_metadata = m,
#'     by_column = TRUE,
#'     column_feat_ID = "feat_ID"
#' )
#'
#' fDataDT(g)
#' @export
addFeatMetadata <- function(gobject,
    feat_type = NULL,
    spat_unit = NULL,
    new_metadata,
    vector_name = NULL,
    by_column = FALSE,
    column_feat_ID = NULL) {
    # NSE variables
    feat_ID <- NULL

    # 0. set feat_type and spat_unit
    spat_unit <- set_default_spat_unit(
        gobject = gobject,
        spat_unit = spat_unit
    )
    feat_type <- set_default_feat_type(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type
    )


    # 1. check hierarchical slots
    # Expression information must first exist in the gobject for the
    # corresponding metadata information to be added.
    avail_ex <- list_expression(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type
    )
    if (is.null(avail_ex)) {
        .gstop(
            "No matching expression information discovered for:
      spat_unit:", spat_unit, "\nfeature type:", feat_type,
            "\nPlease add expression information first"
        )
    }


    # 2. get the cell metadata to add to
    feat_metadata <- getFeatureMetadata(
        gobject,
        spat_unit = spat_unit,
        feat_type = feat_type,
        output = "featMetaObj",
        copy_obj = TRUE
    )

    ordered_feat_IDs <- featIDs(feat_metadata)


    # 3. format input metadata
    # [vector/factor input]
    # Values are assumed to be in the same order as the existing metadata info.
    # Convert vector or factor into a single-column data.table
    # Colname is the variable name of the vector or factor.
    # [all other inputs]
    # Coerce to data.table
    if (is.vector(new_metadata) || is.factor(new_metadata)) {
        original_name <- deparse(substitute(new_metadata))
        new_metadata <- data.table::as.data.table(
            new_metadata,
            keep.rownames = TRUE
        )
        if ("rn" %in% colnames(new_metadata) && ncol(new_metadata) > 1L) {
            # should only be TRUE when an "rn" col for rownames was added based
            # on the vector or factor's names
            data.table::setnames(new_metadata, old = "rn", new = "feat_ID")
        }

        # add column name for new meta info.
        # if a cell_ID col was added via rownames, it should be in the first
        # position.
        # ncol(new_metadata) should be the col index of the new information.
        if (!is.null(vector_name) && is.character(vector_name)) {
            colnames(new_metadata)[ncol(new_metadata)] <- vector_name
        } else {
            colnames(new_metadata)[ncol(new_metadata)] <- original_name
        }
    } else {
        # [DF or DT-like input]
        new_metadata <- data.table::as.data.table(new_metadata)
    }

    # If no specific column_feat_ID is provided, assume "feat_ID"
    if (is.null(column_feat_ID)) {
        column_feat_ID <- "feat_ID"
    }


    # 4. combine with existing metadata
    # get old and new meta colnames that are not the ID col
    new_col_names <- colnames(new_metadata)
    new_col_names <- new_col_names[new_col_names != column_feat_ID]
    old_col_names <- colnames(feat_metadata[])
    old_col_names <- old_col_names[old_col_names != "feat_ID"]

    # overwrite columns with same name
    same_col_names <- new_col_names[new_col_names %in% old_col_names]
    if (length(same_col_names) >= 1) {
        wrap_msg(
            "\nThese column names were already used: ", same_col_names, "\n",
            "and will be overwritten \n"
        )
        feat_metadata[][, (same_col_names) := NULL]
    }


    if (!isTRUE(by_column)) {
        feat_metadata[] <- cbind(feat_metadata[], new_metadata)
    } else {
        if (!column_feat_ID %in% colnames(new_metadata)) {
            stop("'by_column' is TRUE and 'column_feat_ID' not found in
                new_metadata")
        }
        feat_metadata[] <- data.table::merge.data.table(
            x = feat_metadata[],
            by.x = "feat_ID",
            y = new_metadata,
            by.y = column_feat_ID,
            all.x = TRUE
        )
    }


    # 5. ensure data is in same order and set data
    feat_metadata[] <- feat_metadata[][match(ordered_feat_IDs, feat_ID)]

    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    gobject <- setFeatureMetadata(gobject,
        x = feat_metadata,
        verbose = FALSE,
        initialize = FALSE
    )
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

    return(gobject)
}





# expression ####


#' @title create_average_DT
#' @description calculates average gene expression for a cell metadata
#' factor (e.g. cluster)
#' @param gobject giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param meta_data_name name of metadata column to use
#' @param expression_values which expression values to use
#' @returns data.table with average gene expression values for each factor
#' @keywords internal
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' create_average_DT(g, meta_data_name = "leiden_clus")
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
    values <- match.arg(
        expression_values,
        unique(c("normalized", "scaled", "custom", expression_values))
    )
    expr_data <- get_expression_values(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type,
        values = values,
        output = "matrix"
    )

    # metadata
    cell_metadata <- getCellMetadata(gobject,
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
#' @description calculates average gene detection for a cell metadata
#' factor (e.g. cluster)
#' @param gobject giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param meta_data_name name of metadata column to use
#' @param expression_values which expression values to use
#' @param detection_threshold detection threshold to consider a gene detected
#' @returns data.table with average gene epression values for each factor
#' @keywords internal
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' create_average_detection_DT(g, meta_data_name = "leiden_clus")
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
    values <- match.arg(
        expression_values,
        unique(c("normalized", "scaled", "custom", expression_values))
    )
    expr_data <- get_expression_values(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type,
        values = values,
        output = "matrix"
    )

    # metadata
    cell_metadata <- getCellMetadata(gobject,
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
#' @returns matrix
#' @keywords internal
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' create_cluster_matrix(g, cluster_column = "leiden_clus")
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

    values <- match.arg(
        expression_values,
        unique(c("normalized", "scaled", "custom", expression_values))
    )

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
        feat_subset_detected <- feat_subset[
            feat_subset %in% rownames(testmatrix)
        ]
        testmatrix <- testmatrix[
            rownames(testmatrix) %in% feat_subset_detected,
        ]
    }

    return(testmatrix)
}













#' @title calculateMetaTable
#' @name calculateMetaTable
#' @description calculates the average gene expression for one or
#' more (combined) annotation columns.
#' @param gobject giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param expression_values expression values to use
#' @param metadata_cols annotation columns found in \code{pDataDT(gobject)}
#' @param selected_feats subset of features to use
#' @param selected_genes subset of genes to use (deprecated)
#' @returns data.table with average expression values for each gene
#' per (combined) annotation
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' calculateMetaTable(g, metadata_cols = "leiden_clus")
#' @export
calculateMetaTable <- function(gobject,
    spat_unit = NULL,
    feat_type = NULL,
    expression_values = c("normalized", "scaled", "custom"),
    metadata_cols = NULL,
    selected_feats = NULL,
    selected_genes = NULL) {
    if (is.null(metadata_cols)) stop("\n You need to select one or more
                                    valid column names from pDataDT() \n")

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
        warning("selected_genes is deprecated, use selected_feats in the
                future \n")
    }

    # data.table variables
    uniq_ID <- NULL

    ## get metadata and create unique groups
    metadata <- data.table::copy(pDataDT(
        gobject,
        feat_type = feat_type, spat_unit = spat_unit
    ))
    if (length(metadata_cols) > 1) {
        metadata[, uniq_ID := paste(.SD, collapse = "-"),
            by = seq_len(nrow(metadata)), .SDcols = metadata_cols
        ]
    } else {
        metadata[, uniq_ID := get(metadata_cols)]
    }

    ## possible groups
    possible_groups <- unique(metadata[, metadata_cols, with = FALSE])
    if (length(metadata_cols) > 1) {
        possible_groups[, uniq_ID := paste(.SD, collapse = "-"),
            by = seq_len(nrow(possible_groups)), .SDcols = metadata_cols
        ]
    } else {
        possible_groups[, uniq_ID := get(metadata_cols)]
    }

    ## get expression data
    values <- match.arg(
        expression_values,
        unique(c(
            "normalized", "scaled", "custom",
            expression_values
        ))
    )
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

    for (row in seq_len(nrow(possible_groups))) {
        uniq_identifiier <- possible_groups[row][["uniq_ID"]]
        selected_cell_IDs <- metadata[uniq_ID == uniq_identifiier][["cell_ID"]]
        sub_expr_values <- expr_values[
            ,
            colnames(expr_values) %in% selected_cell_IDs
        ]

        if (is.vector(sub_expr_values) == FALSE) {
            subvec <- rowMeans_flex(sub_expr_values)
        } else {
            subvec <- sub_expr_values
        }
        result_list[[row]] <- subvec
    }
    finaldt <- data.table::as.data.table(do.call("rbind", result_list))
    possible_groups_res <- cbind(possible_groups, finaldt)
    possible_groups_res_melt <- data.table::melt.data.table(
        possible_groups_res,
        id.vars = c(metadata_cols, "uniq_ID")
    )

    return(possible_groups_res_melt)
}


#' @title calculateMetaTableCells
#' @name calculateMetaTableCells
#' @description calculates the average metadata values for one or
#' more (combined) annotation columns.
#' @param gobject giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param value_cols metadata or enrichment value columns to use
#' @param metadata_cols annotation columns found in \code{pDataDT(gobject)}
#' @param spat_enr_names which spatial enrichment results to include
#' @returns data.table with average metadata values per (combined) annotation
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' calculateMetaTableCells(g,
#'     metadata_cols = "cell_ID",
#'     value_cols = "leiden_clus"
#' )
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

    if (is.null(metadata_cols)) stop("\n You need to select one or more
                                  valid column names from pDataDT() \n")
    if (is.null(value_cols)) stop("\n You need to select one or more valid
                                  value column names from pDataDT() \n")

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
        wrap_msg("These value columns were not found: ", missing_value_cols)
    }
    value_cols <- value_cols[value_cols %in% cell_metadata_cols]

    if (!all(metadata_cols %in% cell_metadata_cols)) {
        missing_metadata_cols <- metadata_cols[!metadata_cols %in%
            cell_metadata_cols]
        wrap_msg(
            "These metadata columns were not found: ",
            missing_metadata_cols
        )
    }
    metadata_cols <- metadata_cols[metadata_cols %in% cell_metadata_cols]

    if (!length(metadata_cols) > 0 | !length(value_cols) > 0) {
        stop("\n missing sufficient metadata or value columns \n")
    }

    workdt <- cell_metadata[, lapply(.SD, mean),
        by = metadata_cols, .SDcols = value_cols
    ]
    workdtmelt <- data.table::melt.data.table(workdt, measure.vars = value_cols)

    return(workdtmelt)
}






#' @title createMetafeats
#' @name createMetafeats
#' @description This function creates an average metafeat/metagene/module
#' for clusters.
#' @param gobject Giotto object
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param expression_values expression values to use
#' @param feat_clusters numerical vector with features as names
#' @param stat statistical transformation to calculate across the cluster
#' features per cell to get the metafeature value. Provided defaults are
#' "mean", 'sum', "max", and "min". If a function  is supplied instead,
#' it will be run via `apply(MARGIN = 2)` across the expression matrix to
#' calculate  the metafeature value.
#' @param rescale_to numeric vector of length 2. (optional) New maximum and
#' minimum to rescale all features to before calculating the metafeature score
#' using `stat`. Values are first zeroed by setting the minimum value to 0,
#' then they are rescaled. In cases where all values are the same across a
#' feature, the rescale produces the middle point of the range specified by
#' `rescale_to`
#' @param name name of the metagene results
#' @param return_gobject return giotto object
#' @param verbose be verbose
#' @returns giotto object
#' @details An example for the 'gene_clusters' could be like this:
#' cluster_vector = c(1, 1, 2, 2)
#' names(cluster_vector) = c('geneA', 'geneB', 'geneC', 'geneD')
#' @examples
#' # load a dataset
#' g <- GiottoData::loadGiottoMini("viz")
#' # set a spat unit to use
#' activeSpatUnit(g) <- "aggregate"
#'
#' # define the metafeats
#' # We do this by making a set of annotations. These annotations can be
#' # made as either a vector or data.frame
#'
#' # 1.1 annotation vector (numeric)
#' feats_to_use <- featIDs(g)[seq_len(6)]
#' clust_to_use1 <- c(1, 1, 1, 2, 2, 2)
#' names(clust_to_use1) <- feats_to_use
#' force(clust_to_use1)
#'
#' g <- createMetafeats(
#'     gobject = g,
#'     feat_clusters = clust_to_use1,
#'     name = "metagene_1"
#' )
#'
#' # 1.2 annotation vector (character)
#' clust_to_use2 <- c(rep("sig_a", 3), rep("sig_b", 3))
#' names(clust_to_use2) <- feats_to_use
#' force(clust_to_use2)
#'
#' g <- createMetafeats(
#'     gobject = g,
#'     feat_clusters = clust_to_use2,
#'     name = "metagene_2"
#' )
#'
#' # 2.1 annotation data.frame (no weights)
#' # Cols must be:
#' # `clus` - which metafeature
#' # `feat` - features to assign to this metafeature
#' # `w` - (optional) weight to assign the feature before metafeature score
#' #       calculation
#' clust_to_use3 <- data.frame(
#'     clus = c(rep("sig_a", 3), rep("sig_b", 3)),
#'     feat = feats_to_use
#' )
#' force(clust_to_use3)
#' g <- createMetafeats(
#'     gobject = g,
#'     feat_clusters = clust_to_use3,
#'     name = "metagene_3"
#' )
#'
#' # 2.1 annotation data.frame (weights)
#' clust_to_use4 <- data.frame(
#'     clus = c(rep("sig_a", 3), rep("sig_b", 3)),
#'     feat = feats_to_use,
#'     w = c(1, 1, 1, 2, 3, 4)
#' )
#' force(clust_to_use4)
#' g <- createMetafeats(
#'     gobject = g,
#'     feat_clusters = clust_to_use4,
#'     name = "weighted_metagene"
#' )
#'
#' # 3. Other statistic transforms can be calculated
#' # The default and what has been used so far is just finding the mean
#' g <- createMetafeats(
#'     gobject = g,
#'     feat_clusters = clust_to_use4,
#'     stat = "sum",
#'     expression_values = "raw", # find summed raw expr for these feats
#'     name = "raw_sums"
#' )
#'
#' # 4. A custom stat function can also be supplied
#' # These custom functions must be summary functions, as in, they must
#' # produce only a single numeric output from many
#' custom_stat <- function(x) {
#'     if (max(x) == 0) return(0)
#'     return(mean(x/max(x)))
#' }
#' g <- createMetafeats(
#'     gobject = g,
#'     feat_clusters = clust_to_use4,
#'     stat = custom_stat,
#'     expression_values = "raw",
#'     name = "raw_custom"
#' )
#'
#' # 5. A rescale of the values to a specified numeric range can be applied
#' # before the stat score calculation.
#' g <- createMetafeats(
#'     gobject = g,
#'     feat_clusters = clust_to_use4,
#'     stat = "mean",
#'     expression_values = "normalized",
#'     rescale_to = c(0, 1),
#'     name = "norm_scaled_mean_metafeat"
#' )
#' showGiottoSpatEnrichments(g)
#' @seealso [GiottoVisuals::spatCellPlot()]
#' @export
createMetafeats <- function(gobject,
    spat_unit = NULL,
    feat_type = NULL,
    expression_values = c("normalized", "scaled", "custom"),
    feat_clusters,
    stat = c("mean", "sum", "max", "min"),
    rescale_to = NULL,
    name = paste0("metafeat_", ifelse(is.function(stat), "custom", stat)),
    return_gobject = TRUE,
    verbose = NULL
) {

    # Set feat_type and spat_unit
    spat_unit <- set_default_spat_unit(
        gobject = gobject, spat_unit = spat_unit
    )
    feat_type <- set_default_feat_type(
        gobject = gobject, spat_unit = spat_unit, feat_type = feat_type
    )

    if (!is.null(rescale_to)) {
        if (!is.numeric(rescale_to) || length(rescale_to) != 2L) {
            stop(wrap_txt(
                "`rescale_to` must be a numeric vector with length of 2
                Example input: `c(0, 1)`",
                errWidth = TRUE
            ), call. = FALSE)
        }
    }

    preset_stats <- c("mean", "sum", "max", "min")
    if (is.character(stat)) {
        # preset stat_fun
        stat <- match.arg(stat, choices = preset_stats)
        stat_fun <- switch(stat,
            "mean" = colMeans_flex,
            "sum" = colSums_flex,
            "max" = function(x) apply(x, MARGIN = 2L, FUN = max),
            "min" = function(x) apply(x, MARGIN = 2L, FUN = min),
        )
    } else {
        # custom stat_fun
        if (!inherits(stat, "function")) {
            stop(wrap_txtf(
                "%s or %s:\n '%s'",
                "`stat` is not a custom function",
                "one of the preset stats",
                paste(preset_stats, collapse = "', '"),
                errWidth = TRUE
            ), call. = FALSE)
        }
        # apply() the stat function on cols
        stat_fun <- function(x) apply(x, MARGIN = 2L, FUN = stat)
    }

    # expression values to be used
    values <- match.arg(
        expression_values,
        unique(c(
            "normalized", "scaled", "custom",
            expression_values
        ))
    )
    exprobj <- get_expression_values(
        gobject = gobject,
        spat_unit = spat_unit,
        feat_type = feat_type,
        values = values,
        output = "exprObj"
    )

    # drop to matrix representation
    expr_values <- exprobj[]

    # apply features rescale (rowwise) if needed
    if (!is.null(rescale_to)) {
        expr_values <- apply(expr_values, MARGIN = 1, function(feat) {
            .rescalev(feat, rmin = min(rescale_to), rmax = max(rescale_to))
        }) %>%
            t_flex()
    }

    calc_args <- list(
        x = feat_clusters,
        stat_fun = stat_fun,
        expr_values = expr_values,
        verbose = verbose
    )

    ## calculate metafeat ##
    res_final <- if (is.vector(feat_clusters)) {
        do.call(.calc_metafeat_vec, args = calc_args)
    } else if (inherits(feat_clusters, "data.frame")) {
        do.call(.calc_metafeat_dt, args = calc_args)
    } else {
        stop(wrap_txtf(
            "`feat_clusters` is neither a vector nor a data.frame-like input",
            errWidth = TRUE
        ), call. = FALSE)
    }

    # NSE variables
    cell_ID <- NULL

    res_final[, cell_ID := colnames(expr_values)]

    # create spatial enrichment object
    enrObj <- create_spat_enr_obj(
        name = name,
        method = "metafeat",
        enrichDT = res_final,
        spat_unit = spat_unit,
        feat_type = feat_type,
        provenance = exprobj@provenance,
        misc = list(expr_values_used = expression_values)
    )

    if (isTRUE(return_gobject)) {
        ## enrichment scores
        spenr_names <- list_spatial_enrichments_names(
            gobject = gobject,
            spat_unit = spat_unit,
            feat_type = feat_type
        )

        if (name %in% spenr_names) {
            wrap_msg(name, " has already been used, will be overwritten")
        }

        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
        gobject <- set_spatial_enrichment(
            gobject = gobject,
            spatenrichment = enrObj
        )
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

        ## update parameters used ##
        gobject <- update_giotto_params(gobject,
            description = "_create_metafeat"
        )
        return(gobject)
    } else {
        return(enrObj)
    }
}


# `x` should be a vector (numeric or character) with names.
# The metafeatures/clusters are defined by the numerical values
# The features to use are given by the names.
# Which metafeature/cluster to assign the feature(s) to is encoded by which
# numbers are named that feature.
# `expr_values` should be a matrix-like
.calc_metafeat_vec <- function(
        x, expr_values, stat_fun, verbose = NULL
) {
    res_list <- list()
    clusters <- mixedsort(unique(x))

    # iterate through clusters
    for (clus_id in clusters) {
        vmsg(.v = verbose, "calculating metafeature: ", clus_id)
        # subset to features requested for cluster
        selected_feats <- names(x[x == clus_id])
        sub_mat <- expr_values[
            rownames(expr_values) %in% selected_feats, , drop = FALSE
        ]

        # calculate score
        score <- stat_fun(sub_mat)

        res_list[[clus_id]] <- score
    }

    res_final <- data.table::as.data.table(t(do.call("rbind", res_list)))
    colnames(res_final) <- as.character(clusters)

    return(res_final)
}

# `x` is a data.table with at least cols `clus` and `feat` for
# cluster/metafeature and feature to assign respectively. A third `w` numeric
# col can be provided which is a weight to apply to the values before
# performing the stat_fun score calculation.
.calc_metafeat_dt <- function(
        x, expr_values, stat_fun, verbose = NULL
) {
    # NSE vars
    clus <- feat <- w <- NULL

    x <- data.table::as.data.table(x)
    res_list <- list()
    clusters <- x[, mixedsort(unique(clus))]

    # iterate through clusters
    for (clus_id in clusters) {
        vmsg(.v = verbose, "calculating metafeature: ", clus_id)
        # subset to features requested for cluster
        selected_feats <- x[clus == clus_id, feat]
        sub_mat <- expr_values[
            rownames(expr_values) %in% selected_feats, , drop = FALSE
        ]
        expr_feats <- rownames(sub_mat) # subset of `selected_feats` from `x`

        # apply weights if any
        if ("w" %in% names(x)) {
            w <- x[clus == clus_id, c("feat", "w")][match(expr_feats, feat), w]
            sub_mat <- w * sub_mat
        }

        # calculate score
        score <- stat_fun(sub_mat)

        res_list[[clus_id]] <- score
    }

    res_final <- data.table::as.data.table(t(do.call("rbind", res_list)))
    colnames(res_final) <- as.character(clusters)

    return(res_final)
}

# rescale a numeric vector input to a new min and max
# values are zeroed before scaling
.rescalev <- function(x, rmin, rmax) {
    rx <- range(x)
    xmin <- rx[1L]
    xmax <- rx[2L]
    if (xmax == xmin) {
        return(mean(rmin, rmax)) # return midpoint of rescaled range
    }

    # zero values
    x <- x - xmin
    xmax <- xmax - xmin # update xmax

    # rescale
    rrange <- rmax - rmin
    scalef <- rrange / xmax

    # pad values with new min val
    res <- x * scalef
    if (rmin != 0) {
        res <- res + rmin
    }

    return(res)
}



# DT overallocation ####


#' @title Over-allocation for giotto DT-based info
#' @description Finds DT based objects, overallocates the data.tables, then sets
#' the objects back in the giotto object
#' @param gobject giotto object
#' @keywords internal
#' @returns giotto object
.giotto_alloc_dt <- function(gobject) {
    # data.table vars
    spat_unit <- feat_type <- name <- NULL

    # metadata
    avail_cm <- list_cell_metadata(gobject)
    if (!is.null(avail_cm)) {
        for (cm_i in seq(nrow(avail_cm))) {
            cm <- getCellMetadata(
                gobject = gobject,
                spat_unit = avail_cm[cm_i, spat_unit],
                feat_type = avail_cm[cm_i, feat_type],
                output = "cellMetaObj",
                copy_obj = FALSE
            )
            if (!is.null(cm[])) {
                cm[] <- data.table::setalloccol(cm[])
                gobject <- setCellMetadata(
                    gobject = gobject,
                    x = cm,
                    verbose = FALSE,
                    initialize = FALSE
                )
            }
        }
    }

    avail_fm <- list_feat_metadata(gobject)
    if (!is.null(avail_fm)) {
        for (fm_i in seq(nrow(avail_fm))) {
            fm <- getFeatureMetadata(
                gobject = gobject,
                spat_unit = avail_fm[fm_i, spat_unit],
                feat_type = avail_fm[fm_i, feat_type],
                output = "featMetaObj",
                copy_obj = FALSE
            )
            if (!is.null(fm[])) {
                fm[] <- data.table::setalloccol(fm[])
                gobject <- setFeatureMetadata(
                    gobject = gobject,
                    x = fm,
                    verbose = FALSE,
                    initialize = FALSE
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
                slot(sn, "networkDT_before_filter") <- data.table::setalloccol(
                    slot(sn, "networkDT_before_filter")
                )
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

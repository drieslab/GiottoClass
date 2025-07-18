# EVALUATION FUNCTIONS
# --------------------------------------------------------------------------- #
# Internal functions to evaluate raw inputs into formats that are
# directly compatible with Giotto's functionality.
# Note that these only format the data the output from this function still needs
# to be put into a Giotto subobject.
# --------------------------------------------------------------------------- #



# These functions are difficult to convert to an S4 generic since they have
# since many data types have overlapping allowed signatures.
# If we want to make these functionalities available to other modules, using a
# centralized wrapper function like below is a solution.
#
# TODO switch the internal functions to have the same set of core params so that
# they can properly be documented together

#' @name evaluate_input
#' @title Evaluate raw inputs to Giotto formatting
#' @description
#' Experimental. Evaluate raw inputs into formats that are directly compatible
#' with Giotto's functionality. Note that this function only formats the data.
#' The output from this function still needs to be put into a Giotto
#' subobject.\cr
#' This is a wrapper function for the individual GiottoClass evaluation
#' functions.
#' @param type character. Type of giotto data to evaluate to.
#' @param x data to evaluate
#' @param \dots additional params to pass
#' @returns character or the same class of x
#' @examples
#' x <- GiottoData::loadSubObjectMini("exprObj", 1)
#'
#' evaluate_input(type = "expression", x)
#' @export
evaluate_input <- function(type, x, ...) {
    type <- match.arg(
        type,
        c(
            "expression",
            "cell_meta",
            "feat_meta",
            "spat_locs",
            "spat_net",
            "spat_enr",
            "dim_reduc",
            "nn_net",
            "spat_info",
            "feat_info"
        )
    )
    switch(type,
        "expression" = .evaluate_expr_matrix(x, ...),
        "cell_meta" = .evaluate_cell_metadata(x, ...),
        "feat_meta" = .evaluate_feat_metadata(x, ...),
        "spat_locs" = .evaluate_spatial_locations(x, ...),
        "spat_net" = .evaluate_spatial_network(x, ...),
        "spat_enr" = .evaluate_spatial_enrichment(x, ...),
        "dim_reduc" = .evaluate_dimension_reduction(x, ...),
        "nn_net" = .evaluate_nearest_networks(x, ...),
        "spat_info" = .evaluate_spatial_info(x, ...),
        "feat_info" = .evaluate_feat_info(x, ...)
    )
}




# Expression ####

#' @title Evaluate expression matrix
#' @name .evaluate_expr_matrix
#' @description Evaluate expression matrices that are provided as input and
#' converts them to preferred format for Giotto object. A filepath can also be
#' provided through \code{inputmatrix} param. If this is done, the function
#' will attempt to read the matrix file in using \code{\link{readExprMatrix}}.
#' @param inputmatrix inputmatrix to evaluate
#' @param sparse create sparse matrix (default = TRUE)
#' @param cores how many cores to use
#' @details The inputmatrix can be a matrix, sparse matrix, data.frame,
#' data.table or path to any of these.
#' @keywords internal
#' @returns sparse matrix
#' @noRd
.evaluate_expr_matrix <- function(
        inputmatrix,
        sparse = TRUE,
        cores = determine_cores(),
        feat_type = "rna",
        expression_matrix_class = c("dgCMatrix", "DelayedArray", "dbMatrix")) {
    target_class <- match.arg(
        expression_matrix_class[1],
        c("dgCMatrix", "DelayedArray", "dbMatrix")
    )

    # Return early if inputmatrix is already of the target class
    if (!inherits(inputmatrix, "character") &&
        inherits(inputmatrix, target_class)) {
        return(inputmatrix)
    }

    # Main decision tree for converting inputmatrix
    if (inherits(inputmatrix, "character") && length(inputmatrix) == 1) {
        inputmatrix <- path.expand(inputmatrix)
        mymatrix <- readExprMatrix(
            path = inputmatrix,
            cores = cores,
            expression_matrix_class = target_class,
            feat_type = feat_type
        )
    } else if (target_class == "dbMatrix") {
        .gstop(
            "Automatic conversion to 'dbMatrix' is not supported within ",
            "createExprObj(). Please provide a pre-constructed ",
            "'dbMatrix' object instead. See ?dbMatrix for details."
        )
    } else if (target_class == "DelayedArray") {
        mymatrix <- DelayedArray::DelayedArray(inputmatrix)
    } else if (inherits(inputmatrix, "Matrix")) {
        mymatrix <- inputmatrix
    } else if (inherits(inputmatrix, "DelayedMatrix")) {
        mymatrix <- inputmatrix
    } else if (inherits(inputmatrix, "data.table")) {
        if (sparse == TRUE) {
            # force sparse class
            mymatrix <- Matrix::Matrix(as.matrix(inputmatrix[, -1]),
                dimnames = list(
                    inputmatrix[[1]],
                    colnames(inputmatrix[, -1])
                ), sparse = TRUE
            )
        } else {
            # let Matrix decide
            mymatrix <- Matrix::Matrix(as.matrix(inputmatrix[, -1]),
                dimnames = list(
                    inputmatrix[[1]],
                    colnames(inputmatrix[, -1])
                )
            )
        }
    } else if (inherits(inputmatrix, what = c("data.frame", "matrix"))) {
        mymatrix <- methods::as(as.matrix(inputmatrix), "sparseMatrix")
    } else if (inherits(inputmatrix, "exprObj")) {
        inputmatrix[] <- .evaluate_expr_matrix(
            inputmatrix[],
            sparse = sparse, cores = cores,
            expression_matrix_class = target_class
        )
        mymatrix <- inputmatrix
    } else {
        .gstop(
            "expression input needs to be a path to matrix-like data or an",
            "object of class 'Matrix', 'data.table', 'data.frame', 'matrix'",
      "'DelayedMatrix' or 'dbSparseMatrix'."
        )
    }


    # check rownames and colnames
    if (any(duplicated(rownames(mymatrix)))) {
        stop("row names contains duplicates, please remove or rename")
    }

    if (any(duplicated(colnames(mymatrix)))) {
        stop("column names contains duplicates, please remove or rename")
    }


    return(mymatrix)
}





# Metadata ####

#' @param metadata metadata input
#' @param cores cores to use if reading in the information
#' @keywords internal
#' @noRd
.evaluate_cell_metadata <- function(metadata,
    cores = determine_cores(),
    verbose = TRUE) {
    # data.table vars
    cell_ID <- NULL

    # Get data as data.table
    if (!any(class(metadata) %in% c(
        "data.table", "data.frame", "matrix", "character"
    ))) {
        .gstop(
            "metadata needs to be a data.table or data.frame-like object",
            "or a path to one of these"
        )
    }
    if (inherits(metadata, "character")) {
        metadata <- path.expand(metadata)
        if (!file.exists(metadata)) {
            .gstop("path to metadata does not exist")
        }
        metadata <- data.table::fread(input = metadata, nThread = cores)
    } else {
        metadata <- tryCatch(
            data.table::setDT(metadata),
            error = function(e) data.table::as.data.table(metadata)
        )
    }


    # assign cell_ID col
    if ("cell_ID" %in% colnames(metadata)) {
        data.table::setcolorder(metadata, neworder = c("cell_ID")) # set as 1rst
        metadata[, cell_ID := as.character(cell_ID)] # ensure character

        # ensure unique entries
        if (any(metadata[, duplicated(cell_ID)])) {
            .gstop("Cell metadata: duplicates found in cell_ID column.")
        }
    } else {
        warning(wrap_txt("Cell metadata input: no col named cell_ID.
                    Setting temporary NA values"))
        # set temporary NA values
        metadata[, cell_ID := NA_character_]
        # re-order so that cell_ID is the first column
        data.table::setcolorder(metadata, neworder = "cell_ID")
    }

    return(metadata)
}









#' @keywords internal
#' @noRd
.evaluate_feat_metadata <- function(metadata,
    cores = determine_cores(),
    verbose = TRUE) {
    # data.table vars
    feat_ID <- NULL

    # Get data as data.table
    if (!any(class(metadata) %in% c(
        "data.table", "data.frame", "matrix", "character"
    ))) {
        .gstop(
            "metadata needs to be a data.table or data.frame-like object",
            "or a path to one of these"
        )
    }
    if (inherits(metadata, "character")) {
        metadata <- path.expand(metadata)
        if (!file.exists(metadata)) {
            .gstop("path to metadata does not exist")
        }
        metadata <- data.table::fread(input = metadata, nThread = cores)
    } else {
        metadata <- tryCatch(
            data.table::setDT(metadata),
            error = function(e) data.table::as.data.table(metadata)
        )
    }


    # assign feat_ID col
    if ("feat_ID" %in% colnames(metadata)) {
        data.table::setcolorder(metadata, neworder = c("feat_ID")) # set as 1rst
        metadata[, feat_ID := as.character(feat_ID)] # ensure character

        # ensure unique entries
        if (any(metadata[, duplicated(feat_ID)])) {
            .gstop("Feature metadata: duplicates found in feat_ID column.")
        }
    } else {
        warning(wrap_txt("Feature metadata input: no col named feat_ID.
                    Setting temporary NA values"))
        # set temporary NA values
        metadata[, feat_ID := NA_character_]
    }

    return(metadata)
}









# Spatial ####

# s3 generic to process inputs for spatial locations objects
.evaluate_spatial_locations <- function(x, ...) {
    UseMethod(".evaluate_spatial_locations")
}

.evaluate_spatial_locations.default <- function(x, ...) {
    stop(wrap_txt(
        "spatial_locs needs to be a `data.table` or `data.frame`-like object",
        "a filepath to one of these, numeric matrix, or numeric xy pairs"
    ), call. = FALSE)
}

.evaluate_spatial_locations.matrix <- function(x, verbose = NULL, ...) {
    id <- NULL # NSE vars
    has_colnames <- !is.null(colnames(x))
    .spatlocs_check_ncol(x)
    dt <- data.table::as.data.table(x)
    if (!has_colnames) {
        colnames(dt) <- c("x", "y", "z")[seq_len(ncol(x))]
    }
    rnames <- rownames(x)
    if (!is.null(rnames)) {
        vmsg(.v = verbose, "[spatlocs] matrix input has rownames.
             Using these as IDs.")
        dt[, id := rnames]
    }
    .evaluate_spatial_locations(dt, verbose = verbose, ...)
}

.evaluate_spatial_locations.numeric <- function(
        x, numeric_format = c("pair", "triplet"), ...) {
    numeric_format <- match.arg(numeric_format, choices = c("pair", "triplet"))
    x <- switch(numeric_format,
        "pair" = .spatlocs_numeric_parse_pair(x, ...),
        "triplet" = .spatlocs_numeric_parse_triplet(x, ...)
    )
    .evaluate_spatial_locations(x, ...)
}

.spatlocs_numeric_parse_pair <- function(x, ...) {
    if (length(x) %% 2 != 0 || length(x) == 0) {
        stop(wrap_txt(
            "[spatlocs] numeric 'pair' inputs must be given as x,y pairs.
                For example: c(x1, y1, x2, y2)"
        ), call. = FALSE)
    }
    data.table::data.table(
        x = x[c(TRUE, FALSE)],
        y = x[c(FALSE, TRUE)]
    )
}

.spatlocs_numeric_parse_triplet <- function(x, ...) {
    if (length(x) %% 3 != 0 || length(x) == 0) {
        stop(wrap_txt(
            "[spatlocs] numeric 'triplet' inputs must be given as x,y,z",
            "triplets.\n For example: c(x1, y1, z1, x2, y2, z2)"
        ), call. = FALSE)
    }
    data.table::data.table(
        x = x[c(TRUE, FALSE, FALSE)],
        y = x[c(FALSE, TRUE, FALSE)],
        z = x[c(FALSE, FALSE, TRUE)]
    )
}

.evaluate_spatial_locations.character <- function(
        x, cores = determine_cores(), ...) {
    if (!file.exists(x)) {
        stop("path to spatial locations does not exist\n", call. = FALSE)
    }
    x <- data.table::fread(input = x, nThread = cores)
    .evaluate_spatial_locations(x, cores = cores, ...)
}

.evaluate_spatial_locations.data.frame <- function(
        x, verbose = NULL, ...) {
    .spatlocs_check_ncol(x)
    # coerce to data.table
    x <- tryCatch(
        data.table::setDT(x),
        error = function(e) data.table::as.data.table(x)
    )

    # check if all columns are numeric
    column_classes <- lapply(x, FUN = class)
    non_numeric_classes <- column_classes[!column_classes %in%
        c("numeric", "integer")]

    # determine cell_ID -------------------- #
    potential_cell_ids <- NULL

    # 1. find non-numeric cols (possible cell_ID col)
    # 2. collect first one as expected cell IDs
    # 3. remove all cols except numeric cols
    if (length(non_numeric_classes) > 0L) {
        non_numeric_indices <- which(!column_classes %in%
            c("numeric", "integer"))

        .spatlocs_multiple_non_numerics_msg(
            non_num_cols = non_numeric_indices,
            verbose = verbose
        )

        potential_cell_ids <- x[[names(non_numeric_classes)[[1]]]]

        x <- x[, -non_numeric_indices, with = FALSE]
        keep_ncols <- min(3L, ncol(x))
        x <- x[, seq_len(keep_ncols), with = FALSE] # restrict to first 3 cols
    }

    # add spatial dimension colnames
    spatial_dimensions <- c("x", "y", "z")[seq_len(ncol(x))]
    colnames(x) <- paste0("sdim", spatial_dimensions)

    # Assign first non-numeric as cell_ID
    if (!is.null(potential_cell_ids)) {
        x[, "cell_ID" := potential_cell_ids]
    }
    x
}

.spatlocs_check_ncol <- function(x) {
    if (is.matrix(x) || is.array(x)) {
        n_numeric_cols <- if (is.numeric(x)) ncol(x) else 0L
    } else {
        # For data.frame-like: use column-wise check
        numeric_cols <- vapply(x, is.numeric, FUN.VALUE = logical(1L))
        n_numeric_cols <- sum(numeric_cols)
    }

    # too few
    if (n_numeric_cols < 2L) {
        stop(wrap_txt(
            "There need to be at least 2 numeric columns for spatial locations"
        ), call. = FALSE)
    }
    # too many
    if (n_numeric_cols > 3L) {
        warning(wrap_txt(
            "There are more than 3 numeric columns for spatial locations, only",
            "the first 3 will be used"
        ), call. = FALSE)
    }
}

# when more than one non-numeric col is present,
# inform about handling (extra are dropped.)
.spatlocs_multiple_non_numerics_msg <- function(
    non_num_cols,
    verbose = NULL,
    type = "locations") {
    if (length(non_num_cols) > 1L) {
        vmsg(.v = verbose, sprintf(
            "[spatial %s]
                Input has multiple non numeric columns at positions: %s
                The first non-numeric column will be considered as a %s\n%s",
            type,
            toString(non_num_cols),
            "cell ID to test for consistency with the expression matrix",
            "Other non numeric columns will be removed"
        ))
    }
}







#' @title Evaluate spatial network
#' @name .evaluate_spatial_network
#' @description function to evaluate a spatial network
#' @keywords internal
#' @noRd
.evaluate_spatial_network <- function(spatial_network) {
    if (inherits(spatial_network, "spatialNetworkObj")) {
        spatial_network[] <- .evaluate_spatial_network(spatial_network[])
        return(spatial_network)
    }

    if (!inherits(spatial_network, "data.frame")) {
        .gstop("The spatial network must be a data.frame(-like) object")
    }
    if (!inherits(spatial_network, "data.table")) {
        spatial_network <- data.table::setDT(spatial_network)
    }

    netw_names <- colnames(spatial_network)
    required_cols <- c(
        "from", "to",
        "sdimx_begin", "sdimy_begin",
        "sdimx_end", "sdimy_end",
        "distance", "weight"
    )

    missing_cols <- required_cols[!required_cols %in% netw_names]

    if (length(missing_cols) > 0) {
        .gstop("missing columns: ", list(missing_cols))
    }

    return(spatial_network)
}










#' @name .evaluate_spatial_enrichment
#' @description evaluate spatial enrichment input into data.table format
#' compatible with spatEnrObj
#' @keywords internal
#' @noRd
.evaluate_spatial_enrichment <- function(spatial_enrichment,
    provenance = NULL,
    cores = determine_cores(),
    verbose = TRUE) {
    # data.table vars
    cell_ID <- NULL

    if (!any(class(unlist(spatial_enrichment)) %in%
        c("data.table", "data.frame", "matrix", "character"))) {
        stop(
            "[spatial enrichment] input needs to be a data.frame-like ",
            "object or a path to one\n", call. = FALSE
        )
    }
    if (inherits(spatial_enrichment, "character")) {
        # assume filepath
        checkmate::assert_file_exists(spatial_enrichment)

        spatial_enrichment <- data.table::fread(
            input = spatial_enrichment,
            nThread = cores
        )
    } else {
        spatial_enrichment <- tryCatch(
            data.table::setDT(spatial_enrichment),
            error = function(e) data.table::as.data.table(spatial_enrichment)
        )
    }

    # check which columns are numeric (contain enrichment info)
    column_classes <- lapply(spatial_enrichment, FUN = class)
    non_numeric_classes <- column_classes[!column_classes %in%
        c("numeric", "integer")]


    potential_cell_IDs <- NULL

    # find non-numeric cols (possible cell_ID col)
    if (length(non_numeric_classes) > 0L) {
        non_numeric_indices <- which(!column_classes %in%
            c("numeric", "integer"))

        .spatlocs_multiple_non_numerics_msg(
            type = "enrichment",
            non_num_cols = non_numeric_indices,
            verbose = verbose
        )

        potential_cell_IDs <- spatial_enrichment[[names(
            non_numeric_classes
        )[[1L]]]]

        # subset to only numeric cols for testing
        spatial_enrichment <- spatial_enrichment[,
            -non_numeric_indices,
            with = FALSE
        ]
    }

    # check number of columns: too few
    if (ncol(spatial_enrichment) < 1L) {
        stop(wrap_txt(errWidth = TRUE,
            "[spatial enrichment] There has to be at least 2 columns:
            1 for cell IDs, and at least one other for enrichment data\n"
        ), call. = FALSE)
    }

    # Assign first non-numeric as cell_ID
    if (!is.null(potential_cell_IDs)) {
        spatial_enrichment[, cell_ID := potential_cell_IDs]
    }

    return(spatial_enrichment)
}







# Embeddings ####




#' @name .evaluate_dimension_reduction
#' @description evaluate dimension reduction input into dimObj matrix
#' @keywords internal
#' @noRd
.evaluate_dimension_reduction <- function(dimension_reduction) {
    # object level
    if (inherits(dimension_reduction, "dimObj")) {
        dimension_reduction[] <- .evaluate_dimension_reduction(
            dimension_reduction[]
        )
        return(dimension_reduction)
    }

    # coordinates slot matrix
    dimension_reduction <- try(as.matrix(dimension_reduction), silent = TRUE)
    if (inherits(dimension_reduction, "try-error")) {
        .gstop("Dimension reduction coordinate input must be coercible to
            matrix")
    }
    return(dimension_reduction)
}








#' @title Evaluate nearest networks
#' @name .evaluate_nearest_networks
#' @description Evaluate nearest networks input into igraph for input into
#' nnNetObj
#' @keywords internal
#' @details Minimal input is a data.frame-like input containing 'from', 'to',
#' and 'distance' information
#' @noRd
.evaluate_nearest_networks <- function(nn_network) {
    # data.table vars
    weight <- distance <- NULL

    if (inherits(nn_network, "nnNetObj")) {
        nn_network[] <- .evaluate_nearest_networks(nn_network = nn_network[])
        return(nn_network)
    } else if (inherits(nn_network, "igraph")) {
        v_attr <- igraph::vertex_attr_names(nn_network)
        e_attr <- igraph::edge_attr_names(nn_network)

        if (!"name" %in% v_attr) {
            .gstop(
                'nearest network igraph input MUST have vertex attribute "name".
        Discovered vertex attributes:', v_attr
            )
        }

        if (!"distance" %in% e_attr) {
            .gstop('nearest network igraph input MUST have edge attribute
                "distance". Discovered edge attributes:', e_attr)
        }

        if (!"weight" %in% e_attr) {
            igDT <- data.table::setDT(igraph::as_data_frame(nn_network))
            igDT[, weight := 1 / (1 + distance)]
            data.table::setcolorder(igDT, neworder = c(
                "from", "to", "weight", "distance"
            ))
            nn_network <- igraph::graph_from_data_frame(igDT)
        }

        return(nn_network)
    } else if (inherits(nn_network, "data.frame")) {
        if (!inherits(nn_network, "data.table")) {
            nn_network <-
                data.table::setDT(nn_network)
        }

        # if minimal input not given, throw error
        if (!all(c("from", "to", "distance") %in% colnames(nn_network))) {
            .gstop("Unable to coerce data.frame type object to nnNetObj igraph
            Needed columns: from, to, distance")
        }

        # generate weights
        nn_network[, weight := 1 / (1 + distance)]
        data.table::setcolorder(nn_network, neworder = c(
            "from", "to", "weight", "distance"
        ))

        nn_network <- igraph::graph_from_data_frame(nn_network)
        return(nn_network)
    }
}







# Spatial/Polygon info ####

#' @describeIn createGiottoPolygonsFromDfr Examines provided data.frame type
#' object for columns that should correspond to x/y vertices and the polygon
#' ID. Returns a data.table with those key columns renamed to 'x', 'y', and
#' 'poly_ID' if necessary.
#' @keywords internal
#' @noRd
.evaluate_gpoly_dfr <- function(input_dt,
    verbose = TRUE) {
    x <- y <- poly_ID <- NULL

    # data.frame like object needs to have 2 coordinate columns and
    # at least one other column as the poly_ID
    if (ncol(input_dt) < 3) {
        stop("At minimum, columns for xy coordinates and poly ID are needed.\n")
    }

    col_classes <- vapply(input_dt, class, FUN.VALUE = character(1L))


    # 1. detect poly_ID
    ## find poly_ID as either first character col or named column
    ## if neither exist, pick the 3rd column
    if ("poly_ID" %in% colnames(input_dt)) {
        poly_ID_col <- which(colnames(input_dt) == "poly_ID")
    } else {
        poly_ID_col <- which(col_classes == "character")
        if (length(poly_ID_col) < 1) {
            poly_ID_col <- 3
        } # case if no char found: default to 3rd
        else {
            poly_ID_col <- poly_ID_col[[1]]
        } # case if char is found
    }


    # 2. detect x and y
    ## find first two numeric cols as x and y respectively or named column
    ## if neither exist, pick the 1st and 2nd cols respectively for x and y
    if (all(c("x", "y") %in% colnames(input_dt))) {
        x_col <- which(colnames(input_dt) == "x")
        y_col <- which(colnames(input_dt) == "y")
    } else {
        x_col <- which(col_classes == "numeric")
        if (length(x_col) < 2) {
            x_col <- 1
        } # case if no/too few num found: default to 1st
        else {
            x_col <- x_col[[1]]
        } # case if num found
        y_col <- which(col_classes == "numeric")
        if (length(y_col) < 2) {
            y_col <- 2
        } # case if no/too few num found: default to 2nd
        else {
            y_col <- y_col[[2]]
        } # case if num found
    }


    # 3. print selections and ensure correct data type
    if (isTRUE(verbose)) {
        message(paste0(
            '  Selecting col "',
            colnames(input_dt[, poly_ID_col, with = FALSE]),
            '" as poly_ID column'
        ))
    }
    colnames(input_dt)[poly_ID_col] <- "poly_ID"
    if (!input_dt[, inherits(poly_ID, "character")]) {
        input_dt[, poly_ID := as.character(poly_ID)]
    }

    if (isTRUE(verbose)) {
        message(paste0(
            '  Selecting cols "',
            colnames(input_dt[, x_col, with = FALSE]), '" and "',
            colnames(input_dt[, y_col, with = FALSE]),
            '" as x and y respectively'
        ))
    }
    colnames(input_dt)[x_col] <- "x"
    colnames(input_dt)[y_col] <- "y"
    if (!input_dt[, inherits(x, "numeric")]) input_dt[, x := as.numeric(x)]
    if (!input_dt[, inherits(y, "numeric")]) input_dt[, y := as.numeric(y)]

    input_dt
}







#' @keywords internal
#' @param input_sv SpatVector to evaluate
#' @param verbose be verbose
#' @return list of SpatVector and unique_IDs
#' @noRd
.evaluate_gpoly_spatvector <- function(input_sv,
    make_valid = FALSE,
    verbose = TRUE) {
    # determine sv type
    sv_type <- terra::geomtype(input_sv)

    if (sv_type != "polygons") {
        stop('SpatVector is of type "', sv_type, '" instead of "polygons"')
    }

    # 0. process spatvector
    # strip crs info
    terra::set.crs(input_sv, NULL)
    # ensure valid
    if (make_valid) input_sv <- terra::makeValid(input_sv)

    col_classes <- vapply(
        sample(x = input_sv, size = 1L),
        FUN = class,
        FUN.VALUE = character(1L)
    )

    # 1. detect poly_ID
    ## find poly_ID as either first character col or named column
    ## if neither exist, pick the 1st column
    sv_names <- names(input_sv)
    if ("poly_ID" %in% sv_names) {
        poly_ID_col <- which(sv_names == "poly_ID")
    } else if (ncol(input_sv) == 0L) {
        poly_ID_col <- 1L
        input_sv$poly_ID <- as.character(seq_len(nrow(input_sv)))
        col_classes[[1]] <- "character"
    } else {
        poly_ID_col <- which(col_classes == "character")
        if (length(poly_ID_col) < 1L) {
            poly_ID_col <- 1L
        } # case if no char found: default to 1st
        else {
            poly_ID_col <- poly_ID_col[[1L]]
        } # case if char is found
    }


    # 2. print selections and ensure correct data type
    if (isTRUE(verbose)) {
        wrap_msg('  Selecting attribute "', names(input_sv[[poly_ID_col]]),
            '" as poly_ID',
            sep = ""
        )
    }
    sv_names[[poly_ID_col]] <- "poly_ID"
    terra::set.names(input_sv, sv_names)
    unique_names <- make.unique(terra::values(input_sv)[[poly_ID_col]])
    # only select as many names as there are poly geometries.
    # With `makeValid()`, if a polygon is lost due to the process, the
    # attributes table length ends up being longer than the number of geoms.
    input_sv[[poly_ID_col]] <- unique_names[seq_len(nrow(input_sv))]

    unique_IDs <- NULL
    if (col_classes[[poly_ID_col]] != "character") {
        poly_ID_vals <- terra::as.list(input_sv)$poly_ID
        poly_ID_vals <- as.character(poly_ID_vals)

        input_sv$poly_ID <- poly_ID_vals
        unique_IDs <- unique(poly_ID_vals)
    }

    return_list <- list(
        spatvector = input_sv,
        unique_IDs = unique_IDs
    )

    return_list
}








#' @title Evaluate spatial info
#' @name .evaluate_spatial_info
#' @description Evaluate spatial information input into a SpatVector for
#' giottoPolygon creation
#' @param spatial_info spatial information to evaluate
#' @param part_col character (optional). If provided, a column in the data
#' when processing will be indexed along as parts to generate a multipolygon.
#' @param skip_eval_dfr (default FALSE) skip evaluation of data.frame like input
#' @param make_valid logical. Whether to run `terra::makeValid()`
#' @param copy_dt (default TRUE) if segmdfr is provided as dt, this determines
#' whether a copy is made
#' @param cores how many cores to use
#' @param verbose be verbose
#' @return list of SpatVector and unique polygon IDs that it contains
#' @keywords internal
#' @noRd
.evaluate_spatial_info <- function(spatial_info,
    part_col = NULL,
    skip_eval_dfr = FALSE,
    copy_dt = TRUE,
    make_valid = FALSE,
    cores = determine_cores(),
    verbose = TRUE) {
    # NSE vars
    geom <- poly_ID <- part <- hole <- NULL

    ## 1. load or read spatial information data ##
    ## 1.1 read from file
    if (inherits(spatial_info, "character")) {
        spatial_info <- path.expand(spatial_info)
        if (!file.exists(spatial_info)) {
            stop("path to spatial information does not exist", call. = FALSE)
        }

        if (tolower(file_extension(spatial_info)) %in% c("geojson", "json")) {
            spatial_info <- .json_try_read_poly(spatial_info) # to spatvector
            spatial_info <- .evaluate_gpoly_spatvector(
                spatial_info,
                make_valid = make_valid, verbose = verbose
            )
            return(spatial_info)
        } else if (tolower(file_extension(spatial_info)) %in% c("shp", "wkt")) {
            spatial_info <- terra::vect(spatial_info)
            spatial_info <- .evaluate_gpoly_spatvector(
                spatial_info,
                make_valid = make_valid, verbose = verbose
            )
            return(spatial_info)
        } else {
            spatial_info <- data.table::fread(
                input = spatial_info, nThread = cores
            )
        }


        ## 1.2 data.frame-like input
    } else if (inherits(spatial_info, "data.table")) {
        if (isTRUE(copy_dt)) spatial_info <- data.table::copy(spatial_info)
    } else if (inherits(spatial_info, "data.frame")) {
        spatial_info <- data.table::setDT(spatial_info)

        ## 1.3 SpatVector input
    } else if (inherits(spatial_info, "SpatVector")) {
        spatial_info <- .evaluate_gpoly_spatvector(
            spatial_info,
            make_valid = make_valid, verbose = verbose
        )
        return(spatial_info)

        ## 1.4 Other inputs
    } else {
        spatial_info <- try(data.table::as.data.table(
            spatial_info
        ), silent = TRUE)
        if (inherits(spatial_info, "try-error")) {
            stop(
                "If spatial information is provided then it needs to be a",
                "file path or a data.frame-like object",
                call. = FALSE
            )
        }
    }


    # 2. data.frame info evaluation
    if (!isTRUE(skip_eval_dfr)) {
        spatial_info <- .evaluate_gpoly_dfr(
            input_dt = spatial_info,
            verbose = verbose
        )
    }


    # 3. add other necessary cols for the input data.table
    nr_of_cells_vec <- seq_along(unique(spatial_info$poly_ID))
    names(nr_of_cells_vec) <- unique(spatial_info$poly_ID)
    new_vec <- nr_of_cells_vec[as.character(spatial_info$poly_ID)]
    spatial_info[, geom := new_vec]

    if (!"part" %in% colnames(spatial_info)) {
        if (!is.null(part_col)) {
            part_idx <- seq_along(unique(spatial_info[[part_col]]))
            names(part_idx) <- unique(spatial_info[[part_col]])
            part_vec <- part_idx[as.character(spatial_info[[part_col]])]
            spatial_info[, part := part_vec]
            spatial_info[, part := part - min(part) + 1, by = geom]
            # drop the part col since it is incompatible with attributes
            spatial_info[, (part_col) := NULL]
        } else {
            spatial_info[, part := 1]
        }
    }
    if (!"hole" %in% colnames(spatial_info)) {
        spatial_info[, hole := 0]
    }

    data.table::setcolorder(spatial_info,
        c("geom", "part", "x", "y", "hole", "poly_ID")
    )

    # get unique IDs
    unique_IDs <- spatial_info[, unique(poly_ID)]

    # 4. create spatvector
    spatial_info <- .dt_to_spatvector_polygon(spatial_info,
        include_values = TRUE
    )

    return_list <- list(
        spatvector = spatial_info,
        unique_IDs = unique_IDs
    )

    return(return_list)
}









# Feature info ####

#' @title Evaluate feature info
#' @name .evaluate_feat_info
#' @description Evaluate spatial feature information input
#' @param spatial_feat_info spatial feature information to evaluate
#' @param cores how many cores to use
#' @param feat_ID feature IDs to check with
#' @return data.table
#' @keywords internal
#' @noRd
.evaluate_feat_info <- function(spatial_feat_info,
    feat_type,
    cores = determine_cores(),
    feat_ID) {
    ## 1. load or read spatial information data ##
    if (inherits(spatial_feat_info, "character")) {
        if (!file.exists(spatial_feat_info)) {
            .gstop("path to spatial information does not exist")
        }
        spatial_feat_info <- data.table::fread(
            input = spatial_feat_info, nThread = cores
        )
    } else if (inherits(spatial_feat_info, "data.frame")) {
        spatial_feat_info <- data.table::as.data.table(spatial_feat_info)
    } else {
        .gstop("If spatial feature information is provided then it needs to
            be a file path or a data.frame-like object")
    }


    ## 2. check and name columns ##
    nr_cols <- ncol(spatial_feat_info)

    if (nr_cols < 3) {
        .gstop(
            "Spatial feature information needs to have at least 3 columns: \n",
            "x, y, (z) information columns \n",
            "and feature ID column \n"
        )
    }

    column_classes <- lapply(spatial_feat_info, FUN = class)


    # 3D or 2D data
    if (all(column_classes[seq_len(3)] == "numeric")) {
        colnames(spatial_feat_info)[seq_len(4)] <- c(
            "sdimx", "sdimy", "sdimz", "feat_ID"
        )
    } else if (all(column_classes[seq_len(2)] == "numeric")) {
        colnames(spatial_feat_info)[seq_len(3)] <- c(
            "sdimx", "sdimy", "feat_ID"
        )
    } else {
        .gstop("First 3 or 2 columns need to be numeric for 3D and 2D data
            respectively")
    }



    ## 3. check cell ID ##

    spatial_feature_info_feat_IDs <- spatial_feat_info[["feat_ID"]]

    if (all(spatial_feature_info_feat_IDs %in% feat_ID)) {
        return(spatial_feat_info)
    } else {
        .gstop("feat IDs in spatial feature information are missing in the
            feature ID slot")
    }
}




# json poly reading ####

.json_try_read_poly <- function(x) {
    errors <- list()
    res <- tryCatch(.json_read_poly_custom(x), error = function(e) {
        errors$custom <<- e$message
    })
    if (!inherits(res, "character")) {
        return(res)
    }

    res <- tryCatch(terra::vect(x), error = function(e) {
        errors$terra <<- e$message
    })
    if (!inherits(res, "character")) {
        return(res)
    }

    stop(wrap_txtf(
        "json readers failed.\ncustom: %s\nterra: %s",
        errors$custom,
        errors$terra
    ), call. = FALSE)
}

.json_read_poly_custom <- function(x) {
    json_list <- GiottoUtils::read_json(x)
    type <- json_list$type

    switch(tolower(type),
        "featurecollection" = .json_read_poly_feat_collection(json_list),
        "geometrycollection" = .json_read_poly_geom_collection(json_list)
    )
}



.json_read_poly_feat_collection <- function(x) {
    vmsg(.is_debug = TRUE, "Reading FeatureCollection")
    checkmate::assert_list(x)
    p <- x$features
    npoly <- length(p)

    # SpatVector
    mat <- lapply(seq_along(p), function(poly_i) {
        coordslist <- p[[poly_i]]$geometry$coordinates
        .json_poly_coordslist_to_geommat(coordslist, poly_i)
    }) |> do.call(what = rbind)
    sv <- terra::vect(mat, type = "polygon")

    # fields/attributes
    fields <- .json_extract_fields(p)
    if (nrow(fields) > 0L) terra::values(sv) <- fields
    # return
    sv
}

.json_read_poly_geom_collection <- function(x) {
    vmsg(.is_debug = TRUE, "Reading GeometryCollection")
    checkmate::assert_list(x)
    p <- x$geometries
    npoly <- length(p)

    # SpatVector
    mat <- lapply(seq_len(npoly), function(poly_i) {
        coordslist <- p[[poly_i]]$coordinates
        .json_poly_coordslist_to_geommat(coordslist, poly_i)
    }) |>
        do.call(what = rbind)
    sv <- terra::vect(mat, type = "polygon")

    # fields/attributes
    fields <- .json_extract_fields(p)
    if (nrow(fields) > 0L) terra::values(sv) <- fields
    # return
    sv
}

.json_poly_coordslist_to_geommat <- function(x, idx) {
    coords <- unlist(x)
    nvtx <- length(coords) / 2 # div 2 since these are pairs
    matrix(
        c(
            rep(idx, nvtx), # geom
            rep(1L, nvtx), # part
            coords[c(TRUE, FALSE)], # x
            coords[c(FALSE, TRUE)], # y
            rep(0L, nvtx) # hole
        ),
        nrow = nvtx,
        ncol = 5,
        byrow = FALSE
    )
}

.json_extract_fields <- function(features) {
    nfeat <- length(features)

    # Skip these GeoJSON structural fields
    skip_fields <- c("coordinates", "type", "bbox")

    # Initialize results list
    # fields vectors will be accumulated here
    all_fields <- list()

    feat_i <- function(i) {
        features[[i]]
    }

    # Recursive function to process nested fields
    process_field <- function(feat_fun, field_name) {
        # get feature data from previous feature function
        field_data <- feat_fun(1L) # detect from first feature

        # If it's a list/nested structure
        if (is.list(field_data)) {
            subfield_names <- names(field_data)
            subfield_names <- subfield_names[!subfield_names %in% skip_fields]
            # ignore any non-named lists/nesting
            if (length(subfield_names) == 0L) {
                return(NULL)
            }

            # otherwise, recurse across subnesting
            for (subfield in subfield_names) {
                # Create the full field path
                full_field_name <- paste(
                    # `%` is used as an uncommon separator for possible cleanup
                    c(field_name, subfield),
                    collapse = "%"
                )
                subfeat_fun <- function(i) {
                    feat_fun(i)[[subfield]]
                }
                process_field(subfeat_fun, full_field_name)
            }
        } else {
            # evaluate actual field values
            # skip if length > 1 (like array data)
            if (length(field_data) > 1L) {
                return(NULL)
            }

            # base case: simple field
            # return across all features
            all_fields[[field_name]] <<- lapply(seq_len(nfeat), function(f_i) {
                feat_fun(f_i)
            }) |>
                unlist()
        }
    }

    process_field(feat_fun = feat_i, field_name = NULL)

    # Convert to data frame
    result_df <- do.call(data.frame, all_fields)
    names(result_df) <- .json_extract_fields_prune_names(names(all_fields))
    return(result_df)
}

.json_extract_fields_prune_names <- function(x) {
    if (length(x) == 0L) {
        return(x)
    } # skip if none
    checkmate::assert_character(x)
    shortnames <- vapply(
        strsplit(x, "%"), tail,
        n = 1L, FUN.VALUE = character(1L)
    )
    for (i in seq_along(x)) {
        if (sum(duplicated(c(shortnames[[i]], x))) > 0) next
        x[[i]] <- shortnames[[i]]
    }

    # final cleanup
    gsub("%", ".", x)
}

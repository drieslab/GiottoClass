# ------------------------------------------------------------------------- #
# This script contains reusable dummy documentation / templates for
# commonly used params.
#
# Use the @inheritParams tag to use these templates in documentation
# ------------------------------------------------------------------------- #

# No @title to prevent .Rd generation
# No @noRd tags can be used on these dummy documentations, otherwise
# @inheritParams
# cannot be used

# Note that these dummy documentations WILL be flagged with warnings when
# building
# the documentation, but this should be fine.








# Data Access ####


#' data_access_params
#'
#' @name data_access_params
#' @param gobject giotto object
#' @param spat_unit spatial unit (e.g. "cell")
#' @param feat_type feature type (e.g. "rna", "dna", "protein")
#' @param return_uniques return unique nesting names (ignores if final object exists/is correct class)
#' @param output what format in which to get information (e.g. "data.table")
#' @param set_defaults set default spat_unit and feat_type. Change to FALSE only when
#' expression and spat_info are not expected to exist.
#' @param copy_obj whether to deep copy/duplicate when getting the object (default = TRUE)
#' @param initialize (default = FALSE) whether to initialize the gobject before
#' returning
#' @keywords internal
#' @returns data.table
NULL




# Read Functions ####

#' read_data_params
#' @name read_data_params
#' @param data_list (nested) list of input data to read
#' @param default_spat_unit (optional) default spat_unit to use
#' @param default_feat_type (optional) default feat_type to use
#' @param provenance (optional) provenance information
#' @param verbose be verbose
#' @returns data params
#' @keywords internal
NULL

#' @include generics.R
#' @include instructions.R
NULL

# docs ----------------------------------------------------------- #
#' @title Access giotto instructions
#' @name instructions-generic
#' @aliases instructions instructions<-
#' @description Retrieve or set giotto instructions. Specific instructions can
#' be replaced using the \code{field} param. Additionally, when using
#' instructions<-, \code{initialize()} will be called on the giotto object if
#' initialize param is TRUE
#' @inheritParams data_access_params
#' @param param Specific param in instructions to access or modify
#' @param initialize (boolean, default = TRUE) whether to initialize the giotto
#' object
#' @param value value to set
#' @returns giotto instructions
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' showGiottoInstructions(g)
NULL

#' @title Active spatial unit
#' @name activeSpatUnit-generic
#' @aliases activeSpatUnit activeSpatUnit<-
#' @description Retrieve or set the active spatial unit. This value will be the
#' default spatial unit that the giotto object uses.
#' @inheritParams data_access_params
#' @param value spat_unit to set as default
#' @returns active spatial unit
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' activeSpatUnit(g)
NULL

#' @title Active feature type
#' @name activeFeatType-generic
#' @aliases activeFeatType activeFeatType<-
#' @description Retrieve or set the active feature type. This value will be the
#' default feature type that the giotto object uses.
#' @inheritParams data_access_params
#' @param value feat_type to set as default
#' @returns active feature type
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#' activeFeatType(g)
NULL

# ---------------------------------------------------------------- #


# instructions() method ####

# Get instructions object
#' @rdname instructions-generic
#' @export
setMethod(
    "instructions", signature(gobject = "giotto", param = "missing"),
    function(gobject) {
        return(showGiottoInstructions(gobject))
    }
)

# Set instructions object
#' @rdname instructions-generic
#' @export
setMethod(
    "instructions<-",
    signature(
        gobject = "giotto",
        param = "missing", initialize = "missing", value = "ANY"
    ),
    function(gobject, initialize, value) {
        gobject <- replaceGiottoInstructions(gobject,
            instructions = value,
            init_gobject = TRUE
        )
        return(gobject)
    }
)
#' @rdname instructions-generic
#' @export
setMethod(
    "instructions<-",
    signature(
        gobject = "giotto",
        param = "missing", initialize = "logical", value = "ANY"
    ),
    function(gobject, initialize, value) {
        gobject <- replaceGiottoInstructions(gobject,
            instructions = value,
            init_gobject = initialize
        )
        return(gobject)
    }
)

# Get specific field
#' @rdname instructions-generic
#' @export
setMethod(
    "instructions", signature(gobject = "giotto", param = "character"),
    function(gobject, param) {
        instrs <- showGiottoInstructions(gobject = gobject)
        return(readGiottoInstructions(
            giotto_instructions = instrs,
            param = param
        ))
    }
)

# Set specific field
#' @rdname instructions-generic
#' @export
setMethod(
    "instructions<-",
    signature(
        gobject = "giotto",
        param = "character", initialize = "missing", value = "ANY"
    ),
    function(gobject, param, initialize, value) {
        gobject <- changeGiottoInstructions(
            gobject = gobject,
            params = param,
            new_values = value,
            return_gobject = TRUE,
            init_gobject = TRUE
        )
        return(gobject)
    }
)
#' @rdname instructions-generic
#' @export
setMethod(
    "instructions<-",
    signature(
        gobject = "giotto",
        param = "character", initialize = "logical", value = "ANY"
    ),
    function(gobject, param, initialize, value) {
        gobject <- changeGiottoInstructions(
            gobject = gobject,
            params = param,
            new_values = value,
            return_gobject = TRUE,
            init_gobject = initialize
        )
        return(gobject)
    }
)









# Default Settings ####
## activeSpatUnit ####
#' @rdname activeSpatUnit-generic
#' @export
setMethod("activeSpatUnit", signature(gobject = "giotto"), function(gobject) {
    su_try <- try(instructions(gobject, "active_spat_unit"), silent = TRUE)
    if (inherits(su_try, "try-error")) su_try <- NULL
    return(su_try)
})


#' @rdname activeSpatUnit-generic
#' @export
setMethod(
    "activeSpatUnit<-", signature(gobject = "giotto", value = "character"),
    function(gobject, value) {
        instructions(gobject, "active_spat_unit") <- value
        return(gobject)
    }
)


## activeFeatType ####
setGeneric(
    "activeFeatType",
    function(gobject, ...) standardGeneric("activeFeatType")
)
setGeneric(
    "activeFeatType<-",
    function(gobject, ..., value) standardGeneric("activeFeatType<-")
)

#' @rdname activeFeatType-generic
#' @export
setMethod("activeFeatType", signature(gobject = "giotto"), function(gobject) {
    ft_try <- try(instructions(gobject, "active_feat_type"), silent = TRUE)
    if (inherits(ft_try, "try-error")) ft_try <- NULL
    return(ft_try)
})


#' @rdname activeFeatType-generic
#' @export
setMethod(
    "activeFeatType<-", signature(gobject = "giotto", value = "character"),
    function(gobject, value) {
        instructions(gobject, "active_feat_type") <- value
        return(gobject)
    }
)

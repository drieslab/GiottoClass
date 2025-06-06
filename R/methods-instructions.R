#' @include generics.R
#' @include instructions.R
NULL

# docs ----------------------------------------------------------- #
#' @title Giotto instructions
#' @name giotto_instructions
#' @aliases instructions instructions<-
#' @description
#' Giotto instructions are default settings that are applied at the `giotto`
#' object level. Once added to an object, they affect the way that the object
#' behaves. You can create a `giottoInstructions` object using
#' `createGiottoInstructions()` and add them to the `giotto` object during
#' creation or using the `instructions()` generic. Specific settings can be
#' replaced or retrieved using the `param` argument. Additionally, when using
#' `instructions<-()` as a replacement function, `initialize()` will be called
#' on the `giotto` object if `initialize = TRUE`.
#'
#' If no `giottoInstructions` object is provided during `giotto` object
#' creation, then a default one will be created during `giotto` object
#' initialization.
#'
#' @inheritParams data_access_params
#' @param param Specific param in instructions to access or modify
#' @param initialize (boolean, default = TRUE) whether to initialize the giotto
#' object
#' @param value value to set
#' @param \dots params to pass to `createGiottoInstructions()`
#' @returns `giottoInstructions`, instructions settings, or `giotto` objects
#' with modified instructions
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' # create instructions
#' ins <- instructions()
#'
#' # get instructions
#' instrs <- instructions(g)
#' force(instrs)
#'
#' # get single instructions param
#' instructions(g, "show_plot")
#'
#' # replace an instruction param
#' instructions(g, "show_plot") <- FALSE
#' instructions(g, "show_plot")
#'
#' # replace multiple instruction params
#' instructions(g)
#' instructions(g, c("show_plot", "dpi")) <- list(TRUE, 600)
#' instructions(g)
#'
#' # replace instructions
#' i <- createGiottoInstructions()
#' instructions(g) <- i
#' instructions(g)
#'
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

# create instructions object
#' @rdname giotto_instructions
#' @export
setMethod(
    "instructions", signature(gobject = "missing", param = "missing"),
    function(...) createGiottoInstructions(...)
)


# Get instructions object
#' @rdname giotto_instructions
#' @export
setMethod(
    "instructions", signature(gobject = "giotto", param = "missing"),
    function(gobject) {
        return(showGiottoInstructions(gobject))
    }
)


# Get specific field
#' @rdname giotto_instructions
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

#' @rdname giotto_instructions
#' @export
setMethod(
    "instructions",
    signature(gobject = "giottoInstructions", param = "character"),
    function(gobject, param) gobject[[param]]
)


# Set instructions object
#' @rdname giotto_instructions
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
#' @rdname giotto_instructions
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


# Set specific field
#' @rdname giotto_instructions
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
#' @rdname giotto_instructions
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
#' @rdname giotto_instructions
#' @export
setMethod(
    "instructions<-",
    signature(
        gobject = "giottoInstructions", param = "character", value = "ANY"
    ),
    function(gobject, param, value) {
        gobject[[param]] <- value
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

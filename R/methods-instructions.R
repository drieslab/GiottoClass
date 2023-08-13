
#' @include generics.R

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
#' @param initialize (boolean, default = TRUE) whether to initialize the giotto object
#' @param value value to set
NULL

#' @title Active spatial unit
#' @name activeSpatUnit-generic
#' @aliases activeSpatUnit activeSpatUnit<-
#' @description Retrieve or set the active spatial unit. This value will be the
#' default spatial unit that the giotto object uses.
#' @inheritParams data_access_params
NULL

#' @title Active feature type
#' @name activeFeatType-generic
#' @aliases activeFeatType activeFeatType<-
#' @description Retrieve or set the active feature type. This value will be the
#' default feature type that the giotto object uses.
#' @inheritParams data_access_params
NULL

# ---------------------------------------------------------------- #


# instructions() method ####

# Get instructions object
#' @rdname instructions-generic
#' @export
setMethod('instructions', signature(gobject = 'giotto', param = 'missing'),
          function(gobject) {
            return(showGiottoInstructions(gobject))
          })

# Set instructions object
#' @rdname instructions-generic
#' @export
setMethod('instructions<-',
          signature(gobject = 'giotto', param = 'missing', initialize = 'missing', value = 'ANY'),
          function(gobject, initialize, value) {
            gobject = replaceGiottoInstructions(gobject,
                                                instructions = value,
                                                init_gobject = TRUE)
            return(gobject)
          })
#' @rdname instructions-generic
#' @export
setMethod('instructions<-',
          signature(gobject = 'giotto', param = 'missing', initialize = 'logical', value = 'ANY'),
          function(gobject, initialize, value) {
            gobject = replaceGiottoInstructions(gobject,
                                                instructions = value,
                                                init_gobject = initialize)
            return(gobject)
          })

# Get specific field
#' @rdname instructions-generic
#' @export
setMethod('instructions', signature(gobject = 'giotto', param = 'character'),
          function(gobject, param) {
            instrs = showGiottoInstructions(gobject = gobject)
            return(readGiottoInstructions(giotto_instructions = instrs, param = param))
          })

# Set specific field
#' @rdname instructions-generic
#' @export
setMethod('instructions<-',
          signature(gobject = 'giotto', param = 'character', initialize = 'missing', value = 'ANY'),
          function(gobject, param, initialize, value) {
            gobject = changeGiottoInstructions(gobject = gobject,
                                               params = param,
                                               new_values = value,
                                               return_gobject = TRUE,
                                               init_gobject = TRUE)
            return(gobject)
          })
#' @rdname instructions-generic
#' @export
setMethod('instructions<-',
          signature(gobject = 'giotto', param = 'character', initialize = 'logical', value = 'ANY'),
          function(gobject, param, initialize, value) {
            gobject = changeGiottoInstructions(gobject = gobject,
                                               params = param,
                                               new_values = value,
                                               return_gobject = TRUE,
                                               init_gobject = initialize)
            return(gobject)
          })







# Giotto instructions ####


#' @title Create instructions for giotto functions
#' @name createGiottoInstructions
#' @description Function to set global instructions for giotto functions
#' @param python_path path to python binary to use
#' @param show_plot print plot to console, default = TRUE
#' @param return_plot return plot as object, default = TRUE
#' @param save_plot automatically save plot, dafault = FALSE
#' @param save_dir path to directory where to save plots
#' @param plot_format format of plots (defaults to png)
#' @param dpi resolution for raster images
#' @param units units of format (defaults to in)
#' @param height height of plots
#' @param width width of  plots
#' @param is_docker using docker implementation of Giotto (defaults to FALSE)
#' @param plot_count [global option] start count for creating automatic unique plots
#' @param fiji_path path to fiji executable
#' @param no_python_warn turn off warning that no compatible python env has been detected
#' @return named vector with giotto instructions
#' @seealso More online information can be found here \url{https://rubd.github.io/Giotto_site/articles/instructions_and_plotting.html}
#' @export
createGiottoInstructions <- function(python_path =  NULL,
                                     show_plot = NULL,
                                     return_plot = NULL,
                                     save_plot = NULL,
                                     save_dir = NULL,
                                     plot_format = NULL,
                                     dpi = NULL,
                                     units = NULL,
                                     height = NULL,
                                     width = NULL,
                                     is_docker = FALSE,
                                     plot_count = 0,
                                     fiji_path = NULL,
                                     no_python_warn = FALSE) {

  # python path to use
  # try used here to allow instructions to be made in the absence of a compatible
  # python env
  python_path = try(
    if(is_docker){
      set_giotto_python_path(python_path = "/usr/bin/python3") # fixed path in docker version
    }  else{
      set_giotto_python_path(python_path = python_path)
    },
    silent = TRUE
  )

  if ((is.null(python_path) | inherits(python_path, 'try-error')) & !no_python_warn) {
    warning(wrap_txt("Python is required for full Giotto functionality."))
  }

  # print plot to console
  if(is.null(show_plot)) {
    show_plot = TRUE
  }

  # print plot to console
  if(is.null(return_plot)) {
    return_plot = TRUE
  }

  # print plot to console
  if(is.null(save_plot)) {
    save_plot = FALSE
  }

  # directory to save results to
  if(is.null(save_dir)) {
    save_dir = getwd()
  }
  save_dir = as.character(save_dir)

  # plot format
  if(is.null(plot_format)) {
    plot_format = "png"
  }
  plot_format = as.character(plot_format)

  # dpi of raster images
  if(is.null(dpi)) {
    dpi = 300
  }
  dpi = as.numeric(dpi)

  # units for height and width
  if(is.null(units)) {
    units = 'in'
  }
  units = as.character(units)

  # height of plot
  if(is.null(height)) {
    height = 9
  }
  height = as.numeric(height)

  # width of plot
  if(is.null(width)) {
    width = 9
  }
  width = as.numeric(width)


  ## global options ##
  # ---------------- #

  # plot count
  options('giotto.plot_count' = plot_count)

  # fiji path
  options('giotto.fiji' = fiji_path)


  # return instructions list

  instructions_list = create_giotto_instructions(
    python_path = python_path,
    show_plot = show_plot,
    return_plot = return_plot,
    save_plot = save_plot,
    save_dir = save_dir,
    plot_format = plot_format,
    dpi = dpi,
    units = units,
    height = height,
    width = width,
    is_docker = is_docker
  )

  return(instructions_list)

}


#' @keywords internal
create_giotto_instructions = function(python_path = NULL,
                                      show_plot = NULL,
                                      return_plot = NULL,
                                      save_plot = NULL,
                                      save_dir = NULL,
                                      plot_format = NULL,
                                      dpi = NULL,
                                      units = NULL,
                                      height = NULL,
                                      width = NULL,
                                      is_docker = NULL) {
  instructions_list = list(python_path = python_path,
                           show_plot = show_plot,
                           return_plot = return_plot,
                           save_plot = save_plot,
                           save_dir = save_dir,
                           plot_format = plot_format,
                           dpi = dpi,
                           units = units,
                           height = height,
                           width = width,
                           is_docker = is_docker)
  class(instructions_list) = c('giottoInstructions', 'list')
  return(instructions_list)
}


#' @title Read giotto instructions associated with giotto object
#' @name readGiottoInstructions
#' @description Retrieves the instruction associated with the provided parameter
#' @param giotto_instructions giotto object or result from createGiottoInstructions()
#' @param param parameter to retrieve
#' @return specific parameter
#' @export
readGiottoInstructions <- function(giotto_instructions,
                                   param = NULL) {

  # get instructions if provided the giotto object
  if(inherits(giotto_instructions, 'giotto')) {
    giotto_instructions = giotto_instructions@instructions
  }

  # stop if parameter is not found
  if(is.null(param)) {
    stop('\t readGiottoInstructions needs a parameter to work \t')
  } else if(!param %in% names(giotto_instructions)) {
    stop('\t parameter ', param, ' is not in Giotto instructions \t')
  } else {
    specific_instruction = giotto_instructions[[param]]
  }
  return(specific_instruction)
}


#' @title Show giotto instructions associated with giotto object
#' @name showGiottoInstructions
#' @description Function to display all instructions from giotto object
#' @param gobject giotto object
#' @return named vector with giotto instructions
#' @export
showGiottoInstructions = function(gobject) {

  instrs = gobject@instructions
  return(instrs)
}


#' @title Change giotto instruction(s) associated with giotto object
#' @name changeGiottoInstructions
#' @description Function to change one or more instructions from giotto object.
#' If more than one item is supplied to \code{params} and \code{new_values}, use
#' a vector of values. Does not call \code{initialize} on the giotto object
#' @param gobject giotto object
#' @param params parameter(s) to change
#' @param new_values new value(s) for parameter(s)
#' @param return_gobject (boolean, default = TRUE) return giotto object
#' @param init_gobject (boolean, default = TRUE) initialize gobject if returning
#' @return giotto object with one or more changed instructions
#' @export
changeGiottoInstructions = function(gobject,
                                    params = NULL,
                                    new_values = NULL,
                                    return_gobject = TRUE,
                                    init_gobject = TRUE) {

  instrs = gobject@instructions

  if(is.null(params) | is.null(new_values)) {
    stop('\t params and new_values can not be NULL \t')
  }

  if(length(params) != length(new_values)) {
    stop('\t length of params need to be the same as new values \t')
  }

  # if(!all(params %in% names(instrs))) {
  #   stop('\t all params need to be part of Giotto instructions \t')
  # }

  ## swap with new values
  instrs[params] = new_values

  ## make sure that classes remain consistent
  new_instrs = lapply(1:length(instrs), function(x) {

    if(names(instrs[x]) %in% c('dpi', 'height', 'width')) {
      instrs[[x]] = as.numeric(instrs[[x]])
    } else if(names(instrs[x]) %in% c('show_plot', 'return_plot', 'save_plot', 'is_docker')) {
      instrs[[x]] = as.logical(instrs[[x]])
    } else if(names(instrs[x]) %in% c('active_spat_unit', 'active_feat_type', 'plot_format', 'units')) {
      instrs[[x]] = as.character(instrs[[x]])
    } else {
      instrs[[x]] = instrs[[x]]
    }

  })

  names(new_instrs) = names(instrs)



  if(isTRUE(return_gobject)) {
    gobject@instructions = new_instrs
    if(isTRUE(init_gobject)) gobject = initialize(gobject)
    return(gobject)
  } else {
    return(new_instrs)
  }

}



#' @title Replace all giotto instructions in giotto object
#' @name replaceGiottoInstructions
#' @description Function to replace all instructions from giotto object. Does
#' not call \code{initialize} on the giotto object
#' @param gobject giotto object
#' @param instructions new instructions (e.g. result from createGiottoInstructions)
#' @param init_gobject (boolean, default = TRUE) initialize gobject when returning
#' @return giotto object with replaces instructions
#' @export
replaceGiottoInstructions = function(gobject,
                                     instructions = NULL,
                                     init_gobject = TRUE) {

  instrs_needed = names(create_giotto_instructions())

  # validate new instructions
  if(!all(instrs_needed %in% names(instructions)) | is.null(instructions)) {
    stop(wrap_txt('You need to provide a named list for all instructions,',
                  'like the outcome of createGiottoInstructions',
                  errWidth = TRUE))
  } else {
    gobject@instructions = instructions
    if(isTRUE(init_gobject)) gobject = initialize(gobject)
    return(gobject)
  }

}





# Default Settings ####
## activeSpatUnit ####
#' @rdname activeSpatUnit-generic
#' @export
setMethod('activeSpatUnit', signature(gobject = 'giotto'), function(gobject) {
  su_try = try(instructions(gobject, 'active_spat_unit'), silent = TRUE)
  if(inherits(su_try, 'try-error')) su_try = NULL
  return(su_try)
})


#' @rdname activeSpatUnit-generic
#' @export
setMethod('activeSpatUnit<-', signature(gobject = 'giotto', value = 'character'),
          function(gobject, value) {
            instructions(gobject, 'active_spat_unit') = value
            return(gobject)
          })


## activeFeatType ####
setGeneric('activeFeatType', function(gobject, ...) standardGeneric('activeFeatType'))
setGeneric('activeFeatType<-', function(gobject, ..., value) standardGeneric('activeFeatType<-'))

#' @rdname activeFeatType-generic
#' @export
setMethod('activeFeatType', signature(gobject = 'giotto'), function(gobject) {
  ft_try = try(instructions(gobject, 'active_feat_type'), silent = TRUE)
  if(inherits(ft_try, 'try-error')) ft_try = NULL
  return(ft_try)
})


#' @rdname activeFeatType-generic
#' @export
setMethod('activeFeatType<-', signature(gobject = 'giotto', value = 'character'),
          function(gobject, value) {
            instructions(gobject, 'active_feat_type') = value
            return(gobject)
          })






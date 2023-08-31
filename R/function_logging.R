#### logging of giotto functions ####


# Determine the name of the function n levels above the current evaluation frame,
# where n is toplevel - 1
#' @name get_prev_fname
#' @title Get previous function name
#' @param toplevel integer. Relative stack where the previous function call was made
#' @param verbose be verbose
#' @export
get_prev_fname = function(toplevel = 3L, verbose = FALSE) {
  as.character(sys.call(-toplevel)[[1]])
}





#' @title Log args used
#' @name get_args
#' @description
#' Get the arguments that were passed for a function call.
#' @param toplevel integer. Relative stack where the function call was made.
#' @param verbose be verbose
#' @export
get_args <- function(toplevel = 2L, verbose = FALSE) {

  nframes = sys.nframe()

  if(isTRUE(verbose)) {
    cat('\n number of frames: ')
    print(nframes)
    cat('\n')
  }


  cl = sys.call(-toplevel)

  if(isTRUE(verbose)) {
    cat('\n system call: ')
    print(cl)
    cat('\n')
  }


  # function name
  fname = as.character(cl[[1]])

  if(length(fname) > 1) {
    fname = fname[[3]]
  }

  if(isTRUE(verbose)) {
    cat('\n function name: ')
    print(fname)
    cat('\n')
  }


  # function
  #f = get(x = fname, mode = "function", pos = 'package:Giotto')
  f = get(x = fname, mode = "function", pos = sys.frame(-2))

  # get used arguments
  cl = match.call(definition=f, call=cl)
  user_args = as.list(cl)[-1]

  # all fun arguments
  fun_args <- formals(fun = fname)
  fun_args[names(user_args)] = user_args

  unl_args = unlist(fun_args)
  final_args = as.character(unl_args)
  names(final_args) = names(unl_args)

  # select first from vector
  bool_det = grepl("c\\(", final_args)
  if(any(bool_det) == TRUE) {

    for(bool_name in names(final_args[bool_det])) {

      bool_vec = final_args[bool_name]
      new_vec = strsplit(bool_vec, split = "\"")[[1]][2]

      final_args[bool_name] = new_vec

    }
  }

  return(final_args)

}



#' @title Update giotto parameters
#' @name update_giotto_params
#' @param gobject giotto object
#' @param description description of function run
#' @param return_gobject logical. Whether the giotto object should be returned
#' @param toplevel expected relative stackframe where call that is being recorded
#' was made
#' @export
update_giotto_params = function(gobject,
                                description = '_test',
                                return_gobject = TRUE,
                                toplevel = 2) {

  parameters_list = gobject@parameters
  number_of_rounds = length(parameters_list)
  update_name = paste0(number_of_rounds, description)

  parameters_list[[update_name]] = get_args(toplevel = toplevel)

  if(return_gobject == TRUE) {
    gobject@parameters = parameters_list
    return(gobject)
  } else {
    return(list(plist = parameters_list, newname = update_name))
  }
}



#' @title Giotto object history
#' @name objHistory
#' @description Print and return giotto object history
#' @param object giotto object
#' @export
objHistory = function(object) {
  cat('Steps and parameters used: \n \n')
  print(object@parameters)
  cat('\n\n')
  invisible(x = object@parameters)
}



#' @title showProcessingSteps
#' @name showProcessingSteps
#' @description shows the sequential processing steps that were performed
#' on a Giotto object in a summarized format
#' @param gobject giotto object
#' @return list of processing steps and names
#' @export
showProcessingSteps <- function(gobject) {

  parameters = gobject@parameters

  cat('Processing steps: \n \n')

  for(step in names(parameters)) {
    cat('\n', step, '\n')

    sub_step = parameters[[step]]

    if(any(grepl('name', names(sub_step)) == TRUE)) {

      selected_names = grep('name', names(sub_step), value = T)
      cat('\t name info: ', sub_step[selected_names], '\n')
    }
  }
}



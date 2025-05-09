#' @title Giotto python environment
#' @name giotto_python
#' @description
#' Giotto has several functions that utilize python packages. To
#' facilitate this, utilities are provided for creating, removing, and
#' attaching python environments. Python environments are currently handled
#' entirely through \pkg{reticulate}.
#'
#' **Creating an environment**
#'
#' `installGiottoEnvironment()` can be used to create a default miniconda
#' environment called `giotto_env` that includes some commonly used python
#' packages. See the **python versions** section for specific packages and
#' version numbers.
#'
#' Custom environments manageable through \pkg{reticulate} are also compatible
#' and can be hooked into by Giotto after creation.
#'
#' `checkGiottoEnvironment()` can be used in order to test if an envname or
#' full python path is accessible by Giotto. It will also check the
#' `"giotto.py_path"` option.
#'
#' **Choosing environments**
#'
#' Only one python environment may be initialized and used by \pkg{reticulate}
#' during a single R session. In order to switch to another environment, the
#' R session must be restarted.
#'
#' Whenever any of the following happens for the first time in a session:
#'
#' - `giotto` object creation (due to creation of a default
#' `giottoInstructions`)
#' - `giottoInstructions` creation (`createGiottoInstructions()`)
#' - `GiottoClass::set_giotto_python_path()` is called (most direct)
#'
#' For the above, Giotto automatically detects AND activates a python
#' environment based on the following defaults in decreasing priority:
#'
#'   1. User provided (when `python_path` param is not `NULL`)
#'   2. Any provided path or envname in option `"giotto.py_path"`
#'   3. Default expected giotto environment location based on
#'   [reticulate::miniconda_path()]
#'   4. Envname `"giotto_env"`
#'   5. System default python environment
#'
#' This behavior is mediated by the `set_giotto_python_path()` utility
#' function, which will find an environment and then initialize it.
#'
#'
#' # python versions
#' By default, Python v3.10.2 will be used with the following python modules
#' for giotto environment installation:
#' \preformatted{
#'    - pandas==1.5.1
#'    - networkx==2.8.8
#'    - python-igraph==0.10.2
#'    - leidenalg==0.9.0
#'    - python-louvain==0.16
#'    - python.app==1.4
#'    - scikit-learn==1.1.3
#' }
#'
#'  The giotto environment can be custom installed by changing the
#'  python_version parameter and module versions in the
#'  `packages_to_install` parameter.
#'
#'  For example, this giotto environment works as well, and was the
#'  default environment status for past releases of Giotto.
#'  Python  v3.6
#'  \preformatted{
#'   - pandas==1.1.5
#'   - networkx==2.6.3
#'   - python-igraph==0.9.6
#'   - leidenalg==0.8.7
#'   - python-louvain==0.15
#'   - python.app==2 # macOS only
#'   - scikit-learn==0.24.2
#' }
#'
#' # .yml installs
#' Please note that multiple .yml files are provided in the
#' repository for advanced installation and convenience. To install the most
#' up-to-date Giotto environment using a .yml file, open a shell compatible
#' with conda/miniconda and navigate to the directory specified by
#' system.file(package = "Giotto", "python/configuration"). Once in this
#' directory, run the following to create your environment in one step:
#'
#' \preformatted{conda env create -n giotto_env -f ./genv.yml}
#'
#' @param envname character. (optional) The name of a miniconda or conda
#' environment OR path to a python executable. When using
#' `installGiottoEnvironment()`, the default is `"giotto_env"`
#' @param conda either "auto" (default) to allow reticulate to handle it, or
#' the full filepath to the conda executable. You can also set the option
#' `"reticulate.conda_binary"` or `Sys.setenv()` `"RETICULATE_CONDA"` to tell
#' reticulate where to look.
#' @param verbose be verbose
#'
NULL







# check ####

#' @describeIn giotto_python
#'
#' - Based on `envname`, detect if there a conda or miniconda environment
#' accessible by Giotto. By default, the `envname` `"giotto_env"`, then
#' the option `"giotto.py_path"` is checked, but an alternative can be
#' provided.
#' - Setting `envname` as `":auto:"` will let  Giotto autodetect a python
#' env to use. See section for `set_giotto_python_path()` for details on the
#' autodetection.
#' - Returns `TRUE` if an env is detected and accessible by Giotto. `FALSE`
#' if not. Will not initialize a python environment during detection.
#' @examples
#' # detect without initialization
#' # check default env location
#' checkGiottoEnvironment()
#'
#' # use environment name
#' checkGiottoEnvironment("giotto_env")
#'
#' # full path
#' # (use this if a different install location specified with .condarc)
#' if (FALSE) {
#'     checkGiottoEnvironment(
#'         "/Users/example/Library/r-miniconda-arm64/envs/giotto_env/bin/pythonw"
#'     )
#' }
#' @export
checkGiottoEnvironment <- function(
        envname = NULL,
        mini_install_path = deprecated(),
        verbose = NULL) {
    if (is_present(mini_install_path)) {
        deprecate_warn(
            when = "0.3.2",
            what = "checkGiottoEnvironment(mini_install_path)",
            with = "checkGiottoEnvironment(envname)"
        )
        envname <- mini_install_path
    }

    if (identical(envname, ":auto:")) {
        envname <- NULL
    } else {
        envname <- envname %null% getOption("giotto.py_path")
        envname <- envname %null% "giotto_env"
    }

    if (!file.exists(envname) && isFALSE(.check_conda(error = FALSE))) {
        # - a conda binary must be detected to use an envname
        # - if an envname is not provided, the full path to the python binary
        # is needed
        vmsg(.v = verbose, "Unable to find a conda binary.
            Use `installGiottoEnvironment()` or install a custom conda.")
        return(FALSE)
        # this is also checked in set_giotto_python_path() within
        # .envname_to_pypath(), but that throws an error instead.
    }

    py_path <- set_giotto_python_path(
        python_path = envname, verbose = FALSE, initialize = FALSE
    )
    # set_giotto_python_path() returns the path if found.
    found <- is.character(py_path)

    if (found) {
        vmsg(.v = verbose, sprintf(
            "Giotto can access environment found at: \n'%s'", py_path
        ))
    } else {
        vmsg(.v = verbose, sprintf(
            "Giotto cannot find python environment with `envname`: '%s'",
            envname
        ))
    }

    vmsg(
        .v = verbose, .initial = " ",
        "If this is the wrong environment, try specifying `envname` param
        or set option \"giotto.py_path\" with the desired envname or path"
    )

    return(found)

    #  # check for envnames, if found, get the path
    #  if (!.is_path(envname)) {
    #      # if a condaenv matches envname, return fullpath
    #      # otherwise return envname without modification
    #      envname <- .envname_to_pypath(envname, must_exist = FALSE)
    #  }
    #
    #  # complete any directory inputs
    #  # if path does not exist, return NULL
    #  py_path <- .full_miniconda_path(path = envname)
    #
    # if (is.null(py_path)) {
    #     vmsg(
    #         .v = verbose,
    #         " Unable to find conda directory", envname,
    #         "\nPlease ensure the directory exists and is provided as",
    #         "character."
    #     )
    #     return(FALSE)
    # }
    #
    #  vmsg(.v = verbose, "giotto environment found at\n", py_path)
    #  return(TRUE)
}





#' @name .check_giotto_python_modules
#' @title Check if Giotto python modules are in python environment
#' @description
#' Check for some core python packages that Giotto uses. This is an internal
#' currently only called by the `giotto` `initialize()` method
#' @param my_python_path path to python environment
#' @returns character or NULL
#' @keywords internal
.check_giotto_python_modules <- function(my_python_path) {
    if (isFALSE(getOption("giotto.has_conda", TRUE))) {
        return(invisible(NULL))
    }
    if (isFALSE(getOption("giotto.use_conda", TRUE))) {
        return(invisible(NULL))
    }
    if (getOption("giotto.checked_py_modules", FALSE)) {
        # only do this check once per session
        return(invisible(NULL))
    }

    python_modules <- c(
        "pandas", "igraph", "leidenalg", "community",
        "networkx", "sklearn"
    )

    missing_modules <- vector(mode = "character")
    for (module in python_modules) {
        if (reticulate::py_module_available(module) == FALSE) {
            missing_modules <- c(missing_modules, module)
        }
    }

    if (length(missing_modules) > 0L) {
        warning(wrap_txt(sprintf(
            "Some of Giotto's expected python module(s) were not found:
            %s\n %s\n\n** Python path used: \"%s\"",
            paste(missing_modules, collapse = ", "),
            "(This is fine if python-based functions are not needed)",
            my_python_path
        ), .prefix = ""), call. = FALSE)
    }
    options("giotto.checked_py_modules" = TRUE)
    return(invisible())
}



# install ####



#' @title .install_giotto_environment_specific
#' @description installation of giotto environment
#' @param packages_to_install python packages to install with giotto env
#' @param pip_packages python packages mush installed with pip, only names
#' are needed
#' @param python_version python version to install
#' @param mini_install_path directory to install the environment to.
#' @param create_dir whether to create the directory specified by
#' `mini_install_path` if it does not already exist. (default = FALSE)
#' @param conda path to conda executable. See ?reticulate::`conda-tools`
#' **finding conda** section. (Default = "auto")
#' @param verbose be verbose
#' @keywords internal
#' @noRd
#' @returns character or NULL
.install_giotto_environment_specific <- function(packages_to_install = c(
        "pandas", "networkx", "python-igraph",
        "leidenalg", "python-louvain", "python.app",
        "scikit-learn", "smfishhmrf", "session-info"
    ),
    pip_packages = c("python-louvain", "smfishhmrf", "session-info"),
    python_version = "3.10.2",
    mini_install_path = NULL,
    confirm = TRUE,
    envname = "giotto_env",
    conda = "auto",
    verbose = NULL) {
    vmsg(.v = verbose, "\n |---- install giotto environment ----| \n")

    ## paths ##
    ## ----- ##
    # conda (let reticulate handle it when possible)
    conda <- conda %null% "auto"
    conda_path <- reticulate::conda_binary(conda)

    # environment
    if (is.null(mini_install_path)) {
        # giotto environment path defaults
        if (!is.null(envname)) {
            mini_install_path <- envname
        } else {
            mini_install_path <- reticulate::miniconda_path()
        }
        # only the envname or default path should be used.
    } else {
        # checks if not following defaults
        if (!is.character(mini_install_path)) {
            stop(wrap_txt("`mini_install_path` input must be character"))
        }
        if (checkmate::test_file_exists(mini_install_path)) {
            stop(wrap_txt(
                "`mini_install_path` should be a path to a directory;
                not an executable."
            ))
        }
        # complete path
        mini_install_path <- file.path(mini_install_path, "envs", envname)

        # confirm location
        vmsg(.v = verbose, sprintf(
            "Installing env to directory:\n\"%s\"", mini_install_path
        ))
        if (isTRUE(confirm)) {
            # if not confirmed, return early
            input <- readline("Is this the right location? [y/n] ")
            if (!input %in% c("y", "Y", "n", "N")) {
                stop("Invalid input. Please try again.")
            }
            if (!input %in% c("y", "Y")) stop("aborting")
        }

        # create directory if not existing
        if (!dir.exists(mini_install_path)) {
            dir.create(mini_install_path, recursive = TRUE)
        }
        # user defined path will be used
    }


    ## identify operating system and adjust the necessary packages ##
    ## ----------------------------------------------------------- ##
    os_specific_system <- get_os()

    if (os_specific_system != "osx") { # only osx needs python.app
        packages_to_install <- packages_to_install[!grepl(
            pattern = "python.app", x = packages_to_install
        )]
    }

    # some python packages must be installed with pip, not with conda-forge
    # `pip_packages` will be installed with pip
    # `forge_packages` will be installed with conda-forge
    pip_pkg_indices <- grep(paste0(
        "^(", paste(pip_packages, collapse = "|"),
        ")"
    ), packages_to_install, ignore.case = TRUE)
    forge_packages <- packages_to_install[-pip_pkg_indices]
    pip_packages <- packages_to_install[pip_pkg_indices]

    ## create conda env ##
    ## ---------------- ##

    a <- list(
        python_version = python_version,
        envname = mini_install_path,
        conda = conda_path
    )

    do.call(reticulate::conda_create, args = a)

    ## install python packges ##
    ## ---------------------- ##

    if (length(forge_packages) > 0L) {
        do.call(
            reticulate::py_install,
            args = c(a, list(
                packages = forge_packages,
                method = "conda",
                channel = c("conda-forge", "vtraag")
            ))
        )
    }

    if (length(pip_packages) > 0L) {
        do.call(
            reticulate::py_install,
            args = c(a, list(
                packages = pip_packages,
                method = "conda",
                pip = TRUE
            ))
        )
    }
}


#' @title .install_giotto_environment
#' @description installation options of giotto environment
#' @returns character or NULL
#' @keywords internal
.install_giotto_environment <- function(
        force_environment = FALSE,
        packages_to_install = c(
            "pandas", "networkx", "python-igraph",
            "leidenalg", "python-louvain", "python.app",
            "scikit-learn", "smfishhmrf", "session-info"
        ),
        pip_packages = c("python-louvain", "smfishhmrf", "session-info"),
        python_version = "3.10.2",
        mini_install_path = NULL,
        confirm = TRUE,
        envname = "giotto_env",
        conda = "auto",
        verbose = NULL) {
    # first see if Giotto environment is already installed
    giotto_installed <- checkGiottoEnvironment(
        envname = envname,
        verbose = FALSE
    )

    # already installed and no force: do nothing & return
    if (isTRUE(giotto_installed) && !isTRUE(force_environment)) {
        vmsg(
            .v = verbose,
            "An environment usable by Giotto is already installed
            Run `checkGiottoEnvironment()` to see which is being detected.
            set force_environment = TRUE to reinstall"
        )
        return(invisible()) # return early
    }

    # find conda binary (let reticulate handle it when possible)
    conda <- conda %null% "auto"
    conda_path <- reticulate::conda_binary(conda)

    # already installed and force: remove original env
    if (isTRUE(giotto_installed) && isTRUE(force_environment)) {
        # first remove giotto environment, then install
        reticulate::conda_remove(
            envname = envname,
            conda = conda_path
        )
    }

    # install giotto environment
    .install_giotto_environment_specific(
        packages_to_install = packages_to_install,
        pip_packages = pip_packages,
        python_version = python_version,
        mini_install_path = mini_install_path,
        confirm = confirm,
        envname = envname,
        conda = conda_path,
        verbose = verbose
    )
}




#' @describeIn giotto_python
#'
#' - Install a giotto python environment using miniconda through
#' \pkg{reticulate}. By default, the envname used will be `"giotto_env"`. If
#' another name is used, you will have to provide that envname at the start of
#' a session (see **Choosing an environment** above). \cr This includes a
#' miniconda installation and also a set of python packages that Giotto
#' may often use. See details for further information on setting up an
#' environment with a .yml
#' - Returns `NULL`
#' @param packages_to_install python modules (packages) to install for Giotto.
#' @param pip_packages python packages mush installed with pip, only names
#' are needed
#' @param python_version python version to use within the giotto conda
#' environment. Default is v3.10.2
#' @param mini_install_path (optional) desired miniconda installation location.
#' Default is chosen by `reticulate::install_miniconda()`
#' @param confirm whether to pause for confirmation of conda environment
#' install location (default = TRUE)
#' @param force_miniconda force reinstallation of miniconda
#' @param force_environment force reinstallation of the giotto environment
#'
#' @examples
#' if (FALSE) {
#'     # default environment installation
#'     installGiottoEnvironment()
#'
#'     # install to alternate location
#'     temp_env <- tempdir()
#'     installGiottoEnvironment(mini_install_path = temp_env)
#' }
#' @returns installed Giotto environment
#' @export
installGiottoEnvironment <- function(packages_to_install = c(
        "pandas==1.5.1",
        "networkx==2.8.8",
        "python-igraph==0.10.2",
        "leidenalg==0.9.0",
        "python-louvain==0.16",
        "python.app==1.4",
        "scikit-learn==1.1.3",
        "smfishhmrf",
        "session-info"
    ),
    pip_packages = c("python-louvain", "smfishhmrf", "session-info"),
    python_version = "3.10.2",
    mini_install_path = NULL,
    confirm = TRUE,
    envname = "giotto_env",
    conda = "auto",
    force_miniconda = FALSE,
    force_environment = FALSE,
    verbose = NULL) {
    ## 1. check and install miniconda locally if necessary
    conda_found <- .check_conda(conda = conda, error = FALSE)

    # install miniconda if needed
    if (isFALSE(conda_found) || isTRUE(force_miniconda)) {
        vmsg(
            .v = verbose, .initial = " ",
            "|---- install local miniconda ----|"
        )

        if (identical(conda, "auto")) {
            conda_path <- reticulate::miniconda_path()
        }

        reticulate::install_miniconda(
            path = conda_path,
            force = force_miniconda
        )
    }

    ## 2. install giotto environment
    if (is.null(mini_install_path)) {
        confirm <- FALSE # following defaults, no confirm needed
    }

    .install_giotto_environment(
        force_environment = force_environment,
        packages_to_install = packages_to_install,
        pip_packages = pip_packages,
        python_version = python_version,
        mini_install_path = mini_install_path,
        confirm = confirm,
        envname = envname,
        conda = conda,
        verbose = verbose
    )
}


# remove ####



#' @describeIn giotto_python
#'
#' - Remove a python environment
#' - Returns `NULL`
#' @param mini_path deprecated
#' @export
removeGiottoEnvironment <- function(
        envname = "giotto_env",
        mini_path = deprecated(),
        conda = "auto",
        verbose = TRUE) {
    if (is_present(mini_path)) {
        deprecate_warn(
            when = "0.3.2",
            what = "removeGiottoEnvironment(mini_path)",
            details = "See new params `envname` and `conda` instead"
        )
    }

    # first see if Giotto is already installed
    giotto_installed <- checkGiottoEnvironment(
        envname = envname, verbose = verbose
    )

    if (!isTRUE(giotto_installed)) {
        wrap_msg(
            "Giotto environment is not found and probably never installed"
        )
    }

    # if envname was provided, get pypath from conda_list,
    # then convert to envpath
    if (!.is_path(envname)) {
        # if a condaenv matches envname, return fullpath
        # otherwise throw error
        envname <- .envname_to_pypath(envname, must_exist = TRUE) %>%
            .pypath_to_envpath() # fullpath to envpath
    }

    reticulate::conda_remove(
        envname = envname,
        conda = conda
    )
}




# detect and activate ####

#' @describeIn giotto_python
#'
#' - Detect and activate a python path. The `python_path` param
#' accepts both full filepaths to the python executable and envnames. The
#' final path to use is determined as follows in decreasing priority:
#'
#'   1. User provided (when `python_path` is not `NULL`)
#'   2. Any provided path or envname in option `"giotto.py_path"`
#'   3. Default expected giotto environment location based on
#'   [reticulate::miniconda_path()]
#'   4. Envname "giotto_env"
#'   5. System default python environment
#'
#' - This function exits without doing anything if option `"giotto.use_conda"`
#' is `FALSE`.
#' - By default this function will force initialization of the python
#' environment to set, locking the session to that environment.
#' This can be skipped if `initialize = FALSE`, however in that case, the
#' actual python path set downstream may differ from what is expected and
#' reported by this function.
#' - Returns detected path to python binary or `NULL` if none found.
#' @param python_path character. Name of environment or full path to python
#' executable.
#' @param initialize force initialization of set python path. Default = TRUE.
#' @keywords internal
#' @examples
#' # detect AND initialize a python environment
#' if (FALSE) {
#'     set_giotto_python_path()
#' }
#' @export
set_giotto_python_path <- function(
        python_path = NULL,
        verbose = NULL,
        initialize = TRUE) {
    if (isFALSE(getOption("giotto.use_conda", TRUE))) {
        return(invisible(NULL)) # exit early
    }

    # if py_active_env() is character then an environment has already been
    # initialized. Return early with a verbose message
    py <- py_active_env()
    if (is.character(py)) {
        vmsg(.v = verbose, sprintf(
            "%s\n%s '%s'\n%s %s",
            "python already initialized in this session",
            "active environment :", py,
            "python version :", getOption("giotto.py_active_ver")
        ))
        return(getOption("giotto.py_active_path"))
    }

    # get path in order of DECREASING priority #
    # ---------------------------------------- #
    found <- vector(mode = "numeric")
    found_msg <- c(
        "a python path has been provided",
        "found python path from option 'giotto.py_path'",
        "a giotto python environment was found",
        "", # skip 4 since it's always printed
        "a system default python environment was found"
    )

    # `specified` flag
    # flag for when path is directly intended. When not NULL, instead of
    # quietly passing to next default, send message and immediately return
    # NULL
    specified <- NULL

    # (1.) from user (when `python_path` != NULL)
    if (!is.null(python_path)) {
        found <- c(found, 1)
        specified <- sprintf("`envname` = '%s'", python_path)
    }

    # (2.) check option (default is null)
    python_path <- python_path %null% getOption("giotto.py_path")
    if (!is.null(python_path) && length(found) == 0L) {
        found <- c(found, 2)
        specified <- sprintf(
            "%s: \"%s\"",
            "option 'giotto.py_path'",
            getOption("giotto.py_path")
        )
    }

    # (3.) check default install path; if not existing, returns NULL
    # will return NULL for .condarc alternate location "giotto_env" installs
    python_path <- python_path %null% .os_py_path(must_exist = TRUE)
    if (!is.null(python_path)) found <- c(found, 3)

    # (4.) check default envname, relying on reticulate::conda_list()
    # catches .condarc alternate location "giotto_env"
    if (is.null(python_path)) {
        python_path <- "giotto_env"
        vmsg(.v = verbose, "checking default envname \'giotto_env\'")
    }

    # if an envname was provided, convert to a full python path to test
    # if no existing python path found, return the envname without changes
    if (!.is_path(python_path)) {
        python_path <- .envname_to_pypath(python_path, must_exist = FALSE)
    }
    # if python_path thus far is not completable to an existing path
    # return NULL, otherwise return existing path
    python_path <- .full_miniconda_path(path = python_path)

    # early return NULL if specified and NOT found.
    if (is.null(python_path) && !is.null(specified)) {
        vmsg(
            .v = verbose,
            sprintf("specified py env from %s not found\n", specified)
        )
        return(invisible())
    }

    # (5.) detect from system call; return NULL if not found
    python_path <- python_path %null% .sys_detect_py()
    if (!is.null(python_path)) found <- c(found, 5)

    # print any found messages #
    # ------------------------ #
    if (length(found) > 0) {
        found_at <- min(found)
        vmsg(.v = verbose, found_msg[found_at])
    }

    # if any working python path found; activate the environment and return #
    # --------------------------------------------------------------------- #
    if (!is.null(python_path)) {
        if (isTRUE(initialize)) {
            vmsg(
                .v = verbose,
                sprintf("Using python path:\n\"%s\"", python_path)
            )

            # `use_python()` applies a setting in `reticulate:::.globals`
            # but, python is still not initialized
            reticulate::use_python(required = TRUE, python = python_path)
            # `py_config()` will force initialization
            reticulate::py_config()
        }

        return(python_path)
    }

    # otherwise, not found -- helpful prints
    vmsg("no default python path found.
        For full functionality, install python and/or use
        strategy 1 or 2:")
    vmsg("1. use installGiottoEnvironment() to install
        a local miniconda python environment along with required modules")
    vmsg("2. provide an existing python path to
        python_path to use your own python path which has all modules
        installed")
    vmsg('Set options(\"giotto.use_conda\" = FALSE) if
        python functionalities are not needed')
    return(invisible())
}





# py package installs ####

#' @title Prompt User for Python Install
#' @name .py_install_prompt
#' @param package python package/github url
#' @param env environment into which package will be installed
#' @description prompts user to install a package
#' @keywords internal
#' @returns numeric
.py_install_prompt <- function(package = NULL,
    env = NULL) {
    if (is.null(package) || is.null(env)) {
        stop(GiottoUtils::wrap_txt("Incorrect Usage.\n", errWidth = TRUE))
    }

    install_py_pkg_msg <- paste0(
        "Python package `",
        package,
        "` is required and not installed.\n"
    )
    warning(install_py_pkg_msg, immediate. = TRUE)
    install_py_pkg_msg <- paste0("Enter 0 to skip installation and quit.\n")
    install_py_pkg_msg <- paste0(
        install_py_pkg_msg, "Enter any other number ",
        "to install\n`",
        package,
        "`\nto python environment: `",
        env, "`\n\n"
    )
    resp <- as.integer(readline(prompt = install_py_pkg_msg))
    return(resp)
}

#' @title Install Package from GitHub Link
#' @name .install_github_link_pip
#' @param link link to github repository containing a python package,
#' e.g. `git+https://github.com/TencentAILabHealthcare/pysodb.git`
#' @param env conda environment to which `link` will be installed via pip
#' @description
#' Installs `link` to python `env`
#' @keywords internal
#' @returns character or NULL
.install_github_link_pip <- function(link = NULL,
    env = NULL) {
    # Guard
    if (is.null(link) | is.null(env)) {
        stop(GiottoUtils::wrap_txt("Incorrect Usage.", errWidth = TRUE))
    }

    config <- reticulate::py_discover_config(use_environment = env)
    # system commands return 0 if ran successfully, 1 otherwise
    successful_install <- !as.logical(system2(
        config$python,
        c(
            "-m",
            "pip",
            "install",
            link
        )
    ))
    if (successful_install) {
        return(TRUE)
    } else {
        git_url_err_msg <- "Provided GitHub URL `"
        git_url_err_msg <- paste0(git_url_err_msg, link, "`")
        git_url_err_msg <- paste0(
            git_url_err_msg,
            " Could not be installed.\n"
        )
        git_url_err_msg <- paste0(
            git_url_err_msg,
            "Please try again with a different URL."
        )
        stop(GiottoUtils::wrap_txt(git_url_err_msg, errWidth = TRUE))
    }
}

#' @title Install Python Package with Reticulate
#' @name .install_py_pkg_reticulate
#' @param package name of python package
#' @param env name of the environment into which the python
#' package should be installed.
#' @details
#' Installs `package` to python `env` after prompting user.
#' Installation is done via `py_install` from the
#' `reticulate` package.
#' @keywords internal
#' @returns character or NULL
.install_py_pkg_reticulate <- function(package = NULL,
    env = NULL) {
    resp <- .py_install_prompt(
        package = package,
        env = env
    )
    if (resp != 0) {
        try_install <- tryCatch(expr = {
            reticulate::py_install(
                package = package,
                envname = env
            )
            return(TRUE)
        }, error = function(e) {
            return(FALSE)
        })

        if (!try_install) {
            cannot_install_msg <- paste0("Could not install `", package)
            cannot_install_msg <- paste0(
                cannot_install_msg,
                "` using `reticulate::py_install()`\n"
            )

            GiottoUtils::wrap_msg(cannot_install_msg,
                errWidth = TRUE
            )
            return(FALSE)
        }
    } else {
        stop(GiottoUtils::wrap_txt("Package not installed.\n", errWidth = TRUE))
    }
}

#' @title Check Python Package Installation
#' @name checkPythonPackage
#' @param package_name name of python package. See details.
#' @param github_package_url URL linking to github repository containing
#' a python package that may be installed with pip,
#' e.g. `git+https://github.com/TencentAILabHealthcare/pysodb.git`;
#' see details.
#' @param env_to_use name of the environment into which the python
#' package should be installed.
#' @description checks python environment for a
#' provided package, installs if it is not found.
#' @returns character or NULL
#' @details
#' Parameter `github_package_url` takes precedent over
#' `package_name`, i.e. if both are provided, only the github
#' URL will be installed. This function should only be provided
#' one parameter, or the other.
#' @keywords internal
checkPythonPackage <- function(package_name = NULL,
    github_package_url = NULL,
    env_to_use = "giotto_env") {
    # Guard clauses
    if (is.null(package_name) & is.null(github_package_url)) {
        null_input_err_msg <- "A python package name must be provided,
        e.g. `scanpy==1.9.0`"
        null_input_err_msg <- paste0(
            null_input_err_msg,
            "\nAlternatively, provide a github package URL, "
        )
        null_input_err_msg <- paste0(
            null_input_err_msg,
            "e.g. `git+https://github.com/TencentAILabHealthcare/pysodb.git` "
        )
        stop(GiottoUtils::wrap_txt(null_input_err_msg,
            errWidth = TRUE
        ))
    }
    # Find path to currently initialized python env
    path_to_env <- reticulate::py_config()$pythonhome
    if (!grepl(env_to_use, path_to_env)) {
        env_err_msg <- paste0(
            "Provided python environment `",
            env_to_use, "` is not initialized."
        )
        env_err_msg <- paste0(
            env_err_msg,
            "\nThe following python environment is in use: `",
            path_to_env, "`"
        )
        env_err_msg <- paste0(
            env_err_msg,
            "\nTo initialize `", env_to_use, "`, you must ",
            "restart your R session."
        )
        stop(GiottoUtils::wrap_txt(env_err_msg,
            errWidth = TRUE
        ))
    }
    env_str_location <- GiottoUtils::str_locate2(path_to_env, env_to_use)[2]
    # Change env_to_use from name of environment
    # to the full environment path
    env_to_use <- substr(path_to_env, 1, env_str_location)

    env_to_use <- path_to_env


    # If a github link is provided, install it and exit
    if (!is.null(github_package_url)) {
        resp <- .py_install_prompt(
            package = github_package_url,
            env = env_to_use
        )
        if (resp != 0) {
            install_status <- .install_github_link_pip(
                link = github_package_url,
                env = env_to_use
            )
            return(install_status)
        } else {
            stop(GiottoUtils::wrap_txt("Package not installed.\n",
                errWidth = TRUE
            ))
        }
    }

    package_config <- reticulate::py_list_packages()
    pkgs_in_py_env <- package_config$package
    versions <- package_config$version

    # package installed, right version --> exit
    # package installed, but wrong version --> prompt for install
    # package not installed --> prompt for install

    version_number <- NULL

    contains_version_number <- grepl("==", package_name)
    if (contains_version_number) {
        split_package_version <- strsplit(package_name,
            split = "=="
        )
        package_name <- split_package_version[[1]][1]
        version_number <- split_package_version[[1]][2]
    }

    if (package_name %in% pkgs_in_py_env) {
        if (!contains_version_number) {
            # If a version number is not provided,
            # and the package exists within the
            # reticulate package list, exit,
            # since it is already installed.
            return(TRUE)
        } else {
            # Check that the version numbers match, if provided
            idx <- which(pkgs_in_py_env == package_name)
            version_match <- (version_number == versions[idx])

            if (version_match) {
                # if the versions match, the right version
                # is installed
                return(TRUE)
            } else {
                # Otherwise, install the provided version
                inst_result <- .install_py_pkg_reticulate(
                    package = paste0(
                        package_name,
                        "==",
                        version_number
                    ),
                    env = env_to_use
                )
                return(inst_result)
            }
        }
    } else {
        if (!contains_version_number) {
            # If it is not installed, and has no version
            # number, install it.
            inst_result <- .install_py_pkg_reticulate(
                package = package_name,
                env = env_to_use
            )
        } else {
            # If it is not installed, and has a version
            # number, concatenate the package and verion
            # strings, and install
            inst_result <- .install_py_pkg_reticulate(
                package = paste0(
                    package_name,
                    "==",
                    version_number
                ),
                env = env_to_use
            )
        }
        return(inst_result)
    }
}







# common internals ####


# determine if a conda binary is accessible by reticulate
# return path to binary if found
# return FALSE if `error` != TRUE ignoring, reticulate's thrown error
#
# param conda - what conda path to use.
# param error - whether to stop execution when conda not found
.check_conda <- function(conda = "auto", error = TRUE) {
    res <- try(reticulate::conda_binary(conda = conda), silent = TRUE)
    if (inherits(res, "try-error")) res <- FALSE

    if (isFALSE(res) && isTRUE(error)) {
        stop(
            wrap_txt(
                "Unable to find a conda binary.
            Use `installGiottoEnvironment()` or install a custom conda."
            ),
            call. = FALSE
        )
    }
    return(res)
}





# construct path to miniconda executable when the python directory is given
# python directory should be provided in the same way as
# `reticulate::miniconda_path()` where it is one level above the `envs`
# subdirectory.
# NULL is returned if the executable is not found if `must_exist` is TRUE
# when `must_exist` is FALSE, the built path is always returned
.os_py_path <- function(
        path = reticulate::miniconda_path(),
        envname = "giotto_env",
        os = get_os(),
        must_exist = TRUE) {
    if (!checkmate::test_directory_exists(path)) {
        vmsg(.is_debug = TRUE, ".os_py_path: base dir not found!")
    }
    env_level <- file.path(path, "envs", envname)
    full_path <- switch(os,
        "osx" = file.path(env_level, "bin/pythonw"),
        "windows" = file.path(env_level, "python.exe"),
        "linux" = file.path(env_level, "bin/python")
    )
    # if not must exist or the file exists, return
    if (!must_exist || file.exists(full_path)) {
        return(full_path)
    }
    # call again with python instead of pythonw for mac
    if (os == "osx") {
        full_path <- gsub("pythonw", "python", full_path)
        if (file.exists(full_path)) {
            return(full_path)
        }
    }
    # not found, return NULL
    return(NULL)
}

# convert a full python executable path to the miniconda install directory
.pypath_to_envpath <- function(python_path) {
    os <- get_os()
    remove <- switch(os,
        "osx" = "bin/pythonw$|bin/python$",
        "windows" = "python.exe",
        "linux" = "bin/python"
    )
    gsub(remove, "", python_path)
}

# first function in detection path to check conda envs
# if found, return the fullpath
# if not, return without modification
.envname_to_pypath <- function(envname, must_exist = TRUE) {
    .check_conda()

    envs <- reticulate::conda_list()
    enames <- envs$name
    epaths <- envs$python
    if (envname %in% enames) {
        envname <- epaths[enames == envname]
    } else if (isTRUE(must_exist)) {
        stop(sprintf(
            "envname '%s' not found in reticulate::conda_list()",
            envname
        ), call. = FALSE)
    }
    return(envname)
}

# Guess full path to miniconda executable
# if a full path to the executable is provided, it will be used
# if a directory is provided, it will be completed with `.os_py_path`
# if an envname is given, it will be completed with `.os_py_path` based on
# `reticulate::miniconda_path()` as the base.
# If no file is detected, NULL is returned.
.full_miniconda_path <- function(path = NULL) {
    # default giotto_env install location
    if (is.null(path)) {
        return(.os_py_path())
    }

    if (checkmate::test_file_exists(path)) {
        # fullpath
        res <- path
    } else if (dir.exists(path)) {
        # specific install location (.condarc) + giotto_env default name
        res <- .os_py_path(path)
    } else {
        # specific envname under reticulate::miniconda_path() directory
        res <- .os_py_path(envname = path)
    }

    return(res)
}

# system call to detect python location
# if found, returns path, if not returns NULL
.sys_detect_py <- function() {
    res <- try(
        {
            switch(.Platform[["OS.type"]],
                "unix" = system("which python3", intern = TRUE),
                "windows" = system("where python3", intern = TRUE)
            )
        },
        silent = TRUE
    )
    if (inherits(res, "try-error")) res <- NULL
    return(res)
}

# detect if something is likely a path based on slashes (forward and back)
# also if a file exists (also covers directories)
.is_path <- function(x) {
    grepl("\\\\|/", x) || file.exists(x)
}

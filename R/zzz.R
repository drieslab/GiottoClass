# Run on library loading


.onAttach <- function(libname, pkgname) {
    ## Giotto Utils ##
    # utils_try = try(
    #   library(GiottoUtils),
    #   silent = TRUE
    # )
    #
    # if(inherits(utils_try, 'try-error')) {
    #   if (utils::menu(c("Yes", "No"),
    #                   title = "GiottoUtils is required. Proceed with 
    #                   installation?") == "1") {
    #     devtools::install_github('drieslab/GiottoUtils')
    #
    #   } else {
    #     print("Cancelling installation")
    #     stop('There is no package called GiottoUtils', call. = FALSE)
    #   }
    # }

    # set GiottoClass options
    # options("giotto.use_conda" = FALSE)

    ## print version number ##

    check_ver <- getOption("giotto.check_version", TRUE)
    if (isTRUE(check_ver)) {
        GiottoUtils::check_github_suite_ver("GiottoClass")
        options("giotto.check_version" = FALSE)
    }
}

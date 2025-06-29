# Run on library loading


.onAttach <- function(libname, pkgname) {
    check_ver <- getOption("giotto.check_version", TRUE)
    if (isTRUE(check_ver)) {
        GiottoUtils::check_github_suite_ver("GiottoClass")
        options("giotto.check_version" = FALSE)
    }

    init_option("giotto.py_path", NULL)
    init_option("giotto.init", TRUE)
    init_option("giotto.check_valid", TRUE)
    init_option("giotto.plotengine3d", "plotly")
    init_option("giotto.update_param", TRUE)
    init_option("giotto.no_python_warn", FALSE)
}

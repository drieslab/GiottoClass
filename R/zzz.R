# Run on library loading


.onAttach <- function(libname, pkgname) {
    check_ver <- getOption("giotto.check_version", TRUE)
    if (isTRUE(check_ver)) {
        GiottoUtils::check_github_suite_ver("GiottoClass")
        options("giotto.check_version" = FALSE)
    }
}

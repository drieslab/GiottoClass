#### logging of giotto functions ####


# Functions to find the previous function name or args have been moved to
# GiottoUtils prev_call.R


#' @title Update giotto parameters
#' @name update_giotto_params
#' @description
#' For developer use. Adds an entry to the `giotto` object object history.
#' Care currently needs to be taken when a function that contains a call to
#' this function is called from within yet another function. In such cases,
#' a `toplevel < 0` or setting a temporary `"giotto.update_param" = FALSE` with
#' `GiottoUtils::gwith_option()` may be the best option to avoid either
#' evaluation errors or strange history entries. A new `update_giotto_params()`
#' call can then be added that describes the function of the topmost function
#' if desired.
#' @param gobject giotto object
#' @param description description of function run
#' @param return_gobject logical. Whether the giotto object should be returned
#' @param toplevel expected relative stackframe where call that is being
#' recorded was made. If negative, param recording is skipped
#' @param attachments named list. Items to attach. These are intended for lightweight
#' param classes containing settings. No large items should be added here.
#' @returns giotto object or list of parameters
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' update_giotto_params(g, toplevel = 1)
#' @export
update_giotto_params <- function(
        gobject,
        description = "_test",
        return_gobject = TRUE,
        toplevel = 2,
        attachments = NULL) {
    checkmate::assert_list(attachments, null.ok = TRUE, names = "unique")
    parameters_list <- gobject@parameters
    number_of_rounds <- length(parameters_list)
    update_name <- paste0(number_of_rounds, description)

    # return before updating if toplevel negative or global setting is FALSE
    if (skip_update <- toplevel < 0 ||
        !getOption("giotto.update_param", TRUE)) {
        if (isTRUE(return_gobject)) {
            return(gobject)
        } else {
            return(list(
                plist = parameters_list,
                newname = names(tail(parameters_list, 1L))
            ))
        }
    }

    # `get_args()` can be problematic. Allow skip right before this step.

    # update parameters list
    new_entry <- get_args(toplevel = toplevel)
    class(new_entry) <- c("ghistory_item", "character")
    if (!is.null(attachments)) {
        attr(new_entry, "attachments") <- attachments
    }

    # Structured record, carried as an attribute so the entry stays the
    # character vector every existing reader expects. `params` here are the
    # deparsed argument expressions, which keep `1:30` intact where the
    # flattened character form of `get_args()` reduces it to its first element.
    call_info <- .ghistory_call_info(toplevel = toplevel)
    attr(new_entry, "step") <- .ghistory_step(
        step_id = update_name,
        fn = call_info$fn,
        params = call_info$params
    )

    parameters_list[[update_name]] <- new_entry
    class(parameters_list) <- "ghistory"

    if (isTRUE(return_gobject)) {
        gobject@parameters <- parameters_list
        return(gobject)
    } else {
        return(list(plist = parameters_list, newname = update_name))
    }
}

#' @export
#' @keywords internal
print.ghistory <- function(x, ...) {
    message("Steps and parameters used:")
    for (i in seq_along(x)) {
        cat(GiottoUtils::color_blue(sprintf("<%s>\n", names(x)[[i]])))
        print(x[[i]])
    }
}

#' @export
#' @keywords internal
print.ghistory_item <- function(x, ...) {
    GiottoUtils::print_list(x, pre = "  ")
    atts <- attr(x, "attachments")
    if (!is.null(atts)) {
        for (a in names(atts)) {
            cat(" ", GiottoUtils::color_yellow(sprintf("<%s> :\n", a)))
            cat(str_reformat(atts[[a]], indent = 4))
        }
    }
}

#' @title Giotto object history
#' @name objHistory
#' @description Print and return giotto object history
#' @param object giotto object
#' @param summarized logical. whether print should be summarized
#' @returns list
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' objHistory(g)
#' objHistory(g, summarized = TRUE)
#' @export
objHistory <- function(object, summarized = FALSE) {
    p <- object@parameters

    if (summarized) {
        message("Processing steps:")
        for (step in names(p)) {
            message(step)
            sub_step <- p[[step]]
            if (any(grepl("name", names(sub_step)) == TRUE)) {
                selected_names <- grep("name", names(sub_step), value = TRUE)
                wrap_msg("\t name info: ", sub_step[selected_names])
            }
        }
    }
    object@parameters
}



#' @title showProcessingSteps
#' @name showProcessingSteps
#' @description shows the sequential processing steps that were performed
#' on a Giotto object in a summarized format
#' @param gobject giotto object
#' @returns list of processing steps and names
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' showProcessingSteps(g)
#' @export
showProcessingSteps <- function(gobject) {
    deprecate_warn(
        when = "0.4.0",
        what = "showProcessingSteps()",
        with = "objHistory()",
        details = "objHistory with arg `summarized = TRUE` replaces this functionality"
    )

    parameters <- gobject@parameters

    message("Processing steps:")

    for (step in names(parameters)) {
        message(step)

        sub_step <- parameters[[step]]

        if (any(grepl("name", names(sub_step)) == TRUE)) {
            selected_names <- grep("name", names(sub_step), value = TRUE)
            wrap_msg("\t name info: ", sub_step[selected_names])
        }
    }
}


#### structured history records ####

# The op log answers "why does the object look like this". It is provenance,
# not state: coverage is partial (functions must opt in), args are recorded as
# written rather than as resolved, and a failed call historically left no trace
# at all. Object state therefore comes from `objManifest()` and is never
# reconstructed by replaying this log. See [manifestDiff()].

.GHISTORY_STATUS <- c("ok", "error", "unattributed")

# Name and deparsed argument expressions of the recorded call.
#
# `sys.call(-n)` counts evaluation frames, so this must walk the stack BEFORE
# entering any tryCatch: those add four frames of their own and the walk lands
# on `tryCatchOne` instead of the function being recorded. Called at the same
# depth as `get_args()` so both see the same frame.
.ghistory_call_info <- function(toplevel = 2L) {
    none <- list(fn = NA_character_, params = list())
    if (sys.nframe() <= toplevel) return(none)

    cl <- sys.call(-toplevel)
    f <- sys.function(-toplevel)
    if (is.null(cl)) return(none)

    fn <- tryCatch(
        {
            nm <- as.character(cl[[1]])
            # `pkg::fn(...)` deparses to c("::", "pkg", "fn")
            if (length(nm) > 1L) nm[[length(nm)]] else nm
        },
        error = function(e) NA_character_
    )
    params <- tryCatch(
        {
            mc <- match.call(definition = f, call = cl)
            # defaults first, then whatever the call supplied: the record is
            # the parameters the function actually ran with, not only the ones
            # typed out
            args <- formals(f)
            supplied <- as.list(mc)[-1]
            args[names(supplied)] <- supplied
            args <- args[names(args) != "..."]
            args <- args[!vapply(args, function(a) {
                is.symbol(a) && !nzchar(as.character(a))
            }, logical(1L))]
            lapply(args, function(a) paste(deparse(a), collapse = " "))
        },
        error = function(e) list()
    )
    list(fn = fn, params = params)
}

# Cheap RNG state marker: a full .Random.seed is ~626 integers, which has no
# place in a record meant to stay small. A digest of it is enough to tell two
# runs apart.
.ghistory_seed <- function() {
    tryCatch(
        {
            has_seed <- exists(
                ".Random.seed", envir = globalenv(), inherits = FALSE
            )
            if (!has_seed) return(NULL)
            if (!requireNamespace("digest", quietly = TRUE)) return(NULL)
            list(
                kind = RNGkind()[[1]],
                state = digest::digest(
                    get(".Random.seed", envir = globalenv()),
                    algo = "xxhash64"
                )
            )
        },
        error = function(e) NULL
    )
}

.ghistory_step <- function(step_id,
    fn = NA_character_,
    params = list(),
    status = "ok",
    diff = NULL,
    error = NULL) {
    status <- match.arg(status, .GHISTORY_STATUS)
    list(
        step_id = step_id,
        fn = fn,
        params = params,
        timestamp = format(
            as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"
        ),
        seed = .ghistory_seed(),
        status = status,
        error = error,
        diff = diff
    )
}

#' @title Record a giotto object history step
#' @name recordGiottoStep
#' @description
#' Append a history entry that `update_giotto_params()` cannot produce itself:
#' a call that failed, or a change made outside a logging function.
#'
#' Only 52 of the suite's functions call [update_giotto_params()], so a
#' manifest can move without any entry claiming it. An execution tool that
#' diffs [objManifest()] before and after a chunk closes that gap by recording
#' the unclaimed change here with `status = "unattributed"`, rather than
#' letting it vanish.
#' @param gobject giotto object
#' @param fn character. Name of the function or code that ran
#' @param params list. Parameters, as deparsed strings
#' @param status character. One of "ok", "error", "unattributed"
#' @param diff list. Manifest delta from [manifestDiff()]
#' @param error character. Error message, when `status = "error"`
#' @param description character. Suffix for the step name
#' @returns giotto object
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' g <- recordGiottoStep(g, fn = "manual edit", status = "unattributed")
#' tail(names(objHistory(g)), 1)
#' @export
recordGiottoStep <- function(gobject,
    fn = NA_character_,
    params = list(),
    status = c("ok", "error", "unattributed"),
    diff = NULL,
    error = NULL,
    description = NULL) {
    status <- match.arg(status)
    description <- description %||% paste0("_", status)

    parameters_list <- gobject@parameters
    update_name <- paste0(length(parameters_list), description)

    entry <- vapply(params, function(p) paste(as.character(p), collapse = " "),
        character(1L)
    )
    if (length(entry) == 0L) entry <- character(0)
    class(entry) <- c("ghistory_item", "character")
    attr(entry, "step") <- .ghistory_step(
        step_id = update_name, fn = fn, params = params,
        status = status, diff = diff, error = error
    )

    parameters_list[[update_name]] <- entry
    class(parameters_list) <- "ghistory"
    gobject@parameters <- parameters_list
    gobject
}

#' @title Structured giotto object history
#' @name ghistory_records
#' @description
#' The `@parameters` history as structured records: `step_id`, `fn`, `params`,
#' `timestamp`, `seed`, `status`, `error` and `diff`. Entries written before
#' structured records existed are reported with `status = "ok"` and their
#' recorded arguments, so old objects still read.
#' @param object giotto object
#' @returns list of records
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' str(head(ghistory_records(g), 1))
#' @export
ghistory_records <- function(object) {
    p <- object@parameters
    if (length(p) == 0L) return(list())

    lapply(names(p), function(nm) {
        item <- p[[nm]]
        step <- attr(item, "step")
        if (!is.null(step)) return(step)
        # legacy entry: no structured record was written at the time
        list(
            step_id = nm,
            fn = sub("^[0-9]+_", "", nm),
            params = as.list(unclass(item)),
            timestamp = NULL,
            seed = NULL,
            status = "ok",
            error = NULL,
            diff = NULL
        )
    })
}

#' @title Giotto object history as NDJSON
#' @name objHistory_ndjson
#' @description
#' Serialize [ghistory_records()] as newline-delimited JSON, one operation per
#' line. Append-only by construction: writing later steps never rewrites
#' earlier lines.
#' @param object giotto object
#' @param file character. Optional path to write to. When `NULL` the text is
#' returned.
#' @returns character scalar, or the file path invisibly when `file` is given
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' cat(substr(objHistory_ndjson(g), 1, 120))
#' @export
objHistory_ndjson <- function(object, file = NULL) {
    GiottoUtils::package_check("jsonlite", repository = "CRAN:jsonlite")

    recs <- ghistory_records(object)
    lines <- vapply(recs, function(r) {
        as.character(jsonlite::toJSON(
            .manifest_json_prep(r),
            auto_unbox = TRUE, null = "null", na = "null"
        ))
    }, character(1L))

    if (is.null(file)) return(paste0(paste(lines, collapse = "\n"), "\n"))

    writeLines(lines, con = file)
    invisible(file)
}

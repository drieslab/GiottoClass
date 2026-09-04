#' @include generics.R
NULL

#### giotto object manifest ####

# The manifest is a machine-readable inventory of what a giotto object
# contains: a nested description mirroring the object's own
# spat_unit x feat_type nesting, with cheap per-leaf statistics and optional
# content fingerprints. It is DERIVED, never stored, so it cannot go stale.
#
# DESIGN RULE (do not relax): every accessor call is individually guarded. An
# accessor that errors degrades one field to NULL and records its path in
# `warnings`; it never errors the manifest. A state report that crashes is
# worse than no state report.

.MANIFEST_SCHEMA_VERSION <- "0.1.0"

# slots inventoried, in emission order. `cell_ID`/`feat_ID` are handled
# separately since they hold plain character vectors rather than subobjects.
.MANIFEST_SLOTS <- c(
    "expression", "spatial_locs", "spatial_info", "feat_info",
    "cell_metadata", "feat_metadata", "dimension_reduction", "nn_network",
    "spatial_network", "spatial_grid", "spatial_enrichment", "images"
)

## guarded field collection ####

# Collector shared by every leaf describer of a single manifest build.
# `wenv` accumulates dotted paths of fields that could not be read.
.manifest_wenv <- function() {
    e <- new.env(parent = emptyenv())
    e$warnings <- character()
    e
}

# Evaluate `f()`, degrading to NULL and recording `path` when it ERRORS.
# A field that reads cleanly and is simply absent (an empty slot, a subobject
# with no provenance) returns NULL without being reported: `warnings` means
# "could not be read", not "is empty".
.mfield <- function(wenv, path, f) {
    ok <- TRUE
    v <- tryCatch(f(), error = function(e) {
        ok <<- FALSE
        NULL
    })
    if (!ok) .mwarn(wenv, path)
    v
}

.mwarn <- function(wenv, path) {
    if (!is.null(wenv)) wenv$warnings <- c(wenv$warnings, path)
    invisible(NULL)
}

## uid ####

# Object uid, minted at creation so it survives renames and copies. Uses
# tempfile() for entropy rather than the RNG, so a user's seed is untouched
# and results stay reproducible.
.make_gobject_uid <- function() {
    rand <- sub("^gid", "", basename(tempfile("gid")))
    sprintf(
        "g-%s-%s",
        format(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y%m%d%H%M%S"), rand
    )
}

# Objects serialized before the `versions` slot existed are repaired by
# updateGiottoObject(), which runs during initialize(). Until then the slot is
# genuinely absent, so both accessors treat that as "no uid" rather than
# erroring - a legacy object must still be loadable.
.gobject_uid <- function(gobject) {
    if (!methods::.hasSlot(gobject, "versions")) return(NULL)
    v <- slot(gobject, "versions")
    if (!is.list(v)) return(NULL)
    v$uid
}

`.gobject_uid<-` <- function(gobject, value) {
    if (!methods::.hasSlot(gobject, "versions")) return(gobject)
    v <- slot(gobject, "versions")
    if (!is.list(v)) v <- list()
    v$uid <- value
    slot(gobject, "versions") <- v
    gobject
}

# mint on first sight; a uid already present is never replaced
.gobject_uid_init <- function(gobject) {
    if (is.null(.gobject_uid(gobject))) {
        .gobject_uid(gobject) <- .make_gobject_uid()
    }
    gobject
}

#' @name objManifest
#' @title Giotto object manifest
#' @description
#' Machine-readable inventory of a `giotto` object's contents: identity,
#' a summary block, and a slot-by-slot description nested the same way the
#' object is (`spat_unit` x `feat_type` x name). Derived on demand, so it is
#' always current.
#'
#' Companion to [objHistory()], which records *why* an object looks the way it
#' does. The manifest records *what it is*. Object state is never reconstructed
#' by replaying history.
#'
#' Every accessor used is individually guarded: a field that cannot be read
#' becomes `NULL` and its path is listed in the `warnings` element, rather than
#' erroring the manifest.
#' @param x giotto object
#' @param level character. `"summary"` (default) omits fingerprints.
#' `"full"` adds them, which is what detects an operation that overwrites a
#' matrix or a column in place - a change `"summary"` cannot see, since the
#' shape and the names are identical on both sides. Sampled fingerprints cost
#' little: they read a fixed number of values regardless of object size.
#' @param fingerprint character. `"none"`, `"sample"` (hash of a deterministic
#' fixed-stride slice of the content) or `"full"` (hash of all content).
#' Defaults to `"none"` for `level = "summary"` and `"sample"` for
#' `level = "full"`. Overrides `level` when given.
#'
#' Numbers are compared to 12 significant digits, so a fingerprint identifies
#' values rather than bit patterns. Re-reading a saved object does not report a
#' change when a storage format has dropped the last bits of a double, and by
#' the same token a difference below roughly 1e-12 relative is not reported at
#' all.
#' @param ... additional params (none implemented)
#' @returns list of class `gmanifest`
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' m <- objManifest(g)
#' names(m$slots)
#' @export
setMethod("objManifest", signature("giotto"), function(
        x, level = c("summary", "full"), fingerprint = NULL, ...) {
    level <- match.arg(level)
    if (is.null(fingerprint)) {
        fingerprint <- if (level == "full") "sample" else "none"
    }
    fingerprint <- match.arg(fingerprint, c("none", "sample", "full"))

    wenv <- .manifest_wenv()

    slots <- list()
    for (sn in .MANIFEST_SLOTS) {
        node <- .mfield(wenv, sn, function() slot(x, sn))
        walked <- .manifest_walk(node, fp = fingerprint, wenv = wenv, path = sn)
        if (!is.null(walked) && length(walked) > 0L) slots[[sn]] <- walked
    }

    ids <- .manifest_ids(x, wenv)
    if (length(ids) > 0L) slots <- c(slots, ids)

    out <- list(
        schema_version = .MANIFEST_SCHEMA_VERSION,
        generated_by = paste0(
            "GiottoClass ", as.character(utils::packageVersion("GiottoClass"))
        ),
        generated_at = format(
            as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"
        ),
        object = .manifest_object_info(x, wenv),
        summary = .manifest_summary(x, wenv),
        slots = slots[order(names(slots))],
        warnings = unique(wenv$warnings)
    )
    class(out) <- c("gmanifest", "list")
    out
})

## identity and summary ####

.manifest_object_info <- function(x, wenv) {
    versions <- .mfield(wenv, "object.versions", function() {
        v <- slot(x, "versions")
        # package_version objects do not survive JSON encoding
        lapply(v, function(i) if (is.null(i)) NULL else as.character(i))
    })
    list(
        uid = .mfield(wenv, "object.uid", function() .gobject_uid(x)),
        class = class(x)[[1]],
        versions = versions[order(names(versions %||% list()))]
    )
}

.manifest_summary <- function(x, wenv) {
    # spatUnit() / featType() fold over every subobject the object holds, so a
    # unit that carries polygons and locations but no expression matrix - a
    # Xenium `nucleus`, say - is reported. Reading `names(@expression)` instead
    # dropped exactly those units while `n_cells` still counted them, leaving
    # the two halves of this block disagreeing.
    su <- .mfield(wenv, "summary.spat_units", function() spatUnit(x))
    ft <- .mfield(wenv, "summary.feat_types", function() featType(x))

    # a unit with no ids is an empty slot entry, not a unit
    nonempty <- function(ids) {
        n <- vapply(ids, length, integer(1L))
        as.list(n[n > 0L])
    }
    n_cells <- .mfield(wenv, "summary.n_cells", function() {
        nonempty(slot(x, "cell_ID"))
    })
    n_features <- .mfield(wenv, "summary.n_features", function() {
        nonempty(slot(x, "feat_ID"))
    })

    filled <- function(sn) {
        v <- tryCatch(slot(x, sn), error = function(e) NULL)
        length(v) > 0L
    }

    list(
        spat_units = su,
        feat_types = ft,
        n_cells = n_cells,
        n_features = n_features,
        has_images = filled("images"),
        has_spatial_info = filled("spatial_info"),
        has_feat_info = filled("feat_info"),
        has_spatial_network = filled("spatial_network"),
        has_dim_reduction = filled("dimension_reduction"),
        has_nn_network = filled("nn_network"),
        has_spatial_enrichment = filled("spatial_enrichment"),
        is_joined = !is.null(tryCatch(slot(x, "join_info"),
            error = function(e) NULL
        )),
        n_history_steps = length(
            tryCatch(slot(x, "parameters"), error = function(e) list())
        )
    )
}

# cell_ID / feat_ID hold plain character vectors keyed by spat_unit /
# feat_type. Report counts rather than the ids themselves: an id vector is
# millions of entries wide and defeats the point of a manifest.
.manifest_ids <- function(x, wenv) {
    out <- list()
    for (sn in c("cell_ID", "feat_ID")) {
        node <- .mfield(wenv, sn, function() slot(x, sn))
        if (is.null(node) || length(node) == 0L) next
        entry <- lapply(node, function(ids) list(n = length(ids)))
        out[[sn]] <- entry[order(names(entry))]
    }
    out
}

## walker ####

# Recurse the plain nested lists of a slot and describe each subobject leaf.
# Nesting keys are the list names, so the manifest mirrors the object exactly
# without hardcoding any slot's nesting depth. Names are sorted at every level
# so output is canonical and diffs are stable.
.manifest_walk <- function(node, fp, wenv, path) {
    if (is.null(node)) return(NULL)
    if (isS4(node) || is.object(node)) {
        return(.manifest_leaf(node, fp = fp, wenv = wenv, path = path))
    }
    if (is.list(node)) {
        nms <- names(node)
        if (is.null(nms)) return(NULL)
        out <- list()
        for (nm in nms) {
            child <- .manifest_walk(
                node[[nm]], fp = fp, wenv = wenv,
                path = paste(path, nm, sep = ".")
            )
            if (!is.null(child)) out[[nm]] <- child
        }
        if (length(out) == 0L) return(NULL)
        return(out[order(names(out))])
    }
    NULL
}

## leaf describers ####

# Internal generic. One method per subobject family; every field guarded.
setGeneric(".manifest_leaf", function(x, fp, wenv, path, ...) {
    standardGeneric(".manifest_leaf")
})

# NULL, or the single NA that the nesting accessors return when a class does
# not carry that piece of information
.is_blank <- function(v) {
    is.null(v) || (length(v) == 1L && is.atomic(v) && is.na(v))
}

# fields shared by all giotto subobjects
.manifest_leaf_base <- function(x, wenv, path) {
    g <- function(nm, f) .mfield(wenv, paste(path, nm, sep = "."), f)
    out <- list(
        class = class(x)[[1]],
        name = g("name", function() objName(x)),
        spat_unit = g("spat_unit", function() spatUnit(x)),
        feat_type = g("feat_type", function() featType(x)),
        # prov() has no ANY fallback, so asking a class that carries no
        # provenance would error and be reported as unreadable
        provenance = if (!methods::is(x, "provData")) {
            NULL
        } else {
            g("provenance", function() {
                p <- prov(x)
                if (is.null(p)) NULL else as.character(p)
            })
        }
    )
    # a subobject that legitimately has no name / unit / type is not a warning,
    # and the NA the accessors return for it is not worth emitting
    out[!vapply(out, .is_blank, logical(1L))]
}

# describe the columns of a data.table: name, type, and distinct count so
# that an added cluster column is visible with its number of levels
.manifest_columns <- function(dt) {
    if (!inherits(dt, "data.frame")) return(NULL)
    lapply(colnames(dt), function(cn) {
        col <- dt[[cn]]
        entry <- list(
            name = cn,
            dtype = class(col)[[1]],
            n_levels = tryCatch(
                as.integer(data.table::uniqueN(col)),
                error = function(e) NULL
            )
        )
        if (is.factor(col)) entry$levels <- as.integer(nlevels(col))
        entry[!vapply(entry, is.null, logical(1L))]
    })
}

.manifest_ext <- function(x) {
    e <- ext(x)
    v <- as.vector(e)
    list(xmin = v[["xmin"]], xmax = v[["xmax"]],
        ymin = v[["ymin"]], ymax = v[["ymax"]])
}

setMethod(".manifest_leaf", signature("ANY"), function(x, fp, wenv, path, ...) {
    list(
        class = class(x)[[1]],
        length = .mfield(wenv, paste(path, "length", sep = "."),
            function() length(x))
    )
})

setMethod(".manifest_leaf", signature("exprData"), function(
        x, fp, wenv, path, ...) {
    g <- function(nm, f) .mfield(wenv, paste(path, nm, sep = "."), f)
    m <- g("exprMat", function() slot(x, "exprMat"))

    out <- .manifest_leaf_base(x, wenv, path)
    out$shape <- g("shape", function() as.integer(dim(x)))
    out$dtype <- if (is.null(m)) NULL else class(m)[[1]]
    if (!is.null(m) && inherits(m, "sparseMatrix")) {
        nnz <- g("nnz", function() length(slot(m, "x")))
        out$sparse <- TRUE
        out$nnz <- nnz
        out$density <- g("density", function() {
            d <- dim(m)
            round(nnz / (as.numeric(d[[1L]]) * as.numeric(d[[2L]])), 6L)
        })
    } else if (!is.null(m)) {
        out$sparse <- FALSE
    }
    out$fingerprint <- .manifest_fp(x, fp, wenv, path)
    out
})

setMethod(".manifest_leaf", signature("metaData"), function(
        x, fp, wenv, path, ...) {
    g <- function(nm, f) .mfield(wenv, paste(path, nm, sep = "."), f)
    dt <- g("metaDT", function() slot(x, "metaDT"))

    out <- .manifest_leaf_base(x, wenv, path)
    out$shape <- g("shape", function() as.integer(dim(x)))
    out$columns <- g("columns", function() .manifest_columns(dt))
    out$col_desc <- g("col_desc", function() {
        cd <- slot(x, "col_desc")
        if (length(cd) == 1L && is.na(cd)) NULL else as.list(cd)
    })
    out$fingerprint <- .manifest_fp(x, fp, wenv, path)
    out
})

setMethod(".manifest_leaf", signature("coordDataDT"), function(
        x, fp, wenv, path, ...) {
    g <- function(nm, f) .mfield(wenv, paste(path, nm, sep = "."), f)
    dt <- g("coordinates", function() slot(x, "coordinates"))

    out <- .manifest_leaf_base(x, wenv, path)
    out$shape <- g("shape", function() as.integer(dim(x)))
    out$columns <- if (is.null(dt)) NULL else colnames(dt)
    out$bbox <- g("bbox", function() .manifest_ext(x))
    out$fingerprint <- .manifest_fp(x, fp, wenv, path)
    out
})

setMethod(".manifest_leaf", signature("dimObj"), function(
        x, fp, wenv, path, ...) {
    g <- function(nm, f) .mfield(wenv, paste(path, nm, sep = "."), f)
    out <- .manifest_leaf_base(x, wenv, path)
    out$reduction <- g("reduction", function() slot(x, "reduction"))
    out$reduction_method <- g("reduction_method",
        function() slot(x, "reduction_method"))
    out$shape <- g("shape", function() {
        as.integer(dim(slot(x, "coordinates")))
    })
    out$fingerprint <- .manifest_fp(x, fp, wenv, path)
    out
})

setMethod(".manifest_leaf", signature("nnData"), function(
        x, fp, wenv, path, ...) {
    g <- function(nm, f) .mfield(wenv, paste(path, nm, sep = "."), f)
    net <- g("network", function() slot(x, "network"))

    out <- .manifest_leaf_base(x, wenv, path)
    out$nn_type <- g("nn_type", function() slot(x, "nn_type"))
    out$n_nodes <- g("n_nodes", function() .net_n_nodes(net))
    out$n_edges <- g("n_edges", function() .net_n_edges(net))
    out$directed <- g("directed", function() {
        if (inherits(net, "igraph")) igraph::is_directed(net) else NULL
    })
    out$fingerprint <- .manifest_fp(x, fp, wenv, path)
    out
})

setMethod(".manifest_leaf", signature("spatNetData"), function(
        x, fp, wenv, path, ...) {
    g <- function(nm, f) .mfield(wenv, paste(path, nm, sep = "."), f)
    net <- g("network", function() slot(x, "network"))

    out <- .manifest_leaf_base(x, wenv, path)
    out$method <- g("method", function() slot(x, "method"))
    out$n_nodes <- g("n_nodes", function() .net_n_nodes(net))
    out$n_edges <- g("n_edges", function() .net_n_edges(net))
    out$has_unfiltered <- !is.null(
        tryCatch(slot(x, "unfiltered"), error = function(e) NULL)
    )
    out$fingerprint <- .manifest_fp(x, fp, wenv, path)
    out
})

setMethod(".manifest_leaf", signature("enrData"), function(
        x, fp, wenv, path, ...) {
    g <- function(nm, f) .mfield(wenv, paste(path, nm, sep = "."), f)
    dt <- g("enrichDT", function() slot(x, "enrichDT"))

    out <- .manifest_leaf_base(x, wenv, path)
    out$method <- g("method", function() slot(x, "method"))
    out$shape <- g("shape", function() as.integer(dim(x)))
    out$columns <- if (is.null(dt)) NULL else colnames(dt)
    out$fingerprint <- .manifest_fp(x, fp, wenv, path)
    out
})

setMethod(".manifest_leaf", signature("spatGridData"), function(
        x, fp, wenv, path, ...) {
    g <- function(nm, f) .mfield(wenv, paste(path, nm, sep = "."), f)
    dt <- g("gridDT", function() slot(x, "gridDT"))

    out <- .manifest_leaf_base(x, wenv, path)
    out$method <- g("method", function() slot(x, "method"))
    out$shape <- if (is.null(dt)) NULL else as.integer(dim(dt))
    out$fingerprint <- .manifest_fp(x, fp, wenv, path)
    out
})

setMethod(".manifest_leaf", signature("terraVectData"), function(
        x, fp, wenv, path, ...) {
    g <- function(nm, f) .mfield(wenv, paste(path, nm, sep = "."), f)
    out <- .manifest_leaf_base(x, wenv, path)
    out$n_geom <- g("n_geom", function() as.integer(nrow(x)))
    out$extent <- g("extent", function() .manifest_ext(x))
    out$crs <- g("crs", function() {
        cr <- terra::crs(slot(x, "spatVector"), describe = TRUE)$name
        if (is.na(cr)) NULL else cr
    })

    if (inherits(x, "giottoPolygon")) {
        out$centroids_cached <- !is.null(
            tryCatch(slot(x, "spatVectorCentroids"), error = function(e) NULL)
        )
        out$overlaps_computed <- g("overlaps_computed", function() {
            o <- slot(x, "overlaps")
            if (is.null(o)) NULL else names(o)
        })
    }
    out$fingerprint <- .manifest_fp(x, fp, wenv, path)
    out
})

# giottoBinPoints carries `featData` and `giottoSubobject` but not
# `terraVectData`, so without its own method it matched only the ANY fallback
# and reported nothing but its class: a binpoints object could be added,
# rewritten or dropped and no diff would say so. Its geometry lives in
# `@spatial` and its detections in `@counts`, which is why the generic
# terra describer does not fit it.
setMethod(".manifest_leaf", signature("giottoBinPoints"), function(
        x, fp, wenv, path, ...) {
    g <- function(nm, f) .mfield(wenv, paste(path, nm, sep = "."), f)
    out <- .manifest_leaf_base(x, wenv, path)
    out$n_geom <- g("n_geom", function() {
        as.integer(terra::nrow(slot(x, "spatial")))
    })
    # one record per (bin, feature) detection, which is what nrow() reports
    out$n_records <- g("n_records", function() as.integer(nrow(x)))
    out$n_bins <- g("n_bins", function() length(slot(x, "bid")))
    out$n_feats <- g("n_feats", function() length(slot(x, "fid")))
    out$compact <- g("compact", function() isTRUE(slot(x, "compact")))
    out$extent <- g("extent", function() .manifest_ext(x))
    out$fingerprint <- .manifest_fp(x, fp, wenv, path)
    out
})

setMethod(".manifest_leaf", signature("giottoLargeImage"), function(
        x, fp, wenv, path, ...) {
    g <- function(nm, f) .mfield(wenv, paste(path, nm, sep = "."), f)
    r <- g("raster_object", function() slot(x, "raster_object"))

    out <- list(
        class = class(x)[[1]],
        name = g("name", function() objName(x)),
        dims = g("dims", function() {
            as.integer(c(terra::ncol(r), terra::nrow(r), terra::nlyr(r)))
        }),
        extent = g("extent", function() .manifest_ext(x)),
        resolution = g("resolution", function() slot(x, "resolution")),
        file_path = g("file_path", function() {
            p <- slot(x, "file_path")
            if (is.null(p) || all(is.na(p))) NULL else as.character(p)
        }),
        on_disk = g("on_disk", function() {
            any(nzchar(terra::sources(r)))
        })
    )
    if (inherits(x, "giottoAffineImage")) {
        out$affine <- g("affine", function() {
            as.numeric(slot(slot(x, "affine"), "affine"))
        })
    }
    out$fingerprint <- .manifest_fp(x, fp, wenv, path)
    out[!vapply(out, is.null, logical(1L))]
})

setMethod(".manifest_leaf", signature("giottoImage"), function(
        x, fp, wenv, path, ...) {
    g <- function(nm, f) .mfield(wenv, paste(path, nm, sep = "."), f)
    out <- list(
        class = class(x)[[1]],
        name = g("name", function() objName(x)),
        resolution = g("resolution", function() slot(x, "resolution")),
        file_path = g("file_path", function() {
            p <- slot(x, "file_path")
            if (is.null(p) || all(is.na(p))) NULL else as.character(p)
        })
    )
    out[!vapply(out, is.null, logical(1L))]
})

## fingerprints ####

# Number of values sampled per object at fingerprint = "sample".
.MANIFEST_FP_N <- 1000L

# Significant digits a fingerprint compares numbers to.
#
# A fingerprint answers "are these the same values", not "are these the same
# bit patterns". Serialization formats lose the last bits of a double -
# shapefile DBF stores numerics as fixed-width text, so a coordinate attribute
# comes back differing by ~5e-16 relative - and hashing that difference reports
# a reloaded object as modified in exactly the same words as a real edit. Two
# decimal digits of slack removes that without approaching anything
# analytically meaningful: a change of 1e-9 relative still registers.
.MANIFEST_FP_DIGITS <- 12L

.fp_num <- function(x) {
    if (is.double(x)) signif(x, .MANIFEST_FP_DIGITS) else x
}

# Deterministic fixed-stride slice. No RNG, so the user's seed is untouched
# and two calls on unchanged content always agree.
.fp_stride <- function(v, k = .MANIFEST_FP_N) {
    n <- length(v)
    if (n == 0L) return(v)
    if (is.infinite(k) || n <= k) return(v)
    v[unique(as.integer(seq.int(1L, n, length.out = k)))]
}

# A digest of NULL is a constant, so hashing "nothing" would make every
# object whose content could not be read look identical to every other - and
# permanently unchanged. Absent is reported as absent.
.fp_hash <- function(parts) {
    if (is.null(parts)) return(NULL)
    digest::digest(parts, algo = "xxhash64")
}

.fp_n <- function(fp) if (identical(fp, "full")) Inf else .MANIFEST_FP_N

# Sampled values of an in-memory matrix. NULL for representations that would
# have to be materialised (DelayedArray, BPCells, on-disk stores) - those
# degrade to no fingerprint rather than pulling gigabytes through memory.
#
# Never flattens the matrix first: `as.vector()` on a dense expression matrix
# copies the whole thing to sample a thousand values from it. Matrix classes
# expose their values as an `x` slot (sparse and dense alike); a base matrix
# is indexed linearly at the strided positions.
.fp_matrix_sample <- function(m, k) {
    if (methods::.hasSlot(m, "x")) {
        return(.fp_num(.fp_stride(slot(m, "x"), k)))
    }
    if (is.matrix(m)) {
        n <- length(m)
        if (n == 0L) return(numeric(0))
        idx <- if (is.infinite(k) || n <= k) {
            seq_len(n)
        } else {
            unique(as.integer(seq.int(1L, n, length.out = k)))
        }
        return(.fp_num(m[idx]))
    }
    NULL
}

.fp_dt <- function(dt, k) {
    if (!inherits(dt, "data.frame")) return(NULL)
    list(
        cols = colnames(dt),
        n = nrow(dt),
        # stride first: converting a whole column to sample a thousandth of it
        # is work thrown away
        vals = lapply(dt, function(col) {
            as.character(.fp_num(.fp_stride(col, k)))
        })
    )
}

# Dispatcher used by the leaf describers. Never errors: a missing {digest},
# an unfingerprintable representation, or an accessor failure all degrade to
# NULL with the path recorded in `warnings`.
.manifest_fp <- function(x, fp, wenv, path) {
    if (identical(fp, "none")) return(NULL)
    if (!requireNamespace("digest", quietly = TRUE)) {
        .mwarn(wenv, paste(path, "fingerprint", sep = "."))
        return(NULL)
    }
    .mfield(wenv, paste(path, "fingerprint", sep = "."), function() {
        .fingerprint(x, fp = fp)
    })
}

setGeneric(".fingerprint", function(x, fp, ...) standardGeneric(".fingerprint"))

setMethod(".fingerprint", signature("ANY"), function(x, fp, ...) NULL)

setMethod(".fingerprint", signature("exprData"), function(x, fp, ...) {
    m <- slot(x, "exprMat")
    vals <- .fp_matrix_sample(m, .fp_n(fp))
    if (is.null(vals)) return(NULL)
    .fp_hash(list(
        dim = dim(m),
        rn = rownames(m), cn = colnames(m),
        vals = vals
    ))
})

setMethod(".fingerprint", signature("metaData"), function(x, fp, ...) {
    .fp_hash(.fp_dt(slot(x, "metaDT"), .fp_n(fp)))
})

setMethod(".fingerprint", signature("coordDataDT"), function(x, fp, ...) {
    .fp_hash(.fp_dt(slot(x, "coordinates"), .fp_n(fp)))
})

setMethod(".fingerprint", signature("dimObj"), function(x, fp, ...) {
    co <- slot(x, "coordinates")
    .fp_hash(list(
        dim = dim(co),
        rn = rownames(co), cn = colnames(co),
        vals = .fp_matrix_sample(co, .fp_n(fp))
    ))
})

# A network slot holds an igraph since 0.6.0, but objects saved before the
# migration still hold a data.table of edges. Both carriers are described the
# same way, so a migrated object and an unmigrated one do not read as
# different kinds of thing.
.net_n_nodes <- function(net) {
    if (is.null(net)) return(NULL)
    if (inherits(net, "igraph")) return(as.integer(igraph::vcount(net)))
    NULL
}

.net_n_edges <- function(net) {
    if (is.null(net)) return(NULL)
    if (inherits(net, "igraph")) return(as.integer(igraph::ecount(net)))
    if (inherits(net, "data.frame")) return(as.integer(nrow(net)))
    NULL
}

.fp_network <- function(net, k) {
    if (is.null(net)) return(NULL)
    if (inherits(net, "igraph")) {
        el <- igraph::as_edgelist(net, names = TRUE)
        return(list(
            v = igraph::vcount(net), e = igraph::ecount(net),
            edges = .fp_stride(apply(el, 1L, paste, collapse = ">"), k)
        ))
    }
    .fp_dt(net, k)
}

setMethod(".fingerprint", signature("nnData"), function(x, fp, ...) {
    .fp_hash(.fp_network(slot(x, "network"), .fp_n(fp)))
})

setMethod(".fingerprint", signature("spatNetData"), function(x, fp, ...) {
    .fp_hash(.fp_network(slot(x, "network"), .fp_n(fp)))
})

setMethod(".fingerprint", signature("enrData"), function(x, fp, ...) {
    .fp_hash(.fp_dt(slot(x, "enrichDT"), .fp_n(fp)))
})

setMethod(".fingerprint", signature("spatGridData"), function(x, fp, ...) {
    .fp_hash(.fp_dt(slot(x, "gridDT"), .fp_n(fp)))
})

# terra objects are external pointers and do not hash stably, so hash a
# canonical extraction instead: geometry is subset FIRST, then materialised,
# so a 3M-point object never pulls all its coordinates into memory.
setMethod(".fingerprint", signature("terraVectData"), function(x, fp, ...) {
    sv <- slot(x, "spatVector")
    if (is.null(sv)) return(NULL)
    n <- terra::nrow(sv)
    k <- .fp_n(fp)
    idx <- if (is.infinite(k) || n <= k) {
        seq_len(n)
    } else {
        unique(as.integer(seq.int(1L, n, length.out = k)))
    }
    sub <- sv[idx]
    .fp_hash(list(
        n = n,
        ext = .fp_num(as.vector(terra::ext(sv))),
        crds = .fp_canonical_crds(sub),
        att = lapply(terra::values(sub), function(cc) as.character(.fp_num(cc)))
    ))
})

# Coordinates as a canonical multiset: same vertices in the same order
# regardless of how the geometry happens to be laid out.
#
# Writing a SpatVector to a shapefile and reading it back returns the same
# shapes with their ring winding and start vertex normalised, so a positional
# hash of the coordinates reports every saved-and-reloaded object as modified.
# Ordering the vertices removes that false positive. Vertex multiplicity is
# preserved, so a moved, added or dropped vertex still changes the hash; what
# is given up is sensitivity to a rearrangement that leaves the whole
# coordinate multiset intact, which `n`, the extent and the attributes are
# there to catch.
.fp_canonical_crds <- function(sv) {
    cr <- .fp_num(terra::crds(sv))
    if (nrow(cr) == 0L) return(cr)
    cr[order(cr[, 1L], cr[, 2L]), , drop = FALSE]
}

# The counts table is what changes when a binpoints object is rewritten, and
# it is already an in-memory data.table - so hash that plus the id vectors and
# the extent, and leave `@spatial` alone. Materialising the geometry would be
# the expensive half of the object for no added signal.
setMethod(".fingerprint", signature("giottoBinPoints"), function(x, fp, ...) {
    k <- .fp_n(fp)
    .fp_hash(list(
        counts = .fp_dt(slot(x, "counts"), k),
        bid = .fp_stride(slot(x, "bid"), k),
        fid = .fp_stride(slot(x, "fid"), k),
        ext = tryCatch(.fp_num(as.vector(ext(x))), error = function(e) NULL)
    ))
})

# An image's payload is its file. Hash the source identity rather than the
# pixels, matching how GiottoDisk hashes a delayed representation.
setMethod(".fingerprint", signature("giottoLargeImage"), function(x, fp, ...) {
    r <- slot(x, "raster_object")
    src <- terra::sources(r)
    src <- src[nzchar(src)]
    if (length(src) == 0L) return(NULL)
    info <- file.info(src)
    .fp_hash(list(
        src = src,
        size = info$size,
        mtime = as.character(info$mtime),
        ext = .fp_num(as.vector(terra::ext(r)))
    ))
})

## serialization ####

# Explicit schema rule: NA / NaN / Inf are encoded as strings, not as JSON
# null (jsonlite's default), so a missing value and a not-a-number stay
# distinguishable on the far side of the contract.
.manifest_json_prep <- function(x) {
    if (is.list(x)) return(lapply(x, .manifest_json_prep))
    if (is.factor(x)) x <- as.character(x)
    if (is.numeric(x)) {
        bad <- !is.finite(x)
        if (any(bad)) {
            out <- as.character(x)
            out[is.na(x) & !is.nan(x)] <- "NA"
            out[is.nan(x)] <- "NaN"
            out[!is.na(x) & is.infinite(x) & x > 0] <- "Inf"
            out[!is.na(x) & is.infinite(x) & x < 0] <- "-Inf"
            return(out)
        }
        return(x)
    }
    if (is.character(x) && anyNA(x)) {
        x[is.na(x)] <- "NA"
        return(x)
    }
    x
}

#' @name objManifest_json
#' @title Giotto object manifest as JSON
#' @description
#' Serialize the manifest from [objManifest()]. Keys are emitted in canonical
#' (sorted) order and `NA`/`NaN`/`Inf` are encoded as the strings `"NA"`,
#' `"NaN"`, `"Inf"` and `"-Inf"` by schema rule.
#' @param x giotto object or a `gmanifest` from [objManifest()]
#' @param file character. Optional path to write to. When `NULL` (default) the
#' JSON is returned as a character scalar.
#' @param pretty logical. Whether to indent the output
#' @param ... additional params passed to [objManifest()] when `x` is a
#' `giotto` object
#' @returns character scalar of JSON, or the file path, invisibly, when
#' `file` is given
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' cat(substr(objManifest_json(g), 1, 200))
#' @export
objManifest_json <- function(x, file = NULL, pretty = TRUE, ...) {
    GiottoUtils::package_check("jsonlite", repository = "CRAN:jsonlite")

    m <- if (inherits(x, "gmanifest")) x else objManifest(x, ...)
    txt <- jsonlite::toJSON(
        .manifest_json_prep(unclass(m)),
        auto_unbox = TRUE, null = "null", na = "null", pretty = pretty,
        digits = NA
    )
    if (is.null(file)) return(as.character(txt))

    writeLines(as.character(txt), con = file)
    invisible(file)
}

# count of described leaves under a manifest slot
.manifest_n_leaves <- function(node) {
    if (!is.list(node)) return(0L)
    # `n` rather than `class` identifies the cell_ID / feat_ID leaves
    if (!is.null(node[["class"]]) || !is.null(node[["n"]])) return(1L)
    sum(vapply(node, .manifest_n_leaves, integer(1L)))
}

#' @export
#' @keywords internal
print.gmanifest <- function(x, ...) {
    cat(sprintf("<gmanifest> schema %s\n", x$schema_version))
    cat(sprintf("  uid: %s\n", x$object$uid %||% "<none>"))
    s <- x$summary
    cat(sprintf(
        "  spat_units: %s | feat_types: %s\n",
        paste(s$spat_units, collapse = ", "),
        paste(s$feat_types, collapse = ", ")
    ))
    for (sn in names(x$slots)) {
        cat(sprintf("  %s: %d\n", sn, .manifest_n_leaves(x$slots[[sn]])))
    }
    if (length(x$warnings)) {
        cat(sprintf(
            "  unreadable fields: %s\n", paste(x$warnings, collapse = ", ")
        ))
    }
    invisible(x)
}

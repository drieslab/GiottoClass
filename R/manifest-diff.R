#' @include manifest.R
NULL

#### manifest diffs ####

# PURE: manifest in, manifest out. No giotto object is ever touched here, so
# this logic is unit-testable with synthetic manifests and cannot be broken by
# an accessor change.

# human-readable label per slot, used when phrasing a diff
.MANIFEST_SLOT_LABELS <- c(
    expression = "expression",
    spatial_locs = "spatial locations",
    spatial_info = "polygons",
    feat_info = "points",
    cell_metadata = "cell metadata",
    feat_metadata = "feature metadata",
    dimension_reduction = "dim reduction",
    nn_network = "NN network",
    spatial_network = "spatial network",
    spatial_grid = "spatial grid",
    spatial_enrichment = "spatial enrichment",
    images = "image",
    cell_ID = "cell IDs",
    feat_ID = "feature IDs"
)

# Flatten a manifest's `slots` into leaves keyed by dotted path, e.g.
# "expression.cell.rna.normalized". A node is a leaf once it carries a
# `class` field (or, for the id slots, an `n` field).
.manifest_flatten <- function(node, prefix = character()) {
    if (!is.list(node)) return(list())
    # `[[` rather than `$`: partial matching would make the `slots` list itself
    # look like a leaf, because `slots$n` resolves to `nn_network`
    if (!is.null(node[["class"]]) || !is.null(node[["n"]])) {
        # Carry the final key alongside the joined path. Splitting the path
        # back apart loses any key that itself contains a dot, and Giotto
        # names routinely do - an `sNN.pca` network read as "pca".
        attr(node, "key") <- if (length(prefix)) prefix[[length(prefix)]] else ""
        out <- list(node)
        names(out) <- paste(prefix, collapse = ".")
        return(out)
    }
    out <- list()
    for (nm in names(node)) {
        out <- c(out, .manifest_flatten(node[[nm]], c(prefix, nm)))
    }
    out
}

.manifest_slot_of <- function(path) sub("\\..*$", "", path)

.manifest_label <- function(path) {
    slot <- .manifest_slot_of(path)
    .MANIFEST_SLOT_LABELS[[slot]] %||% slot
}

# trailing key: the name a user recognises ("normalized", "pca.RNA")
.manifest_leafname <- function(path) sub("^.*\\.", "", path)

# The key a leaf sits under - which is where a reader would go looking for it.
# Recorded during flattening; the path-tail fallback covers a leaf that reached
# here without one.
.manifest_display_name <- function(leaf, path) {
    key <- attr(leaf, "key")
    if (is.null(key) || !nzchar(key)) .manifest_leafname(path) else key
}

# columns of a metadata leaf, as a named vector of level counts
.manifest_colmap <- function(leaf) {
    cols <- leaf$columns
    if (is.null(cols) || length(cols) == 0L) {
        return(stats::setNames(integer(0), character(0)))
    }
    if (is.character(cols)) {
        return(stats::setNames(rep(NA_integer_, length(cols)), cols))
    }
    nms <- vapply(cols, function(cc) cc$name %||% NA_character_, character(1L))
    lv <- vapply(cols, function(cc) {
        as.integer(cc$n_levels %||% NA_integer_)
    }, integer(1L))
    stats::setNames(lv, nms)
}

# Is a level count worth saying out loud?
#
# "leiden_clus (14 levels)" is the whole point of carrying the count;
# "total_expr (1428 levels)" over 1522 cells says only that the column is
# continuous, which the name already told you. The count stays in the manifest
# either way - this governs the sentence, not the data.
.manifest_levels_informative <- function(n, nrow = NULL) {
    if (is.null(n) || is.na(n)) return(FALSE)
    if (n <= 50L) return(TRUE)
    if (is.null(nrow) || is.na(nrow) || nrow <= 0L) return(FALSE)
    n < 0.1 * nrow
}

# rows of the table a metadata leaf describes, for the test above
.manifest_leaf_nrow <- function(leaf) {
    shp <- leaf[["shape"]]
    if (is.null(shp) || length(shp) == 0L) return(NULL)
    as.integer(shp[[1L]])
}

# "name (N levels)" when the count means something, otherwise just "name"
.manifest_col_label <- function(nm, n, nrow) {
    if (.manifest_levels_informative(n, nrow)) {
        sprintf("%s (%s levels)", nm, n)
    } else {
        nm
    }
}

# fields whose change is worth reporting; everything else is noise
.MANIFEST_CMP_FIELDS <- c(
    "shape", "dtype", "sparse", "nnz", "n_geom", "n_nodes", "n_edges",
    "n_points", "n", "method", "reduction_method", "nn_type", "bbox",
    "extent", "fingerprint", "columns", "provenance", "on_disk"
)

#' @name manifestDiff
#' @title Diff two giotto manifests
#' @description
#' Compare two manifests from [objManifest()] and report what changed, both as
#' data and as a single human-readable sentence. Pure: it reads manifests only,
#' never a `giotto` object.
#'
#' This is what an execution tool attaches to each result so a model, a critic
#' or a scorer knows what an operation actually did, rather than inferring it
#' from printed console output.
#' @param before `gmanifest` or `NULL` (treated as "object did not exist")
#' @param after `gmanifest`
#' @returns list with `changed` (logical), `summary` (character scalar) and
#' `detail` (added / removed / modified / object)
#' @examples
#' g <- GiottoData::loadGiottoMini("visium")
#'
#' before <- objManifest(g)
#' g <- subsetGiotto(g, cell_ids = head(spatIDs(g), 100))
#' manifestDiff(before, objManifest(g))$summary
#' @export
manifestDiff <- function(before, after) {
    if (is.null(after)) {
        return(list(
            changed = TRUE, summary = "object removed",
            detail = list(object = list(event = "removed"))
        ))
    }
    if (is.null(before)) {
        s <- after$summary
        return(list(
            changed = TRUE,
            summary = sprintf(
                "created (%s cells x %s features)",
                paste(unlist(s$n_cells), collapse = "/"),
                paste(unlist(s$n_features), collapse = "/")
            ),
            detail = list(object = list(event = "created"))
        ))
    }

    fb <- .manifest_flatten(before$slots)
    fa <- .manifest_flatten(after$slots)

    added <- setdiff(names(fa), names(fb))
    removed <- setdiff(names(fb), names(fa))
    shared <- intersect(names(fa), names(fb))

    modified <- list()
    for (p in shared) {
        fields <- character()
        for (f in .MANIFEST_CMP_FIELDS) {
            if (identical(f, "columns")) {
                # compare as name -> level count, so a reordered column list
                # is not reported as a change while a changed level count is
                cb <- .manifest_colmap(fb[[p]])
                ca <- .manifest_colmap(fa[[p]])
                if (!identical(cb[sort(names(cb))], ca[sort(names(ca))])) {
                    fields <- c(fields, f)
                }
                next
            }
            if (!identical(fb[[p]][[f]], fa[[p]][[f]])) fields <- c(fields, f)
        }
        if (length(fields)) {
            modified[[p]] <- list(
                fields = fields,
                before = fb[[p]][fields],
                after = fa[[p]][fields]
            )
        }
    }

    phrases <- .manifest_diff_phrases(
        fb, fa, added, removed, modified,
        counts = .manifest_count_changes(before, after)
    )

    obj <- list()
    if (!identical(before$object$uid, after$object$uid)) {
        obj$uid <- list(before = before$object$uid, after = after$object$uid)
        phrases <- c(phrases, "object identity changed")
    }

    detail <- list(
        added = fa[added],
        removed = removed,
        modified = modified,
        counts = .manifest_count_changes(before, after),
        object = obj
    )
    changed <- length(added) > 0L || length(removed) > 0L ||
        length(modified) > 0L || length(obj) > 0L

    list(
        changed = changed,
        summary = if (length(phrases)) {
            paste(phrases, collapse = " | ")
        } else {
            "no state change"
        },
        detail = detail
    )
}

# Object-level count changes, read from the summary block rather than inferred
# from leaves. A filter or subset moves every object at once; reporting it once
# here is what keeps the rendering compact.
.manifest_count_changes <- function(before, after) {
    out <- list()
    for (f in c("n_cells", "n_features")) {
        b <- before$summary[[f]] %||% list()
        a <- after$summary[[f]] %||% list()
        for (k in union(names(b), names(a))) {
            if (!identical(b[[k]], a[[k]])) {
                out[[paste(f, k, sep = ".")]] <- list(
                    what = f, key = k, before = b[[k]], after = a[[k]]
                )
            }
        }
    }
    out
}

# Build the one-sentence rendering. Grouped by slot so a step that adds one
# matrix reads "expression added: normalized" rather than a path dump.
#
# When the object's cell or feature counts moved, every leaf's shape moved with
# them. Enumerating all of those turns one subset into sixteen clauses, so the
# count change is stated once and the resized leaves are collapsed to a tally.
# `detail` still carries every leaf.
.manifest_diff_phrases <- function(fb, fa, added, removed, modified,
    counts = list()) {
    phrases <- character()

    for (cc in counts) {
        phrases <- c(phrases, sprintf(
            "%s [%s]: %s -> %s",
            if (identical(cc$what, "n_cells")) "cells" else "features",
            cc$key, cc$before %||% "?", cc$after %||% "?"
        ))
    }
    resized <- 0L

    group <- function(paths) {
        if (!length(paths)) return(list())
        split(paths, vapply(paths, .manifest_slot_of, character(1L)))
    }

    for (grp in names(group(added))) {
        paths <- group(added)[[grp]]
        phrases <- c(phrases, sprintf(
            "%s added: %s", .MANIFEST_SLOT_LABELS[[grp]] %||% grp,
            paste(vapply(paths, function(pp) {
                .manifest_display_name(fa[[pp]], pp)
            }, character(1L)), collapse = ", ")
        ))
    }
    for (grp in names(group(removed))) {
        paths <- group(removed)[[grp]]
        phrases <- c(phrases, sprintf(
            "%s removed: %s", .MANIFEST_SLOT_LABELS[[grp]] %||% grp,
            paste(vapply(paths, function(pp) {
                .manifest_display_name(fb[[pp]], pp)
            }, character(1L)), collapse = ", ")
        ))
    }

    for (p in names(modified)) {
        fields <- modified[[p]]$fields
        label <- .manifest_label(p)

        # Tracks whether this leaf has been accounted for - by a phrase or by
        # the resize tally. A modified leaf that goes unaccounted reads as
        # "no state change", which is the one thing a diff must never say
        # when something did change.
        accounted <- FALSE

        # metadata columns: name the added columns and their level counts,
        # since that is how a clustering result becomes visible

        if ("columns" %in% fields) {
            cb <- .manifest_colmap(fb[[p]])
            ca <- .manifest_colmap(fa[[p]])
            nrow_a <- .manifest_leaf_nrow(fa[[p]])
            new_cols <- setdiff(names(ca), names(cb))
            gone_cols <- setdiff(names(cb), names(ca))
            if (length(new_cols)) {
                phrases <- c(phrases, sprintf(
                    "%s added: %s", label,
                    paste(vapply(new_cols, function(cc) {
                        .manifest_col_label(cc, ca[[cc]], nrow_a)
                    }, character(1L)), collapse = ", ")
                ))
            }
            if (length(gone_cols)) {
                phrases <- c(phrases, sprintf(
                    "%s removed: %s", label,
                    paste(gone_cols, collapse = ", ")
                ))
            }
            # a column that stayed but was re-levelled: re-clustering writes
            # its result into the existing column, and the count is the whole
            # story ("leiden_clus 5 -> 14 levels")
            kept <- intersect(names(cb), names(ca))
            relev <- kept[vapply(kept, function(cc) {
                !identical(cb[[cc]], ca[[cc]])
            }, logical(1L))]
            # A subset re-levels every column at once. That is the count
            # change already stated, not news about the columns, so it joins
            # the resize tally instead of listing itself.
            if (length(relev)) {
                if (length(counts)) {
                    resized <- resized + 1L
                } else {
                    phrases <- c(phrases, sprintf(
                        "%s changed: %s", label,
                        paste(vapply(relev, function(cc) {
                            keep <- .manifest_levels_informative(
                                ca[[cc]], nrow_a
                            )
                            if (!keep) return(cc)
                            sprintf("%s (%s -> %s levels)", cc,
                                cb[[cc]], ca[[cc]])
                        }, character(1L)), collapse = ", ")
                    ))
                }
            }
            accounted <- length(new_cols) > 0L || length(gone_cols) > 0L ||
                length(relev) > 0L
        }

        # a column change already implies the shape change; saying both is
        # noise
        if ("shape" %in% fields && !("columns" %in% fields)) {
            accounted <- TRUE
            if (length(counts)) {
                resized <- resized + 1L
            } else {
                sb <- modified[[p]]$before$shape
                sa <- modified[[p]]$after$shape
                phrases <- c(phrases, sprintf(
                    "%s %s dimensions %s -> %s",
                    label, .manifest_display_name(fa[[p]], p),
                    paste(sb, collapse = " x "), paste(sa, collapse = " x ")
                ))
            }
        }

        if ("n" %in% fields) {
            accounted <- TRUE
            if (!length(counts)) {
                phrases <- c(phrases, sprintf(
                    "%s %s: %s -> %s", label,
                    .manifest_display_name(fa[[p]], p),
                    modified[[p]]$before$n, modified[[p]]$after$n
                ))
            }
        }

        # Anything else the leaf changed - most often just the fingerprint,
        # meaning a step overwrote content in place while leaving every shape
        # and name identical.
        if (!accounted) {
            rest <- setdiff(fields, c("shape", "n"))
            phrases <- c(phrases, sprintf(
                "%s %s modified (%s)", label,
                .manifest_display_name(fa[[p]], p),
                paste(rest, collapse = ", ")
            ))
        }
    }

    if (resized > 0L) {
        phrases <- c(phrases, sprintf("%d objects resized", resized))
    }

    phrases
}

# Diff logic is PURE: manifests in, diff out. These tests build manifests by
# hand and never touch a giotto object, so a change to any accessor cannot
# break them - and they still pass when Giotto is unavailable.

.mf <- function(slots = list(), n_cells = list(cell = 100L),
    n_features = list(rna = 50L), uid = "g-test") {
    structure(
        list(
            schema_version = "0.1.0",
            object = list(uid = uid, class = "giotto"),
            summary = list(n_cells = n_cells, n_features = n_features),
            slots = slots,
            warnings = character()
        ),
        class = c("gmanifest", "list")
    )
}

.expr <- function(name, shape = c(50L, 100L), fp = "aaaa") {
    list(class = "exprObj", name = name, shape = shape, dtype = "dgCMatrix",
        sparse = TRUE, fingerprint = fp)
}

.meta <- function(cols, shape = c(100L, length(cols))) {
    list(
        class = "cellMetaObj", shape = shape,
        columns = lapply(names(cols), function(cc) {
            list(name = cc, dtype = "numeric", n_levels = cols[[cc]])
        })
    )
}

describe("manifestDiff", {
    it("reports no change for identical manifests", {
        m <- .mf(list(expression = list(cell = list(rna = list(
            raw = .expr("raw")
        )))))
        d <- manifestDiff(m, m)
        expect_false(d$changed)
        expect_identical(d$summary, "no state change")
    })

    it("reports an added leaf by name", {
        b <- .mf(list(expression = list(cell = list(rna = list(
            raw = .expr("raw")
        )))))
        a <- .mf(list(expression = list(cell = list(rna = list(
            raw = .expr("raw"), normalized = .expr("normalized")
        )))))
        d <- manifestDiff(b, a)
        expect_true(d$changed)
        expect_match(d$summary, "expression added: normalized")
        expect_named(d$detail$added, "expression.cell.rna.normalized")
    })

    it("reports a removed leaf", {
        b <- .mf(list(expression = list(cell = list(rna = list(
            raw = .expr("raw"), scaled = .expr("scaled")
        )))))
        a <- .mf(list(expression = list(cell = list(rna = list(
            raw = .expr("raw")
        )))))
        d <- manifestDiff(b, a)
        expect_match(d$summary, "expression removed: scaled")
        expect_identical(d$detail$removed, "expression.cell.rna.scaled")
    })

    it("names a new metadata column with its level count", {
        b <- .mf(list(cell_metadata = list(cell = list(rna =
            .meta(list(cell_ID = 100L, total_expr = 100L))))))
        a <- .mf(list(cell_metadata = list(cell = list(rna =
            .meta(list(cell_ID = 100L, total_expr = 100L,
                leiden_clus = 14L))))))
        d <- manifestDiff(b, a)
        expect_match(d$summary, "leiden_clus \\(14 levels\\)")
    })

    it("does not repeat the shape change a column change implies", {
        b <- .mf(list(cell_metadata = list(cell = list(rna =
            .meta(list(cell_ID = 100L))))))
        a <- .mf(list(cell_metadata = list(cell = list(rna =
            .meta(list(cell_ID = 100L, leiden_clus = 14L))))))
        expect_false(grepl("dimensions", manifestDiff(b, a)$summary))
    })

    it("states a cell count change once and tallies the resized objects", {
        leaves <- function(n) list(
            expression = list(cell = list(rna = list(
                raw = .expr("raw", c(50L, n)),
                normalized = .expr("normalized", c(50L, n))
            ))),
            spatial_locs = list(cell = list(
                raw = list(class = "spatLocsObj", shape = c(n, 3L))
            ))
        )
        d <- manifestDiff(
            .mf(leaves(100L), n_cells = list(cell = 100L)),
            .mf(leaves(40L), n_cells = list(cell = 40L))
        )
        expect_match(d$summary, "cells \\[cell\\]: 100 -> 40")
        expect_match(d$summary, "3 objects resized")
        # the per-leaf detail is still complete
        expect_length(d$detail$modified, 3L)
    })

    it("spells out shape changes when no count changed", {
        b <- .mf(list(expression = list(cell = list(rna = list(
            raw = .expr("raw", c(50L, 100L))
        )))))
        a <- .mf(list(expression = list(cell = list(rna = list(
            raw = .expr("raw", c(20L, 100L))
        )))))
        expect_match(
            manifestDiff(b, a)$summary, "dimensions 50 x 100 -> 20 x 100"
        )
    })

    it("detects content changes that leave the shape untouched", {
        b <- .mf(list(expression = list(cell = list(rna = list(
            raw = .expr("raw", fp = "aaaa")
        )))))
        a <- .mf(list(expression = list(cell = list(rna = list(
            raw = .expr("raw", fp = "bbbb")
        )))))
        d <- manifestDiff(b, a)
        expect_true(d$changed)
        expect_match(d$summary, "modified \\(fingerprint\\)")
    })

    it("flags a swapped object identity", {
        b <- .mf(uid = "g-one")
        a <- .mf(uid = "g-two")
        d <- manifestDiff(b, a)
        expect_match(d$summary, "object identity changed")
        expect_identical(d$detail$object$uid$after, "g-two")
    })

    it("treats a NULL before as creation and a NULL after as removal", {
        m <- .mf()
        expect_match(manifestDiff(NULL, m)$summary, "created")
        expect_match(manifestDiff(m, NULL)$summary, "object removed")
    })

    it("keeps id-slot counts out of the resize tally when nothing else moved", {
        b <- .mf(list(cell_ID = list(cell = list(n = 100L))),
            n_cells = list(cell = 100L))
        a <- .mf(list(cell_ID = list(cell = list(n = 40L))),
            n_cells = list(cell = 40L))
        d <- manifestDiff(b, a)
        expect_match(d$summary, "cells \\[cell\\]: 100 -> 40")
    })

    it("does not mistake the slots list itself for a leaf", {
        # `slots$n` partial-matches `nn_network`, which once collapsed every
        # manifest to a single unnamed leaf and reported "no state change"
        # for a subset that dropped 84% of the cells
        m <- .mf(list(nn_network = list(cell = list(rna = list(sNN = list(
            sNN.pca = list(class = "nnNetObj", n_nodes = 100L, n_edges = 300L)
        ))))))
        flat <- .manifest_flatten(m$slots)
        expect_named(flat, "nn_network.cell.rna.sNN.sNN.pca")
    })

    it("never reports 'no state change' for a leaf it marked modified", {
        # a re-clustering rewrites an existing column in place: same column
        # set, same shape, different values. The leaf was modified, so the
        # sentence must say so.
        b <- .mf(list(cell_metadata = list(cell = list(rna =
            .meta(list(cell_ID = 100L, leiden_clus = 5L))))))
        a <- .mf(list(cell_metadata = list(cell = list(rna =
            .meta(list(cell_ID = 100L, leiden_clus = 14L))))))
        d <- manifestDiff(b, a)
        expect_true(d$changed)
        expect_false(identical(d$summary, "no state change"))
        expect_match(d$summary, "leiden_clus \\(5 -> 14 levels\\)")
    })

    it("does not treat a reordered column list as a change", {
        cols <- function(order) {
            list(class = "cellMetaObj", shape = c(100L, 2L),
                columns = lapply(order, function(cc) {
                    list(name = cc, dtype = "numeric", n_levels = 5L)
                }))
        }
        b <- .mf(list(cell_metadata = list(cell = list(rna = cols(c("a", "b"))))))
        a <- .mf(list(cell_metadata = list(cell = list(rna = cols(c("b", "a"))))))
        expect_false(manifestDiff(b, a)$changed)
    })

    it("reports a content-only change on any leaf type", {
        leaf <- function(fp) list(class = "spatialNetworkObj",
            name = "Delaunay_network", method = "deldir",
            n_edges = 1400L, fingerprint = fp)
        d <- manifestDiff(
            .mf(list(spatial_network = list(cell = list(
                Delaunay_network = leaf("aaaa"))))),
            .mf(list(spatial_network = list(cell = list(
                Delaunay_network = leaf("bbbb")))))
        )
        expect_match(d$summary, "modified \\(fingerprint\\)")
    })

    it("keeps the resize tally free of spurious extra phrases", {
        leaves <- function(n) list(expression = list(cell = list(rna = list(
            raw = .expr("raw", c(50L, n))))))
        d <- manifestDiff(
            .mf(leaves(100L), n_cells = list(cell = 100L)),
            .mf(leaves(40L), n_cells = list(cell = 40L))
        )
        expect_match(d$summary, "1 objects resized")
        expect_false(grepl("modified \\(\\)", d$summary))
    })
})

describe("level counts in the sentence", {
    .meta_n <- function(cols, nrow) {
        list(class = "cellMetaObj", shape = c(nrow, length(cols)),
            columns = lapply(names(cols), function(cc) {
                list(name = cc, dtype = "numeric", n_levels = cols[[cc]])
            }))
    }
    wrap <- function(leaf) .mf(list(cell_metadata = list(cell = list(rna = leaf))))

    it("keeps the count for a categorical column", {
        d <- manifestDiff(
            wrap(.meta_n(list(cell_ID = 1522L), 1522L)),
            wrap(.meta_n(list(cell_ID = 1522L, leiden_clus = 11L), 1522L))
        )
        expect_match(d$summary, "leiden_clus \\(11 levels\\)")
    })

    it("drops the count for a near-unique column but still reports it", {
        # "total_expr (1428 levels)" over 1522 cells only says "continuous"
        d <- manifestDiff(
            wrap(.meta_n(list(cell_ID = 1522L), 1522L)),
            wrap(.meta_n(list(cell_ID = 1522L, total_expr = 1428L), 1522L))
        )
        expect_match(d$summary, "added: total_expr")
        expect_false(grepl("1428 levels", d$summary))
    })

    it("keeps small counts even when rows are few", {
        d <- manifestDiff(
            wrap(.meta_n(list(cell_ID = 20L), 20L)),
            wrap(.meta_n(list(cell_ID = 20L, hvf = 2L), 20L))
        )
        expect_match(d$summary, "hvf \\(2 levels\\)")
    })

    it("names a re-levelled continuous column without a count", {
        d <- manifestDiff(
            wrap(.meta_n(list(total_expr = 1428L), 1522L)),
            wrap(.meta_n(list(total_expr = 1400L), 1522L))
        )
        expect_match(d$summary, "changed: total_expr")
        expect_false(grepl("levels", d$summary))
    })
})

describe("display names", {
    it("does not truncate a leaf name that contains a dot", {
        # nn_network paths end ".sNN.sNN.pca", so splitting on the last dot
        # reported an sNN.pca network as "pca"
        leaf <- function(fp) list(class = "nnNetObj", name = "sNN.pca",
            nn_type = "sNN", n_edges = 100L, fingerprint = fp)
        wrap <- function(l) .mf(list(nn_network = list(cell = list(rna = list(
            sNN = list(sNN.pca = l))))))
        d <- manifestDiff(wrap(leaf("aaaa")), wrap(leaf("bbbb")))
        expect_match(d$summary, "sNN\\.pca modified")
    })

    it("names an added leaf by the key it sits under", {
        after <- .mf(list(expression = list(cell = list(rna = list(
            raw = .expr("raw"), pearson.resid = .expr("pearson.resid")
        )))))
        before <- .mf(list(expression = list(cell = list(rna = list(
            raw = .expr("raw")
        )))))
        expect_match(manifestDiff(before, after)$summary,
            "expression added: pearson\\.resid")
    })

    it("falls back to the path when a leaf carries no name", {
        b <- .mf(list(cell_ID = list(cell = list(n = 100L))))
        a <- .mf(list(cell_ID = list(cell = list(n = 40L))))
        expect_match(manifestDiff(b, a)$summary, "cell IDs cell")
    })
})

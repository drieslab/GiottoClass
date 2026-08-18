# Tests for the giottoMulti class, its construction path, and the id_map
# identity registry.
#
# Scope matches R/gmulti.R: class, gAny dispatch, constructor, @source
# resolution, id_map + @id_sig caching, and the @cell_ID / @feat_ID narrowing
# contract. The @mapping federation API (setter, validation, invalidation) and
# the view/space recipes arrive later with their own tests.

.mk_minimal <- function(ncell, nfeat) {
    m <- matrix(0, nrow = nfeat, ncol = ncell)
    rownames(m) <- paste0("f", seq_len(nfeat))
    colnames(m) <- paste0("c", seq_len(ncell))
    createGiottoObject(expression = m, verbose = FALSE)
}


# class + gAny dispatch ####

test_that("empty giottoMulti constructs", {
    mg <- new("giottoMulti")
    expect_s4_class(mg, "giottoMulti")
    expect_true(is(mg, "gAny"))
    expect_length(mg, 0L)
    expect_null(mg@id_map$cells)
    expect_identical(spatIDs(mg), character())
    expect_identical(featIDs(mg), character())
})

test_that("giottoMulti declares the full slot set up front", {
    nms <- slotNames("giottoMulti")
    # populated from stage 1
    expect_true(all(c("objects", "id_map", "id_sig", "source") %in% nms))
    # declared now, populated by later stages — see R/gmulti.R header
    expect_true(all(c("mapping", "cell_ID", "feat_ID", "view", "spaces") %in% nms))
})

test_that("empty giottoMulti seeds @mapping with both axes", {
    mg <- new("giottoMulti")
    expect_named(mg@mapping, c("spat_unit", "feat_type"), ignore.order = TRUE)
    expect_length(mg@mapping$spat_unit, 0L)
    expect_length(mg@mapping$feat_type, 0L)
})

test_that("giottoMulti is a gAny but deliberately not a giotto", {
    mg <- new("giottoMulti")
    expect_true(is(mg, "gAny"))
    # not contains="giotto": spatial methods must fail loudly rather than
    # read absent slots
    expect_false(is(mg, "giotto"))
})

test_that("gAny inheritance does not change giotto dispatch", {
    g <- .mk_minimal(5, 4)
    expect_true(is(g, "giotto"))
    expect_true(is(g, "gAny"))
    # spatIDs("giotto", ...) still wins
    expect_identical(spatIDs(g), c("c1", "c2", "c3", "c4", "c5"))
})

test_that("populated giottoMulti exposes children", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    expect_s4_class(mg, "giottoMulti")
    expect_identical(names(mg), c("a", "b"))
    expect_length(mg, 2L)
    expect_s4_class(mg[["a"]], "giotto")
    expect_identical(mg[[1]], mg[["a"]])
})

test_that("`[[` on a giottoMulti selects a child, not a slot", {
    g1 <- .mk_minimal(5, 4)
    mg <- createGiottoMulti(list(a = g1))
    # contrast with `[[` on a giotto, which returns slot contents
    expect_s4_class(mg[["a"]], "giotto")
    expect_null(mg[["expression"]])
    expect_type(g1[["expression"]], "list")
})


# constructor validation ####

test_that("createGiottoMulti requires a uniquely named list of giotto", {
    g1 <- .mk_minimal(5, 4)
    expect_error(createGiottoMulti(list(g1)), "names")
    expect_error(createGiottoMulti(list(a = g1, a = g1)), "names")
    expect_error(createGiottoMulti(list(a = "not a giotto")), "giotto")
})

test_that("createGiottoMulti populates instructions", {
    g1 <- .mk_minimal(5, 4)
    mg <- createGiottoMulti(list(a = g1))
    expect_false(is.null(mg@instructions))
})


# id_map identity registry, read via spatIDs / featIDs ####

test_that("id_map namespaces cells globally and leaves feats passthrough", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    cells <- mg@id_map$cells
    expect_identical(nrow(cells), 8L)
    expect_identical(unique(cells$object), c("a", "b"))
    expect_true(all(grepl("^[ab]::c[0-9]+$", cells$global_id)))

    feats <- mg@id_map$feats
    # 4 features per object x 2 objects = 8 rows in long form
    expect_identical(nrow(feats), 8L)
    # but feature names are shared (passthrough), so global = local
    expect_identical(feats$global_id, feats$local_id)
})

test_that("spatIDs on a giottoMulti returns global ids, local on request", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    expect_identical(spatIDs(mg),
        c(paste0("a::c", 1:5), paste0("b::c", 1:3)))
    expect_identical(spatIDs(mg, local = TRUE),
        c(paste0("c", 1:5), paste0("c", 1:3)))
})

test_that("spatIDs honors object= to restrict to children", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    expect_identical(spatIDs(mg, object = "b"), paste0("b::c", 1:3))
    expect_length(spatIDs(mg, object = c("a", "b")), 8L)
    expect_error(spatIDs(mg, object = "nope"), "unknown object")
})

test_that("featIDs uniques by default, long form on request", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    # features are a shared vocabulary: 4 uniques across both children
    expect_identical(featIDs(mg), paste0("f", 1:4))
    expect_length(featIDs(mg, uniques = FALSE), 8L)
    expect_identical(featIDs(mg, object = "a"), paste0("f", 1:4))
})

test_that("spatIDs on giotto is unaffected by the giottoMulti method", {
    g <- .mk_minimal(5, 4)
    expect_identical(spatIDs(g), paste0("c", 1:5))
    expect_identical(featIDs(g), paste0("f", 1:4))
})


# id_map caching: fast-path initialize ####

test_that("constructor populates id_sig alongside id_map", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    # id_sig is a per-child list of cell/feat lengths
    expect_identical(names(mg@id_sig), c("a", "b"))
    expect_identical(mg@id_sig$a$cell, lengths(g1@cell_ID))
    expect_identical(mg@id_sig$a$feat, lengths(g1@feat_ID))
    expect_identical(mg@id_sig$b$cell, lengths(g2@cell_ID))
})

test_that("initialize fast-path: id_map unchanged when children unchanged", {
    g1 <- .mk_minimal(5, 4)
    mg <- createGiottoMulti(list(a = g1))
    before <- mg@id_map

    # mutate id_map to detect whether a rebuild fired
    mg@id_map$cells <- before$cells[1, ]
    mg2 <- initialize(mg)
    # signatures match (children unchanged), so the narrowed id_map is kept
    expect_identical(nrow(mg2@id_map$cells), 1L)
})

test_that("initialize rebuilds id_map when child length signature changes", {
    g1 <- .mk_minimal(5, 4)
    mg <- createGiottoMulti(list(a = g1))
    expect_identical(nrow(mg@id_map$cells), 5L)

    # swap in a child with a different cell count — simulates direct mutation
    g1_smaller <- .mk_minimal(2, 4)
    mg@objects$a <- g1_smaller

    mg2 <- initialize(mg)
    # signature changed -> full rebuild
    expect_identical(nrow(mg2@id_map$cells), 2L)
    expect_identical(mg2@id_sig$a$cell, lengths(g1_smaller@cell_ID))
})

test_that("clearing @id_sig forces a rebuild even when length matches", {
    g1 <- .mk_minimal(5, 4)
    mg <- createGiottoMulti(list(a = g1))

    # narrow id_map manually
    mg@id_map$cells <- mg@id_map$cells[1:2, ]
    # signatures still match children's actual lengths, so initialize fast-paths
    expect_identical(nrow(initialize(mg)@id_map$cells), 2L)
    # clearing @id_sig before initialize forces a full rebuild
    mg@id_sig <- list()
    expect_identical(nrow(initialize(mg)@id_map$cells), 5L)
})

test_that("bare re-init does not require re-supplying children", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4)))
    mg2 <- initialize(mg)
    expect_identical(names(mg2), "a")
    expect_length(mg2, 1L)
})


# @source slot + acquisition + validation ####

test_that("giottoMulti has @source slot defaulting to NULL", {
    g <- .mk_minimal(5, 4)
    mg <- createGiottoMulti(list(a = g))
    expect_true("source" %in% slotNames("giottoMulti"))
    expect_null(mg@source)
})

test_that("multi inherits source from first sourced child", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    fake <- structure(list(tag = "src1"), class = "fakeSource")
    g1@source <- fake

    mg <- createGiottoMulti(list(a = g1, b = g2))
    expect_identical(mg@source, fake)
})

test_that("createGiottoMulti accepts explicit source arg", {
    g <- .mk_minimal(5, 4)
    fake <- structure(list(tag = "explicit"), class = "fakeSource")
    mg <- createGiottoMulti(list(a = g), source = fake)
    expect_identical(mg@source, fake)
})

test_that("mixed-class child sources error at construction", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    g1@source <- structure(list(), class = "srcA")
    g2@source <- structure(list(), class = "srcB")
    expect_error(
        createGiottoMulti(list(a = g1, b = g2)),
        "different classes"
    )
})

test_that("explicit source class mismatch with children errors", {
    g <- .mk_minimal(5, 4)
    g@source <- structure(list(), class = "srcA")
    expect_error(
        createGiottoMulti(list(a = g),
            source = structure(list(), class = "srcB")),
        "does not match"
    )
})

test_that("explicit source is accepted when no child carries one", {
    g <- .mk_minimal(5, 4)
    fake <- structure(list(), class = "srcB")
    mg <- createGiottoMulti(list(a = g), source = fake)
    expect_identical(mg@source, fake)
})


# @mapping auto-discovery ####
# Discovery is here (not with the rest of the federation API) because the
# narrowing contract below depends on it: .gm_narrowing_keys() treats
# @mapping as the authoritative key universe.

test_that("construction auto-discovers the symmetric trivial mapping", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    m <- mg@mapping
    expect_identical(names(m$spat_unit), "cell")
    expect_identical(names(m$feat_type), "rna")
    # every participating sample maps handle -> its own child-level name
    expect_identical(m$spat_unit$cell, c(a = "cell", b = "cell"))
    expect_identical(m$feat_type$rna, c(a = "rna", b = "rna"))
})

test_that("a user-set mapping survives bare re-init", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4)))
    mg@mapping$spat_unit <- list(custom = c(a = "cell"))
    expect_identical(names(initialize(mg)@mapping$spat_unit), "custom")
})


# @cell_ID / @feat_ID narrowing contract ####

test_that("subset narrows @cell_ID and is non-destructive on the parent", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    keep <- c("a::c1", "a::c2", "b::c1")

    mg2 <- subset(mg, cells = keep)
    expect_identical(spatIDs(mg2), keep)
    expect_identical(names(mg2@cell_ID), "cell")
    # value semantics: the original is the widen-back handle
    expect_length(spatIDs(mg), 8L)
})

test_that("subset narrows @feat_ID", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    mg2 <- subset(mg, features = c("f1", "f2"))
    expect_identical(featIDs(mg2), c("f1", "f2"))
    expect_identical(names(mg2@feat_ID), "rna")
})

test_that("the identity registry is never narrowed by subset", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    mg2 <- subset(mg, cells = c("a::c1"))
    # @id_map still covers every child ID; only the read path narrows
    expect_identical(nrow(mg2@id_map$cells), 8L)
    expect_identical(spatIDs(mg2), "a::c1")
})

test_that("narrowing keys come from @mapping, not the empty joint slots", {
    # Regression guard: deriving keys from the lazily-populated joint slots
    # alone makes a default-scoped subset() record nothing and silently
    # no-op, since those slots are empty on a fresh multi.
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4)))
    expect_length(mg@expression, 0L) # joint cache genuinely empty
    expect_identical(.gm_narrowing_keys(mg, "spat_unit"), "cell")

    mg_nomap <- mg
    mg_nomap@mapping <- list(spat_unit = list(), feat_type = list())
    expect_length(.gm_narrowing_keys(mg_nomap, "spat_unit"), 0L)
})

test_that("subset composes additively across calls", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    mg2 <- subset(mg, cells = c("a::c1", "a::c2", "b::c1"))
    mg3 <- subset(mg2, cells = c("a::c2", "b::c1", "b::c2"))
    # intersection of the two narrowings, not replacement
    expect_identical(spatIDs(mg3), c("a::c2", "b::c1"))
})

test_that("subset silently ignores globals absent from the registry", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4)))
    mg2 <- subset(mg, cells = c("a::c1", "nope::c9"))
    expect_identical(spatIDs(mg2), "a::c1")
})

test_that("spatIDs narrowing respects object= and local=", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    mg2 <- subset(mg, cells = c("a::c1", "a::c2", "b::c1"))
    expect_identical(spatIDs(mg2, object = "a"), c("a::c1", "a::c2"))
    expect_identical(spatIDs(mg2, object = "b"), "b::c1")
    expect_identical(spatIDs(mg2, local = TRUE), c("c1", "c2", "c1"))
})

test_that("a population change resets narrowing", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4)))
    mg2 <- subset(mg, cells = "a::c1")
    expect_identical(spatIDs(mg2), "a::c1")

    # replacing a child with a different cell count changes @id_sig, which
    # invalidates a narrowing recorded against the old population
    mg2@objects$a <- .mk_minimal(3, 4)
    mg3 <- initialize(mg2)
    expect_null(mg3@cell_ID)
    expect_length(spatIDs(mg3), 3L)
})

test_that("children are never mutated by a subset on the parent", {
    g1 <- .mk_minimal(5, 4)
    mg <- createGiottoMulti(list(a = g1, b = .mk_minimal(3, 4)))
    mg2 <- subset(mg, cells = "a::c1")
    expect_identical(spatIDs(mg2@objects$a), paste0("c", 1:5))
    expect_identical(spatIDs(mg2@objects$b), paste0("c", 1:3))
})


# narrowing applies on read through the gAny accessors ####

test_that("a joint expression matrix is filtered on read by @cell_ID", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    # write a joint matrix spanning both children
    ids <- spatIDs(mg)
    m <- matrix(1, nrow = 4, ncol = length(ids),
        dimnames = list(paste0("f", 1:4), ids))
    mg <- setExpression(mg, createExprObj(m, spat_unit = "cell",
        feat_type = "rna", name = "raw"), verbose = FALSE)
    expect_identical(ncol(getExpression(mg, output = "matrix")), 8L)

    mg2 <- subset(mg, cells = c("a::c1", "b::c1"))
    got <- getExpression(mg2, output = "matrix")
    expect_identical(colnames(got), c("a::c1", "b::c1"))
})

test_that(".gm_apply_view is a no-op on a plain giotto", {
    g <- .mk_minimal(5, 4)
    e <- getExpression(g, output = "exprObj")
    expect_identical(.gm_apply_view(e, g), e)
})

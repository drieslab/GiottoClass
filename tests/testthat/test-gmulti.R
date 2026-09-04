# Tests for the giottoMulti class, its construction path, and the id_map
# identity registry.
#
# Scope matches R/gmulti.R: class, gAny dispatch, constructor, @source
# resolution, id_map + @id_sig caching, the @cell_ID / @feat_ID narrowing
# contract, and (stage 3) the @mapping federation API + access layer. The
# view/space recipes arrive later with their own tests.

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

test_that("empty giottoMulti seeds @mapping with all three axes", {
    mg <- new("giottoMulti")
    expect_named(mg@mapping, c("spat_unit", "feat_type", "values"),
        ignore.order = TRUE)
    expect_length(mg@mapping$spat_unit, 0L)
    expect_length(mg@mapping$feat_type, 0L)
    expect_length(mg@mapping$values, 0L)
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
    # every sample is keyed; participating samples map handle -> their own
    # child-level name
    expect_identical(m$spat_unit$cell, c(a = "cell", b = "cell"))
    expect_identical(m$feat_type$rna, c(a = "rna", b = "rna"))
    # values axis is seeded to the ingest convention
    expect_identical(m$values$raw, c(a = "raw", b = "raw"))
})

test_that("discovery keys every sample; non-carriers get the NA sentinel", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    # give g1 an extra spat_unit g2 doesn't have
    e_extra <- g1@expression$cell$rna$raw
    spatUnit(e_extra) <- "extra"
    g1@expression$extra <- list(rna = list(raw = e_extra))
    g1@cell_ID$extra <- g1@cell_ID$cell
    g1 <- initialize(g1)

    mg <- createGiottoMulti(list(a = g1, b = g2))
    m <- mg@mapping
    # the shared handle keys both; the a-only handle keys both, with b as
    # a declared skip rather than simply absent
    expect_identical(m$spat_unit$cell, c(a = "cell", b = "cell"))
    expect_identical(m$spat_unit$extra, c(a = "extra", b = NA_character_))
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


# ============================================================================
# stage 3 — @mapping federation API + access layer
# ============================================================================

# gmultiMapping accessor ####

test_that("gmultiMapping returns the full mapping or one axis", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    expect_identical(gmultiMapping(mg), mg@mapping)
    expect_identical(gmultiMapping(mg, "spat_unit"), mg@mapping$spat_unit)
    expect_identical(gmultiMapping(mg, "values"), mg@mapping$values)
})

test_that("gmultiMapping<- full replacement validates and assigns", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    m <- gmultiMapping(mg)
    names(m$spat_unit) <- "unified"
    gmultiMapping(mg) <- m
    expect_identical(names(gmultiMapping(mg, "spat_unit")), "unified")
})

test_that("gmultiMapping<- NULL triggers fresh auto-discovery", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    m <- gmultiMapping(mg)
    names(m$spat_unit) <- "unified"
    gmultiMapping(mg) <- m
    gmultiMapping(mg) <- NULL
    expect_identical(names(gmultiMapping(mg, "spat_unit")), "cell")
    expect_identical(names(gmultiMapping(mg, "values")), "raw")
})

test_that("axis-scoped setter replaces just one axis", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    gmultiMapping(mg, "spat_unit") <-
        list(unified = c(a = "cell", b = "cell"))
    expect_identical(names(gmultiMapping(mg, "spat_unit")), "unified")
    # other axes untouched
    expect_identical(names(gmultiMapping(mg, "feat_type")), "rna")
    expect_identical(names(gmultiMapping(mg, "values")), "raw")
})

test_that("entry-scoped setter replaces one handle's per-sample vector", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    gmultiMapping(mg, "values", "raw") <- c(a = "raw", b = NA)
    expect_identical(gmultiMapping(mg, "values")$raw,
        c(a = "raw", b = NA_character_))
})

test_that("entry-scoped setter with NULL drops that entry", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    gmultiMapping(mg, "values", "raw") <- NULL
    expect_length(gmultiMapping(mg, "values"), 0L)
})

# validation ####

test_that("gmultiMapping<- rejects unknown sample names", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    expect_error(
        gmultiMapping(mg, "spat_unit", "cell") <-
            c(a = "cell", b = "cell", nope = "cell"),
        "unknown sample")
})

test_that("gmultiMapping<- enforces the every-entry-keys-every-sample rule", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    expect_error(
        gmultiMapping(mg, "spat_unit", "cell") <- c(a = "cell"),
        "does not key sample")
})

test_that("gmultiMapping<- rejects child names that don't exist", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    expect_error(
        gmultiMapping(mg, "spat_unit", "cell") <-
            c(a = "cell", b = "nucleus"),
        "not present")
})

test_that("NA sentinel passes validation (deliberate skip)", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    expect_no_error(
        gmultiMapping(mg, "spat_unit", "cell") <- c(a = "cell", b = NA))
})

test_that("declaring a 'scaled' values federation warns on cbind validity", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    for (g in c("g1", "g2")) {
        gg <- get(g)
        e <- gg@expression$cell$rna$raw
        objName(e) <- "scaled"
        gg@expression$cell$rna$scaled <- e
        assign(g, gg)
    }
    mg <- createGiottoMulti(list(a = g1, b = g2))
    expect_warning(
        gmultiMapping(mg, "values", "zsc") <- c(a = "scaled", b = "scaled"),
        "not.*comparable|comparable")
})

# invalidation + block-on-expansion ####

test_that("mapping edit invalidates only the affected universe", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    e <- getExpression(mg)
    mg <- setExpression(mg, e, name = "raw", verbose = FALSE)
    expect_length(list_expression_names(mg,
        spat_unit = "cell", feat_type = "rna"), 1L)

    # editing an unrelated (new, unmaterialized) handle leaves it alone
    gmultiMapping(mg, "spat_unit", "other") <- c(a = NA, b = NA)
    expect_length(list_expression_names(mg,
        spat_unit = "cell", feat_type = "rna"), 1L)

    # editing the materialized universe's own entry (non-expansion: a
    # shrink) drops the joint content for it
    gmultiMapping(mg, "spat_unit", "cell") <- c(a = "cell", b = NA)
    expect_length(mg@expression, 0L)
})

test_that("participation expansion is blocked once a universe materializes", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    gmultiMapping(mg, "values", "raw") <- c(a = "raw", b = NA)
    e <- getExpression(mg)
    mg <- setExpression(mg, e, name = "raw", verbose = FALSE)

    expect_error(
        gmultiMapping(mg, "values", "raw") <- c(a = "raw", b = "raw"),
        "cannot expand participation")

    # dropping the joint content unblocks the re-declaration
    mg@expression <- NULL
    expect_no_error(
        gmultiMapping(mg, "values", "raw") <- c(a = "raw", b = "raw"))
})

test_that("expanding an unmaterialized universe is fine", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    gmultiMapping(mg, "values", "raw") <- c(a = "raw", b = NA)
    expect_no_error(
        gmultiMapping(mg, "values", "raw") <- c(a = "raw", b = "raw"))
})

# child-add auto-seeding (plan Q1) ####

test_that("[[<- auto-seeds the new sample on every axis", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    mg[["c"]] <- .mk_minimal(2, 4)
    m <- gmultiMapping(mg)
    expect_identical(m$spat_unit$cell,
        c(a = "cell", b = "cell", c = "cell"))
    expect_identical(m$values$raw, c(a = "raw", b = "raw", c = "raw"))
})

test_that("[[<- seeds NA for a handle the new child lacks", {
    g1 <- .mk_minimal(5, 4)
    e_extra <- g1@expression$cell$rna$raw
    spatUnit(e_extra) <- "extra"
    g1@expression$extra <- list(rna = list(raw = e_extra))
    g1@cell_ID$extra <- g1@cell_ID$cell
    g1 <- initialize(g1)
    mg <- createGiottoMulti(list(a = g1))

    mg[["b"]] <- .mk_minimal(3, 4)
    m <- gmultiMapping(mg)
    expect_identical(m$spat_unit$extra, c(a = "extra", b = NA_character_))
    expect_identical(m$spat_unit$cell, c(a = "cell", b = "cell"))
})

test_that("[[<- seeds NA into a materialized universe (expansion blocked)", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    e <- getExpression(mg)
    mg <- setExpression(mg, e, name = "raw", verbose = FALSE)

    expect_warning(mg[["c"]] <- .mk_minimal(2, 4), "joint shared slot")
    m <- gmultiMapping(mg)
    # materialized universes get the NA skip; opting in = drop + re-declare
    expect_identical(unname(m$values$raw["c"]), NA_character_)
    expect_identical(unname(m$spat_unit$cell["c"]), NA_character_)
})

test_that("a user-removed handle is not re-added by bare re-init", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    gmultiMapping(mg, "values", "raw") <- NULL
    mg2 <- initialize(mg)
    expect_length(gmultiMapping(mg2, "values"), 0L)
})

# assembly — identity tags, participation stamp, error paths ####

test_that("assembled joint expression carries the federation handles", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    # unify under parent-namespace handles that differ from child names
    gmultiMapping(mg, "spat_unit") <- list(joint_cell = c(a = "cell", b = "cell"))
    gmultiMapping(mg, "feat_type") <- list(joint_rna = c(a = "rna", b = "rna"))

    e <- getExpression(mg)
    # Q5c: tags describe the federation, not whichever child sorted first
    expect_identical(spatUnit(e), "joint_cell")
    expect_identical(featType(e), "joint_rna")
    expect_identical(objName(e), "raw")
    expect_identical(prov(e), "joint_cell")
})

test_that("assembled joint expression stamps the participation set", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    e <- getExpression(mg)
    stamp <- e@misc$gmulti
    expect_identical(stamp$participation, c("a", "b"))
    expect_identical(stamp$resolved$a,
        list(su = "cell", ft = "rna", values = "raw"))
})

test_that("assembled joint cell metadata carries handles and list_ID", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    gmultiMapping(mg, "spat_unit") <- list(joint_cell = c(a = "cell", b = "cell"))
    cm <- getCellMetadata(mg)
    expect_identical(spatUnit(cm), "joint_cell")
    dt <- cm[]
    expect_identical(unique(dt$list_ID), c("a", "b"))
    expect_identical(nrow(dt), 8L)
})

test_that("NA sentinel: a declared skip is excluded without error", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    gmultiMapping(mg, "values", "raw") <- c(a = "raw", b = NA)
    e <- getExpression(mg)
    expect_identical(ncol(e[]), 5L)
    expect_identical(e@misc$gmulti$participation, "a")
})

test_that("a keyed child missing the values entry errors loudly by default", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    names(g2@expression$cell$rna) <- "counts"
    mg <- createGiottoMulti(list(a = g1, b = g2))
    # discovery keys b at "raw" even though its matrix is named "counts" —
    # the first read must fail loudly naming the sample, not silently drop
    expect_error(getExpression(mg), "unsatisfiable.*b|b.*unsatisfiable")
})

test_that("the unsatisfiable-child remedy of re-pointing the entry works", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    names(g2@expression$cell$rna) <- "counts"
    mg <- createGiottoMulti(list(a = g1, b = g2))
    gmultiMapping(mg, "values", "raw") <- c(a = "raw", b = "counts")
    e <- getExpression(mg)
    expect_identical(ncol(e[]), 8L)
    expect_identical(e@misc$gmulti$resolved$b$values, "counts")
})

test_that("on_missing = 'drop' downgrades the unsatisfiable child to a warning", {
    g1 <- .mk_minimal(5, 4)
    g2 <- .mk_minimal(3, 4)
    names(g2@expression$cell$rna) <- "counts"
    mg <- createGiottoMulti(list(a = g1, b = g2))
    expect_warning(e <- getExpression(mg, on_missing = "drop"), "dropping")
    expect_identical(ncol(e[]), 5L)
    expect_identical(e@misc$gmulti$participation, "a")
})

test_that("mismatched feature panels error by default; drop intersects; fill unions", {
    g1 <- .mk_minimal(5, 4)
    m2 <- matrix(0, nrow = 3, ncol = 3)
    rownames(m2) <- paste0("f", 1:3)
    colnames(m2) <- paste0("c", 1:3)
    g2 <- createGiottoObject(expression = m2, verbose = FALSE)
    mg <- createGiottoMulti(list(a = g1, b = g2))

    expect_error(getExpression(mg), "feature panels differ")

    e_drop <- getExpression(mg, on_missing = "drop")
    expect_identical(nrow(e_drop[]), 3L)
    expect_identical(ncol(e_drop[]), 8L)

    e_fill <- getExpression(mg, on_missing = "fill")
    expect_identical(nrow(e_fill[]), 4L)
    # b lacks f4 — 0-filled
    expect_true(all(e_fill[]["f4", startsWith(colnames(e_fill[]), "b::")] == 0))
})

test_that("no name + no raw handle + several declared handles errors", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    gmultiMapping(mg, "values") <- list(
        one = c(a = "raw", b = "raw"),
        two = c(a = "raw", b = "raw"))
    expect_error(getExpression(mg), "several values handles")
})

# sample addressing ####

test_that(".parse_sample_qualified_name handles bare + prefixed names", {
    expect_identical(.parse_sample_qualified_name("raw"),
        list(sample = NULL, name = "raw"))
    p <- .parse_sample_qualified_name("B191::raw")
    expect_identical(p$sample, "B191")
    expect_identical(p$name, "raw")
    # split at FIRST :: only
    p2 <- .parse_sample_qualified_name("B191::raw_x::y")
    expect_identical(p2$sample, "B191")
    expect_identical(p2$name, "raw_x::y")
})

test_that("getExpression(samples = ) slices joint matrix columns", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    e <- getExpression(mg, samples = "a")
    expect_identical(ncol(e[]), 5L)
    expect_true(all(startsWith(colnames(e[]), "a::")))
})

test_that("getExpression(name = 'sample::name') parses the prefix", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    e <- getExpression(mg, name = "a::raw")
    expect_identical(ncol(e[]), 5L)
    # conflicting sample selections error; matching ones are fine
    expect_error(getExpression(mg, name = "a::raw", samples = "b"),
        "conflicting sample selection")
    expect_no_error(getExpression(mg, name = "a::raw", samples = "a"))
})

test_that("samples = errors on unknown and non-participating samples", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    expect_error(getExpression(mg, samples = "nope"), "not in @objects")

    gmultiMapping(mg, "values", "raw") <- c(a = "raw", b = NA)
    expect_error(getExpression(mg, samples = "b"),
        "do not participate")
})

test_that("getCellMetadata(samples = ) slices joint cmeta to one sample", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    cm <- getCellMetadata(mg, output = "data.table", samples = "b")
    expect_identical(nrow(cm), 3L)
    expect_true(all(startsWith(cm$cell_ID, "b::")))
})

test_that("getFeatureMetadata(samples = ) validates but is a no-op", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    fm_all <- getFeatureMetadata(mg, output = "data.table")
    fm_a <- getFeatureMetadata(mg, output = "data.table", samples = "a")
    expect_identical(fm_all, fm_a)
    expect_error(getFeatureMetadata(mg, samples = "nope"), "not in @objects")
})

# spatial-domain per-child dispatch ####

test_that("getSpatialLocations on giottoMulti returns named per-child list", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    out <- getSpatialLocations(mg)
    expect_type(out, "list")
    expect_identical(names(out), c("a", "b"))
    expect_s4_class(out$a, "spatLocsObj")
    expect_identical(nrow(out$a[]), 5L)
    expect_identical(nrow(out$b[]), 3L)
})

test_that("getSpatialLocations honors samples= / object= (and their conflict)", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    out <- getSpatialLocations(mg, samples = "b")
    expect_identical(names(out), "b")
    expect_identical(out, getSpatialLocations(mg, object = "b"))
    expect_error(getSpatialLocations(mg, object = "a", samples = "b"),
        "conflicting")
})

test_that("sample-scoped getSpatialLocations composes with view narrowing", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    mg2 <- subset(mg, cells = c("a::c1", "a::c3", "b::c1"))

    sl_a <- getSpatialLocations(mg2, object = "a")$a
    expect_identical(sort(sl_a[]$cell_ID), c("c1", "c3"))
    sl_b <- getSpatialLocations(mg2, object = "b")$b
    expect_identical(sort(sl_b[]$cell_ID), "c1")

    # children themselves untouched
    expect_length(spatIDs(mg2@objects$a), 5L)
})

test_that("setSpatialLocations requires a single object= target", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    sl <- getSpatialLocations(mg@objects$b)
    expect_error(setSpatialLocations(mg, x = sl), "must name the child")
    expect_error(setSpatialLocations(mg, x = sl, object = c("a", "b")),
        "length 1")
    mg2 <- setSpatialLocations(mg, x = sl, object = "b", verbose = FALSE)
    expect_s4_class(mg2, "giottoMulti")
})

test_that("getFeatureInfo narrows by the feature axis (A6 gap closed)", {
    # needs children with giottoPoints feat_info — synthesize from points
    sv <- terra::vect(
        data.frame(x = c(0, 1, 2), y = c(0, 1, 2),
            feat_ID = c("f1", "f2", "f3")),
        geom = c("x", "y"))
    gp <- createGiottoPoints(sv, feat_type = "rna")
    g1 <- .mk_minimal(5, 4)
    g1 <- setFeatureInfo(g1, gp, feat_type = "rna", verbose = FALSE)
    mg <- createGiottoMulti(list(a = g1))

    mg2 <- subset(mg, features = c("f1", "f3"))
    out <- getFeatureInfo(mg2)
    expect_identical(
        sort(terra::values(out$a)$feat_ID), c("f1", "f3"))
})

# container surface ####

test_that("as(g, 'giottoMulti') wraps a single giotto with default name", {
    g <- .mk_minimal(5, 4)
    mg <- as(g, "giottoMulti")
    expect_s4_class(mg, "giottoMulti")
    expect_identical(names(mg), "sample1")
    expect_length(spatIDs(mg), 5L)
})

test_that("mg[i] subsets children and prunes mapping + joint slots", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4),
        c = .mk_minimal(2, 4)))
    sub <- mg[c("a", "c")]
    expect_identical(names(sub), c("a", "c"))
    expect_identical(spatIDs(sub),
        c(paste0("a::c", 1:5), paste0("c::c", 1:2)))
    # mapping entries key only survivors
    expect_identical(names(gmultiMapping(sub, "spat_unit")$cell),
        c("a", "c"))
    expect_error(mg["nope"], "unknown child")
})

test_that("names(mg) <- renames children, id_map, and mapping keys", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    names(mg) <- c("x", "y")
    expect_identical(names(mg), c("x", "y"))
    expect_identical(sort(unique(mg@id_map$cells$object)), c("x", "y"))
    # mapping keys followed the rename — resolution still works
    expect_identical(names(gmultiMapping(mg, "values")$raw), c("x", "y"))
    e <- getExpression(mg)
    expect_identical(ncol(e[]), 8L)
    expect_error(names(mg) <- c("a", "a"), "unique")
    expect_error(names(mg) <- "a", "length")
})

test_that("names(mg) <- rewrites joint shared slot keys", {
    g1 <- .mk_minimal(5, 4)
    mg <- createGiottoMulti(list(a = g1))
    e <- getExpression(mg)
    mg <- setExpression(mg, e, name = "raw", verbose = FALSE)
    names(mg) <- "A"
    new_cn <- colnames(mg@expression$cell$rna$raw[])
    expect_true(all(grepl("^A::", new_cn)))
})

test_that("show(mg) surfaces children, mapping and joint slots", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    out <- paste(capture.output(show(mg)), collapse = "\n")
    expect_match(out, "2 child object")
    expect_match(out, "spat_unit: cell \\(2\\)")
    expect_match(out, "values: raw \\(2\\)")
    e <- getExpression(mg)
    mg <- setExpression(mg, e, name = "raw", verbose = FALSE)
    out2 <- paste(capture.output(show(mg)), collapse = "\n")
    expect_match(out2, "joint slots: expression")
})

# combine_metadata routing (D6 — no injector, access layer only) ####

test_that("combineMetadata(mg, spat_loc_name = NULL) returns one joint DT", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    dt <- combineMetadata(mg, spat_loc_name = NULL, verbose = FALSE)
    expect_s3_class(dt, "data.table")
    expect_identical(nrow(dt), 8L)
})

test_that("combineMetadata(mg) walks children with joint-only columns added", {
    mg <- createGiottoMulti(list(a = .mk_minimal(5, 4), b = .mk_minimal(3, 4)))
    # materialize a joint-only analysis column
    cm <- getCellMetadata(mg)
    dt <- data.table::copy(cm[])
    dt[, leiden := seq_len(.N)]
    cm[] <- dt
    mg <- setCellMetadata(mg, cm, verbose = FALSE)

    out <- combineMetadata(mg, verbose = FALSE)
    expect_type(out, "list")
    expect_identical(names(out), c("a", "b"))
    # the joint-only column reached each per-child combined table
    expect_true("leiden" %in% names(out$a))
    expect_identical(nrow(out$b), 3L)
})

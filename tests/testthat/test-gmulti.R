# Tests for the giottoMulti class, its construction path, and the id_map
# identity registry.
#
# STAGE 1 of the port sequence in vignettes/articles/PLAN_gmulti2_port.md.
# Scope matches R/gmulti.R: class, gAny dispatch, constructor, @source
# resolution, id_map + @id_sig caching. Narrowing (@cell_ID / @feat_ID),
# @mapping federation, and the view/space recipes arrive in stages 2-4 with
# their own tests.

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

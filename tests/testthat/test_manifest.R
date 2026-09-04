# Manifest generation against a real giotto object. The pure diff logic is
# covered separately in test_manifest_diff.R.

g <- GiottoData::loadGiottoMini("visium", verbose = FALSE)

describe("objManifest", {
    m <- objManifest(g)

    it("emits the documented top-level shape", {
        expect_s3_class(m, "gmanifest")
        expect_identical(m$schema_version, "0.1.0")
        expect_true(all(c("object", "summary", "slots", "warnings") %in%
            names(m)))
        expect_match(m$generated_by, "^GiottoClass ")
    })

    it("nests slots the way the object nests them", {
        expect_true("expression" %in% names(m$slots))
        expect_named(m$slots$expression, "cell")
        expect_named(m$slots$expression$cell, "rna")
        expect_true("raw" %in% names(m$slots$expression$cell$rna))
        expect_identical(m$slots$expression$cell$rna$raw$class, "exprObj")
    })

    it("reports expression shape as features x cells", {
        expect_identical(
            m$slots$expression$cell$rna$raw$shape,
            as.integer(dim(g))
        )
    })

    it("counts ids rather than listing them", {
        expect_identical(m$slots$cell_ID$cell$n, length(spatIDs(g)))
        expect_null(m$slots$cell_ID$cell$ids)
    })

    it("describes metadata columns with their level counts", {
        cols <- m$slots$cell_metadata$cell$rna$columns
        nms <- vapply(cols, function(cc) cc$name, character(1L))
        expect_true("leiden_clus" %in% nms)
        lc <- cols[[which(nms == "leiden_clus")]]
        expect_identical(
            lc$n_levels, length(unique(pDataDT(g)$leiden_clus))
        )
    })

    it("sorts keys at every level so output is canonical", {
        expect_identical(names(m$slots), sort(names(m$slots)))
        expect_identical(
            names(m$slots$expression$cell$rna),
            sort(names(m$slots$expression$cell$rna))
        )
    })

    it("reads a clean object without unreadable fields", {
        expect_identical(m$warnings, character(0))
    })

    it("is insensitive to the insertion order of a slot's contents", {
        g2 <- g
        g2@expression$cell$rna <- rev(g2@expression$cell$rna)
        expect_identical(objManifest(g2)$slots, m$slots)
    })
})

describe("objManifest accessor guarding", {
    it("degrades one field to NULL and records its path", {
        testthat::local_mocked_bindings(
            .manifest_ext = function(x) stop("accessor exploded")
        )
        m <- objManifest(g)
        expect_null(m$slots$spatial_locs$cell$raw$bbox)
        expect_true("spatial_locs.cell.raw.bbox" %in% m$warnings)
        # and the rest of the manifest is intact
        expect_identical(m$slots$expression$cell$rna$raw$class, "exprObj")
    })
})

describe("manifest fingerprints", {
    it("are absent at summary level and present at full level", {
        expect_null(objManifest(g)$slots$expression$cell$rna$raw$fingerprint)
        expect_type(
            objManifest(g, level = "full")$
                slots$expression$cell$rna$raw$fingerprint,
            "character"
        )
    })

    it("are stable across repeated calls", {
        expect_identical(
            objManifest(g, level = "full")$slots,
            objManifest(g, level = "full")$slots
        )
    })

    it("change when content changes but the shape does not", {
        before <- objManifest(g, level = "full")
        g2 <- g
        e <- g2@expression$cell$rna$raw
        e@exprMat[1, 1] <- e@exprMat[1, 1] + 1000
        g2@expression$cell$rna$raw <- e
        after <- objManifest(g2, level = "full")

        expect_identical(
            before$slots$expression$cell$rna$raw$shape,
            after$slots$expression$cell$rna$raw$shape
        )
        expect_false(identical(
            before$slots$expression$cell$rna$raw$fingerprint,
            after$slots$expression$cell$rna$raw$fingerprint
        ))
        expect_match(manifestDiff(before, after)$summary, "fingerprint")
    })

    it("do not disturb the RNG", {
        set.seed(42)
        target <- runif(3)
        set.seed(42)
        invisible(objManifest(g, level = "full"))
        expect_identical(runif(3), target)
    })
})

describe("objManifest_json", {
    it("round-trips through JSON", {
        j <- objManifest_json(g)
        back <- jsonlite::fromJSON(j, simplifyVector = FALSE)
        expect_identical(back$schema_version, "0.1.0")
        expect_identical(
            back$slots$expression$cell$rna$raw$class, "exprObj"
        )
    })

    it("encodes NA, NaN and Inf as strings rather than null", {
        prepped <- .manifest_json_prep(list(
            a = c(1, NA, NaN, Inf, -Inf), b = NA_character_, c = 1.5
        ))
        expect_identical(prepped$a, c("1", "NA", "NaN", "Inf", "-Inf"))
        expect_identical(prepped$b, "NA")
        expect_identical(prepped$c, 1.5)
    })

    it("validates against the shipped schema's required fields", {
        schema_path <- system.file(
            "schema", "giotto-manifest-0.1.0.json",
            package = "GiottoClass"
        )
        skip_if(!nzchar(schema_path))
        schema <- jsonlite::fromJSON(schema_path, simplifyVector = FALSE)
        m <- jsonlite::fromJSON(objManifest_json(g), simplifyVector = FALSE)
        expect_true(all(unlist(schema$required) %in% names(m)))
        expect_identical(m$schema_version, schema$properties$schema_version$const)
    })
})

describe("object uid", {
    it("is minted at creation and is unique per object", {
        expect_false(is.null(.gobject_uid(g)))
        expect_false(identical(.gobject_uid(giotto()), .gobject_uid(giotto())))
    })

    it("survives a copy", {
        g2 <- g
        expect_identical(.gobject_uid(g2), .gobject_uid(g))
    })
})

describe("history records", {
    it("reads legacy character entries", {
        recs <- ghistory_records(g)
        expect_gt(length(recs), 0L)
        expect_true(all(vapply(recs, function(r) r$status, character(1L))
            == "ok"))
    })

    it("records an unattributed step for an unlogged mutation", {
        before <- objManifest(g)
        g2 <- g
        g2@expression$cell$rna$scaled <- NULL
        d <- manifestDiff(before, objManifest(g2))
        expect_true(d$changed)

        g2 <- recordGiottoStep(
            g2,
            fn = "direct slot assignment", status = "unattributed",
            diff = d$detail
        )
        last <- tail(ghistory_records(g2), 1L)[[1L]]
        expect_identical(last$status, "unattributed")
        expect_identical(last$fn, "direct slot assignment")
        expect_false(is.null(last$diff))
        expect_match(last$timestamp, "^\\d{4}-\\d{2}-\\d{2}T")
    })

    it("records a failed step", {
        g2 <- recordGiottoStep(
            g, fn = "runPCA", status = "error", error = "not enough features"
        )
        last <- tail(ghistory_records(g2), 1L)[[1L]]
        expect_identical(last$status, "error")
        expect_identical(last$error, "not enough features")
    })

    it("serializes as one JSON object per line", {
        txt <- objHistory_ndjson(g)
        lines <- strsplit(txt, "\n")[[1]]
        expect_length(lines, length(objHistory(g)))
        parsed <- jsonlite::fromJSON(lines[[1]], simplifyVector = FALSE)
        expect_true(all(c("step_id", "fn", "params", "status") %in%
            names(parsed)))
    })
})

describe("update_giotto_params structured record", {
    it("carries the call as deparsed expressions", {
        f <- function(gobject, dims_to_use = 1:30, k = 30) {
            update_giotto_params(gobject, description = "_testfn")
        }
        g2 <- f(g)
        last <- tail(ghistory_records(g2), 1L)[[1L]]
        expect_identical(last$status, "ok")
        expect_identical(last$fn, "f")
        # the flattened character form reduces `1:30` to "1"; the structured
        # params keep the expression the user actually wrote
        expect_identical(last$params$dims_to_use, "1:30")
    })

    it("leaves the legacy character entry untouched", {
        f <- function(gobject) update_giotto_params(gobject, "_legacy")
        entry <- tail(objHistory(f(g)), 1L)[[1L]]
        expect_s3_class(entry, "ghistory_item")
        expect_type(unclass(entry), "character")
    })
})

describe("save / load sidecars", {
    td <- file.path(tempdir(), "manifest_sidecars")
    on.exit(unlink(td, recursive = TRUE), add = TRUE)
    dir.create(td, showWarnings = FALSE, recursive = TRUE)
    saveGiotto(g, dir = td, foldername = "obj", verbose = FALSE,
        overwrite = TRUE)
    saved <- file.path(td, "obj")

    it("writes manifest.json and history.ndjson next to the object", {
        expect_true(file.exists(file.path(saved, "manifest.json")))
        expect_true(file.exists(file.path(saved, "history.ndjson")))
    })

    it("writes a manifest that matches the object saved", {
        sc <- jsonlite::fromJSON(
            file.path(saved, "manifest.json"), simplifyVector = FALSE
        )
        expect_identical(sc$object$uid, .gobject_uid(g))
        expect_identical(
            sc$slots$expression$cell$rna$raw$shape[[2]], ncol(g)
        )
    })

    it("preserves the uid and data fingerprints through a round trip", {
        g2 <- loadGiotto(saved, verbose = FALSE)
        expect_identical(.gobject_uid(g2), .gobject_uid(g))

        a <- objManifest(g, level = "full")
        b <- objManifest(g2, level = "full")
        expect_identical(
            a$slots$expression$cell$rna$raw$fingerprint,
            b$slots$expression$cell$rna$raw$fingerprint
        )
        expect_identical(
            a$slots$cell_metadata$cell$rna$fingerprint,
            b$slots$cell_metadata$cell$rna$fingerprint
        )
        # images are deliberately re-fingerprinted: saveGiotto exports them to
        # new files, so their source identity legitimately changes
    })
})

describe("network slots", {
    it("describes and fingerprints an igraph-carried network", {
        # spatialNetworkObj/nnNetObj hold an igraph since 0.6.0; objects saved
        # before that hold an edge data.table. Both must describe the same way
        sn <- g@spatial_network$cell$Delaunay_network
        skip_if(is.null(sn))
        m <- objManifest(g, level = "full")
        leaf <- m$slots$spatial_network$cell$Delaunay_network
        expect_false(is.null(leaf$n_edges))
        expect_gt(leaf$n_edges, 0L)
        expect_type(leaf$fingerprint, "character")
    })

    it("gives different networks different fingerprints", {
        # hashing an unreadable carrier as NULL made every network hash the
        # same constant, so no network ever appeared to change
        sn <- g@spatial_network$cell$Delaunay_network
        nn <- g@nn_network$cell$rna$sNN$sNN.pca
        skip_if(is.null(sn) || is.null(nn))
        expect_false(identical(
            GiottoClass:::.fingerprint(sn, fp = "sample"),
            GiottoClass:::.fingerprint(nn, fp = "sample")
        ))
    })

    it("returns no fingerprint rather than a constant when content is absent", {
        sn <- g@spatial_network$cell$Delaunay_network
        skip_if(is.null(sn))
        sn@network <- NULL
        expect_null(GiottoClass:::.fingerprint(sn, fp = "sample"))
    })
})

describe("fingerprint sampling", {
    it("samples a dense matrix without copying it", {
        m <- matrix(seq_len(1e6), nrow = 1000)
        expect_length(GiottoClass:::.fp_matrix_sample(m, 1000L), 1000L)
        # Matrix classes expose values through @x, sparse and dense alike
        mm <- Matrix::Matrix(m[1:50, 1:50])
        expect_length(GiottoClass:::.fp_matrix_sample(mm, 1000L), 1000L)
    })

    it("reads a fixed number of values regardless of object size", {
        small <- Matrix::rsparsematrix(100, 100, density = 0.5)
        big <- Matrix::rsparsematrix(5000, 5000, density = 0.5)
        expect_length(GiottoClass:::.fp_matrix_sample(small, 1000L),
            min(1000L, length(small@x)))
        expect_length(GiottoClass:::.fp_matrix_sample(big, 1000L), 1000L)
    })
})

describe("summary block", {
    it("reports a spat_unit that has no expression matrix", {
        # Xenium's `nucleus` carries polygons, locations and IDs but no
        # matrix. Reading spat_units off @expression dropped exactly those.
        gv <- GiottoData::loadGiottoMini("vizgen", verbose = FALSE)
        expect_true("z1" %in% names(gv@expression))
        gv@expression$z1 <- NULL

        m <- objManifest(gv)
        expect_true("z1" %in% m$summary$spat_units)
        expect_true("z1" %in% names(m$slots$spatial_info))
        expect_false("z1" %in% names(m$slots$expression))
    })

    it("omits units that hold no ids", {
        gv <- GiottoData::loadGiottoMini("vizgen", verbose = FALSE)
        gv@cell_ID$phantom <- character(0)
        expect_false("phantom" %in% names(objManifest(gv)$summary$n_cells))
    })
})

describe("giottoBinPoints leaf", {
    make_gbp <- function(n_bins = 50L, seed = 1L) {
        set.seed(seed)
        ids <- sprintf("bin_%d", seq_len(n_bins))
        sl <- createSpatLocsObj(rnorm(n_bins * 2L))
        sl$cell_ID <- ids
        m <- matrix(floor(runif(n_bins * 10L) * 3),
            ncol = n_bins, dimnames = list(letters[1:10], ids)
        )
        createGiottoBinPoints(createExprObj(m), sl)
    }

    it("describes counts, bins, features and extent", {
        gbp <- make_gbp()
        leaf <- .manifest_leaf(gbp, fp = "sample", wenv = NULL, path = "t")

        expect_identical(leaf$class, "giottoBinPoints")
        expect_identical(leaf$n_bins, 50L)
        expect_identical(leaf$n_feats, 10L)
        expect_identical(leaf$n_records, as.integer(nrow(gbp)))
        expect_type(leaf$compact, "logical")
        expect_named(leaf$extent, c("xmin", "xmax", "ymin", "ymax"))
        expect_type(leaf$fingerprint, "character")
    })

    it("no longer falls through to the ANY fallback", {
        leaf <- .manifest_leaf(make_gbp(), fp = "none", wenv = NULL, path = "t")
        expect_false(identical(names(leaf), c("class", "length")))
    })

    it("fingerprints differently when the content differs", {
        a <- .fingerprint(make_gbp(seed = 1L), fp = "sample")
        b <- .fingerprint(make_gbp(seed = 2L), fp = "sample")
        expect_type(a, "character")
        expect_false(identical(a, b))
    })

    it("is visible to a diff when set on a giotto object", {
        gv <- GiottoData::loadGiottoMini("vizgen", verbose = FALSE)
        before <- objManifest(gv, level = "full")
        gv@feat_info$binpoints <- make_gbp()
        d <- manifestDiff(before, objManifest(gv, level = "full"))
        expect_true(d$changed)
        expect_match(d$summary, "points added: binpoints")
    })
})

describe("geometry fingerprints", {
    make_poly <- function() {
        d <- data.table::data.table(
            poly_ID = rep(c("a", "b"), each = 4L),
            x = c(0, 4, 4, 0, 10, 14, 14, 10),
            y = c(0, 0, 4, 4, 0, 0, 4, 4)
        )
        createGiottoPolygon(d)
    }

    it("is invariant to vertex ordering", {
        # writeVector -> shapefile -> vect returns the same shapes with ring
        # winding and start vertex normalised. A positional hash called every
        # saved-and-reloaded object modified.
        gp <- make_poly()
        a <- GiottoClass:::.fingerprint(gp, fp = "sample")

        sv <- gp@spatVector
        gp2 <- gp
        gp2@spatVector <- rbind(sv[2], sv[1])[c(2L, 1L)]
        skip_if(!identical(
            sort(paste(terra::crds(gp@spatVector)[, 1],
                       terra::crds(gp@spatVector)[, 2])),
            sort(paste(terra::crds(gp2@spatVector)[, 1],
                       terra::crds(gp2@spatVector)[, 2]))
        ), "reordering changed the vertex multiset")

        expect_identical(GiottoClass:::.fingerprint(gp2, fp = "sample"), a)
    })

    it("still changes when a vertex actually moves", {
        gp <- make_poly()
        a <- GiottoClass:::.fingerprint(gp, fp = "sample")

        moved <- terra::shift(gp@spatVector, dx = 0.5)
        gp2 <- gp
        gp2@spatVector <- moved
        expect_false(identical(GiottoClass:::.fingerprint(gp2, fp = "sample"), a))
    })

    it("survives a saveGiotto / loadGiotto round trip", {
        gv <- GiottoData::loadGiottoMini("vizgen", verbose = FALSE)
        td <- file.path(tempdir(), "geom_rt")
        unlink(td, recursive = TRUE)
        dir.create(td, recursive = TRUE)
        on.exit(unlink(td, recursive = TRUE), add = TRUE)

        saveGiotto(gv, dir = td, foldername = "obj", verbose = FALSE,
            overwrite = TRUE)
        gv2 <- loadGiotto(file.path(td, "obj"), verbose = FALSE)

        a <- objManifest(gv, level = "full")
        b <- objManifest(gv2, level = "full")
        # points as well as polygons: a lossy attribute table is what made
        # feat_info report a change on every save/load
        for (slot_nm in c("spatial_info", "feat_info")) {
            for (nm in names(a$slots[[slot_nm]])) {
                expect_identical(
                    a$slots[[slot_nm]][[nm]]$fingerprint,
                    b$slots[[slot_nm]][[nm]]$fingerprint
                )
            }
        }
    })
})

describe("numeric tolerance", {
    # Serialization drops the last bits of a double - shapefile DBF stores
    # numerics as text - and hashing that reported a reloaded object as
    # modified in the same words as a real edit.
    ulp <- function(x) x + .Machine$double.eps * abs(x)

    it("ignores a one-ulp change in a metadata column", {
        cm <- GiottoData::loadSubObjectMini("cellMetaObj")
        cm[]$total_expr <- as.double(seq_len(nrow(cm)))
        a <- GiottoClass:::.fingerprint(cm, fp = "full")

        cm2 <- cm
        cm2[]$total_expr <- ulp(cm2[]$total_expr)
        expect_false(identical(cm[]$total_expr, cm2[]$total_expr))
        expect_identical(GiottoClass:::.fingerprint(cm2, fp = "full"), a)
    })

    it("still sees a change of 1e-9 in a metadata column", {
        cm <- GiottoData::loadSubObjectMini("cellMetaObj")
        cm[]$total_expr <- as.double(seq_len(nrow(cm)))
        a <- GiottoClass:::.fingerprint(cm, fp = "full")

        cm2 <- cm
        cm2[]$total_expr <- cm2[]$total_expr + 1e-9
        expect_false(identical(GiottoClass:::.fingerprint(cm2, fp = "full"), a))
    })

    it("applies the same rule to expression values", {
        ex <- GiottoData::loadSubObjectMini("exprObj")
        ex[] <- ex[] * 1.0 # ensure double storage
        a <- GiottoClass:::.fingerprint(ex, fp = "full")

        nudged <- ex
        nudged[]@x <- ulp(nudged[]@x)
        expect_identical(GiottoClass:::.fingerprint(nudged, fp = "full"), a)

        changed <- ex
        changed[]@x <- changed[]@x + 1e-9
        expect_false(
            identical(GiottoClass:::.fingerprint(changed, fp = "full"), a)
        )
    })

    it("applies the same rule to SpatVector attributes", {
        d <- data.table::data.table(
            poly_ID = rep(c("a", "b"), each = 4L),
            x = c(0, 4, 4, 0, 10, 14, 14, 10),
            y = c(0, 0, 4, 4, 0, 0, 4, 4)
        )
        gp <- createGiottoPolygon(d)
        gp@spatVector$score <- c(1.4511308670043945, 2.5)
        a <- GiottoClass:::.fingerprint(gp, fp = "full")

        nudged <- gp
        # the exact value DBF returns for that first score
        nudged@spatVector$score <- c(1.451130867004395, 2.5)
        expect_identical(GiottoClass:::.fingerprint(nudged, fp = "full"), a)

        changed <- gp
        changed@spatVector$score <- c(1.4511308680043945, 2.5)
        expect_false(
            identical(GiottoClass:::.fingerprint(changed, fp = "full"), a)
        )
    })
})

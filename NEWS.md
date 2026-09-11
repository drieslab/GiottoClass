# GiottoClass 0.6.0

## new

- `objManifest()` returns a machine-readable inventory of a `giotto` object:
  identity, a summary block, and a slot-by-slot description nested as the
  object nests it. Derived on demand, so it cannot go stale. `level = "full"`
  adds content fingerprints. `objManifest_json()` serializes it against the
  schema in `inst/schema/giotto-manifest-0.1.0.json`.
- `manifestDiff()` compares two manifests and reports what changed, as data and
  as one sentence. Pure: manifests in, diff out. Use `level = "full"` on both
  sides to see a step that overwrote content in place: re-running a clustering
  or a normalization leaves every shape and name identical, so only the
  fingerprints move.
- `@parameters` entries now carry a structured record (`step_id`, `fn`,
  `params`, `timestamp`, `seed`, `status`, `diff`) as an attribute; the
  character entry every existing reader expects is unchanged. `params` holds
  the deparsed argument expressions, so `1:30` is no longer recorded as `1`.
  Read them with `ghistory_records()` or `objHistory_ndjson()`.
- `recordGiottoStep()` logs a failed call or a change made outside a logging
  function (`status = "error"` / `"unattributed"`).
- `giotto` objects carry a `uid` in `@versions`, minted at creation and kept
  through copies and save/load.
- `saveGiotto()` writes `manifest.json` and `history.ndjson` beside the saved
  object. Requires \pkg{jsonlite} (Suggests); skipped when absent.
- `hnswKNN()` restored to GiottoClass, so `createNearestNetwork(engine =
  "hnsw")` works again. It had errored with `'hnswKNN' is not an exported
  object from 'namespace:GiottoDisk'` since 2026-08-11, when {GiottoDisk}
  removed the function intending to move it here and the move did not land,
  leaving `.nn_search()` calling a function that existed in neither package.
  Requires \pkg{RcppHNSW} (Suggests); `engine = "dbscan"` remains the default
  and is unchanged.
- `ef` and `n_threads_build` are now exposed on `kNNNetworkParam()`,
  `sNNNetworkParam()` and `createNearestNetwork()` rather than only reachable
  through `...`. Both apply to `engine = "hnsw"` and are ignored by
  `"dbscan"`, so engines can be swapped without changing the call.
    - `ef` (default `200`, was `50`) is the recall/speed dial. Measured on a
      158,662-cell Xenium sample at `k = 30`: `ef = 50` reproduced 99.225% of
      the exact network's undirected edges, `ef = 200` reproduced 99.995%, for
      2.30s against 2.83s.
    - `n_threads_build` (default `1`) makes the search reproducible. Only the
      index build is nondeterministic -- concurrent insertion makes the graph
      depend on thread interleaving, while the search is read-only and
      deterministic at any thread count. With a parallel build, two runs of a
      seeded Leiden gave ARI 0.9368-0.9655; building on one thread they are
      identical (ARI 1.000000). Costs 2.82s -> 11.8s, still 6.8x faster than
      the 79.85s exact `dbscan::kNN()`, with accuracy unchanged (recall
      0.999980). Set to `NULL` to inherit `n_threads` and trade
      reproducibility for speed while exploring.
    - An `engine = "auto"` that selects by dataset size is planned; for now
      prefer `"dbscan"` on small data, where it is both exact and faster.

- `giotto` class gains a `source` slot for attaching a `gsource`-inheriting
  backend manager (see {GiottoDisk}).
- `createGiottoObject()` gains a `backend` param: accepts a filepath or a
  `gsource` object.
    - filepath is converted to a `GiottoDisk::gDirSource` automatically.
    - if `backend = NULL` -- the usual in-memory path is used.
- `saveGiotto()` and `loadGiotto()` now delegate to
  `GiottoDisk::snapshotSave()` / `GiottoDisk::snapshotLoad()` when a
  `gsource` backend is attached to the object. `foldername` and `dir` params
  are ignored in that case.
- On-disk persistence via `GiottoDisk::sourceWrite()` is now triggered inside
  `set_expression_values()`, `set_polygon_info()`, and `set_feature_info()`
  when a `gsource` backend is present.
- `createNetwork()` is now an S4 generic with method dispatch on
  `matrix`, `spatLocsObj`, `dimObj`, and `giotto` inputs.
- `networkParam` virtual class with concrete constructors
  `kNNNetworkParam()`, `sNNNetworkParam()`, and `delaunayNetworkParam()`
  configure `createNetwork()` calls. The legacy `type` string arg is
  superseded by passing a `*NetworkParam` object.
- `spatRelate()` generic — filter-form complement to `relate()`: returns `x`
  narrowed by a spatial predicate rather than a relation matrix. Eager method
  on `(giottoSpatial, giottoSpatial)` wraps `relate() + subset`; the on-disk
  lazy form lives in GiottoDisk via methods on `parquetGeomBase`.
- `as.igraph()` works on `spatialNetworkObj` and `nnNetObj`, registered on
  {igraph}'s generic. `@network` holds the graph directly, so this is an
  accessor rather than a construction and returns the slot unchanged. When the
  slot is backed, the contents are handed to `as.igraph()` again and dispatch
  finds the backend's own method -- {GiottoDisk} registers one for
  `parquetEdgeStore`. This is how a backed network should be read from here,
  rather than by naming a package GiottoClass only Suggests.

## changes
- `.ome.tif` and other tifs GDAL cannot open directly are now read through a GDAL VRT built over their JPEG-2000 tiles, so JPEG-2000 images load without python. This covers every 10x Xenium morphology image, and Aperio SVS whole-slide images. `to_simple_tif()` is unchanged and remains the fallback for qptiff and other codecs.
- `tif_metadata()` reads the XML from the `ImageDescription` tag in R. \pkg{tifffile} is only needed now for formats that keep their metadata in private binary tags (lsm, fluoview, nih, micromanager).
- `createGiottoPolygon()` on a JPEG-2000 mask now takes the mask workflow. It previously fell through to the vector-file workflow and failed there, because GDAL's missing-codec warning aborted the raster branch.

- `h5_file` param in `createGiottoObject()` is deprecated; use `backend`
  instead.
- `overlapInfo` class exported as an extension point.
- `updateGiottoObject()` now upgrades pre-0.6.0 objects to initialize the
  new `source` slot, and migrates `spatialNetworkObj` / `nnNetObj` to the
  new igraph-based storage (see breaking changes).
- `createNearestNetwork()`, `createSpatialDelaunayNetwork()`, and
  `createSpatialKNNnetwork()` are now thin wrappers over `createNetwork()`.
  Behavior is preserved.

## breaking changes

- Network subobject storage migrated from `data.table` to `igraph`:
    - `spatialNetworkObj`: `@networkDT` → `@network` (igraph),
      `@networkDT_before_filter` → `@unfiltered` (igraph)
    - `nnNetObj`: `@igraph` → `@network`
    - `updateGiottoObject()` migrates serialized pre-0.6.0 objects. The
      same migration runs on-load via `initialize()` so legacy subobjects
      passed to setters (`setSpatialNetwork()`, `setNearestNetwork()`)
      are upgraded transparently.
- `getSpatialNetwork()` `output` choices changed:
  `"networkDT_before_filter"` → `"unfiltered"`; new option `"igraph"`
  returns the underlying graph directly.
- Removed exported helpers `convert_to_full_spatial_network()` and
  `convert_to_reduced_spatial_network()`. The edge table is now an
  igraph; use `igraph::as_data_frame(net, what = "edges")` if a
  data.table form is needed.
- Geometric-transform methods (`flip()`, `spatShift()`, `t()`, `spin()`,
  `rescale()`, `affine()`) are no longer defined on `spatialNetworkObj`.
  Graph topology is invariant under these transforms; gobject-level
  walkers skip the spatial-network slot.
- `create_average_detection_DT()` removed. No callers remained in the suite
  once Giotto's gini markers moved onto
  `analyzeData(x, analyzeParam("feat_stats"), groups = )`, whose `perc_cells`
  column is the same statistic. `create_average_DT()` is retained because
  `create_cluster_matrix()` needs it and GiottoClass cannot depend on Giotto,
  but it duplicates that verb's `mean_expr` and should not be used in new code.

## bug fixes
- `createGiottoPolygon(make_valid = TRUE)` now has an effect on `data.frame`
  input. The `data.frame` method declared `make_valid` but never forwarded it,
  and `.evaluate_spatial_info()` ignored it on the table branch, so the
  argument was accepted and dropped. Only file and `SpatVector` input were
  ever made valid.
- Making polygons valid no longer shifts the attribute table. `makeValid()`
  drops geometries that GEOS repairs into lines, but leaves their attribute
  rows in place, so every `poly_ID` after the first dropped polygon named the
  wrong geometry. Affected `combineGeom()`, z-stack aggregation, `spatQuery()`
  and `createGiottoPolygon(make_valid = TRUE)`, which previously errored
  instead. Degenerate polygons are now dropped with their attributes and
  reported by `poly_ID`.
- Polygons built from a `data.frame` now warn, naming the `poly_ID`s, when a
  ring has too few vertices to close.
- `instructions()` and `instructions<-()` no longer emit a deprecation
  warning on every access. They were implemented on top of the deprecated
  `showGiottoInstructions()` / `readGiottoInstructions()` /
  `changeGiottoInstructions()` / `replaceGiottoInstructions()`, so each read
  or write raised the warning belonging to a function the caller never used.
  The implementation now lives in internals; the four deprecated functions
  remain exported and keep warning, but only for code that calls them
  directly.
- `spatIDs()` on an in-memory `spatialNetworkObj` returned `character(0)` for
  every network. It read `@network` as the `from`/`to` table the slot held
  before 0.6.0; `$` on an igraph is `NULL`, so a 855-edge network reported zero
  nodes. The disk-backed branch was unaffected.
- `spat_net_to_igraph()` failed with *please supply names for attributes*, from
  the same cause, and took its only two callers with it:
  `Giotto::spatialSplitCluster()` and `Giotto::identifyTMAcores()` were
  unusable on any in-memory spatial network. It now returns the graph the slot
  already holds rather than rebuilding it, undirecting a kNN network with
  `mode = "each"` so reciprocal pairs stay two edges, and dropping edge
  attributes not named in `attr` so the default stays bare as before. Backed
  networks are read through `as.igraph()`, which dispatches to the backend's
  own method.
- `tif_metadata(node =)` returns a one-row `data.frame` when exactly one node matches, rather than transposing it into a single column.

- `create_average_DT()` now selects each group's cells by `cell_ID` rather than
  by position. It fetches the expression matrix and the cell metadata
  independently, and nothing guarantees the two share a cell order. Where they
  diverged, cells were labelled with another cell's group. **Results will
  change for affected objects**; they were wrong before.
- `createGiottoPolygonsFromMask()` no longer loses polygons and `poly_ID`s when
  `mask_method = "single"` is used on a mask that encodes its background as a
  value instead of `NA`. Polygon parts are now indexed across mask values, so
  parts of different values no longer collide.
- `loadGiotto()` can read back a `giottoPolygon` or `giottoPoints` whose
  `SpatVector` has no attribute columns, which previously failed with
  `[names<-,SpatVector] incorrect number of names`.
- `createGiottoPolygon()` and `createGiottoPolygonsFromDfr()` no longer depend
  on the row order of their `data.frame` input. Vertices are now grouped by
  `poly_ID` before the `SpatVector` is built; input ordered by coordinate
  instead of by polygon previously produced too many polygons, with the
  attributes -- and so `poly_ID` -- dropped. Attributes that do not align with
  the geometries now raise an error instead of being discarded silently.
- fix "unused argument (ids = FALSE)" when subsetting a `giottoPolygon` object
- skip 0-entry `giottoPoints` in subset paths
- documentation fix in `methods-extract`

## enhancements

- `addCellMetadata()` and `addFeatMetadata()` now auto-detect a `cell_ID` /
  `feat_ID` column on `new_metadata` (including the column auto-added from a
  named vector) and route through key-based merge regardless of the caller's
  `by_column` value. Positional `cbind` is fragile when input row order does
  not match metadata row order; the key-based path is safe by construction.
  Positional input without a key column still works for backwards compatibility
  but now emits a warning so callers can opt in to safe alignment.
- Slot-accessor S4 generics widened with contract-stable formals
  (`spat_unit`, `feat_type`, `name`, `polygon_name`) so IDE autocomplete
  surfaces them past `gobject`. Setter method defaults for `name`
  (`"raw"`, `"pca"`, `"sNN.pca"`, `"enrichment"`, `"cell"`) moved out of
  the formals and into an explicit body-side fallback after
  `read_s4_nesting()`, allowing `name` on the generic without breaking
  the "user-supplied vs subobject-derived" resolution. `match.call`-based
  detection replaced with plain `is.null(name)` throughout.
- `getExpression` adopts `name` as the canonical formal alongside
  `values` (now a back-compat alias); they error if both supplied and
  differ.
- accessor generic formals aligned to one shared contract. Three generics
  deviated from it, which mattered once the formals moved onto the generics
  (S4 requires methods to match the generic's shared formal names and order,
  so the odd ones out could not be written against the common contract):
  `setMultiomics(result=)` and `setGiottoImage(image=)` are now `x` like every
  other setter, and `getPolygonInfo(polygon_name=)` is now `name`. The old
  names remain as deprecated aliases via `deprecate_param()` and continue to
  work with a warning. Positional calls are unaffected. Following the existing
  convention (`calculateOverlap(spatial_info)`, `getExpression(values)`), the
  aliases live on the methods only and reach them through the generic's
  `...`, so dispatch signatures are unchanged.
- `image_type` formal removed from `getGiottoImage()`, `setGiottoImage()`,
  `plotGiottoImage()`, and `distGiottoImage()`. The param had been
  deprecated for a long time and was a no-op in all four: the accessors
  never read it, `plotGiottoImage()` overwrote any supplied value by
  inspecting the class of the fetched image, and `distGiottoImage()`
  accepted only its default `"largeImage"`. Image class is determined from
  the object itself. Note this does not affect the `img_type` argument of
  `list_images()` / `list_images_names()`, which is a working filter and
  is retained.

# GiottoClass 0.5.1 (2026/05/14)

## changes
- deprecated `area()` in favor of `expanse()`
- `createExprObj()` no longer coerces to exotic matrix formats -- `expression_matrix_class` param is deprecated
  - backed matrices (`HDF5Array`, `dbMatrix`, `IterableMatrix`) must be pre-constructed and directly passed to be used.
- added superseded note to `createGiottoImage()` documentation
- `calculateOverlap()` and `overlapToMatrix()` param harmonization
- refactor of `saveGiotto()` and `loadGiotto()`
- code reorganization for `classes.R`

## new
- `aggregateFeatures()` giotto object wrapper function for running `calculateOverlap()` and `overlapToMatrix()`
- `overlapPointDT()` and `overlapIntensityDT()` classes to store overlaps relationships efficiently and help with aggregation pipeline
- `giottoBinPoints` class for efficient binned spatial points
- `rbind` method for `giottoPoints`
- `affine2d` class is now exported

## bug fixes
- `overlaps()` will now properly find image overlaps
- fix a naming bug when exporting images during save
- `SpatVector` -> `data.table` coercion no longer returns empty when it has no attributes

## enhancements
- `crop()` for `giottoLargeImage`/`giottoAffineImage` is now lazy by default — uses `terra::window()` instead of materializing a crop unless `write = TRUE` or a `filename` is given
- `giottoPoints` `plot()` gains a `sigma` param for Gaussian smoothing of rasterized density; `count = TRUE` (replaces `dens` param) is now the default
- image plotting rework -- more params exposed, better defaults


# GiottoClass 0.4.12 (2025/12/12)

## bug fixes
- `seuratToGiottoV5()`/`giottoToSeuratV5()` updated for `layer` param (replacing `slot`)

## enhancements
- automatic checking for `"count"` column in feature info

## new
- `misc` slot for storing unstructured data

## enhancements
- escape hatch for gobject initialize checking. Set option `"giotto.init_check_severity"` to `"stop"` (default), or `"warning"` depending on needs.

# GiottoClass 0.4.10 (2025/09/30)

## bug fixes
- fix bug in spatial grid getter [#1193](https://github.com/drieslab/Giotto/issues/1193) by RunBelief

## enhancements
- improvements to `tif_metadata()` and `to_simple_tif()`

# GiottoClass 0.4.9 (2025/07/07)

## bug fixes
- fix irregular default x padding/shift behavior [#1140](https://github.com/drieslab/Giotto/issues/1140) by rbutleriii

# GiottoClass 0.4.8 (2025/06/17)

## new
- `calculateLabelProportions()` for label proportions calculation from table, network neighbors, and polygon selections
- `clusterData()` generic for {bluster} integration

## changes
- `calculateSpatCellMetadataProportions()` now deprecated in favor of `calculateLabelProportions()`
- `spatValues()` 2nd arg has been changed to `feats`

# GiottoClass 0.4.7 (2025/05/06)

## new
- `spatIDs()<-` for `giottoPolygon`
- `combineGeom()` and `splitGeom()` for `giottoPolygon`
- `processData()` generic and `processParam` class
- `svkey` metaprogramming object for storing `spatValue()` parameters for later eval.

## bug fixes
- fixes and updates for {spatialdata} and {anndata} interoperability.
- fix bug introduced in 0.4.6 with `shear()` for `giottoPolygon`.
- fix {magick} `giottoAffineImage` realization when extent does not match the image dims ratio.
- fix `ext<-()` for `spatLocsObj`
- fix `ext<-()` for `giottoAffineImage`
- fix external affine matrix compatibility. `affine()` now has `pre_multiply` param to switch between working with affine matrices defined for either pre or post-multiply. Pre is the general convention, but Giotto internally uses post. This will be addressed in a later update.
- fix `giottoToSeuratV5()` selection of a default image to use
- replace internal usage of deprecated create_spat_net_obj -> createSpatNetObj and set_spatialNetwork -> setSpatialNetwork when calculating spatial networks.
- fix `createGiottoPolygon()` not preserving attributes from `data.table` inputs
- fix `loadGiotto()` error when a non-expected reticulate environment is already activated in the session
- fix `createGiottoLargeImage()` and `createGiottoPolygonsFromMask()` to align with {terra} `v1.8-21` `rast(noflip = TRUE)` [#1102](https://github.com/drieslab/Giotto/issues/1102) by StevenWijnen and rbutleriii
- add fallback for when attributes do not match number of geometries in `createGiottoPolygon()` so that poly_ID col is not dropped
- fix `calculateOverlap()`when raster aggregation finds polygons with no values
- fix `createGiottoPolygon()` dispatch on `character` so that it can access poly cleanup params
- fix incorrect `giottoInstructions` class in older objects now possible via `updateGiottoObject()`
- Remove imports on deprecated {terra} `convHull()`, `minRect()`, `minCircle()`, in favor of `hull()` usage [#1153](https://github.com/drieslab/Giotto/issues/1153) by demographix
- Remove import on {terra} `area()`, define as new generic from {GiottoClass}
- fix `loadGiotto()` issue when there are multiple polygons and some only some of them have created centroids [#304](https://github.com/drieslab/GiottoClass/issues/304)
- fix `joinGiottoObjects` polygon joins when there is more than one set of polygons [#305](https://github.com/drieslab/GiottoClass/issues/305)

## changes
- `remove_background_poly` now defaults to `TRUE` during polygon ingestion
- move {magick} from imports to suggests
- {terra} `>=v1.8-21`
- deprecate `spatQueryGiottoPolygons()` in favor of more general `spatQuery()`
- deprecate `ometif_metadata()` in favor of `tif_metadata()`
- deprecate `ometif_to_tif()` in favor of `to_simple_tif()`

## enhancements
- `[[` can now be used to select channels in `giottoLargeImage`-inheriting objects
- `XY()` replacement function for `SpatVector` now has `geomtype` param in case of `"none"` geometries
- `negate` param for negative selection in `sliceGiotto()`
- `spatUnit()` and `featType()` method for `giotto` to find existing spatial units and feature types
- expose `make_valid` param and `...` passing for `createGiottoPolygon()` `data.frame` method
- `createGiottoPolygon()` `part_col` param for generating multipolygons from `data.frame-like` inputs.
- `combineCellData()` `ext`, `xlim`, `ylim` cropping. (also background poly removal in case of cropping artefacts)
- large improvements to anndata and spatialdata converters (see [#294](https://github.com/drieslab/GiottoClass/pull/294))
- `spatLocsObj` can now be created from `numeric` xy pairs and xyz triplets
- improvements to `spatQuery()`
- add support for qptiff in `tif_metadata()` and `to_simple_tif()`
- `as.matrix()` for `nnNetObj()` [#262](https://github.com/drieslab/GiottoClass/issues/262)


# GiottoClass 0.4.6 (2025/01/17)

## bug fixes
- fix `gefToGiotto()` gene column reading [#255](https://github.com/drieslab/GiottoClass/pull/255) by cmubioinformatics
- fix `plot(add = TRUE)` for adding on to rasterized point plots
- fix `calculateOverlap()` when there are duplicate poly_IDs
- fix `calculateOverlap()` `giottoPolygon`, `giottoAffineImage` method. (The `giotto`, `missing` method still needs work)
- fix `calculateOverlap()` `giottoPolygon`, `giottoLargeImage` method that locked `name_overlap` to be `objName()` of `y`
- fix poly_ID generation when `terra::makeValid()` increases number of polys
- fix `giottoPoints`, `giottoPolygon` `as.data.table()` conversion when `row()` = 0

## new
- `names()` and `names<-()` for `giottoLargeImage` inheriting objects to name image layers

## enhancements
- `make_valid` param for `createGiottoPolygonsFromDfr()` and `createGiottoPolygonsFromGeoJSON()`


# GiottoClass 0.4.5 (2024/12/09)

## enhancements
- `spatUnit()<-` and `featType()<-` `list` methods
- `set_default_spat_unit()` and `set_default_feat_type()` now look for defaults when given `NA_character_` inputs as well.
- `update_giotto_params()` can now be turned off with `options("giotto.update_param" = FALSE)`

## bug fixes
- fix `giottoToSeuratV5()` Interoperability for Xenium Image
- fix `createGiottoPolygon()` when no attributes information is provided
- fix `createGiottoPolygonsFromGeoJSON()` reading from json GeometryCollection type inputs

# GiottoClass 0.4.4 (2024/11/14)

## bug fixes
- fix cell metadata desyncing after `joinGiottoObjects()`
- fix `readExprMatrix()` when IDs are numerical barcodes
- fix `giottoAffineImage` not being detected during `saveGiotto()` image export step.
- fix `giottoAffineImage` `reconnect()` method

## enhancements
- `saveGiotto()` now has `include_feat_coord` param. If `FALSE`, transcript coordinates will be dropped during saving, which will make the object much less memory intensive.
- `saveGiotto()` now has a `export_image` param. If `FALSE`, the image will not be re-exported during the save process. (They can still be reconnected)

# GiottoClass 0.4.2 (2024/10/30)

## bug fixes
- fix default method setting in `createNetwork()` for "delaunay" networks
- fix y spacing of `makePseudoVisium()`

## changes
- `makePseudoVisium()` `micron_scale` (multiplicative scalefactor to get micron 
  scaled values) supercedes `micron_size` which used the inverse.


# GiottoClass 0.4.1 (2024/10/28)

## new
- `buffer()` for `giottoPolygon`, `giottoPoints`, `spatLocsObj`. Default is to crop by voronoi borders with `settleGeom()`
- `settleGeom()` for `giottoPolygon` and `SpatVector` for finding non overlapping borders determined by voronoi


# GiottoClass 0.4.0 (2024/10/27)

## breaking changes
- stop exporting deprecated internal accessors
- terra requirement raised to 1.7.41 for `minCircle()`

## bug fixes
- fix `dimnames()` for some subobjects
- fix `joinGiottoObject()` for gobjects with only poly and point data [#233](https://github.com/drieslab/GiottoClass/issues/233)
- fix `joinGiottoObject()` for gobjects with image intensity overlaps features
- fix subsetting error due to expression `matrix` drop to `numeric` when only one cell is left
- `shift_vertical_step` and `shift_horizontal_step` args in `createGiottoPolygonsFromMask()` when numeric now shift by steps based on the dims of the image instead of just by the numerical value provided.
- fix feature metadata not being mixedsorted after join
- fix non-inclusive subsetting when not all minmax values are supplied to `subsetGiottoLocs()` 
- fix `giottoAffineImage` loading after being saved

## enhancements
- python packages to install through pip is now settable in `installGiottoEnvironment()` [#224](https://github.com/drieslab/GiottoClass/issues/224)
- `giotto` `initialize()` and slot checking behavior can be toggled now using `'giotto.init'` and `'giotto.check_valid'` options. [#946](https://github.com/drieslab/Giotto/issues/946) by rbutleriii
- `setGiotto()` now only initializes and performs checks once all items are added if a `list` input is provided.
- `instructions()` with no args will now call `createGiottoInstructions()`. You can also supply named args.
- `instructions(gobject, param)` and `instructions(gobject, param)<-` will now work for `giottoInstructions` objects for convenience.
- `[`, `[[`, `$`, `$<-`, and `subset()` for `giotto` see `?GiottoClass::subset_giotto`
- `subset` for `spatIDs()` and `featIDs()`
- `objName()`, `spatUnit()`, `featType()` generics now return `NA_character_` instead of erroring when used on unsupported classes.
- `ext()` and `ext<-()` can now be used to get and set extent of `affine2d`
- `rownames()`, `colnames()`, `dimnames()` for `giotto`
- `spatValues()` can get values from multiple spatial units.
- `createGiottoPolygonsFromMask()` now works with anything `terra::rast()` can read
- `createGiottoLargeImage()` now works with anything `terra::rast()` can read

## new
- `sliceGiotto()` for pulling out specific spatial units and feature types as independent `giotto` objects
- `splitGiotto()` for splitting a Giotto object into a list of Giotto objects based on a cell metadata column
- `as.list()` method for `giotto` to dump the data as a list of subobjects
- `XY()` and `XY<-()` for accessing and setting coordinate values of subobjects as `matrix`
- terra `convHull()`, `minRect()`, `minCircle()` for Giotto spatial vector classes
- `area()` for `SpatVector` and `giottoPolygon`


# GiottoClass 0.3.5 (2024/08/28)

## breaking changes
- `set_giotto_python_path()` will now also initialize python env to set by default and print which python env is active, but otherwise do nothing if any python env has already been initialized.
- deprecated `readGiottoInstructions()`, `showGiottoInstructions()`, `changeGiottoInstructions()`, `replaceGiottoInstructions()` in favor of `instructions()` generic

## bug fixes
- intensity images now automatically scale to estimated highest value
- `giottoPolygon` `plot()` default `max_poly` raised to `1e6`
- `giottoInstructions` no longer lose class when specific params are replaced
- `ometif_to_tif()` now checks for _imagecodecs_ package as well
- `anndataToGiotto()` and `giottoToAnndata` now check for _anndata_ package as well.
- fix `joinGiottoObjects()` `"z_stack"` join method
- fix error in documentation [#214](https://github.com/drieslab/GiottoClass/issues/214) by shaojunyu
- fix error in `installGiottoEnvironment()` [#1006](https://github.com/drieslab/Giotto/issues/1006) by 13954380607

## enhancements
- `print()` method for `giottoInstructions`
- `rbind()` for `spatLocsObj`


# GiottoClass 0.3.4 (2024/08/04)

## bug fixes
- hotfix anndata matrix support [#216](https://github.com/drieslab/GiottoClass/issues/216) by wwang-chcn

# GiottoClass 0.3.3 (2024/07/29)

## bug fixes
- fix flipping issue with `giottoAffineImage` for certain affine transforms

## enhancements
- `missing` method for `affine()` instantiates an `affine2d` object

# GiottoClass 0.3.2 (2024/07/26)

## breaking changes
- python environment installation and how it relates to default settings such as .condarc may have changed.
- `giottoImage` `name` slot now requires `character` and will not accept `NULL`

## bug fixes
- `loadGiotto()` no longer errors with similarly named spat_units or feat_types (e.g. "cell" and "new_cell" would previously throw an error)
- fix in `giottoToSpatialExperiment()`
- fix for `giottoToSeuratV5` for cosmx mini dataset [#989](https://github.com/drieslab/Giotto/issues/989) by guillermoturiel
- fix issue with prints in `createGiottoCosMxObject()` [#960](https://github.com/drieslab/Giotto/issues/960) by GBeattie

## enhancements
- `verbose` param for `createNearestNetwork()`
- `checkGiottoEnvironment()` in addition to full filepaths, also now supports name of environment or installation directory
- `installGiottoEnvironment()`, `removeGiottoEnvironment()` now have `conda` param for setting path to conda executable and `envname` param for specifying environment by name
- `installGiottoEnvironment()` now has `confirm` param for skipping path input checks
- `t()` for `giotto` now affects images as well.

## new
- `affine()` for `giottoPolygon`, `giottoPoints`, `spatLocsObj`, `giotto`
- `shear()` for `giottoPoints`, `giottoPolygon`, `spatLocsObj`, `affine2d`
- `affine2d` class for accumulating linear transforms to be used with `affine()`
- `initialize()`, `[`, `$`, `show()`, `plot()`, methods for `affine2d`
- `spin()`, `rescale`, `spatShift()`, `affine()`, `flip()`, `shear()` `t()` methods for `affine2d`
- `giottoAffineImage` class for just-in-time affine transformed images
- `initialize()`, method for `giottoLargeImage`
- `initialize()`, `ext()`, `crop()`, `rescale()`, `spatShift()`, `plot()`, methods for `giottoAffineImage`
- `rescale()` method for `giottoImage`
- `spin()`, `shear()`, `affine()`, `flip()`, `t()` methods for `giottoAffineImage` and `giottoLargeImage` (which converts to `giottoAffineImage`)
- `as()` conversion from `giottoLargeImage` to `giottoAffineImage`
- `.get_centroid_xy()` internal for getting numeric centroid xy values of any object that responds to `ext()`
- `.bound_poly()` internal for generating a dummy polygon from the extent of any object that responds to `ext()`
- `.aff_shift_2d()`, `.aff_shift_2d<-()`, `.aff_linear_2d`, `.aff_linear_2d()<-` internals for accessing and manipulating affine matrices


# GiottoClass 0.3.1 (2024/05/21)

## bug fixes
- allow passing of additional params with `setGiotto()` with `...`
- `spatShift()` can now perform z shifts when start `spatLocsObj` has no z information
- fix bug in `joinGiottoObjects()` after v0.3.0 where it looks for the now non-existent `@largeImages` slot
- fix bug in `.update_image_slot()` after v0.3.0 where a NULL `@largeImages` slot will result in an error
- fix bugs in `spatShift()` and `rescale()` methods for `giotto` when setting a default `spat_unit` and `feat_type`

## enhancements
- `joinGiottoObjects()` extent detection and xshift defaults now depend on `ext()` of the gobject instead of any images (when available)
- `joinGiottoObjects()` now has a `dry_run` param for previewing where datasets will be spatially located after the join

## new
- `as()` conversion from `giottoLargeImage` to `array`
- `as.matrix()` method for `spatLocsObj()`


# GiottoClass 0.3.0 (2024/05/13)

## breaking changes
- deprecation of `reconnect_image_object()`, `reconnect_giottoImage_MG()` and `reconnect_giottoLargeImage()` internals in favor of simpler `reconnect()` generic
- `giotto` `@largeImage` slot is removed. All images now exist in `@images` slot.
- backwards compatibility for S3 `spatialNetworkObj` removed
- Not finding a specific `spatialNetworkObj` with `getSpatialNetwork()` is now upgraded to an error instead of returning `NULL` to be in line with other accessors.
- backwards compatibility for bare `data.table` spatial coordinates information is removed

## bug fixes
- fix `plot()` params passing for `giottoPolygon` when `type = "centroid"`
- fix `ext()` output for `giottoImage`
- `spatShift()` and `rescale()` now also affect gobject attached images [#945](https://github.com/drieslab/Giotto/issues/945) by rbutleriii

## enhancements
- use faster `terra::rasterize()` and `terra::plot()` instead of `scattermore::scattermoreplot()` for `giottoPoints` `plot()` method
- `plot()` `giottoPoints` method now plots density when `dens = TRUE`
- `show_max` param in `density()` and `hist()` to plot the image object's `max_window` setting
- `.identify_background_range_polygons()` now finds any polygons larger than a threshold percentage than the overall extent of the `SpatVector` input.
- `ext()` can now be used with `giotto` objects [#945](https://github.com/drieslab/Giotto/issues/945) by rbutleriii
- `ext()<-` can now be used with `giottoImage`
- `as` conversion from `giottoLargeImage` to `giottoImage` (`giottoImage` is sampled)
- `crop()` works for `spatialNetworkObj`

## new
- new `spatValues()` to get specific values from a `giotto` object in `data.table` format
- new `ometif_to_tif` to convert between .ome.tif and .tif
- new `terra::density()` and `terra::hist()` wrappers for `giottoLargeImage`




# GiottoClass 0.2.3 (2024/03/12)

## bug fixes
- fix `saveGiotto` with `overwrite = TRUE` [#870](https://github.com/drieslab/Giotto/issues/870) by rbutlerii
- fix `plot()` method param passing for `giottoLargeImage`. Ensure access to terra params

## enhancements
- `createGiottoPoints` `data.frame` method can now select which columns to use with `x_colname`, `y_colname`, `feat_ID_colname` params
- `giotto` now responds to spatial manipulation generics: `t()`, `flip()`, `rescale()`, `spatShift()`, `spin()`
- `spatUnit()` and `featType()` are now vectorized
- internal `get_spatial_locations_list()` and `get_spatial_network_list()` accessors now accept ":all:" token to get all available, ignoring spat_unit


# GiottoClass 0.2.2 (2024/03/01)

## bug fixes
- fix `createGiottoPolygonsFromMask()` IDs being applied out of sync to mask values
- fix `createGiottoPolygon()` `character` method dispatch for `raster` inputs
- remove unused `fix_multipart` param in `createGiottoPolygonsFromMask()`
- fix `giottoPolygon` ID caching after `rbind()`

## enhancements
- `createGiottoPolygonsFromMask()` now has `ID_fmt` param for finer control of automatic `poly_ID` generation
- `.flip_spatvect()` internal for flipping `SpatVector` across arbitrary x and y vals


# GiottoClass 0.2.1 (2024/02/28)

## breaking changes
- `giotto` slot `versions` supercedes `OS_platform`. Used for tracking GiottoClass version.

## bug fixes
- fix `subsetGiotto` unused `on` argument
- fix `giotto` object saving when image intensities overlaps data are present.
- fix `exprObj` `show()` for small matrices
- fix `giotto` `calculateOverlap()` method when working with image intensities data.

## new
- `createNetwork()` hub function for creation of Giotto NN and spatial networks directly from matrices. Mainly for developers and advanced users.
- `edge_distances()` for calculating euclidean distances from numeric m x n `matrix` (nodes) and a `data.table` with *from* and *to* cols that define node connections.

## enhancements
- `addCellMetadata()` and `addFeatMetadata()` now support merging on the names of provided vector and factor data with metadata *cell_ID*/*feat_ID*.


# GiottoClass 0.1.3 (2024/01/12)

## bug fixes
- fix unexpected sorting in `addCellMetadata()` and `addFeatMetadata()` [#853](https://github.com/drieslab/Giotto/issues/853) by rbutleriii

## new
- `init_gobject` param in `loadGiotto()` to control whether object initialization is also performed after load
- vignette for image tools

## enhancements
- ID sorts now use `gtools::mixedsort()` [#853](https://github.com/drieslab/Giotto/issues/853) by rbutleriii
- more subobjects respond to `colnames`, `rownames`, `dimnames`
- `plot()` and `show()` now handle 3D `spatLocsObj`

# GiottoClass 0.1.2 (2024/01/02)

## Added
- Added: `max_window` and `colors` slots to `giottoLargeImage`. Use `GiottoClass:::.update_giotto_image()` to update outdated objects.
- Added: `.bitdepth()` internal function to detect image bitdepth from sampled values
- Added: re-export `getMonochromeColors()` from *GiottoUtils*
- Added: `giottoPolygon`, `giottoLargeImage` method for `calculateOverlap()`
- Added: vignette for working with spatial classes
- Added: `output` param to `.spatraster_sample_values()`. Can now return as sampled `data.frame`, `array`, `magick`, `EBImage`

## bug fixes
- param fixes in raster `calculateOverlap()` workflows


# GiottoClass 0.1.1 (2023/12/16)

## Breaking Changes
- Removed: `getRainbowColors()` to *GiottoUtils*
- Removed: `get_prev_fname()` and `get_args()` to *GiottoUtils* 
- Removed: `aggregateStacksPolygonsOLD()`

## Added
- Added: `.gstop()` for GiottoClass specific errors
- Added: `plot()` method for `dimObj`
- Added: `ncol()` and `nrow()` methods for `dimObj`
- Added: `dimObj` additional info now accessible using `$` and `$<-`
- Added: `spatEnrObj` cols now accessible using `$` and `$<-`
- Added: `evaluate_input()` as exported wrapper for `evaluate` functions
- Added: `as.polygons()` `data.frame` method for correctly formatted data.tables (replaces internal `dt_to_spatvector_polygon()`)
- Added: `as.polygons()` `data.frame` method for correctly formatted data.tables (replaces internal `dt_to_spatvector_points()`)
- Added: autocompletes for `$` with `spatLocsObj`, `spatEnrObj`, `dimObj`, `cellMetaObj`, `featMetaObj`, `giottoPolygon`, `giottoPoints`
- Added: `toplevel_params` param to `subsetGiottoLocs()`
- Added: `spin`, `spatShift`, `rescale` methods for `data.frame`

## Changes
- Fixed: Provenance now not accidentally created as a list in `addSpatialCentroidLocations()`
- Changed: `giottoPolygon` `plot()` now automatically switches to centroid plotting with more than 1e4 polys
- Changed: Package internal functions now have `.` prefix



# GiottoClass 0.1.0 (2023/11/29)

## Breaking Changes

- Giotto subsetting rework

## Added

- Added: `terraVectData` as virtual parent class for `giottoPolygon` and `giottoPoints` classes
- Added: `$` and `$<-` methods for `terraVectData`
- Added: `ext<-()` method for `giottoPolygon`, `giottoPoints`
- Added: `crop()` method for `giottoLargeImage`, `giottoPoints`
- Added: `[` subsetting for `giottoPoints` and `giottoPolygon` with numerical, logical, and character (by ID)
- Added: `as.sf()` and `as.stars()` converters for `giottoPoints` and `giottoPolygon`
- Added: `setGiotto()` generic
- Added: `gap` param to `tessellate()` which introduces a variable gap between the polygons tessellated
- Added: `triGrid()`
- Added: `orthoGrid()`
- Added:  DelayedMatrixStats to suggests
- Added: `calculateOverlap()` and `overlapToMatrix()` as generic functions for feature and image overlap aggregations

## Changes

- Improved: performance of gefToGiotto()
- Updated: `spatIDs()` and `featIDs()` methods for `giottoPolygon` and `giottoPoints` to allow returning non-unique IDs
- Added: check for `plot()` when `giottoPolygon` or `giottoPoints` objects contain no geometries
- Added: warning for `crop()` when `giottoLargeImage`, `giottoPolygon`, `giottoPoints` objects are being cropped with an extent that does not include any information
- Changed: Conversion of `createGiottoPoints()` to a generic function
- Deprecate: `radius` param in favor of `shape_size` for `tessellate()`
- Fixed: python `.install_github_link_pip()` param
- Fixed: python `create_AnnData()` added to `globals.R`





# GiottoClass 0.4.0

## breaking changes
- stop exporting deprecated internal accessors

## bug fixes
- fix `dimnames()` for some subobjects
- fix `joinGiottoObject()` for gobjects with only poly and point data [#233](https://github.com/drieslab/GiottoClass/issues/233)
- fix `joinGiottoObject()` for gobjects with image intensity overlaps features
- fix subsetting error due to expression `matrix` drop to `numeric` when only one cell is left
- `shift_vertical_step` and `shift_horizontal_step` args in `createGiottoPolygonsFromMask()` when numeric now shift by steps based on the dims of the image instead of just by the numerical value provided.
- fix feature metadata not being mixedsorted after join

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




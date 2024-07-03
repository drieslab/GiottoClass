
# GiottoClass 0.3.1

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
- `spatShift()` and `rescale()` now also affect attached images [#865](https://github.com/drieslab/Giotto/issues/865) by rbutleriii

## enhancements
- use faster `terra::rasterize()` and `terra::plot()` instead of `scattermore::scattermoreplot()` for `giottoPoints` `plot()` method
- `plot()` `giottoPoints` method now plots density when `dens = TRUE`
- `show_max` param in `density()` and `hist()` to plot the image object's `max_window` setting
- `.identify_background_range_polygons()` now finds any polygons larger than a threshold percentage than the overall extent of the `SpatVector` input.
- `ext()` can now be used with `giotto` objects [#865](https://github.com/drieslab/Giotto/issues/865) by rbutleriii
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




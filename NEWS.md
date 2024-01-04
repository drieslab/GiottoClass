

# GiottoClass 0.1.3

## new
- vignette for image tools

## enhancements
- more subobjects respond to `colnames`, `rownames`, `dimnames`
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




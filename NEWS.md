
# Giotto Class 0.1.1 (TBD)

## Breaking Changes

## Added

## Changes
- Fixed: Provenance now not accidentally created as a list in `addSpatialCentroidLocations()`

# Giotto Class 0.1.0 (23/11/29)

## Breaking Changes

- Giotto subsetting rework

## Added

- Added `terraVectData` as virtual parent class for `giottoPolygon` and `giottoPoints` classes
- Added `$` and `$<-` methods for `terraVectData`
- Added `ext<-()` method for `giottoPolygon`, `giottoPoints`
- Added `crop()` method for `giottoLargeImage`, `giottoPoints`
- Added `[` subsetting for `giottoPoints` and `giottoPolygon` with numerical, logical, and character (by ID)
- Added `as.sf()` and `as.stars()` converters for `giottoPoints` and `giottoPolygon`
- Added `setGiotto()` generic
- Added `gap` param to `tessellate()` which introduces a variable gap between the polygons tessellated
- Added `triGrid()`
- Added `orthoGrid()`
- Added  DelayedMatrixStats to suggests

## Changes

- Improved performance of gefToGiotto()
- Updated `spatIDs()` and `featIDs()` methods for `giottoPolygon` and `giottoPoints` to allow returning non-unique IDs
- Added check for `plot()` when `giottoPolygon` or `giottoPoints` objects contain no geometries
- Added warning for `crop()` when `giottoLargeImage`, `giottoPolygon`, `giottoPoints` objects are being cropped with an extent that does not include any information
- Changed: Conversion of `createGiottoPoints()` to a generic function
- Deprecate: `radius` param in favor of `shape_size` for `tessellate()`
- Fixed: python `install_github_link_pip()` param
- Fixed: python `create_Anndata` added to `globals.R`



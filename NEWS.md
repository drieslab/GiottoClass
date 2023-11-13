# Giotto Class 0.0.1 (Release TBD)

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

## Changes
- Improved performance of gefToGiotto()
- Updated `spatIDs()` and `featIDs()` methods for `giottoPolygon` and `giottoPoints` to allow returning non-unique IDs
- Added check for `plot()` when `giottoPolygon` or `giottoPoints` objects contain no geometries
- Added warning for `crop()` when `giottoLargeImage`, `giottoPolygon`, `giottoPoints` objects are being cropped with an extent that does not include any information
- Changed: Conversion of `createGiottoPoints()` to a generic function

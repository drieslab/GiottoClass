import anndata as ad
import numpy as np
import pandas as pd
from dask_image.imread import imread
from xarray import DataArray
import geopandas as gpd
from shapely.geometry import Point
import glob, os

from spatialdata import SpatialData
from spatialdata.models import Image2DModel, ShapesModel, TableModel
from spatialdata.transformations.transformations import Identity

def createImageModel(temp):
    images = {}
    image_paths = glob.glob(temp+"*.png")
    for path in image_paths:
        image = imread(path).squeeze()
        if len(image.shape) == 2:
            image = np.expand_dims(image, axis=-1)
        image = image.transpose(2,0,1)
        image = DataArray(image, dims=("c","y","x"))
        image_name = os.path.splitext(os.path.basename(path))[0]
        images[image_name] = Image2DModel.parse(image)
    return images

def createShapeModel(spat_locs, spot_radius):
    shapes_df = pd.DataFrame()
    shapes_df['x'] = spat_locs.sdimx
    shapes_df['y'] = spat_locs.sdimy
    shapes_df['geometry'] = shapes_df.apply(lambda row: Point(row['x'], row['y']), axis=1)
    shapes_df = shapes_df.drop(['x','y'], axis=1)
    shapes_df['radius'] = spot_radius
    shapes_df = shapes_df.rename_axis('spot_id')
    gdf = gpd.GeoDataFrame(shapes_df, geometry='geometry')
    gdf.set_crs(epsg=4326, inplace=True)
    shapes = ShapesModel.parse(gdf)
    return shapes

def createTableModel(temp):
    alist = glob.glob(temp+"*.h5ad")
    adata = ad.read_h5ad(alist[0])
    table = TableModel.parse(adata)
    return table

def createSpatialData(temp, spat_locs, spot_radius, save_directory, image_exists):
    if image_exists:
        images = createImageModel(temp)
    table = createTableModel(temp)
    shapes = createShapeModel(spat_locs, spot_radius)
    if image_exists:
        sd = SpatialData(table = table, images = images)
    else:
        sd = SpatialData(table = table)
    sd.shapes["Shapes"] = shapes
    sd.write(save_directory, overwrite = True)

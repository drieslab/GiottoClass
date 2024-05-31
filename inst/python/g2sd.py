import anndata as ad
import numpy as np
import pandas as pd
from dask_image.imread import imread
from xarray import DataArray
import geopandas as gpd
from shapely.geometry import Point
import os

from spatialdata import SpatialData
from spatialdata.models import Image2DModel, ShapesModel, TableModel
from spatialdata.transformations.transformations import Identity

def createImageModel():
    images = {}
    hires_image_path = "temp_image.png"
    hires_img = imread(hires_image_path).squeeze().transpose(2,0,1)
    hires_img = DataArray(hires_img, dims=("c","y","x"))
    images["hires_image"] = Image2DModel.parse(hires_img, transformations={"downscaled_hires": Identity()})
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
    alist = os.listdir(temp)[0]
    adata = ad.read_h5ad(os.path.join(temp, alist))
    table = TableModel.parse(adata)
    return table

def createSpatialData(temp, spat_locs, spot_radius, save_directory):
    images = createImageModel()
    table = createTableModel(temp)
    shapes = createShapeModel(spat_locs, spot_radius)
    sd = SpatialData(table = table, images = images)
    sd.shapes["Shapes"] = shapes
    sd.write(save_directory)

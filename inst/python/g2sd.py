import anndata as ad
import numpy as np
import pandas as pd
from dask_image.imread import imread
from xarray import DataArray
import geopandas as gpd
from shapely.geometry import Point
import glob, os

from spatialdata import SpatialData
from spatialdata.models import Image2DModel, ShapesModel, PointsModel, TableModel
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

def createShapesModel(temp):
    shapes = {}
    shapes_paths = glob.glob(temp+"shapes/*.geojson")
    for path in shapes_paths:
        polygon_gdf = gpd.read_file(path)
        poly_name = os.path.splitext(os.path.basename(path))[0]
        shapes[poly_name] = ShapesModel.parse(polygon_gdf)
    return shapes

def createPointsModel(temp):
    points = {}
    points_paths = glob.glob(temp + "points/*.csv")
    for path in points_paths:
        points_df = pd.read_csv(path)
        points_name = os.path.splitext(os.path.basename(path))[0]
        points[points_name] = PointsModel.parse(points_df, feature_key = "feat_ID")
    return points

def createTableModel(temp):
    alist = glob.glob(temp+"*.h5ad")
    adata = ad.read_h5ad(alist[0])
    table = TableModel.parse(adata)
    return table

def createSpatialData(temp, save_directory, image_exists):
    if image_exists:
        images = createImageModel(temp)
    table = createTableModel(temp)
    if image_exists:
        sd = SpatialData(tables = table, images = images)
    else:
        sd = SpatialData(tables = table)
    shapes = createShapesModel(temp)
    for poly_name, polygon in shapes.items():
        sd.shapes[poly_name] = polygon
    points = createPointsModel(temp)
    for point_name, point in points.items():
        sd.points[point_name] = point
    sd.write(save_directory, overwrite = True)

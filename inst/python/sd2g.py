import warnings
import numpy as np
import pandas as pd
import scipy as sc
from time import perf_counter

from spatialdata import SpatialData

__name__ = "sd2g"
__package__ = "sd2g"

# Imports and safeguards
def read_spatialdata_from_path(sd_path = None):
    if sd_path is None:
        print("Please provide a path to the SpatialData object folder.")
        assert(False)
    try:
        sdata = SpatialData.read(sd_path)
    except (FileNotFoundError):
        print(f"File {sd_path} was not found.")
        print("Please try again.")
        raise(FileNotFoundError)
    return sdata

# Extract gene expression
def extract_expression(sdata = None):
    expr_df_dict = {}
    table_names = list(sdata.tables.keys())
    for tn in table_names:
        expr = sdata.tables[tn].X.transpose().todense()
        expr_df = pd.DataFrame(expr, index=sdata.tables[tn].var.index, columns=sdata.tables[tn].obs.index)
        expr_df_dict[tn] = expr_df
    return expr_df_dict

# Extract cell IDs
def extract_cell_IDs(sdata = None, tn = None):
    if tn is None:
        cID = {}
        for tn in list(sdata.tables.keys()):
            cID[tn] = sdata.tables[tn].obs.index.tolist()
    else:
        cID = sdata.tables[tn].obs.index.tolist()
    return cID

# Extract feature IDs
def extract_feat_IDs(sdata = None, tn = None):
    if tn is None:
        fID = {}
        for tn in list(sdata.tables.keys()):
            fID[tn] = sdata.tables[tn].var.index.tolist()
    else:
        fID = sdata.tables[tn].var.index.tolist()
    return fID

# Alternative expression data
def extract_layer_names(sdata = None):
    layers_dict = {key: list(adata.layers.keys()) for key, adata in sdata.tables.items() if len(adata.layers) > 0}
    return layers_dict

def extract_layered_data(sdata = None, key = None, layer_name = None):
    target_layer = sdata.tables[key].layers[layer_name]
    if type(target_layer) == sc.sparse.csr_matrix:
        target_layer = target_layer.T
    elif type(target_layer) == sc.sparse.csr.csr_matrix:
        target_layer = target_layer.T
    else:
        target_layer = pd.DataFrame(target_layer)
    return target_layer

# Extract spatial locations
def extract_spatial(sdata = None):
    spatial_dict = {}
    table_names = list(sdata.tables.keys())
    for tn in table_names:
        spatial = sdata.tables[tn].obsm['spatial']
        spatial_df = pd.DataFrame(spatial)
        spatial_df.columns = ['X', 'Y']
        spatial_df['Y'] = spatial_df['Y'] * -1
        spatial_dict[tn] = spatial_df
    return spatial_dict

def parse_obsm_for_spat_locs(sdata = None):
    sp_dict = {}
    table_names = list(sdata.tables.keys())
    for tn in table_names:
        cID = np.array(extract_cell_IDs(sdata, tn))
        spat_locs = None
        spat_key = None
        try:
            spat_locs = sdata.tables[tn].obsm["spatial"]
            spat_key = "spatial"
        except (KeyError):
            spat_keys = [i for i in sdata.tables[tn].obsm if 'spatial' in i]
            spat_key = next(iter(spat_keys), None)
            if spat_key:
                spat_locs = sdata.tables[tn].obsm[spat_key]
        if spat_locs is not None:
            if len(cID) != spat_locs.shape[0]:
                print(f"Warning: Mismatch in number of cells for {tn}. cID has {len(cID)}, but spatial locations have {spat_locs.shape[0]} rows.")
            print(f"Spatial locations found for {tn} using key: {spat_key}")
            
            cID = np.array(cID).reshape(len(cID),1)
            spat_locs = np.concatenate((spat_locs,cID), axis = 1)
            num_col = spat_locs.shape[1]

            colnames = ["sdimx", "sdimy", "sdimz", "cell_ID"]
            conv = {"sdimx": float, "sdimy": float,"sdimz": float,"cell_ID": str}

            if num_col > 3:
                spat_locs = pd.DataFrame(spat_locs, columns = colnames)
            else:
                del colnames[2]
                del conv['sdimz']
                spat_locs = pd.DataFrame(spat_locs, columns = colnames)
            
            spat_locs = spat_locs.astype(conv)
            spat_locs["sdimy"] = -1 * spat_locs["sdimy"]
            sp_dict[tn] = spat_locs

        else:
            print(f"Spatial locations were NOT found for {tn}. Skipping...")
    return sp_dict 

# Extract spatial networks
def find_SN_keys(sdata = None, key_added = None, tn = None):
    sn_key_list = []
    prefix = ""
    suffix = "neighbors"

    if key_added is None:
        map_key = prefix + suffix  # Default key to look for
        if map_key not in sdata.tables[tn].uns:
            print(f"Warning: Key '{map_key}' not found in sdata.tables[{tn}].uns. Skipping {tn}...")
            return None
        tmp_keys = sdata.tables[tn].uns[map_key].keys()
        for i in tmp_keys:
            sn_key_list.append(sdata.tables[tn].uns[map_key][i])

    elif ".txt" in key_added:
        line_keys = []
        with open(key_added) as f:
            for line in f.readlines():
                line = line.strip()
                line_key_added = line + "_" + suffix
                line_keys.append(line_key_added)
        for key in line_keys:
            if key not in sdata.tables[tn].uns:
                print(f"Warning: Key '{key}' not found in sdata.tables[{tn}].uns. Skipping {tn}...")
                return None
            map_keys = sdata.tables[tn].uns[key].keys()
            for i in map_keys:
                sn_key_list.append(sdata.tables[tn].uns[key][i])

    elif key_added is not None:
        key_added = key_added + suffix
        if key_added not in sdata.tables[tn].uns:
            print(f"Warning: Key '{key}' not found in sdata.tables[{tn}].uns. Skipping {tn}...")
            return None
        map_keys = sdata.tables[tn].uns[key_added].keys()
        for i in map_keys:
            sn_key_list.append(sdata.tables[tn].uns[key_added][i])
        
    if len(sn_key_list) == 0:
        sn_key_list = None
    return sn_key_list

def extract_SN_connectivities(sdata = None, key_added = None):
    connectivities = {}
    for tn in sdata.tables.keys():
        sn_key_list = find_SN_keys(sdata = sdata, key_added = key_added, tn = tn)
        if type(sn_key_list) is type(None):
            continue
        for sk in sn_key_list:
            if "connectivities" in sk:
                connectivities[(tn, sk)] = sdata.tables[tn].obsp[sk]
    return connectivities

def extract_SN_distances(sdata = None, key_added = None, tn = None, sn_key_list = None):    
    distances = None
    if sn_key_list is None:
        sn_key_list = find_SN_keys(sdata = sdata, key_added = key_added, tn = tn)

    if type(sn_key_list) is type(None):
        return distances
    
    sk_trim = sn_key_list.replace("_connectivities", "")
    distance_key = sk_trim + '_distances'
    distances = sdata.tables[tn].obsp[distance_key]
    
    return distances

# Extract PCA
def extract_pca(sdata = None):
    pca_dict = {}
    for tn in sdata.tables.keys():
        o_keys = getattr(sdata.tables[tn], "obsm_keys", lambda: [])()
        v_keys = getattr(sdata.tables[tn], "varm_keys", lambda: [])()
        u_keys = getattr(sdata.tables[tn], "uns", {}).get("pca", {}).keys()

        pca = dict()

        for ok in o_keys:
            if "X_pca" in ok:
                pca['pca'] = sdata.tables[tn].obsm[ok]
                break
        for vk in v_keys:
            if "PCs" in vk:
                pca['loadings'] = sdata.tables[tn].varm[vk]
                break  # Use first valid PC loading
        if u_keys:
            for uk in u_keys:
                if "variance" in uk:
                    pca['eigenvalues'] = sdata.tables[tn].uns["pca"][uk]
            
        if pca:
            pca_dict[tn] = pca
    return pca_dict

# Extract UMAP
def extract_umap(sdata = None):
    umap_dict = {}
    for tn in sdata.tables.keys():
        o_keys = getattr(sdata.tables[tn], "obsm_keys", lambda: [])()

        umap = None

        for ok in o_keys:
            if "X_umap" in ok:
                umap = sdata.tables[tn].obsm[ok]
        if umap is not None:
            umap_dict[tn] = umap

    return umap_dict

# Extract tSNE
def extract_tsne(sdata = None):
    tsne_dict = {}
    for tn in sdata.tables.keys():
        o_keys = getattr(sdata.tables[tn], "obsm_keys", lambda: [])()

        tsne = None

        for ok in o_keys:
            if "X_tsne" in ok:
                tsne = sdata.tables[tn].obsm[ok]
        if tsne is not None:
            tsne_dict[tn] = tsne

    return tsne_dict

## Extract NN network
def find_NN_keys(sdata = None, key_added = None, tn = None):
    nn_key_list = []
    
    if key_added is None:
        param_keys = list(sdata.tables[tn].uns.keys())
        for pk in param_keys:
            if "neighbors" in pk and "spatial" not in pk:
                try:
                    tmp_keys = sdata.tables[tn].uns[pk].keys()
                except KeyError:
                    tmp_keys = None
                    return None
                for i in tmp_keys:
                    nn_key_list.append(sdata.tables[tn].uns[pk].keys())
                break
    elif ".txt" in key_added:
        line_keys = []
        with open(key_added) as f:
            for line in f.readlines():
                line = line.strip()
                line_keys.append(line)
        for key in line_keys:
            if key not in sdata.tables[tn].uns:
                print(f"Warning: Key '{key}' not found in sdata.tables[{tn}].uns. Skipping {tn}...")
                return None
            map_keys = sdata.tables[tn].uns[key].keys()
            for i in map_keys:
                nn_key_list.append(sdata.tables[tn].uns[key][i])
    elif key_added and key_added.casefold() != "spatial":
        map_keys = sdata.tables[tn].uns[key].keys()
        for i in map_keys:
            nn_key_list.append(sdata.tables[tn].uns[key][i])
    elif key_added and key_added.casefold() == "spatial":
        s1 = "String 'spatial' cannot be used as n_key_added to retrieve a Nearest Neighbor Network. "
        s2 = "This results from conflicting keys for nearest neighbor and spatial networks. "
        s3 = "\nSee defaults here:\nhttps://scanpy.readthedocs.io/en/stable/generated/scanpy.pp.neighbors.html\nhttps://squidpy.readthedocs.io/en/stable/api/squidpy.gr.spatial_neighbors.html"
        msg = s1+ s2 + s3
        warnings.warn(msg)
    
    if len(nn_key_list) == 0:
        nn_key_list = None
    return nn_key_list

def extract_NN_connectivities(sdata = None, key_added = None):
    connectivities = {}
    for tn in sdata.tables.keys():
        nn_key_list = find_NN_keys(sdata = sdata, key_added = key_added, tn = tn)
        if type(nn_key_list) is type(None):
            continue
        for nk in nn_key_list:
            if "connectivities" in nk:
                connectivities[(tn, nk)] = sdata.tables[tn].obsp[nk]
    return connectivities

def extract_NN_distances(sdata = None, key_added = None, tn = None, nn_key_list = None):
    distances = None
    if nn_key_list is None:
        nn_key_list = find_NN_keys(sdata = sdata, key_added = key_added, tn = tn)

    if type(nn_key_list) is type(None):
        return distances
    nk_trim = nn_key_list.replace("_connectivities", "")
    distance_key = nk_trim + '_distances'
    distances = sdata.tables[tn].obsp[distance_key]
    return distances

def extract_NN_info(sdata = None, key_added = None, tn = None):
    nn_keys = find_NN_keys(sdata, key_added=key_added, tn = tn)
    nn_info = None
    for nk in nn_keys:
        if type(nk) is dict:
            nn_info = pd.Series(nk)
    return nn_info

def align_network_data(distances = None, weights = None):
    idx_dist_not_sparse = distances.nonzero()
    blank = [0 for i in range(len(idx_dist_not_sparse[0]))]
    df = pd.DataFrame({"distance":blank.copy(), "weight":blank.copy(), "from":blank.copy(), "to":blank.copy()})
    t0 = perf_counter()

    d_nz = distances[idx_dist_not_sparse]
    d_nz = np.array(d_nz).reshape(len(d_nz.T),)
    w_nz = weights[idx_dist_not_sparse]
    w_nz = np.array(w_nz).reshape(len(w_nz.T),)

    df.loc[:,"distance"] = pd.Series(d_nz)
    df.loc[:,"weight"] = pd.Series(w_nz)
    with warnings.catch_warnings():
        warnings.simplefilter(action='ignore', category=(DeprecationWarning, FutureWarning))
        df.loc[:,"from"] = pd.Series(idx_dist_not_sparse[0])
        df.loc[:,"to"] = pd.Series(idx_dist_not_sparse[1])
    
    df.loc[:,"from"] += 1
    df.loc[:,"to"] += 1

    t1 = perf_counter()
    print("Network extraction time:",t1-t0)
    return df

# Extract images
def extract_image(sdata = None):
    # Retrieve the list of images
    image_list = list(sdata.images.keys())

    # Extract image from SpatialData and convert it to numpy array
    extracted_images = []
    for image_key in image_list:
        image = sdata.images[image_key]
        image_array = np.transpose(image.compute().data, (1, 2, 0))  # Transpose to (y, x, c)
        extracted_images.append(image_array)
    return extracted_images

# Extract image names
def extract_image_names(sdata = None):
    image_names = list(sdata.images.keys())
    return image_names

# Extract points
def extract_points(sdata = None):
    points_dict = {}
    point_dict = sdata.points
    for ft, ddf in point_dict.items():  # Iterate over all feature types (e.g., "rna", "protein", etc.)
        # Convert Dask DataFrame to Pandas
        df = ddf.compute()  # `.compute()` converts Dask -> Pandas
        
        # Select relevant columns
        df = df[["feat_ID", "x", "y"]].copy()

        # Assign a unique integer index for each feature
        df["geom"] = df["feat_ID"].astype("category").cat.codes + 1
        points_dict[ft] = df

    return points_dict

# Extract polygons
def extract_polygons(sdata = None):
    polygons_dict = {}
    polygon_dict = sdata.shapes
    for su in polygon_dict.keys():
        rows = []
        for poly_id, geometry in zip(polygon_dict[su]["poly_ID"], polygon_dict[su]["geometry"]):
            if geometry.geom_type == "Polygon":
                coords = list(geometry.exterior.coords)
                part_id = 1
                for x, y in coords:
                    rows.append({"poly_ID": poly_id, "x": x, "y": y, "part": part_id})

            elif geometry.geom_type == "MultiPolygon":
                part_id = 1
                for poly in geometry.geoms:
                    coords = list(poly.exterior.coords)
                    for x, y in coords:
                        rows.append({"poly_ID": poly_id, "x": x, "y": y, "part": part_id})
                    part_id += 1
        df = pd.DataFrame(rows)

        df["geom"] = df["poly_ID"].astype("category").cat.codes + 1
        df["hole"] = 0

        polygons_dict[su] = df

    return polygons_dict

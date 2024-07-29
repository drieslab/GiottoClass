import warnings
import numpy as np
import pandas as pd
import scipy as sc
import matplotlib.pyplot as plt
from scipy.sparse import csr_matrix, csc_matrix
import dask.array as da
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
    expr = sdata.table.X.transpose().todense()
    expr_df = pd.DataFrame(expr, index=sdata.table.var['gene_ids'].index, columns=sdata.table.obs['array_row'].index)
    return expr_df

# Extract cell IDs
def extract_cell_IDs(sdata = None):
    cell_IDs = sdata.table.obs['array_row'].index.tolist()
    return cell_IDs

# Extract feature IDs
def extract_feat_IDs(sdata = None):
    feat_IDs = sdata.table.var['gene_ids'].index.tolist()
    return feat_IDs

# Metadata
def extract_cell_metadata(sdata = None):
    cell_metadata = sdata.table.obs.reset_index()
    cell_metadata = cell_metadata.rename(columns={"index": "cell_ID"})
    return cell_metadata

def extract_feat_metadata(sdata = None):
    feat_metadata = sdata.table.var.reset_index()
    feat_metadata = feat_metadata.rename(columns={"index": "feat_ID"})
    return feat_metadata

# Alternative expression data
def extract_layer_names(sdata = None):
    layer_names = None
    if len(sdata.table.layers) > 0:
        layer_names = [i for i in sdata.table.layers]
    return layer_names

def extract_layered_data(sdata = None, layer_name = None):
    layer_names = [i for i in sdata.table.layers]
    if layer_name not in layer_names:
        print(f"Invalid Key, {layer_name}, for sdata.table.layers")
        raise(KeyError)
    target_layer = sdata.table.layers[layer_name]
    if type(target_layer) == sc.sparse.csr_matrix:
        target_layer = target_layer.T
    elif type(target_layer) == sc.sparse.csr.csr_matrix:
        target_layer = target_layer.T
    else:
        target_layer = pd.DataFrame(target_layer)
    return target_layer

# Extract spatial information
def extract_spatial(sdata = None):
    spatial = sdata.table.obsm['spatial']
    spatial_df = pd.DataFrame(spatial)
    spatial_df.columns = ['X', 'Y']
    spatial_df['Y'] = spatial_df['Y'] * -1
    return spatial_df

def parse_obsm_for_spat_locs(sdata = None):
    cID = np.array(extract_cell_IDs(sdata))
    spat_locs = None
    spat_key = None
    
    try:
        spat_locs = sdata.table.obsm["spatial"]
    except (KeyError):
        spat_keys = [i for i in sdata.table.obsm if 'spatial' in i]
        if len(spat_keys) > 0:
            spat_key = spat_keys[0]
            spat_locs = sdata.table.obsm[spat_key]

    if spat_locs is None:
        err_mess = '''Spatial locations were not found. If spatial locations should have been found,
        please modify the anndata table within SpatialData object to include a keyword-value pair within the obsm slot,
        in which the keyword contains the phrase "spatial" and the value corresponds to the spatial locations.\n
        In the Giotto Object resulting from this conversion, dummy locations will be used.'''
        print(err_mess)
        spat_locs = None
        return spat_locs
    else:
        print("Spatial locations found.")
    
    cID = np.array(cID).reshape(len(cID),1)
    spat_locs = np.concatenate((spat_locs,cID), axis = 1)
    num_col = spat_locs.shape[1]
    
    colnames = ["sdimx","sdimy","sdimz","cell_ID"]
    conv = {"sdimx":float, "sdimy":float,"sdimz":float,"cell_ID":str}
    
    if num_col > 3:
        spat_locs = pd.DataFrame(spat_locs, columns = colnames)
    else:
        del colnames[2]
        del conv['sdimz']
        spat_locs = pd.DataFrame(spat_locs, columns = colnames)
    
    spat_locs = spat_locs.astype(conv)
    # Giotto y axis convention
    spat_locs["sdimy"] = -1 * spat_locs["sdimy"]
    return spat_locs

# Extract hires image
def extract_image(sdata = None):
    # Find SpatialData image name for hires image
    for key in sdata.images.keys():
        if "hires" in key:
            hires_image_name = key

    # Extract image from SpatialData and convert it to numpy array
    hires_image = sdata.images[hires_image_name]
    hires_image_array = np.transpose(hires_image.compute().data, (1, 2, 0))  # Transpose to (y, x, c)
    return hires_image_array

# Extract PCA
def extract_pca(sdata = None):
    o_keys = sdata.table.obsm_keys()
    v_keys = sdata.table.varm_keys()
    u_keys = None

    pca = dict()

    for ok in o_keys:
        if "X_pca" in ok:
            pca['pca'] = sdata.table.obsm[ok]
            u_keys = sdata.table.uns['pca'].keys()

    for vk in v_keys:
        if "PCs" in vk:
            pca['loadings'] = sdata.table.varm[vk]
        
    if type(u_keys) is not type(None):
        for uk in u_keys:
            if "variance" == uk:
                pca['eigenvalues'] = sdata.table.uns['pca'][uk]
        
    if(len(pca)) == 0:
        pca = None

    return pca

# Extract UMAP
def extract_umap(sdata = None):
    o_keys = sdata.table.obsm_keys()

    umap = None

    for ok in o_keys:
        if "X_umap" in ok:
            umap = sdata.table.obsm[ok]

    return umap

# Extract tSNE
def extract_tsne(sdata = None):
    o_keys = sdata.table.obsm_keys()

    tsne = None

    for ok in o_keys:
        if "X_tsne" in ok:
            tsne = sdata.table.obsm[ok]

    return tsne  

## NN Network

def find_NN_keys(sdata = None, key_added = None):
    nn_key_list = []
    
    if key_added is None:
        param_keys = list(sdata.table.uns.keys())
        for pk in param_keys:
            if "neighbors" in pk and "spatial" not in pk:
                try:
                    tmp_keys = sdata.table.uns[pk].keys()
                except KeyError:
                    tmp_keys = None
                    return None
                for i in tmp_keys:
                    nn_key_list.append(sdata.table.uns[pk].keys())
                break #Only returns connectivity and distance keys for one network
    elif ".txt" in key_added:
        line_keys = []
        with open(key_added) as f:
            for line in f.readlines():
                line = line.strip()
                line_keys.append(line)

        for key in line_keys:
            map_keys = sdata.table.uns[key].keys()
            for i in map_keys:
                nn_key_list.append(sdata.table.uns[key][i])
    elif key_added and key_added.casefold() != "spatial":
        map_keys = sdata.table.uns[key].keys()
        for i in map_keys:
            nn_key_list.append(sdata.table.uns[key][i])
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
    connectivities = None
    nn_key_list = find_NN_keys(sdata = sdata, key_added = key_added)

    if type(nn_key_list) is type(None):
        return connectivities
    
    for nk in nn_key_list:
        if "connectivities" in nk:
            connectivities = sdata.table.obsp[nk]

    return connectivities

def extract_NN_distances(sdata = None, key_added = None):
    distances = None
    nn_key_list = find_NN_keys(sdata = sdata, key_added = key_added)
    if type(nn_key_list) is type(None):
        return distances
    for nk in nn_key_list:
        if "distances" in nk:
            distances = sdata.table.obsp[nk]
    return distances

def extract_NN_info(sdata = None, key_added = None):
    nn_keys = find_NN_keys(sdata, key_added=key_added)
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
        # Ignoring the warning here because the desired behavior is maintained
        df.loc[:,"from"] = pd.Series(idx_dist_not_sparse[0])
        df.loc[:,"to"] = pd.Series(idx_dist_not_sparse[1])
    
    df.loc[:,"from"] += 1
    df.loc[:,"to"] += 1
    # for x, i in enumerate(zip(*dist_not_sparse)):
    #     to = i[-1]
    #     from_ = i[0]
    #     dist = distances[from_,to]
    #     weig = weights[from_,to]

    #     # Correct indexing convention for export to R
    #     from_ += 1
    #     to += 1

    #     df.iloc[x,:] = {"distance":dist, "weight":weig, "from":from_, "to":to}
    t1 = perf_counter()
    print("Network extraction time:",t1-t0)
    return df

def find_SN_keys(sdata = None, key_added = None):
    sn_key_list = []
    prefix = ""
    suffix = "neighbors"

    if key_added is None:
        map_key = prefix + suffix
        try:
            tmp_keys = sdata.table.uns[map_key].keys()
        except KeyError:
            tmp_keys = None
            return None
        
        for i in tmp_keys:
            #if type(adata.uns[pk][i]) == type(dict()): continue
            sn_key_list.append(sdata.table.uns[map_key][i])
    elif ".txt" in key_added:
        line_keys = []
        with open(key_added) as f:
            for line in f.readlines():
                line = line.strip()
                line_key_added = line + suffix
                line_keys.append(line_key_added)
        for key in line_keys:
            map_keys = sdata.table.uns[key].keys()
            for i in map_keys:
                sn_key_list.append(sdata.table.uns[key][i])

    elif key_added is not None:
        key_added = key_added + suffix
        map_keys = sdata.table.uns[key_added].keys()
        for i in map_keys:
            #if type(adata.uns[key_added][i]) == type(dict()): continue
            sn_key_list.append(sdata.table.uns[key_added][i])
        
    if len(sn_key_list) == 0:
        sn_key_list = None
    return sn_key_list

def extract_SN_connectivities(sdata = None, key_added = None):
    connectivities = None
    sn_key_list = find_SN_keys(sdata = sdata, key_added = key_added)

    if type(sn_key_list) is type(None):
        return connectivities
    
    for sk in sn_key_list:
        if "connectivities" in sk:
            connectivities = sdata.table.obsp[sk]
    
    return connectivities

def extract_SN_distances(sdata = None, key_added = None):    
    distances = None
    sn_key_list = find_SN_keys(sdata = sdata, key_added = key_added)

    if type(sn_key_list) is type(None):
        return distances
    
    for sk in sn_key_list:
        if "distances" in sk:
            distances = sdata.table.obsp[sk]
    
    return distances

def extract_SN_info(sdata = None, key_added = None):
    sn_keys = find_SN_keys(sdata, key_added=key_added)
    sn_info = None
    for sk in sn_keys:
        if type(sk) is dict:
            sn_info = pd.Series(sk)
    return sn_info


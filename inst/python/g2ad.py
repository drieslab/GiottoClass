"""
@author: matthew o'brien
"""

import anndata as ad
import pandas as pd
import numpy as np
import scipy
import os


def ad_guard(adata):
    if type(adata) is not type(ad.AnnData()): 
        print("Please provide a valid AnnData object.")
        raise(TypeError)

def ad_obj(x = None):
    '''
    Creates an AnnData object using expression matrix, x
    
    INPUT: x, an expression matrix in csr format
    
    OUTPUT: gad, a minimal AnnData object 
    '''
    if x is None: 
        print("Please provide an expression matrix.")
        assert(False)
    x = x.transpose()
    gad = ad.AnnData(x, dtype=x.dtype)
    return gad

def lay_inv(lay = None):
    try:
        print(type(lay))
    except (ValueError, NameError, SyntaxError):
        print(type(lay))
    finally:
        assert(False)

def set_adg_layer_data(adata = None, lay = None, lay_name = None):#, lay_class = None):
    '''
    Sets additional expression-based information within the
    layers slot of a given AnnData object
    
    INPUTS:
      adata, an AnnData object to which the data, x will be added in the layers slot
             MUST have expression data which lives in slot adata.X
      lay, a matrix with the same dimensions as adata.X
      lay_name, the name of the layer. By default, "'spatial_unit'_'feature_type'_'value'"
    
    OUTPUTS: adata, an AnnData object with additional data in the layers slot
    '''
    #if type(lay_class) is type(target_layer) == scipy.sparse.csr_matrix:
        #pass
    ad_guard(adata)
    x_size = np.shape(adata.X)
    
    if x_size == 0:
        print("Please first load in expression data, then try again")
        assert(False)
    lay = lay.transpose()
    lay_size = np.shape(lay)

    print("X ", x_size)
    print("lay ", lay_size)
    print(type(lay))
    if x_size != lay_size:
        print("The provided matrix must have the same dimensions as adata.X")
        print("Cannot add layer unless the dimensions agree exactly.")
        assert(False)
    adata.layers[lay_name] = lay
    return adata

def set_adg_spat_locs(adata = None, spat_locs = None):
    ad_guard(adata)
    spat_locs = pd.DataFrame(spat_locs, dtype=float)
    spat_locs = np.array(spat_locs, dtype=float)
    adata.obsm["spatial"] = spat_locs
    return adata

def set_adg_metadata(adata = None, cell_meta = None, feat_meta = None):
    ad_guard(adata)
    adata.obs_names = cell_meta["cell_ID"]
    cell_meta.set_index("cell_ID", inplace = True)
    
    #feat_meta = pd.DataFrame(feat_meta)#, dtype = object)
    adata.var_names = feat_meta["feat_ID"]
    feat_meta.set_index("feat_ID", inplace = True)

    for cm_col in cell_meta.columns:
        adata.obs[cm_col] = np.array(cell_meta[cm_col])
    for fm_col in feat_meta.columns:
        adata.var[fm_col] = np.array(feat_meta[fm_col])
    
    return adata

def set_adg_pca(adata = None, pca_coord = None, loadings = None, eigenv = None, feats_used = None, pca_name = None):
    ad_guard(adata)
    hvf = False
    pca_name_initial = pca_name
    if pca_coord is not None:
        adata.obsm['X_' + pca_name] = pca_coord
    if feats_used is not None:
        pca_name = "" if pca_name == "pca" else "_" + pca_name
        all_feats = adata.var_names
        all_false = [False for i in all_feats]
        highly_variable = pd.Series(all_false, index=all_feats)
        for x, i in enumerate(highly_variable.index):
            if i in feats_used:
                highly_variable.iloc[x] = True
        highly_variable.name = 'highly_variable'
        adata.var['highly_variable' + pca_name] = highly_variable
        hvf = True

    if loadings is not None and hvf:
        n_pc = loadings.shape[1]
        pc_placehold = np.zeros(shape = (adata.n_vars, n_pc))
        pc_placehold = pd.DataFrame(pc_placehold, index=adata.var_names, dtype=float)

        loadings.index = feats_used
        for row, i in enumerate(loadings.index):
            test_pc = loadings.iloc[row,:]
            pc_placehold.loc[i] = test_pc
        pc_placehold = pc_placehold.to_numpy(dtype=float)
        adata.varm["PCs" + pca_name] = pc_placehold

    elif loadings is not None:
        adata.varm["PCs" + pca_name] = loadings.to_numpy(dtype=float)
    if eigenv is not None:
        eigenv_shape = len(eigenv)
        eigenv = np.array(eigenv)
        adata.uns[pca_name_initial] = {}
        adata.uns[pca_name_initial] = {'variance':eigenv.reshape(eigenv_shape,)}

    return adata

def set_adg_umap(adata = None, umap_data = None, umap_name = None):
    ad_guard(adata)
    if umap_data is not None:
        adata.obsm['X_' + umap_name] = umap_data
    
    return adata

def set_adg_tsne(adata = None, tsne_data = None, tsne_name = None):
    ad_guard(adata)
    if tsne_data is not None:
        adata.obsm['X_' + tsne_name] = tsne_data
    
    return adata

def set_adg_nn(adata = None, df_NN = None, net_name = None, n_neighbors = None, dim_red_used = None):
    ad_guard(adata)
    dim12 = len(set(df_NN['from']))
    fill_arr = np.zeros((dim12,dim12))
    w_fill_df = pd.DataFrame(fill_arr, columns = adata.obs_names, index = adata.obs_names)
    d_fill_df = pd.DataFrame(fill_arr, columns = adata.obs_names, index = adata.obs_names)

    for i in df_NN.index:
        tar_row = df_NN.iloc[i,:]
        f_id = tar_row["from"]
        t_id = tar_row["to"]
        weight = tar_row["weight"]
        dist = tar_row["distance"]
        w_fill_df.loc[f_id, t_id] = weight
        d_fill_df.loc[f_id, t_id] = dist
    
    weights = scipy.sparse.csr_matrix(w_fill_df)
    distances = scipy.sparse.csr_matrix(d_fill_df)
    dim_red_used = net_name.split(".")[-1]
    
    if "kNN" in net_name:
        adata.obsp['connectivities'] = weights
        adata.obsp['distances'] = distances
        adata.uns['neighbors'] = {'connectivities_key': 'connectivities',
                                  'distances_key': 'distances',
                                  'params': {'n_neighbors': n_neighbors,
                                             'method': dim_red_used}
                                  }
    else:
        cname = net_name + "_connectivities"
        dname = net_name + "_distances"
        adata.obsp[cname] = weights
        adata.obsp[dname] = distances
        adata.uns[net_name] = {'connectivities_key': net_name + '_connectivities',
                               'distances_key': net_name + '_distances',
                               'params': {'n_neighbors': n_neighbors,
                                          'method': dim_red_used}
                               }
    
    return adata

def save_NN_keys(adata = None, network_name = None):
    if isinstance(network_name, str):
        network_name = [network_name]
    adata.uns['NN_keys'] = np.array(network_name, dtype = object)
    return adata

def set_adg_sn(adata = None, df_SN = None, net_name = None, n_neighbors = None, max_distance = None, dim_used = None):
    ad_guard(adata)
    cell_ids = adata.obs_names.to_list()
    cell_index_map = {cell: i for i, cell in enumerate(cell_ids)}

    row_idx, col_idx, weight_vals, dist_vals = [], [], [], []

    df_SN = df_SN.rename(columns={"from": "source", "to": "target"})
    for row in df_SN.itertuples(index=False):
        f_id = row.source
        t_id = row.target
        weight = row.weight
        dist = row.distance
        if f_id in cell_index_map and t_id in cell_index_map:
            row_idx.append(cell_index_map[f_id])
            col_idx.append(cell_index_map[t_id])
            weight_vals.append(weight)
            dist_vals.append(dist)

    dim = len(cell_ids)
    weights = scipy.sparse.csr_matrix((weight_vals, (row_idx, col_idx)), shape=(dim, dim))
    distances = scipy.sparse.csr_matrix((dist_vals, (row_idx, col_idx)), shape=(dim, dim))

    cname = net_name + "_connectivities"
    dname = net_name + "_distances"
    adata.obsp[cname] = weights
    adata.obsp[dname] = distances
    adata.uns[net_name + "_neighbors"] = {
        'connectivities_key': cname,
        'distances_key': dname,
        'params': {
            'n_neighbors': n_neighbors,
            'dimensions_used': dim_used,
            'max_distance': max_distance
        }
    }    
    return adata

def save_SN_keys(adata = None, network_name = None):
    if isinstance(network_name, str):
        network_name = [network_name]
    adata.uns['SN_keys'] = np.array(network_name, dtype = object)
    return adata

def save_SE_keys(adata = None, enrichment_name = None):
    if isinstance(enrichment_name, str):
        enrichment_name = [enrichment_name]
    adata.uns['SE_keys'] = np.array(enrichment_name, dtype = object)

    return adata

def set_adg_spat_enrich(adata = None, enrichment = None, name = None):
    adata.uns[name] = enrichment
    return adata

def write_ad_h5ad(adata = None, save_directory = None, spat_unit = None, feat_type = None):
    ad_guard(adata)
    if os.path.exists(save_directory):
        save_path = f"{save_directory}{spat_unit}_{feat_type}_converted_gobject.h5ad"
        adata.write_h5ad(save_path)
    if os.path.exists(save_path):
        return save_path
    else:
        return None

# DEVELOPER FUNCTIONS - NO NEED TO ADD TO GLOBALS
def type_test(obj = None):
    return type(obj)

def test_ret_hvf(adata):
    return adata.var["highly_variable"]

def test_ret_pcs(adata):
    return adata.varm["PCs"]

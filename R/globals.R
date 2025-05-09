globalVariables(
    names = c(
        ":=", ".N", ".SD", ".", "cast",
        "python_leiden", "python_louvain", "python_spatial_genes",
        "Spatial_DE_AEH", "Spatial_DE", "silhouette_rank",
        "python_scrublet", "python_create_mesmer_app",
        "python_segment_image",
        # anndata interop
        "ad_guard", "ad_obj",
        "lay_inv", "set_adg_layer_data", "set_adg_spat_locs",
        "set_adg_metadata", "set_adg_pca", "set_adg_umap",
        "set_adg_tsne", "write_ad_h5ad", "read_anndata_from_path",
        "extract_expression", "extract_cell_IDs", "extract_feat_IDs",
        "extract_pca", "extract_umap", "extract_tsne",
        "parse_obsm_for_spat_locs", "extract_cell_metadata",
        "extract_feat_metadata", "extract_layer_names",
        "extract_layered_data", "set_adg_nn", "find_NN_keys",
        "extract_NN_connectivities", "extract_NN_distances",
        "extract_NN_info", "align_network_data", "extract_SN_connectivities",
        "extract_SN_distances", "set_adg_sn", "create_AnnData",
        # tifffile
        "py_tif_convert",
        # spatialdata interop
        "createSpatialData", "read_spatialdata_from_path", "extract_spatial",
        "extract_image", "extract_spat_enrich", "save_NN_keys", "save_SN_keys",
        "set_adg_spat_enrich", "save_SE_keys", "extract_polygons",
        "extract_spat_enrich", "extract_points"
    )
)

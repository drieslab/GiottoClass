import tifffile as tf
import numpy as np
import os

try:
    import imagecodecs  
except Exception:
    imagecodecs = None
    
def py_tif_convert(input_file, output_file, page=None,
                   compression='deflate', bigtiff='auto',
                   tile=None, dtype=None):
    """
    Convert one page from a specialized TIFF (OME-TIFF, qptiff, etc.)
    into a simple baseline TIFF (terra-friendly).
    - If page is None, use page 0 (robust for multi-file pyramids).
    - Pass a *file path* for output_file (not a directory).
    """
    if os.path.isdir(output_file):
        raise IsADirectoryError(
            f"output_file must be a file path, not a directory: {output_file}"
        )
        
    if imagecodecs is None:
        print("Warning: 'imagecodecs' not available. JPEG2000/advanced codecs may fail to decode.")


    with tf.TiffFile(input_file) as tif:
        # robust default: page 0 avoids series/levels restrictions
        p = 0 if page is None else int(page)
        if p < 0 or p >= len(tif.pages):
            raise IndexError(f"page {p} out of range 0..{len(tif.pages)-1}")
        arr = tif.pages[p].asarray()

    if dtype is not None:
        arr = arr.astype(np.dtype(dtype), copy=False)

    tf.imwrite(
        output_file,
        arr,
        compression=None if (compression in (None, 'none')) else compression,
        bigtiff=bigtiff,
        tile=tuple(tile) if tile is not None else None,
        metadata=None 
    )

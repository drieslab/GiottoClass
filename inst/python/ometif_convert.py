import tifffile
import os

def ometif_2_tif(input_file, output_file, overwrite=False):
    if overwrite and os.path.exists(output_file):
        os.remove(output_file)
    
    with tifffile.TiffFile(input_file) as ome_tif:
        ome_page = ome_tif.pages[0]
        image_data = ome_page.asarray()

        # Create the output directory if it doesn't exist
        os.makedirs(os.path.dirname(output_file), exist_ok=True)

        # Write the image data to the output file
        with tifffile.TiffWriter(output_file, bigtiff=True) as tif_writer:
            tif_writer.write(image_data, contiguous=False)

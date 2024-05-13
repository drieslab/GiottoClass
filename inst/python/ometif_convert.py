import tifffile
import os

def ometif_2_tif(input_file, output_file, page=None):
    
    with tifffile.TiffFile(input_file) as ome_tif:

        # Write the image data to the output file
        with tifffile.TiffWriter(output_file, bigtiff=True) as tif_writer:
            if 0 <= page < len(ome_tif.pages):
                ome_page = ome_tif.pages[page]
                image_data = ome_page.asarray()
                tif_writer.write(image_data, contiguous=False)
            else:
                print(f"Warning: Page index {page} is out of range.")

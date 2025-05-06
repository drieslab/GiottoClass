import tifffile
import os

def py_tif_convert(input_file, output_file, page=None):
    
    with tifffile.TiffFile(input_file) as input_tif:

        # Write the image data to the output file
        with tifffile.TiffWriter(output_file, bigtiff=True) as tif_writer:
            if 0 <= page < len(input_tif.pages):
                ome_page = input_tif.pages[page]
                image_data = ome_page.asarray()
                tif_writer.write(image_data, contiguous=False)
            else:
                print(f"Warning: Page index {page} is out of range.")

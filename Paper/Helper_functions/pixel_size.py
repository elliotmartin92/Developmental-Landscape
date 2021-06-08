from aicsimageio import AICSImage

def image_pixelsize(file_path):
  """gets pixel size from czi"""
  img = AICSImage(file_path)
  return img.get_physical_pixel_size()[1]

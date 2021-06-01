library(grImport)
library(ggplot2)
library(cowplot)
library(extrafont)
Sys.setenv(R_GSCMD = normalizePath("C:/Program Files/gs/gs9.54.0/bin/gswin64c.exe"))

eps_as_gg = function(path) {
  temp_path = file.path(tempdir(), basename(path))
  PostScriptTrace(path, charpath = FALSE, outfilename = temp_path)
  my_shape = readPicture(temp_path)
  file.remove(temp_path)
  image_gg = grImport::pictureGrob(my_shape)
  return(image_gg)
}
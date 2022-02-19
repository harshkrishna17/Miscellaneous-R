library(magick)

add_image_centre <- function(plot_path, image_path) {
  
# Read in plot
fig <- image_read(plot_path)
fig <- image_resize(fig, "1000x1000")
  
# Read in image
img <- image_read(image_path)
img <- image_scale(img, "62x85")
  
# Overlay
image_composite(fig, img, offset = "+471+425")

}

imagepl <- add_image_centre(plot_path = "pizza_plot.png", image_path = "tammypic.png")
imagepl

image_write(imagepl, "addimage.png")

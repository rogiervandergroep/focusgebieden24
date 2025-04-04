install.packages("magick")  # Install if not already installed
library(magick)
library(pdftools)

# Convert EPS to PNG
img <- image_read("basis_variant_3_alt_roodwit_cmyk.eps")


image_write(img, path = "icon.png", format = "png")


library(ragg)

agg_png("icon.png", width = 800, height = 800, units = "px")
grid::grid.raster(readLines("basis_variant_3_alt_roodwit_cmyk.eps"))
dev.off()


# Figure 1: Forest cover in NRW. Combine the study area figure with the summary statistics table.

## importing files
library(terra)
library(magick)
library(ggplot2)
library(gridExtra)
library(grid)


# NRW map

NRW <- image_read("C:/NRW_figures/NRW figures/Data_figures/figure1_NRW.png")

# tabella con dati che voglio inserire

table_NRW <- data.frame(
  Col1 = c("Valore1", "Valore2"),
  Col2 = c(10, 20)
)

# Crea il plot della tabella
table_NRW_plot <- tableGrob(table_NRW)


# Combina immagine e tabella
grid.arrange(
  rasterGrob(as.raster(NRW)),
  table_NRW_plot,
  ncol = 2
)

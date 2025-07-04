# cerco di riprodurre i 3 grafici di prioritizr combinati con una palette colorblind friendly e con una legenda migliore

# scenario 1 


## create data for plot
library(ggplot2)
library(raster)
library(terra)
library(tidyverse)


#importo i dati che mi servono

existing_spa <-  rast("C:/NRW_figures/NRW figures/Input_Data_figures/Forest_strictly_protected_25832_revised.tif")
  
s1 <- rast("C:/NRW_figures/NRW figures/Outputs_figures/s1_wild.tif")

s3 <- rast("C:/NRW_figures/NRW figures/Outputs_figures/s3.tif")

s5 <- rast("C:/NRW_figures/NRW figures/Outputs_figures/s5.tif")

tfc_costs <- rast("C:/NRW_figures/NRW figures/Input_Data_figures/total_forest_cover_25832.tif")

tfc_const_costs <- (tfc_costs*0) + 1


# scenario 1

d1 <-
  sum(c(existing_spa, s1), na.rm = TRUE) %>%
  mask(tfc_const_costs) %>%
  as.data.frame(xy = TRUE) %>%
  setNames(c("x", "y", "value")) %>%
  mutate(
    label = case_when(
      value == 0 ~ "not selected",
      value == 1 ~ "priority area",
      value == 2 ~ "existing SPA"
    )
  ) %>%
  mutate(
    label = factor(
      label,
      levels = c( "not selected", "existing SPA", "priority area")
    )
  )

## create plot

# plot alternativo senza legenda e colorblind friendly
p1 <-
  ggplot() +
  geom_tile(
    mapping = aes(x = x, y = y, fill = label),
    data = d1,
    height = terra::yres(existing_spa),
    width = terra::xres(existing_spa)
  ) +
  coord_fixed() +
  scale_fill_manual(
    name = "Status",
    values = c(
      "not selected" = "#d9d9d9",
      "priority area" = "#009E73",  # verde-teal
      "existing SPA" = "#000075"    # blu
    )
  ) +
  theme(
    axis.ticks = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    axis.ticks.length = ggplot2::unit(0, "null"),
    panel.border = ggplot2::element_rect(color = "black", fill = NA),
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid = ggplot2::element_blank(),
    legend.position = "none",
    legend.text = ggplot2::element_text(size = 7),
    legend.box.background = ggplot2::element_rect(fill = "white", color = "black"),
    plot.margin = ggplot2::margin(0, 0, 0, 0, "null"),
    strip.background = ggplot2::element_rect(color = "black", fill = "black"),
    strip.text = ggplot2::element_text(color = "white"),
    plot.title = ggplot2::element_text(
      family = "Calibri",   # Font Calibri
      size = 20,            # Dimensione 20
      hjust = 0.5           # Centra il titolo
    )
  ) +
  ggtitle("Public Commitment (1)")



# save plot
#ggsave(p1a, filename = "scenario5.png", height = 4.3, width = 4.5)

### scenario 5

d5 <-
  sum(c(existing_spa, s5), na.rm = TRUE) %>%
  mask(tfc_const_costs) %>%
  as.data.frame(xy = TRUE) %>%
  setNames(c("x", "y", "value")) %>%
  mutate(
    label = case_when(
      value == 0 ~ "not selected",
      value == 1 ~ "priority area",
      value == 2 ~ "existing SPA"
    )
  ) %>%
  mutate(
    label = factor(
      label,
      levels = c( "not selected", "existing SPA", "priority area")
    )
  )

## create plot
p5 <-
  ggplot() +
  geom_tile(
    mapping = aes(x = x, y = y, fill = label),
    data = d5,
    height = terra::yres(existing_spa),
    width = terra::xres(existing_spa)
  ) +
  coord_fixed() +
  scale_fill_manual(
    name = "Status",
    values = c(
      "not selected" = "#d9d9d9",
      "priority area" = "#009E73",  # verde-teal
      "existing SPA" = "#000075"    # blu
    )
  ) +
  theme(
    axis.ticks = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    axis.ticks.length = ggplot2::unit(0, "null"),
    panel.border = ggplot2::element_rect(color = "black", fill = NA),
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid = ggplot2::element_blank(),
    legend.position = "none",  # legenda rimossa
    legend.text = ggplot2::element_text(size = 7),
    legend.box.background = ggplot2::element_rect(fill = "white", color = "black"),
    plot.margin = ggplot2::margin(0, 0, 0, 0, "null"),
    strip.background = ggplot2::element_rect(color = "black", fill = "black"),
    strip.text = ggplot2::element_text(color = "white"),
    plot.title = ggplot2::element_text(
      family = "Calibri",   # Font Calibri
      size = 20,            # Dimensione 20
      hjust = 0.5           # Centra il titolo
    )
  ) +
  ggtitle("Public Commitment For Conservation (5)")
  

### scenario 3

d3 <-
  sum(c(existing_spa, s3), na.rm = TRUE) %>%
  mask(tfc_const_costs) %>%
  as.data.frame(xy = TRUE) %>%
  setNames(c("x", "y", "value")) %>%
  mutate(
    label = case_when(
      value == 0 ~ "not selected",
      value == 1 ~ "priority area",
      value == 2 ~ "existing SPA"
    )
  ) %>%
  mutate(
    label = factor(
      label,
      levels = c( "not selected", "existing SPA", "priority area")
    )
  )

## create plot
p3 <-
  ggplot() +
  geom_tile(
    mapping = aes(x = x, y = y, fill = label),
    data = d3,
    height = terra::yres(existing_spa),
    width = terra::xres(existing_spa)
  ) +
  coord_fixed() +
  scale_fill_manual(
    name = "Status",
    values = c(
      "not selected" = "#d9d9d9",
      "priority area" = "#009E73",  # verde-teal
      "existing SPA" = "#000075"    # blu
    )
  ) +
  theme(
    axis.ticks = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    axis.ticks.length = ggplot2::unit(0, "null"),
    panel.border = ggplot2::element_rect(color = "black", fill = NA),
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid = ggplot2::element_blank(),
    legend.position = "none",  # legenda rimossa
    legend.text = ggplot2::element_text(size = 7),
    legend.box.background = ggplot2::element_rect(fill = "white", color = "black"),
    plot.margin = ggplot2::margin(0, 0, 0, 0, "null"),
    strip.background = ggplot2::element_rect(color = "black", fill = "black"),
    strip.text = ggplot2::element_text(color = "white"),
    plot.title = ggplot2::element_text(
      family = "Calibri",
      size = 20,
      hjust = 0.5
    )
  ) +
  ggtitle("Private Commitment (3)")


# creo plot per legenda
library(patchwork)
library(showtext)

#verifico di avere calibri
font_add(family = "Calibri", regular = "Calibri.ttf") # Specifica il percorso se necessario
showtext_auto()



legend_plot <- ggplot() +
  geom_tile(
    mapping = aes(x = x, y = y, fill = label),
    data = d1,  # o qualsiasi dataset che contiene tutte le label
    height = terra::yres(existing_spa),
    width = terra::xres(existing_spa)
  ) +
  scale_fill_manual(
    name = "Status",
    values = c(
      "not selected" = "#d9d9d9",
      "priority area" = "#009E73",
      "existing SPA" = "#000075"
    )
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 20),    # aumenta dimensione testo legenda
    legend.title = element_text(size = 22),   # aumenta dimensione titolo legenda
    legend.key.size = unit(2, "cm")           # opzionale: aumenta la dimensione dei simboli
  )


#estraggo solo la legenda
#library(cowplot)
legend <- cowplot::get_legend(legend_plot)

# Combina i plot senza legenda e la legenda a destra
final_plot <- (p1 + p3 + p5) + patchwork::plot_layout(ncol = 3, guides = "collect") & theme(legend.position = "right")

# Visualizza il risultato
print(final_plot)




### plot together

library(gridExtra)

joint_plot_0407 <- grid.arrange(p1, p3, p5, ncol = 3)

# save plot
ggsave(joint_plot_0407, filename = "joint_plot_0407.png", height = 5.2, width = 15, dpi=1000)

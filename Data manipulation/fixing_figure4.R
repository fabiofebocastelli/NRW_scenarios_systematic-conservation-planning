# cerco di riprodurre i 3 grafici di prioritizr combinati con una palette colorblind friendly e con una legenda migliore

# cerco di riprodurre i 3 grafici di prioritizr combinati con una palette colorblind friendly e con una legenda migliore


## create data for plot
library(ggplot2)
library(raster)
library(terra)
library(tidyverse)
library(cowplot)
library(patchwork)
library(showtext)
library(gridExtra)


#importo i dati che mi servono

existing_spa <-  rast("C:/NRW_figures/NRW figures/Input_Data_figures/Forest_strictly_protected_25832_revised.tif")
# plot_exspa <- plot(existing_spa)

s1 <- rast("C:/NRW_figures/NRW figures/Outputs_figures/s1_wild.tif")

s3 <- rast("C:/NRW_figures/NRW figures/Outputs_figures/s3.tif")

s5 <- rast("C:/NRW_figures/NRW figures/Outputs_figures/s5.tif")

tfc_costs <- rast("C:/NRW_figures/NRW figures/Input_Data_figures/total_forest_cover_25832.tif")

tfc_const_costs <- (tfc_costs*0) + 1


# SCENARIO 1
r_stack1 <- c(existing_spa, s1)
names(r_stack1) <- c("existing_spa", "priority_area")
d1 <- as.data.frame(r_stack1, xy = TRUE, na.rm = FALSE) %>%
  mutate(
    label = case_when(
      is.na(existing_spa) & is.na(priority_area) ~ NA_character_,
      existing_spa == 1 ~ "existing SPA",
      priority_area == 1 ~ "priority area",
      TRUE ~ "not selected"
    ),
    label = factor(label, levels = c("not selected", "priority area", "existing SPA"))
  )

# SCENARIO 5
r_stack5 <- c(existing_spa, s5)
names(r_stack5) <- c("existing_spa", "priority_area")
d5 <- as.data.frame(r_stack5, xy = TRUE, na.rm = FALSE) %>%
  mutate(
    label = case_when(
      is.na(existing_spa) & is.na(priority_area) ~ NA_character_,
      existing_spa == 1 ~ "existing SPA",
      priority_area == 1 ~ "priority area",
      TRUE ~ "not selected"
    ),
    label = factor(label, levels = c("not selected", "priority area", "existing SPA"))
  )

# SCENARIO 3
r_stack3 <- c(existing_spa, s3)
names(r_stack3) <- c("existing_spa", "priority_area")
d3 <- as.data.frame(r_stack3, xy = TRUE, na.rm = FALSE) %>%
  mutate(
    label = case_when(
      is.na(existing_spa) & is.na(priority_area) ~ NA_character_,
      existing_spa == 1 ~ "existing SPA",
      priority_area == 1 ~ "priority area",
      TRUE ~ "not selected"
    ),
    label = factor(label, levels = c("not selected", "priority area", "existing SPA"))
  )

# DEFINISCI UN TEMA BASE ASSOLUTAMENTE IDENTICO per tutti i plot
theme_map <- theme(
  panel.background = element_rect(fill = "white", colour = NA),
  plot.background = element_rect(fill = "white", colour = NA),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  axis.line = element_blank(),
  axis.ticks.length = unit(0, "null"),
  panel.border = element_rect(color = "black", fill = NA),
  panel.grid = element_blank(),
  legend.position = "right",
  legend.text = element_text(size = 18),
  legend.title = element_text(size = 20),
  legend.key.size = unit(1.2, "cm"),
  plot.margin = margin(0, 0, 0, 0, "cm"),
  strip.background = element_rect(color = "black", fill = "black"),
  strip.text = element_text(color = "white"),
  plot.title = element_text(family = "Calibri", size = 18, hjust = 0.5, margin = margin(b = 10))
)

# CREAZIONE DEI TRE PLOT SENZA legenda
p1 <- ggplot(d1, aes(x = x, y = y, fill = label)) +
  geom_tile(height = terra::yres(existing_spa), width = terra::xres(existing_spa)) +
  coord_fixed() +
  scale_fill_manual(
    name = "Status",
    values = c(
      "not selected" = "#d9d9d9",
      "priority area" = "#009E73",
      "existing SPA" = "#000075"
    ),
    na.value = NA,
    na.translate = FALSE
  ) +
  ggtitle("Public Commitment Scenario (1)") +
  theme_map +
  theme(legend.position = "none")  # togli la legenda

p3 <- ggplot(d3, aes(x = x, y = y, fill = label)) +
  geom_tile(height = terra::yres(existing_spa), width = terra::xres(existing_spa)) +
  coord_fixed() +
  scale_fill_manual(
    name = "Status",
    values = c(
      "not selected" = "#d9d9d9",
      "priority area" = "#009E73",
      "existing SPA" = "#000075"
    ),
    na.value = NA,
    na.translate = FALSE
  ) +
  ggtitle("Private Commitment Scenario (3)") +
  theme_map +
  theme(legend.position = "none")

p5 <- ggplot(d5, aes(x = x, y = y, fill = label)) +
  geom_tile(height = terra::yres(existing_spa), width = terra::xres(existing_spa)) +
  coord_fixed() +
  scale_fill_manual(
    name = "Status",
    values = c(
      "not selected" = "#d9d9d9",
      "priority area" = "#009E73",
      "existing SPA" = "#000075"
    ),
    na.value = NA,
    na.translate = FALSE
  ) +
  ggtitle("Public Commitment/nFor Conservation Scenario (5)") +
  theme_map +
  theme(legend.position = "none")

# CREA UN PLOT SOLO LEGENDA
p_legend <- ggplot(d1, aes(x = x, y = y, fill = label)) +
  geom_tile() +
  scale_fill_manual(
    name = "Status",
    values = c(
      "not selected" = "#d9d9d9",
      "priority area" = "#009E73",
      "existing SPA" = "#000075"
    ),
    na.value = NA,
    na.translate = FALSE
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.key.size = unit(1.2, "cm")
  )

legend <- cowplot::get_legend(p_legend)

# COMBINA I PLOT CON PATCHWORK (allineamento perfetto)
final_plot <- (p1 + p3 + p5 + plot_layout(ncol = 3, guides = "collect")) & theme_map

final_plot

# save plot
ggsave(final_plot, filename = "C:/NRW_figures/NRW figures/Outputs_figures/fig4.png", height = 5.2, width = 15, dpi=1000)



#### code for zoomming
# senza successo, mi sposto in inkscape



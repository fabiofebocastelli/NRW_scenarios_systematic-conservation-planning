### analysis of the results ###


#######################################################################################################################

# check the aggregation values to assess the spatial allocations in the scenarios

# Aggregation index (Aggregation metric)

landscapemetrics::lsm_l_ai(s1)

landscapemetrics::lsm_l_ai(s3)

landscapemetrics::lsm_l_ai(s1_wild)

#clumpiness
library(landscapemetrics)

lsm_c_clumpy(s1_wild)
lsm_c_clumpy(s1)
lsm_c_clumpy(s3)



#####################################################################################################################

# evaluating feature representation/eval_target_coverage_summary

# scenario 1a
stats_1a <- print(eval_target_coverage_summary(p1, s1), n=30)


# scenario 1b
stats_1b <- print(eval_target_coverage_summary(p1_wild, s1_wild), n=30)

# scenario 3
stats_3 <- print(eval_target_coverage_summary(p3, s3), n=30)


#exporting tables
write.table(stats_1a, file = "C:/Users/Fabio Castelli/OneDrive - Alma Mater Studiorum Università di Bologna/Desktop/NRW_thesis_figures_tables/targets_summary_1a.csv")
 
write.table(stats_1b, file = "C:/Users/Fabio Castelli/OneDrive - Alma Mater Studiorum Università di Bologna/Desktop/NRW_thesis_figures_tables/targets_summary_1b_revised.csv")
 
write.table(stats_3, file = "C:/Users/Fabio Castelli/OneDrive - Alma Mater Studiorum Università di Bologna/Desktop/NRW_thesis_figures_tables/targets_summary_3.csv")

#########################################################################################

# nice graphs

# scenario 1a 

# make a nice plot
## create data for plot
library(ggplot2)

d1a <-
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
p1a <-
  ggplot() +
  geom_tile(
    mapping = aes(x = x, y = y, fill = label),
    data = d1a,
    height = terra::yres(existing_spa),
    width = terra::xres(existing_spa)
  ) +
  coord_fixed() +
  scale_fill_manual(
    name = "Status",
    values = c(
      "not selected" = "#d9d9d9",
      "priority area" = "#4DAF4A",
      "existing SPA" = "#FF7F00"
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
    legend.position = c(0.99, 0.01),
    legend.justification = c(1, 0),
    legend.text = ggplot2::element_text(size = 7),
    legend.box.background =
      ggplot2::element_rect(fill = "white", color = "black"),
    plot.margin =  ggplot2::margin(0, 0, 0, 0, "null"),
    strip.background = ggplot2::element_rect(color = "black", fill = "black"),
    strip.text = ggplot2::element_text(color = "white")
  ) +
   ggtitle("Public Commitment for Conservation (5)") +
   theme(plot.title=element_text(hjust=0.5))


# save plot
#ggsave(p1a, filename = "scenario5.png", height = 4.3, width = 4.5)

### scenario 1b

d1b <-
  sum(c(existing_spa, s1_wild), na.rm = TRUE) %>%
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
p1b <-
  ggplot() +
  geom_tile(
    mapping = aes(x = x, y = y, fill = label),
    data = d1b,
    height = terra::yres(existing_spa),
    width = terra::xres(existing_spa)
  ) +
  coord_fixed() +
  scale_fill_manual(
    name = "Status",
    values = c(
      "not selected" = "#d9d9d9",
      "priority area" = "#4DAF4A",
      "existing SPA" = "#FF7F00"
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
    legend.position = c(0.99, 0.01),
    legend.justification = c(1, 0),
    legend.text = ggplot2::element_text(size = 7),
    legend.box.background =
      ggplot2::element_rect(fill = "white", color = "black"),
    plot.margin =  ggplot2::margin(0, 0, 0, 0, "null"),
    strip.background = ggplot2::element_rect(color = "black", fill = "black"),
    strip.text = ggplot2::element_text(color = "white")
  ) +
  ggtitle("Public Commitment (1)") +
  theme(plot.title=element_text(hjust=0.5))

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
      "priority area" = "#4DAF4A",
      "existing SPA" = "#FF7F00"
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
    legend.position = c(0.99, 0.01),
    legend.justification = c(1, 0),
    legend.text = ggplot2::element_text(size = 7),
    legend.box.background =
      ggplot2::element_rect(fill = "white", color = "black"),
    plot.margin =  ggplot2::margin(0, 0, 0, 0, "null"),
    strip.background = ggplot2::element_rect(color = "black", fill = "black"),
    strip.text = ggplot2::element_text(color = "white")
  ) +
  ggtitle("Private Commitment (3)") +
  theme(plot.title=element_text(hjust=0.5))


### plot together

library(gridExtra)

joint_plot <- grid.arrange(p1b, p3, p1a, ncol = 3)

# save plot
ggsave(joint_plot, filename = "joint_plot.png", height = 5.2, width = 15, dpi=1000)




#### mean patch size #################################################
library(terra)
library(landscapemetrics)

s1 <- rast("C:/NRW_figures/NRW figures/Outputs_figures/s1_wild.tif")

s3 <- rast("C:/NRW_figures/NRW figures/Outputs_figures/s3.tif")

s5 <- rast("C:/NRW_figures/NRW figures/Outputs_figures/s5.tif")


## scenario 1
#check values
unique(values(s1))

# Calcola per tutte le classi
res1 <- lsm_c_area_mn(s1)

# Mostra solo la mean patch area per classe 1
mparea_class1 <- res1[res1$class == 1, ]
print(mparea_class1)

## scenario 3
#check values
unique(values(s3))

# Calcola per tutte le classi
res3 <- lsm_c_area_mn(s3)

# Mostra solo la mean patch area per classe 1
mparea_class3 <- res3[res3$class == 1, ]
print(mparea_class3)

## scenario 5
#check values
unique(values(s5))

# Calcola per tutte le classi
res5 <- lsm_c_area_mn(s5)

# Mostra solo la mean patch area per classe 1
mparea_class5 <- res5[res5$class == 1, ]
print(mparea_class5)

  

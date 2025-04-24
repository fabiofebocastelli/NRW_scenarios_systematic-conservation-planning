# Scenario 1

library(prioritizr)
library(sf)
library(terra)
library(vegan)
library(cluster)
library(raster)
library(gurobi)
library(dplyr)


# load planning unit data
tfc_costs <- rast("D:/EFI_Data/NRW_Data/u2018_clc2018_v2020_20u1_geoPackage/total_forest_cover_25832.tif")

# creating a new raster with constant costs
tfc_const_costs <- (tfc_costs*0) + 1

# loading conservation features
existing_spa <- rast("D:/EFI_Data/NRW_Data/Forest strictly protected/Forest_strictly_protected_25832_revised.tif")
N2000 <- rast("D:/EFI_Data/NRW_Data/Occurrence of FFH habitat types in North Rhine-Westphalia/Habitat_directive_FFH_25832.tif")
fht <- rast("D:/EFI_Data/NRW_Data/Habitat_types_AnnexI/Dataset_from_Lanuv/forest_habitat_types_reclas_25832.tif")
pwa_3000 <- rast("D:/EFI_Data/NRW_Data/Potential wilderness areas/PWA_3000_NRW_25832.tif")
state_f <- rast("D:/EFI_Data/NRW_Data/Public forest/State_forest_25832.tif")
# update values in tfc feature
tfc_feature <- rast("D:/EFI_Data/NRW_Data/u2018_clc2018_v2020_20u1_geoPackage/total_forest_cover_25832.tif")

# load in PWA vector data
pwa_vector <-
  read_sf("D:/EFI_Data/NRW_Data/Potential wilderness areas/PWA_1000-3000_NRW_25832.shp") %>%
  sf::st_geometry() %>%
  c(
    read_sf("D:/EFI_Data/NRW_Data/Potential wilderness areas/PWA_100_NRW_25832.shp") %>%
      sf::st_geometry()
  ) %>%
  {tibble::tibble(id = seq_along(.), geom = .)} %>%
  sf::st_as_sf() %>%
  st_transform(st_crs(tfc_costs)) %>%
  mutate(area = sf::st_area(.), value = 1) %>%
  arrange(desc(area)) %>%
  filter(
    lengths(
      st_intersects(
        .,
        st_set_crs(st_as_sfc(st_bbox(ext(tfc_costs))), st_crs(tfc_costs))
      )
    ) > 0
  )


# loading the high vitality decreased layer
vit_dec <- rast("D:/EFI_Data/NRW_Data/Vitality Decrease/vitality_highly_decreased_25832.tif")

# setting value 0.25 for all the cells
reclass_matrix <- matrix(c(3, 0.25), ncol = 2, byrow = TRUE)
vit_dec <-  classify(vit_dec, reclass_matrix)

tfc_feature <- terra::mask(
  tfc_feature,
  mask = (tfc_feature > 0.5) & (vit_dec == 0.25),
  maskvalues = 1,
  updatevalue = 0.25
)
names(tfc_feature) <- "tfc_feature"

# create a binary stack for fht raster
bstacked_fht <- binary_stack(fht)


# set names to keep track of all the different fht
names(bstacked_fht) <- paste0("class_", seq_len(nlyr(bstacked_fht)))

# remove layers with only zeros
idx <- which(global(bstacked_fht, "max", na.rm = TRUE)[[1]] > 0.5)
bstacked_fht <- bstacked_fht[[idx]]

# creating the conservation features object
cons_feat_wild <- c(
  bstacked_fht,
  existing_spa,
  pwa_3000,
  state_f,
  N2000,
  tfc_feature
)

# create a layer to indicate which places are not state forests
not_state_f <- terra::mask(
  tfc_const_costs - subst(state_f, NA, 0),
  tfc_const_costs
)

# calculating the area of existing SPAs so we can include it in the linear constraints
freq(existing_spa) # 29606


# Add targets

# no conservaiton targets, I want to focus on wilderness areas
wild_targets <- c(
  rep(0, nlyr(bstacked_fht)), ## >= =% coverage of each forest type
  0,                                     ## >= 0% coverage of existing_spa
  0,                                     ## >= 0% coverage of pwa_3000
  0,                                     ## >= 0% coverage of state_f
  0,                                    ## >= 0% coverage of N2000
  0                                    ## tfc_feature, should it be 1, or 0 ?
)


# create problem to evaluate solution
p1_wild <-
  problem(tfc_const_costs, cons_feat_wild) %>%
  add_min_set_objective() %>%
  add_relative_targets(wild_targets) %>%
  add_locked_out_constraints(not_state_f)

# generate solution
## assign ranks to each PWA
pwa_vector$rank <- rev(seq_len(nrow(pwa_vector)))
## create raster with ranks for each planning unit
ranks_raster <- rasterize(
  x = pwa_vector,
  y = tfc_const_costs,
  field = "rank",
  fun = "max",
  touches = TRUE,
  background = 0
)
## update ranks with existing SPAs
ranks_raster <- mask(
  ranks_raster,
  existing_spa,
  maskvalue = 1,
  updatevalue = nrow(pwa_vector) + 1
)
## mask ranks by planning unit layer
ranks_raster <- mask(ranks_raster, tfc_const_costs)
## mask ranks by not state forest
ranks_raster <- mask(
  ranks_raster,
  not_state_f,
  maskvalue = 1,
  updatevalue = NA
)
## find grid cells indices needed to meet area threshold
idx_data <-
  ranks_raster %>%
  as.data.frame(cells = TRUE, na.rm = TRUE) %>%
  setNames(c("idx", "rank")) %>%
  arrange(desc(rank)) %>%
  slice_head(n = 90092)
## create raster with solution
s1_wild <- tfc_const_costs * 0
s1_wild[idx_data$idx] <- 1

# check that solution has the right number of planning units selected
terra::global(s1_wild, "sum", na.rm = T)

# plot solution
plot(s1_wild)

# other plot
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
      levels = c("not selected", "existing SPA", "priority area")
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
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks.length = unit(0, "null"),
    panel.border = element_rect(color = "black", fill = NA),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.position = "inside", # Modifica principale qui
    legend.position.inside = c(0.99, 0.01), # Nuovo parametro
    legend.justification = c(1, 0),
    legend.text = element_text(size = 7),
    legend.box.background = element_rect(fill = "white", color = "black"),
    plot.margin = margin(0, 0, 0, 0, "null"),
    strip.background = element_rect(color = "black", fill = "black"),
    strip.text = element_text(color = "white")
  ) +
  ggtitle("Public Commitment (1)") +
  theme(plot.title = element_text(hjust = 0.5))



# evaluating the solution

# calculate statistic
# cost summary
eval_cost_summary(p1_wild, s1_wild)

# Feature representation summary
print(eval_feature_representation_summary(p1_wild, s1_wild), n=30)

# Target coverage summary
# calculate statistics
print(eval_target_coverage_summary(p1_wild, s1_wild), n=30)

# Scenario 5
library(prioritizr)
library(sf)
library(terra)
library(raster)
library(gurobi)
library(dplyr)
library(ggplot2)
library(gridExtra)

# load planning unit data
tfc_costs <- rast("D:/EFI_Data/NRW_Data/u2018_clc2018_v2020_20u1_geoPackage/total_forest_cover_25832.tif")

# creating a new raster with constant costs
tfc_const_costs <- (tfc_costs * 0) + 1

# loading conservation features
existing_spa <- rast("D:/EFI_Data/NRW_Data/Forest strictly protected/Forest_strictly_protected_25832_revised.tif")
N2000 <- rast("D:/EFI_Data/NRW_Data/Occurrence of FFH habitat types in North Rhine-Westphalia/Habitat_directive_FFH_25832.tif")
fht <- rast("D:/EFI_Data/NRW_Data/Habitat_types_AnnexI/Dataset_from_Lanuv/forest_habitat_types_reclas_25832.tif")
state_f <- rast("D:/EFI_Data/NRW_Data/Public forest/State_forest_new_25832.tif")
# no pwa in this scenario

# loading the high vitality decreased layer
vit_dec <- rast("D:/EFI_Data/NRW_Data/Vitality Decrease/vitality_highly_decreased_25832.tif")

# setting value 0.25 for all the cells
reclass_matrix <- matrix(c(3, 0.25), ncol = 2, byrow = TRUE)
vit_dec <-  classify(vit_dec, reclass_matrix)

# update values in tfc feature
tfc_feature <- rast("D:/EFI_Data/NRW_Data/u2018_clc2018_v2020_20u1_geoPackage/total_forest_cover_25832.tif")
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

# I want to prioritize the cells corresponding to ecologically valuable forests
# --> cells with not highly damaged forest should have higher values in order
# to be selected in the cheapest solution when a fixed budget has been set.

# I need to change bstacked_fht layers values in this way: WHEN 'NULL' THEN
# 'NULL'; WHEN '0' THEN '0'; WHEN '1'THEN '1' IF not overlap with vit_dec
# or '0.25' IF overlap with vit_dec
# or in other words, replace the values only when bstacked_fht[[i]] > 0.5 and
# vit_dec > 0.20

modified_bstacked_fht <-
  terra::rast(
    lapply(as.list(bstacked_fht),
           function(x) {
             terra::mask(
               x,
               mask = (x > 0.5) & (vit_dec == 0.25),
               maskvalues = 1,
               updatevalue = 0.25
             )
           }
    )
  )

# maybe doing the same for N2000?

# creating the conservation features object
cons_feat_1 <- c(
  modified_bstacked_fht,
  existing_spa,
  state_f,
  N2000,
  tfc_feature
)

# create a layer to indicate which places are not state forests
not_state_f <- terra::mask(
  tfc_const_costs - subst(state_f, NA, 0),
  tfc_const_costs
)

# metto NA al posto di 0 per avere solo valori 1 dove non state forest
not_state_f_new <- ifel(not_state_f == 1, 1, NA)


# create targets
## setting different relative targets
targets <- c(
  rep(0.3, nlyr(modified_bstacked_fht)), ## >= 30% coverage of each forest type
  0,                                     ## >= 0% coverage of existing_spa
  0,                                     ## >= 0% coverage of state_f
  0.3,                                   ## >= 30% coverage of N2000,
  1                                      ## >= 100%
)

# if I lock in existing spa but i lock out not state forest, then I need to meet a number or cells that are equal to 90092 - the number of cell of SPA not state forest (17810) = 72282

# create problem
## no boundary/connectivity penalties here
p5 <- problem(tfc_const_costs, cons_feat_1) %>%
  add_min_shortfall_objective(budget = 72282) %>%
  add_relative_targets(targets) %>%
  add_locked_in_constraints(existing_spa) %>%
  add_locked_out_constraints(not_state_f_new)  %>%
  add_linear_constraints(
    data = tfc_const_costs,
    sense = "=",
    threshold = 72282
  ) %>%
  add_gurobi_solver(gap = 0)

# solving with Gurobi
s5 <- solve(p5)



# calcola i valori unici delle celle di s1
unique_values <- unique(values(s5))
print(unique_values)
# quante celle con valore 1 in s1
num_cells_selected <- sum(values(s5) == 1, na.rm = TRUE)
print(num_cells_selected)


# make a nice plot
## create data for plot
# Convert rasters to data frames with coordinates
existing_spa_df <- existing_spa %>%
  mask(tfc_const_costs) %>%
  as.data.frame(xy = TRUE) %>%
  setNames(c("x", "y", "existing_spa"))

s5_df <- s5 %>%
  mask(tfc_const_costs) %>%
  as.data.frame(xy = TRUE) %>%
  setNames(c("x", "y", "s5"))

# Merge the data frames
d <- merge(existing_spa_df, s5_df, by = c("x", "y"), all = TRUE) %>%
  mutate(
    # Replace NA with 0 for easier logic
    existing_spa = ifelse(is.na(existing_spa), 0, existing_spa),
    s5 = ifelse(is.na(s5), 0, s5),
    # Create labels based on the priority logic
    label = case_when(
      existing_spa == 1 ~ "existing SPA",  # existing SPA has priority
      s5 == 1 ~ "priority area",           # only s5 selected (not existing SPA)
      TRUE ~ "not selected"                # neither selected
    )
  ) %>%
  mutate(
    label = factor(label, levels = c("not selected", "priority area", "existing SPA"))
  )

## create plot
p <-
  ggplot() +
  geom_tile(
    mapping = aes(x = x, y = y, fill = label),
    data = d,
    height = terra::yres(existing_spa),
    width = terra::xres(existing_spa)
  ) +
  coord_fixed() +
  scale_fill_manual(
    name = "Status",
    values = c(
      "not selected" = "#d9d9d9",
      "priority area" = "#1f78b4",
      "existing SPA" = "#b2df8a"
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
  )

p

# alternative plot

df_existing_spa <- as.data.frame(existing_spa, xy = TRUE, na.rm = FALSE)
df_s5 <- as.data.frame(s5, xy = TRUE, na.rm = FALSE)

# Ora puoi procedere senza usare values(), i dati sono nelle terze colonne
d1 <- df_existing_spa %>%
  dplyr::mutate(
    s5 = df_s5[[3]],  # colonna valori di s5
    label = dplyr::case_when(
      Forest_strictly_protected_25832_revised == 1 ~ "existing SPA",
      s5 == 1 & Forest_strictly_protected_25832_revised != 1 ~ "priority area",
      TRUE ~ "not selected"
    )
  ) %>%
  dplyr::mutate(
    label = factor(label, levels = c("not selected", "priority area", "existing SPA"))
  )



# plot
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
      "priority area" = "#1f78b4",
      "existing SPA" = "#b2df8a"
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
  )

p1



# save plot
ggsave(p, filename = "scenario1a.png", height = 4.3, width = 4.5)

# calculate statistics
## cost summary
eval_cost_summary(p5, s5)

## Feature representation summary
eval_feature_representation_summary(p5, s5)

## evaluate improvements
existing_spa_solution <- mask(
  subst(existing_spa, NA, 0),
  tfc_const_costs
)

x1 <- eval_feature_representation_summary(p1, s1)
x2 <- eval_feature_representation_summary(
  p1,
  existing_spa_solution
)

x1$relative_improvement <-
  x1$relative_held - x2$relative_held

x1$percent_improvement <- x1$relative_improvement * 100


# Target coverage summary
# calculate statistics
eval_target_coverage_summary(p5, s5)



# debugging

library(landscapemetrics)
#check values
unique(values(s5))

# Calcola per tutte le classi
res5 <- lsm_c_area_mn(s5)

# Mostra solo la mean patch area per classe 1
mparea_class5 <- res5[res5$class == 1, ]
print(mparea_class5) # mps  62.1

# aggregation index
landscapemetrics::lsm_l_ai(s5) # 84.9




# clumpiness index
lsm_c_clumpy(s5) # 0.831

# representativeness

print(eval_target_coverage_summary(p5, s5), n=30)



## unisci la soluzione s5 a spa_notstate forest e poi calcola i valori dei risultati 














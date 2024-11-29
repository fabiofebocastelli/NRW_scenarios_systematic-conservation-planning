# Scenario 1a
library(prioritizr)
library(sf)
library(terra)
library(raster)
library(gurobi)
library(dplyr)
library(ggplot2)
library(gridExtra)

# load planning unit data
tfc_costs <- rast("input-data/total_forest_cover_25832.tif")

# creating a new raster with constant costs
tfc_const_costs <- (tfc_costs * 0) + 1

# loading conservation features
existing_spa <- rast("input-data/Forest_strictly_protected_25832_revised.tif")
N2000 <- rast("input-data/Habitat_directive_FFH_25832.tif")
fht <- rast("input-data/forest_habitat_types_reclas_25832.tif")
state_f <- rast("input-data/State_forest_25832.tif")
# no pwa in this scenario

# loading the high vitality decreased layer
vit_dec <- rast("input-data/vitality_highly_decreased_25832.tif")

# setting value 0.25 for all the cells
reclass_matrix <- matrix(c(3, 0.25), ncol = 2, byrow = TRUE)
vit_dec <-  classify(vit_dec, reclass_matrix)

# update values in tfc feature
tfc_feature <- rast("input-data/total_forest_cover_25832.tif")
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

# create targets
## setting different relative targets
targets <- c(
  rep(0.3, nlyr(modified_bstacked_fht)), ## >= 30% coverage of each forest type
  0,                                     ## >= 0% coverage of existing_spa
  0,                                     ## >= 0% coverage of state_f
  0.3,                                   ## >= 30% coverage of N2000,
  1                                      ## >= 100%
)

# create problem
## no boundary/connectivity penalties here
p1 <- problem(tfc_const_costs, cons_feat_1) %>%
  add_min_shortfall_objective(budget = 90092) %>%
  add_relative_targets(targets) %>%
  add_locked_in_constraints(existing_spa) %>%
  add_locked_out_constraints(not_state_f)  %>%
  ## try including this if the solution doesn't have the right amount
  ## of selected planning units
  # add_linear_constraints(
  #   data = tfc_const_costs,
  #   sense = "=",
  #   threshold = 90092
  # ) %>%
  add_gurobi_solver(gap = 0)

# solving with Gurobi
s1 <- solve(p1)

# make a nice plot
## create data for plot
d <-
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


# save plot
ggsave(p, filename = "scenario1a.png", height = 4.3, width = 4.5)

# calculate statistics
## cost summary
eval_cost_summary(p1, s1)

## Feature representation summary
eval_feature_representation_summary(p1, s1)

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
eval_target_coverage_summary(p1, s1)

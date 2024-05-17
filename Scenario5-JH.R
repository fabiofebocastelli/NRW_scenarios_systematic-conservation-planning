# Scenario 1a
library(prioritizr)
library(sf)
library(terra)
library(raster)
library(gurobi)

# load planning unit data
tfc_costs <- rast("input-data/total_forest_cover_25832.tif")
tfc_costs

# creating a new raster with constant costs
tfc_const_costs <- (tfc_costs * 0) + 1
tfc_const_costs

# loading conservation features
existing_spa <- rast("input-data/Forest_strictly_protected_25832_revised.tif")
N2000 <- rast("input-data/Habitat_directive_FFH_25832.tif")
fht <- rast("input-data/forest_habitat_types_reclas_25832.tif")
pwa <- rast("input-data/PWA_3000_NRW_25832.tif")
state_f <- rast("input-data/State_forest_25832.tif")

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

# creating the conservation features object
cons_feat_1 <- c(
  modified_bstacked_fht,
  existing_spa,
  pwa,
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
  0,                                     ## >= 0% coverage of pwa
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
  add_gurobi_solver(gap = 0)

# solving with Gurobi
s1 <- solve(p1)

# plot solution
plot(s1)

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

# load packages
library(prioritizr)
library(sf)
library(terra)
library(vegan)
library(cluster)
library(raster)
library(gurobi)

# import data
## planning unit data
tfc_costs <- rast("input-data/total_forest_cover_25832.tif")

## feature data
existing_spa <- rast("input-data/Forest_strictly_protected_25832.tif")
N2000 <- rast("input-data/Habitat_directive_FFH_25832.tif")
fht <- rast("input-data/forest_habitat_types_reclas_25832.tif")
pwa <- rast("input-data/PWA_3000_NRW_25832.tif")
state_f <- rast("input-data/State_forest_25832.tif")
vit_dec <- rast("input-data/vitality_highly_decreased_25832.tif")

# prepare data
## create a new raster with constant costs
tfc_const_costs <- (tfc_costs * 0) + 1

## convert values in vit_dec from 3 to 0.25 for all the cells.
reclass_matrix <- matrix(c(3, 0.25), ncol = 2, byrow = TRUE)
vit_dec <- classify(vit_dec, reclass_matrix)

## create a binary stack for fht raster
bstacked_fht <- binary_stack(fht)

## set names to keep track of all the different fht
names(bstacked_fht) <- paste0("class_", seq_len(nlyr(bstacked_fht)))

## remove layers with only zeros
bstacked_fht <- bstacked_fht[[
  which(global(bstacked_fht, "max", na.rm = TRUE)[[1]] > 0.5)
]]

## update values in bstacked_fht data, so that any cells with a value of
## 1 in a  bstacked_fht[[i]] and a value of 0.25 in vit_dec are assigned
## a value of 0.25. All other cell values should remain the same
modified_bstacked_fht <- terra::rast(lapply(as.list(bstacked_fht), function(x) {
  terra::mask(
    x,
    mask = (x > 0.5) & (vit_dec == 0.25),
    maskvalues = 1,
    updatevalue = 0.25
  )
}))

## identify which planning units are covered by state forests
is_state_forest <- state_f
is_state_forest[is.na(is_state_forest)] <- 0
is_state_forest <- mask(is_state_forest, mask = tfc_const_costs)

## identify which planning units are not state forests
not_state_forest <- 1 - is_state_forest

## compile data for all features
cons_feat_1 <- c(modified_bstacked_fht, existing_spa, pwa, state_f, N2000)

# main processing
## set relative targets for prioritization,
## based on the
feature_targets <- c(
  rep(0.3, nlyr(modified_bstacked_fht)), ## >= 30% coverage of each forest type
  0,                                     ## >= 0% coverage of existing_spa
  0,                                     ## >= 0% coverage of pwa
  0,                                     ## >= 0% coverage of state_f
  0.3                                    ## >= 30% coverage of N2000
)

stop('hree')

## create problem
p1 <-
  problem(tfc_costs, cons_feat_1) %>%
  add_min_shortfall_objective(budget = 52351) %>%
  add_relative_targets(feature_targets) %>%
  add_locked_in_constraints(pwa) %>%
  add_locked_out_constraints(existing_spa) %>%
  add_locked_out_constraints(not_state_forest)  %>%
  add_gurobi_solver(gap = 0)

## solve problem
s1 <- solve(p1)

## visualize solution
plot(s1)

# calculate statistics
## cost summary
eval_cost_summary(p1, s1)

## ceature representation summary
eval_feature_representation_summary(p1, s1)

## target coverage summary
eval_target_coverage_summary(p1, s1)

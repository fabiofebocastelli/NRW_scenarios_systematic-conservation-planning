# to different area targets based on the ownership type:
# eligible forest: 39254
# state forest: 21662

library(prioritizr)
library(sf)
library(terra)
library(vegan)
library(cluster)
library(raster)
library(gurobi)
library(slam)

# load planning unit data
tfc_costs <- rast("input-data/total_forest_cover_25832.tif")
tfc_costs

# creating a new raster with constant costs
tfc_const_costs <- (tfc_costs*0) + 1
tfc_const_costs

# loading conservation features
existing_spa <- rast("input-data/Forest_strictly_protected_25832.tif")
N2000 <- rast("input-data/Habitat_directive_FFH_25832.tif")
fht <- rast("input-data/forest_habitat_types_reclas_25832.tif")
state_f <- rast("input-data/State_forest_25832.tif")

# create a binary stack for fht raster
bstacked_fht <- binary_stack(fht)

# set names to keep track of all the different fht
names(bstacked_fht) <- paste0("class_", seq_len(nlyr(bstacked_fht)))

# remove layers with only zeros
bstacked_fht <- bstacked_fht[[which(global(bstacked_fht, "max", na.rm = TRUE)[[1]] > 0.5)]]

# I want to prioritize the cells corresponding to highly damaged forest --> cells with highly damaged forest should have higher values because
# we expect private forest owners to prioritize those stands when they want to set aside 5% of their holdings to get the incentives

# loading the high vitality decreased layer
vit_dec <- rast("input-data/vitality_highly_decreased_25832.tif")

# setting value 1 for all the cells
reclass_matrix_vt_3 <- matrix(c(0.25, 1), ncol = 2, byrow = TRUE)
vit_dec <-  classify(vit_dec, reclass_matrix_vt_3)

# creating the conservation feature object
cons_feat_3 <- c(bstacked_fht, N2000, existing_spa, state_f)

#targets3
targets3 <- c(
  rep(0.3, nlyr(bstacked_fht)), ## >= 30% coverage of each forest type
  0.3,                                   ## >= 30% coverage of N2000
  0,                                     ##  >= 0% coverage of existing_spa
  0                                     ## >= 0% coverage of state_f
)

## calculate boundary matrix
bd <- rescale_matrix(boundary_matrix(tfc_const_costs))

# setting the problem
## create a baseline problem
## this should contain all the targtes and locked in and locked out stuff
p3_baseline <-
  problem(tfc_const_costs, cons_feat_3) %>%
  add_min_shortfall_objective(budget = 90092) %>%
  add_relative_targets(targets3) %>%
  add_linear_constraints(
    threshold = 21662,
    sense = "<=",
    data = state_f
  ) %>%
  add_locked_in_constraints(existing_spa) %>%
  add_gurobi_solver(gap = 0)

## solve baseline problem
s3_baseline <- solve(p3_baseline, force = TRUE)

## calculate performance
t3_baseline <- eval_target_coverage_summary(p3_baseline, s3_baseline)
t3_total_cost <- terra::global(
  tfc_const_costs * s3_baseline, "sum", na.rm = TRUE
)[[1]]

## create a version of the problem that minimizes boundary length
## but this has zero cost, positive boundary penalties, and we set
## the targets based on the relative held by the baseline solution.
## so in other words, we want to do just as well as the first solution
## except, we want to try and minimize boundary length as much as possible
p3_real <-
  ## zero costs so we only care about boundary penalties
  problem(tfc_const_costs * 0, cons_feat_3) %>%
  ## boundary penalties so we spatially cluster solution
  add_boundary_penalties(penalty = 1, data =bd) %>%
  ## min set solution with targets set as relative held by baseline solution
  ## meaning that the solution has to do as well at representing the features
  ## as the baseline solution
  add_min_set_objective() %>%
  add_relative_targets(t3_baseline$relative_held) %>%
  ## add constraint to ensure that total cost is the same in baseline run
  add_linear_constraints(
    threshold = t3_total_cost,
    sense = "<=",
    data = tfc_const_costs
  ) %>%
  ## add in other constraints from baseline
  add_linear_constraints(
    threshold = 21662,
    sense = "<=",
    data = state_f
  ) %>%
  add_locked_in_constraints(existing_spa) %>%
  add_gurobi_solver(gap = 0.15)

## generate prioritization
s3_real <- solve(p3_real, force = TRUE)


### ALTERNATIVE APPROACH FOR SPATIAL CLUSTERING
# p3_real <-
#   problem(tfc_const_costs, cons_feat_3) %>%
#   add_min_shortfall_objective(budget = 90092) %>%
#   add_relative_targets(targets3) %>%
#   add_linear_constraints(
#     threshold = 21662,
#     sense = "<=",
#     data = state_f
#   ) %>%
#   add_locked_in_constraints(existing_spa) %>%
#   add_neighbor_constraints(k = 3) %>%
#   add_gurobi_solver(gap = 0)

# evaluating the solution
# calculate statistic
# cost summary
eval_cost_summary(p3_baseline, s3_real)

# Feature representation summary
print(eval_feature_representation_summary(p3_baseline, s3_real), n=30)

# Target coverage summary
# calculate statistics
print(eval_target_coverage_summary(p3_baseline, s3_real), n=30)

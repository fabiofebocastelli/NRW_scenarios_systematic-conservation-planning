# to different area targets based on the ownership type:
# eligible forest: 39254
# state forest: 21662

## note that that you need the developmental version of prioritizr:
## remotes::install_github("prioritizr/prioritizr")
library(prioritizr)
library(sf)
library(terra)
library(vegan)
library(cluster)
library(raster)
library(gurobi)
library(Matrix)


# load planning unit data
tfc_costs <- rast("D:/EFI_Data/NRW_Data/u2018_clc2018_v2020_20u1_geoPackage/total_forest_cover_25832.tif")

# creating a new raster with constant costs
tfc_const_costs <- (tfc_costs*0) + 1

# loading conservation features
existing_spa <- rast("D:/EFI_Data/NRW_Data/Forest strictly protected/Forest_strictly_protected_25832_revised.tif")
N2000 <- rast("D:/EFI_Data/NRW_Data/Occurrence of FFH habitat types in North Rhine-Westphalia/Habitat_directive_FFH_25832.tif")
fht <- rast("D:/EFI_Data/NRW_Data/Habitat_types_AnnexI/Dataset_from_Lanuv/forest_habitat_types_reclas_25832.tif")
state_f <- rast("D:/EFI_Data/NRW_Data/Public forest/State_forest_25832.tif")
eligible_f <- rast("D:/EFI_Data/NRW_Data/Eligible Forest + federal forest/Eligible_forest-Federal_forest_25832.tif")

# create a binary stack for fht raster
bstacked_fht <- binary_stack(fht)

# set names to keep track of all the different fht
names(bstacked_fht) <- paste0("class_", seq_len(nlyr(bstacked_fht)))

# remove layers with only zeros
bstacked_fht <-
  bstacked_fht[[which(global(bstacked_fht, "max", na.rm = TRUE)[[1]] > 0.5)]]

# I want to prioritize the cells corresponding to highly damaged forest -->
# cells with highly damaged forest should have higher values because
# we expect private forest owners to prioritize those stands when they want to
# set aside 5% of their holdings to get the incentives

# loading the high vitality decreased layer
vit_dec <- rast("D:/EFI_Data/NRW_Data/Vitality Decrease/vitality_highly_decreased_25832.tif")

# setting value 1 for all the cells
vit_dec <- (vit_dec * 0) + 1

# I need to change eligible_f layer values in this way:
# when overlap with vit_dec-> value 1, when it doesn't overlap-> 0.25

# first, update eligible_f values as default 0.25 setting value 0.25 for all
# the cells
reclass_matrix_ef_3 <- matrix(c(1, 0.25), ncol = 2, byrow = TRUE)
eligible_f <-  classify(eligible_f, reclass_matrix_ef_3)

# then, trying to change the values according to vit_dec this way:
modified_eligible_f <-terra::mask(
  eligible_f,
  mask = (eligible_f > 0.2) & (vit_dec == 1),
  maskvalues = 1, updatevalue = 1
)

# create  tfc feature to specify a value of each and every forest
tfc_feature <- rast("D:/EFI_Data/NRW_Data/u2018_clc2018_v2020_20u1_geoPackage/total_forest_cover_25832.tif")
tfc_feature <- terra::mask(
  tfc_feature * 0.25,
  mask = (tfc_feature > 0.2) & (vit_dec > 0.5),
  maskvalues = 1,
  updatevalue = 1
)
names(tfc_feature) <- "tfc_feature"

# creating the conservation feature object
cons_feat_3 <- c(
  bstacked_fht,
  N2000,
  existing_spa,
  state_f,
  modified_eligible_f,
  tfc_feature
)

#targets3
targets3 <- c(
  rep(0.3, nlyr(bstacked_fht)), ## >= 30% coverage of each forest type
  0.3,                                   ## >= 30% coverage of N2000
  0,                                     ##  >= 0% coverage of existing_spa
  0,                                     ## >= 0% coverage of state_f
  0,                                     ## eligible_f
  1                                      ## tfc_feature
)

# prepare constraint data
const_existing_spa <- mask(
  state_f,
  existing_spa,
  maskvalue = 0, updatevalue = 0
)
const_modified_eligible_f <- mask(
  modified_eligible_f,
  existing_spa,
  maskvalue = 0, updatevalue = 0
)

# setting the problem
## generate initial problem to minimize costs as much as possible
p3_init <-
  problem(tfc_const_costs, cons_feat_3) %>%
  add_min_shortfall_objective(budget = 90092) %>%
  add_relative_targets(targets3) %>%
  add_linear_constraints(
    threshold = 21662,
    sense = "<=",
    data = const_existing_spa
  ) %>%
  add_linear_constraints(
    threshold = 39254,
    sense = "<=",
    data = const_modified_eligible_f
  ) %>%
  add_locked_in_constraints(existing_spa) %>%
  add_gurobi_solver(gap = 0)

## solve problem
s3_init <- solve(p3_init, force = TRUE)

### generate subsequent problem to minimize spatial fragmentation as much
### as possible whilst ensuring that the feature represent is the same as the
### that in the initial solution
p3 <-
  p3_init %>%
  add_neighbor_penalties(1) %>%
  add_gurobi_solver(
    gap = 0.05, start = s3_init,
    control = list(
      Method = 2, Heuristics = 0.001, Cuts = 0,
      AggFill = 0, PreDual = 1, PrePasses = 2
    )
  )

### modify problem so that it does what we want
### compile problems so we can modify
o3_init <- compile(p3_init)
o3 <- compile(p3)
### set objective to focus only on boundary penalties
o3$set_obj(o3$obj() - c(o3_init$obj(), rep(0, o3$ncol() - o3_init$ncol())))
### set linear constraint to ensure feature representation is at least
### as good as in the initial solution
o3$append_linear_constraints(
  rhs = attr(s3_init, "objective")[[1]],
  sense = "<=",
  A = drop0(
    sparseMatrix(
      i = rep(1, o3$ncol()),
      j = seq_len(o3$ncol()),
      x = c(o3_init$obj(), rep(0, o3$ncol() - o3_init$ncol()))
    )
  ),
  row_ids = "min_shortfall"
)
### solve problem
s3_raw <- p3$portfolio$run(o3, p3$solver)
### convert solution to raster format
s3 <- tfc_const_costs * 0
s3[p3$planning_unit_indices()] <-
  s3_raw[[1]]$x[seq_len(p3$number_of_planning_units())]
s3[is.na(tfc_const_costs)] <- NA_real_


# Imposta il percorso completo del file .tif
path <- "C:/NRW_figures/NRW figures/Outputs_figures/s3.tif"

writeRaster(s3, path, overwrite=TRUE)



# evaluating the solution
# calculate statistic
# cost summary
eval_cost_summary(p3, s3)

# Feature representation summary
print(eval_feature_representation_summary(p3, s3), n=30)

# Target coverage summary
# calculate statistics
print(eval_target_coverage_summary(p3, s3), n=30)
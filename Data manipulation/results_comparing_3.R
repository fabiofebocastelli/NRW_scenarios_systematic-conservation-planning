# Scenario 3 #######################################################################################################################


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
bstacked_fht <- bstacked_fht[[which(global(bstacked_fht, "max", na.rm = TRUE)[[1]] > 0.5)]]

# I want to prioritize the cells corresponding to highly damaged forest --> cells with highly damaged forest should have higher values because
# we expect private forest owners to prioritize those stands when they want to set aside 5% of their holdings to get the incentives

# loading the high vitality decreased layer
vit_dec <- rast("D:/EFI_Data/NRW_Data/Vitality Decrease/vitality_highly_decreased_25832.tif")

# setting value 1 for all the cells
vit_dec <- (vit_dec * 0) + 1

# I need to change eligible_f layer values in this way: when overlap with vit_dec-> value 1, when it doesn't overlap-> 0.25

# first, update eligible_f values as default 0.25 setting value 0.25 for all the cells
reclass_matrix_ef_3 <- matrix(c(1, 0.25), ncol = 2, byrow = TRUE)
eligible_f <-  classify(eligible_f, reclass_matrix_ef_3)

# then, trying to change the values according to vit_dec this way:
modified_eligible_f <-terra::mask(
  eligible_f,
  mask = (eligible_f > 0.2) & (vit_dec == 1),
  maskvalues = 1, updatevalue = 1
)

# create tfc feature to specify a value of each and every forest
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
p3 <-
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

s3 <- solve(p3, force = TRUE)

plot(s3)

# Salvataggio del raster
terra::writeRaster(
  s3,
  filename = "C:/NRW_figures/NRW figures/Outputs_figures/s3.tif",  
  filetype = "GTiff",
  overwrite = TRUE,
  datatype = "INT1U"  # Tipo dati ottimale per valori 0/1
)

# evaluating the solution

# calculate statistic
# cost summary
eval_cost_summary(p3, s3)

# Feature representation summary
print(eval_feature_representation_summary(p3, s3), n=30)

# Target coverage summary
# calculate statistics
print(eval_target_coverage_summary(p3, s3), n=30)

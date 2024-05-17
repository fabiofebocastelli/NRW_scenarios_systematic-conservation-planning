# Scenario 1b

library(prioritizr)
library(sf)
library(terra)
library(vegan)
library(cluster)
library(raster)
library(gurobi)
library(dplyr)


# load planning unit data
tfc_costs <- rast("C:/Users/Fabio Castelli/OneDrive - Alma Mater Studiorum Università di Bologna/Desktop/NRW_Data/u2018_clc2018_v2020_20u1_geoPackage/total_forest_cover_25832.tif")

# creating a new raster with constant costs
tfc_const_costs <- (tfc_costs*0) + 1

# loading conservation features
existing_spa <- rast("C:/Users/Fabio Castelli/OneDrive - Alma Mater Studiorum Università di Bologna/Desktop/NRW_Data/Forest strictly protected/Forest_strictly_protected_25832_revised.tif")
N2000 <- rast("C:/Users/Fabio Castelli/OneDrive - Alma Mater Studiorum Università di Bologna/Desktop/NRW_Data/Occurrence of FFH habitat types in North Rhine-Westphalia/Habitat_directive_FFH_25832.tif")
fht <- rast("C:/Users/Fabio Castelli/OneDrive - Alma Mater Studiorum Università di Bologna/Desktop/NRW_Data/Habitat_types_AnnexI/Dataset_from_Lanuv/forest_habitat_types_reclas_25832.tif") 
pwa_3000 <- rast("C:/Users/Fabio Castelli/OneDrive - Alma Mater Studiorum Università di Bologna/Desktop/NRW_Data/Potential wilderness areas/PWA_3000_NRW_25832.tif")
state_f <- rast("C:/Users/Fabio Castelli/OneDrive - Alma Mater Studiorum Università di Bologna/Desktop/NRW_Data/Public forest/State_forest_25832.tif")
# update values in tfc feature
tfc_feature <- rast("C:/Users/Fabio Castelli/OneDrive - Alma Mater Studiorum Università di Bologna/Desktop/NRW_Data/u2018_clc2018_v2020_20u1_geoPackage/total_forest_cover_25832.tif")

# load in PWA vector data
pwa_vector <-
  read_sf("C:/Users/Fabio Castelli/OneDrive - Alma Mater Studiorum Università di Bologna/Desktop/NRW_Data/Potential wilderness areas/PWA_1000-3000_NRW_25832.shp") %>%
  sf::st_geometry() %>%
  c(
    read_sf("C:/Users/Fabio Castelli/OneDrive - Alma Mater Studiorum Università di Bologna/Desktop/NRW_Data/Potential wilderness areas/PWA_100_NRW_25832.shp") %>%
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
vit_dec <- rast("C:/Users/Fabio Castelli/OneDrive - Alma Mater Studiorum Università di Bologna/Desktop/NRW_Data/Vitality Decrease/vitality_highly_decreased_25832.tif")

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
  add_locked_out_constraints(not_state_f)%>%
  add_locked_out_constraints(existing_spa)


# generate solution
## initialize with empty solution raster polygons
s1_wild <- tfc_const_costs * 0
for (i in seq_len(nrow(pwa_vector))) {
  # add progress message
  message("starting iteration", i)
  # attempt to rasterize next PWA
  curr_pwa <- rasterize(
    x = vect(pwa_vector[i, ]),
    y = tfc_const_costs,
    field = "value",
    touches = TRUE,
    background = 0
  )
  # try adding current PWA to solution
  curr_sol <- s1_wild + curr_pwa
  # ensure not state forest is locked out
  curr_sol <- mask(curr_sol, not_state_f, maskvalue = 1, updatevalue = 0)
  # ensure existing SPA are locked out
  curr_sol <- mask(curr_sol, existing_spa, maskvalue = 1, updatevalue = 0)
  # calculate total extent
  curr_area <- global(curr_sol, "sum", na.rm = TRUE)[[1]]
  # check if total extent of solution exceeds threshold
  if (curr_area > 60486) {
    break
  } else {
    s1_wild <- curr_sol
  }
}


#s1_wild has 2 cells with value 2, I change them into value 1
reclass_matrix_1b <- matrix(c(2, 1), ncol = 2, byrow = TRUE)
s1_wild_reclass <-  classify(s1_wild, reclass_matrix_1b)


plot(s1_wild_reclass)

# evaluating the solution

# calculate statistic 
# cost summary
eval_cost_summary(p1, s1_wild_reclass)

# Feature representation summary
print(eval_feature_representation_summary(p1_wild, s1_wild_reclass), n=30)

# Target coverage summary
# calculate statistics
print(eval_target_coverage_summary(p1_wild, s1_wild_reclass), n=30)



# Scenario 1b
library(prioritizr)
library(sf)
library(dplyr)
library(terra)
library(raster)
library(gurobi)
library(fasterize)

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

# load in PWA vector data
pwa_vector <-
  read_sf("input-data/Potential wilderness areas/PWA_1000_NRW_25832.shp") %>%
  sf::st_geometry() %>%
  c(
    read_sf("input-data/Potential wilderness areas/PWA_100_NRW_25832.shp") %>%
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

# creating the conservation features object
cons_feat_1b <- c(
  bstacked_fht,
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

# define targets
## no conservation targets, I want to focus on wilderness areas
wild_targets <- c(
  rep(0, nlyr(bstacked_fht)), ## >= =% coverage of each forest type
  0,                          ## >= 0% coverage of existing_spa
  0,                          ## >= 0% coverage of pwa
  0,                          ## >= 0% coverage of state_f
  0,                          ## >= 0% coverage of N2000
  0                           ## tfc_feature, should it be 1, or 0 ?
)

# create problem to evaluate solution
p1 <-
  problem(tfc_const_costs, cons_feat_1b) %>%
  add_min_set_objective() %>%
  add_relative_targets(wild_targets)

# generate solution
## initialize with empty solution raster polygons
s1 <- tfc_const_costs * 0
for (i in seq_len(nrow(pwa_vector))) {
  # add progress message
  message("starting iteration ", i)
  # attempt to rasterize next PWA
  curr_pwa <- rasterize(
    x = vect(pwa_vector[i, ]),
    y = tfc_const_costs,
    field = "value",
    touches = TRUE,
    background = 0
  )
  # try adding current PWA to solution
  curr_sol <- s1 + curr_pwa
  # ensure not state forest is locked out
  curr_sol <- mask(curr_sol, not_state_f, maskvalue = 1, updatevalue = 0)
  # calculate total extent
  curr_area <- global(curr_sol, "sum", na.rm = TRUE)[[1]]
  # check if total extent of solution exceeds threshold
  if (curr_area > 60000) {
    break
  } else {
    s1 <- curr_sol
  }
}

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

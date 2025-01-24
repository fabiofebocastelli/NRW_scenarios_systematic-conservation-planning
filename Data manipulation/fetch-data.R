# load package
library(piggyback)

# specify files to download
file_names <- c(
  "Eligible_forest-Federal_forest_25832.tif",
  "forest_habitat_types_reclas_25832.tif",
  "Forest_strictly_protected_25832_revised.tif",
  "Habitat_directive_FFH_25832.tif",
  "PWA_3000_NRW_25832.tif",
  "State_forest_25832.tif",
  "total_forest_cover_25832.tif",
  "vitality_highly_decreased_25832.tif"
)

# download data
pb_download(
  file = file_names,
  dest = "input-data",
  repo = "fabiofebocastelli/systematic-conservation-planning",
  tag = "latest"
)

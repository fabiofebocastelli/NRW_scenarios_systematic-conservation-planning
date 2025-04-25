# non damaged forests ---------------------------------------------------

forest_cover <- rast("D:/EFI_Data/NRW_Data/u2018_clc2018_v2020_20u1_geoPackage/total_forest_cover_25832.tif")
plot(forest_cover)

damaged_forests <- rast("D:/EFI_Data/NRW_Data/Vitality Decrease/vitality_highly_decreased_25832.tif")
plot(damaged_forests)
  
# 1. Creazione maschera delle foreste danneggiate (valore 3 = danneggiate)
damaged_mask <- damaged_forests == 3  # TRUE per celle danneggiate

# 2. Creazione foreste non danneggiate (valore 1 = foreste non danneggiate)
non_damaged_forest <- ifel(
  test = damaged_mask,
  yes = NA,
  no = forest_cover
)

# 3. Verifica struttura finale
freq(non_damaged_forest)  # Dovrebbe mostrare solo NA e 1

# 4. Salvataggio
writeRaster(
  non_damaged_forest,
  filename = "C:/NRW_figures/NRW figures/non_damaged_forest.tif",
  datatype = "INT1U",  # Per valori 0-1 (ma nel tuo caso useremo NA e 1)
  NAflag = 255,        # Valore personalizzato per NA (opzionale)
  overwrite = TRUE
)



# pwas --------------------------------------------------------------------

pwa100 <- rast("D:/EFI_Data/NRW_Data/Potential wilderness areas/PWA_100_notsp_NRW_revised_25832.tif")
pwa1000 <- rast("D:/EFI_Data/NRW_Data/Potential wilderness areas/PWA_1000_notsp_NRW_revised_25832.tif")
pwa3000 <- rast("D:/EFI_Data/NRW_Data/Potential wilderness areas/PWA_3000_notsp_NRW_25832.tif")

# 2. Verifica allineamento
# Controlla CRS, risoluzione e estensione
compareGeom(pwa100, pwa1000) # Deve restituire TRUE
compareGeom(pwa100, pwa3000) # Deve restituire TRUE

#  Unione dei raster
merged_pwa <- mosaic(
  pwa100,
  pwa1000,
  pwa3000,
  fun = "max" # Prende il valore massimo dove c'è sovrapposizione
)

# save
writeRaster(
  merged_pwa, # Usa merged_pwa per mantenere valori originali
  "C:/NRW_figures/NRW figures/PWA_merged_NRW_25832.tif",
  datatype = "INT1U",
  NAflag = 255,
  overwrite = TRUE
)


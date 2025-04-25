library(terra)
library(tidyverse)

# 1. Caricamento dei dati -------------------------------------------------
scenarios <- c(
  scen1 = "C:/NRW_figures/NRW figures/Outputs_figures/s1_wild.tif",
  scen3 = "C:/NRW_figures/NRW figures/Outputs_figures/s3.tif",
  scen5 = "C:/NRW_figures/NRW figures/Outputs_figures/s5.tif"
) %>% 
  map(rast) %>% 
  rast()


# 2. Analisi sovrapposizione spaziale --------------------------------------
# Creazione mappa di accordo
agreement_map <- sum(scenarios, na.rm = TRUE) %>% 
  classify(matrix(c(0,0, 1,1, 2,2, 3,3), ncol = 2, byrow = TRUE))

# Calcolo aree di accordo
agreement_stats <- freq(agreement_map) %>% 
  as_tibble() %>% 
  mutate(
    category = case_when(
      value == 0 ~ "Nessun accordo",
      value == 1 ~ "Accordo parziale (1 scenario)",
      value == 2 ~ "Accordo moderato (2 scenari)", 
      value == 3 ~ "Accordo completo"
    ),
    percent = (count/sum(count))*100
  )


# for ownership/wilderness representation ----------------------------------
landtype <- c(
  non_damaged = "C:/NRW_figures/NRW figures/Input_Data_figures/non_damaged_forest.tif", 
  damaged = "D:/EFI_Data/NRW_Data/Vitality Decrease/vitality_highly_decreased_25832.tif",
  wilderness = "C:/NRW_figures/NRW figures/Input_Data_figures/PWA_merged_NRW_25832.tif"
) %>% 
  map(rast) %>% 
  rast()

# 3. Analisi rappresentazione categorie forestali --------------------------
representation_analysis <- map_dfr(
  names(scenarios),
  function(scen){
    map_dfr(
      names(landtype),
      function(forest_type){
        selected_area <- mask(
          scenarios[[scen]],
          landtype[[forest_type]],
          maskvalue = NA
        ) %>% 
          global("sum", na.rm = TRUE) %>% 
          pull(sum)
        
        total_area <- global(landtype[[forest_type]], "sum", na.rm = TRUE) %>% 
          pull(sum)
        
        tibble(
          Scenario = scen,
          ForestType = forest_type,
          SelectedArea = selected_area,
          TotalArea = total_area,
          PercentProtected = (selected_area/total_area)*100
        )
      }
    )
  }
)

# 4. Visualizzazioni ------------------------------------------------------
# Mappa di accordo
agreement_plot <- plot(agreement_map, 
     col = c("#D3D3D3", "#E66100", "#5D3A9B", "#40B0A6"),  # Palette accessibile
     main = "Conservation Scenarios Overlap",
     col.main = "black",
     axis.args = list(cex.axis = 0.8))

# save
tiff(
  filename = "C:/NRW_figures/NRW figures/Outputs_figures/agreement_plot.tiff",
  width = 7,             # inches
  height = 5,
  units = "in",
  res = 600,             # 600 DPI
  compression = "lzw"    # Lossless compression
)

plot(agreement_map, 
     col = c("#D3D3D3", "#E66100", "#5D3A9B", "#40B0A6"),
     main = "Conservation Scenarios Overlap",
     col.main = "black",
     axis.args = list(cex.axis = 0.8))

dev.off()


# Grafico a barre rappresentazione
landcover_rep <- ggplot(representation_analysis, 
       aes(x = Scenario, y = PercentProtected, fill = ForestType)) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c(
      "damaged" = "#D55E00",        # Vermillion (colorblind-safe red)
      "non_damaged" = "#009E73",    # Bluish green
      "wilderness" = "#56B4E9"      # Sky blue
    ),
    labels = c(
      "damaged" = "Damaged forests",
      "non_damaged" = "Healthy forests",
      "wilderness" = "Potential wilderness areas"
    ),
    guide = guide_legend()          # Remove legend title
  ) +
  scale_x_discrete(
    labels = c("scen1" = "Scenario 1", 
               "scen3" = "Scenario 3", 
               "scen5" = "Scenario 5")
  ) +
  labs(
    y = "Protected area (%)", 
    x = ""
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(), # Remove legend title
    plot.title = element_blank()    # Remove main title
  )

# save

# Prima modifica il plot aggiungendo la nuova scala di colori
landcover_rep <- landcover_rep +
  scale_fill_viridis_d(
    option = "D",
    labels = c("Damaged", "Healthy", "Wilderness"),
    guide = guide_legend(title = NULL)
  )

# Poi salva il plot modificato
ggsave(
  filename = "landcover_rep.png",
  path = "C:/NRW_figures/NRW_figures/Outputs_figures",  
  plot = landcover_rep,
  device = "png",
  dpi = 600,
  width = 210,       # 210 mm = ~8.27 inches (per units="mm")
  height = 125,      # 125 mm = ~4.92 inches
  units = "mm",
  bg = "white"
)


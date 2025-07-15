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
library(viridis)
library(scales)
library(terra)
library(prettymapr)


# Mappa di accordo
mako_cols <- c("#D3D3D3", viridis::mako(3))

png("C:/NRW_figures/NRW figures/Outputs_figures/agreement_map_grad.png", width = 8000, height = 5000, res = 800)


agreement_plot <- plot(
  agreement_map, 
  col = c("#D3D3D3", "#6500A7FF", "#EB7654FF", "#F2F127FF"),
  main = "Conservation Scenarios Overlap",
  col.main = "black",
  axis.args = list(cex.axis = 0.8)
)

# Calcoli automatici per una scala bar ragionevole
ext <- ext(agreement_map)
xrange <- ext[2] - ext[1]
yrange <- ext[4] - ext[3]
preferred_length <- xrange * 0.1
nice_length <- round(preferred_length / 500) * 500

# Barra in basso a destra
# labels: solo due valori (0 e nice_length in km), nessun valore al centro
labels_km <- c(0, NA, nice_length / 1000)

# Scegli adj: con adj = c(0.5, 2.6) la scritta "km" è molto vicina sotto la barra,
# e con "halo=TRUE" i numeri rimangono leggibili (prova anche c(0.5,2.7) se vuoi attaccarla ancora di più)
sbar(
  d = nice_length,
  xy = "bottomright",
  type = "bar",
  divs = 2,
  labels = labels_km,
  cex = 1,
  below = "km",
  adj = c(0.5, 1.2),
  col = "black",
  lwd = 2,
  halo = TRUE
)

# Ora posiziona la freccia nord sopra la barra: 
# prendi la posizione Y della barra (stessa di "bottomright") e aggiungi un piccolo margine in altezza,
# e centra sul lato destro come la barra stessa
x_north <- ext[2] - 0.105 * xrange  # centrato rispetto alla barra: regola 0.18 se la lunghezza cambia molto
y_north <- ext[3] + 0.15 * yrange # poco sopra la barra: regola 0.15 se serve

north(
  xy = c(x_north, y_north),
  type = 2,
  cex = 1.2,
  label = ""
)

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


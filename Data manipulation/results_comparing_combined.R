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

# for ownership/wilderness representation ----------------------------------
ownerships <- c(
  non_damaged = "path/to/non_damaged.tif", #to be calculated
  damaged = "path/to/damaged.tif",
  wilderness = "path/to/wilderness.tif"
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

# 3. Analisi rappresentazione categorie forestali --------------------------
representation_analysis <- map_dfr(
  names(scenario_rasters),
  function(scen){
    map_dfr(
      names(forest_rasters),
      function(forest_type){
        selected_area <- mask(
          scenario_rasters[[scen]],
          forest_rasters[[forest_type]],
          maskvalue = NA
        ) %>% 
          global("sum", na.rm = TRUE) %>% 
          pull(sum)
        
        total_area <- global(forest_rasters[[forest_type]], "sum", na.rm = TRUE) %>% 
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
plot(agreement_map, 
     col = c("grey", "yellow", "orange", "darkgreen"),
     main = "Sovrapposizione scenari di conservazione")

# Grafico a barre rappresentazione
ggplot(representation_analysis, 
       aes(x = ForestType, y = PercentProtected, fill = Scenario)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Rappresentazione categorie forestali negli scenari",
       y = "Percentuale protetta (%)", x = "Tipo forestale")

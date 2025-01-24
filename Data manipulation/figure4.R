#load librariess
library(ggplot2)
library(patchwork)
library(RColorBrewer)

# Dati per la prima serie
data1 <- data.frame(
  scenario = rep(paste("Scenario", 1:5), each = 6),
  ownership = rep(c("Federal forests", "State forests", "Eligible forests"), each = 2, times = 5),
  type = rep(c("Initial", "New"), 15),
  value = c(
    # Scenario 1
    0.016, 0.016, 0.13, 0.802, 0.182, 0.182,
    # Scenario 2
    0.016, 0.016, 0.13, 0.331, 0.182, 0.412,
    # Scenario 3
    0.016, 0.016, 0.13, 0.376, 0.182, 0.617,
    # Scenario 4
    0.016, 0.016, 0.13, 0.467, 0.182, 0.517,
    # Scenario 5
    0.016, 0.016, 0.13, 0.802, 0.182, 0.182
  )
)

# Impostare l'ordine corretto per le forest ownership
data1$ownership <- factor(data1$ownership, levels = c("Federal forests", "State forests", "Eligible forests"))


# Dati per la seconda serie
data2 <- data.frame(
  scenario = rep(paste("Scenario", c(1, 3, 5)), each = 4),
  metric = rep(c("Aggregation Index", "Clumpiness Index", "Forest habitats", "Natura 2000"), 3),
  value = c(
    # Scenario 1
    0.857, 0.939, 0.203, 0.233,
    # Scenario 3
    0.806, 0.625, 0.434, 0.459,
    # Scenario 5
    0.841, 0.852, 0.350, 0.372
  )
)

# Impostare l'ordine corretto per le metriche
data2$metric <- factor(data2$metric, levels = c("Aggregation Index", "Clumpiness Index", "Forest habitats", "Natura 2000"))


# Dati per la terza serie
features <- c("91D0", "91E0", "91E0; NAX0", "91E0; NAC0", "91F0", "NA00", "NAB0", "NAC0", "NAC0; NAX0", "NAD0", 
              "NAV0", "NAW0", "9110", "9110; 91E0", "9130", "9150", "9160", "9170", "9180", "9190", 
              "NAX0", "NAX0; NAY0", "NAY0", "91D1", "NAK0", "Natura 2000")

values_scenario1 <- c(13, 11, 0, 0, 8, 14, 13, 4, 5, 38, 7, 12, 29, 0, 25, 4, 18, 20, 19, 15, 3, 0, 29, 0, 50, 20)
values_scenario3 <- c(33, 36, 33, 38, 38, 41, 41, 32, 37, 66, 32, 42, 50, 41, 45, 33, 44, 51, 45, 45, 31, 100, 57, 40, 20, 43)
values_scenario5 <- c(26, 22, 0, 5, 33, 29, 19, 15, 68, 44, 25, 13, 47, 91, 33, 14, 38, 26, 23, 39, 9, 0, 34, 0, 20, 35)

data3 <- data.frame(
  scenario = rep(paste("Scenario", c(1, 3, 5)), each = 26),
  category = rep(features, 3),
  value = c(values_scenario1, values_scenario3, values_scenario5) / 100  # Convertito in decimali
)

# Impostare l'ordine corretto per le categorie
data3$category <- factor(data3$category, levels = features)


# Funzione per il tema comune
common_theme <- function() {
  theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 12, face = "plain"),
      plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 20)),
      axis.title.y = element_blank()
    )
}

# Prima serie di barplot
p1 <- ggplot(data1, aes(x = ownership, y = value, fill = type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Initial" = "black", "New" = "grey70")) +
  facet_grid(~ scenario, scales = "free_x", space = "free_x") +
  labs(title = "Forest Owners Involvement", x = NULL) +
  common_theme() +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)

#rimuovo griglia in sfondo
p1 <- p1 + theme(
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank()
)

# Seconda serie di barplot
p2 <- ggplot(data2, aes(x = metric, y = value, fill = metric)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  facet_grid(~ scenario, scales = "free_x", space = "free_x") +
  labs(title = "Compactness vs Representativeness", x = NULL, y = "Percentage") +
  common_theme() +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)

p2 <- p2 + theme(
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank()
)

# Terza serie di barplot
p3 <- ggplot(data3, aes(x = category, y = value, fill = factor(scenario))) +
  geom_bar(stat = "identity", width = 0.8) +
  scale_fill_brewer(palette = "Dark2") +
  facet_grid(~ scenario, scales = "free_x", space = "free_x") +
  geom_hline(yintercept = 0.3, color = "black", linetype = "solid", size = 0.5) +
  labs(title = "Priority Areas Coverage of Ecologically Valuable Areas", x = "") +
  common_theme() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
    axis.text.x.bottom = element_text(size = 6, hjust = 1, angle = 45)  # Aggiungi questa riga
  ) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)

p3 <- p3 + theme(
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank()
)


# Combinare i plot verticalmente con più spazio tra loro
final_plot <- p1 / plot_spacer() / p2 / plot_spacer() / p3 +
  plot_layout(heights = c(1.2, 0.1, 1, 0.1, 1))

# Visualizzare il plot
print(final_plot)

# Salvare il plot come immagine ad alta risoluzione
ggsave("figure4.png", final_plot, width = 12, height = 18, dpi = 1000)

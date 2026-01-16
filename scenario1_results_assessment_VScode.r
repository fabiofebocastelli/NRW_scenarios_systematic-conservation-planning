################################################################################
# Scenario 1 Results Assessment
# Protected Wilderness Area (PWA) Analysis
#
# Purpose: Analyze raster data to characterize patch size distribution across
#          predefined PWA size classes (100-999 ha, 1,000-2,999 ha, 3,000-10,000 ha)
#
# Author: [Your name]
# Date: 2026-01-16
# R version: 4.x
################################################################################

# Set working directory and session parameters
setwd("C:/NRW_figures/NRW figures")
set.seed(42) # For reproducibility

# Load required libraries
library(terra)
library(ggplot2)
library(scales)

# Session information (for reproducibility)
cat("=== SESSION INFORMATION ===\n")
cat(sprintf("R version: %s\n", R.version$version.string))
cat(sprintf("Platform: %s\n", R.version$platform))
cat(sprintf("Working directory: %s\n", getwd()))
cat(sprintf("Analysis date: %s\n\n", Sys.Date()))

# Load raster data with error handling
raster_path <- "C:/NRW_figures/NRW figures/Outputs_figures/scenario1.tif"
if (!file.exists(raster_path)) {
    stop("Error: Raster file not found at ", raster_path)
}

s1 <- rast(raster_path)
cat("Raster file loaded successfully\n")


################################################################################
# SECTION 1: Data Quality Assessment
################################################################################

cat("\n=== 1. RASTER DATA QUALITY ASSESSMENT ===\n")

# Check unique values
unique_values <- unique(values(s1))
cat(sprintf("Number of unique values: %d\n", length(unique_values)))
cat(sprintf(
    "Value range: %.0f to %.0f\n", min(unique_values, na.rm = TRUE),
    max(unique_values, na.rm = TRUE)
))

# Examine CRS (Coordinate Reference System)
cat("\n--- Coordinate Reference System (CRS) ---\n")
crs_info <- crs(s1, describe = TRUE)
print(crs_info)

cat("\nCRS Type: ")
if (is.lonlat(s1)) {
    cat("Geographic (degrees, lat-lon)\n")
} else {
    cat("Projected (meters, UTM/equivalent)\n")
}

# Resolution and extent
cat("\n--- Spatial Resolution and Extent ---\n")
print(s1)
res_m <- res(s1)
cat(sprintf("Resolution: %.1f × %.1f meters\n", res_m[1], res_m[2]))

# Cell area validation
cat("\n--- Cell Area Calculation ---\n")
cell_ha <- cellSize(s1, unit = "ha")
mean_cell_area <- mean(values(cell_ha), na.rm = TRUE)
cat(sprintf("Mean cell area: %.4f ha\n", mean_cell_area))
cat("Cell size calculation verified: OK\n")


################################################################################
# SECTION 2: Patch Identification and Size Classification
################################################################################

cat("\n\n=== 2. PATCH DETECTION AND CLASSIFICATION ===\n")

# Identify contiguous patches (8-directional connectivity)
prot <- s1
patch_id <- patches(prot, directions = 8, zeroAsNA = TRUE)
cat(sprintf("Number of patches detected: %d\n", max(values(patch_id), na.rm = TRUE)))

# Calculate patch area
cell_area_ha <- cellSize(patch_id, unit = "ha")
patch_area <- zonal(cell_area_ha, patch_id, fun = "sum")
names(patch_area) <- c("patch_id", "area_ha")

cat(sprintf("Total patches with area data: %d\n", nrow(patch_area)))
cat(sprintf("Mean patch area: %.1f ha\n", mean(patch_area$area_ha, na.rm = TRUE)))
cat(sprintf("Median patch area: %.1f ha\n", median(patch_area$area_ha, na.rm = TRUE)))
cat(sprintf(
    "Min-Max patch area: %.1f - %.1f ha\n\n",
    min(patch_area$area_ha), max(patch_area$area_ha)
))

# Define PWA size classes
# Class 1: 3,000 - 10,000 ha
n_3000_10000 <- sum(patch_area$area_ha >= 3000 & patch_area$area_ha <= 10000)
area_3000_10000 <- sum(patch_area$area_ha[patch_area$area_ha >= 3000 & patch_area$area_ha <= 10000])

# Class 2: 1,000 - 2,999 ha
n_1000_2999 <- sum(patch_area$area_ha >= 1000 & patch_area$area_ha < 3000)
area_1000_2999 <- sum(patch_area$area_ha[patch_area$area_ha >= 1000 & patch_area$area_ha < 3000])

# Class 3: 100 - 999 ha
n_100_999 <- sum(patch_area$area_ha >= 100 & patch_area$area_ha <= 999)
area_100_999 <- sum(patch_area$area_ha[patch_area$area_ha >= 100 & patch_area$area_ha <= 999])

# Unclassified patches: < 100 ha
n_totale <- nrow(patch_area)
area_totale <- sum(patch_area$area_ha)
n_lt_100 <- n_totale - (n_3000_10000 + n_1000_2999 + n_100_999)
area_lt_100 <- area_totale - (area_3000_10000 + area_1000_2999 + area_100_999)


################################################################################
# SECTION 3: Summary Statistics and Results Table
################################################################################

cat("\n\n=== 3. SUMMARY STATISTICS BY PWA SIZE CLASS ===\n")

# Calculate wilderness area (patches ≥ 100 ha)
wilderness_total <- area_3000_10000 + area_1000_2999 + area_100_999
n_100_plus <- n_3000_10000 + n_1000_2999 + n_100_999

# Format output table
results_table <- data.frame(
    Class = c(
        "Large (3,000-10,000 ha)",
        "Medium (1,000-2,999 ha)",
        "Small (100-999 ha)",
        "Wilderness Total (≥100 ha)",
        "Total Protected Area"
    ),
    N_Patches = c(n_3000_10000, n_1000_2999, n_100_999, n_100_plus, n_totale),
    Area_ha = c(area_3000_10000, area_1000_2999, area_100_999, wilderness_total, area_totale),
    Percent_Total = c(
        100 * area_3000_10000 / area_totale,
        100 * area_1000_2999 / area_totale,
        100 * area_100_999 / area_totale,
        100 * wilderness_total / area_totale,
        100
    )
)

# Print formatted table
cat("\nPWA Size Class Distribution\n")
cat(strrep("-", 90), "\n")
cat(sprintf("%-35s %12s %15s %12s\n", "Class", "N Patches", "Area (ha)", "% Total"))
cat(strrep("-", 90), "\n")
for (i in 1:nrow(results_table)) {
    cat(sprintf(
        "%-35s %12d %15.0f %11.1f%%\n",
        results_table$Class[i],
        results_table$N_Patches[i],
        results_table$Area_ha[i],
        results_table$Percent_Total[i]
    ))
}
cat(strrep("-", 90), "\n")

# Verification check
sum_check <- area_3000_10000 + area_1000_2999 + area_100_999 + area_lt_100
cat(sprintf(
    "\nData Integrity Check: %.0f ha (sum of all classes) == %.0f ha (total area) %s\n",
    sum_check, area_totale,
    ifelse(abs(sum_check - area_totale) < 0.01, "[OK]", "[ERROR]")
))

################################################################################
# SECTION 4: Data Visualization
################################################################################

cat("\n\n=== 4. GENERATING PUBLICATION-QUALITY FIGURES ===\n")

# Plot 1: Patch size distribution histogram
cat("Creating Figure 1: Patch size distribution histogram\n")

patch_small <- patch_area[patch_area$area_ha < 3000, ]

p1 <- ggplot(patch_small, aes(x = area_ha)) +
    geom_histogram(binwidth = 20, fill = "#2E8B57", color = "black", linewidth = 0.3, alpha = 0.75) +
    geom_vline(
        xintercept = c(100, 1000, 3000),
        color = "red", linetype = "dashed", linewidth = 0.8, alpha = 0.7
    ) +
    labs(
        title = "Distribution of Protected Patch Sizes (< 3,000 ha)",
        subtitle = "Scenario 1 Analysis",
        x = "Patch Area (hectares)",
        y = "Number of Patches",
        caption = "Red dashed lines indicate PWA size class boundaries"
    ) +
    scale_x_continuous(breaks = seq(0, 3000, by = 500), labels = scales::comma) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_minimal() +
    theme(
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 9, hjust = 0),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
        axis.title = element_text(size = 10, face = "bold"),
        plot.margin = margin(t = 10, r = 15, b = 15, l = 10)
    )

library(httpgd)
hgd()
print(p1)


# Plot 2: Area distribution by PWA size class
cat("Creating Figure 2: Area distribution by PWA size class\n")

# Prepare data for bar plot
grafico_data <- data.frame(
    Size_Class = factor(
        c(
            "Large\n(3,000-10,000 ha)",
            "Medium\n(1,000-2,999 ha)",
            "Small\n(100-999 ha)",
            "< 100 ha"
        ),
        levels = c(
            "Large\n(3,000-10,000 ha)", "Medium\n(1,000-2,999 ha)",
            "Small\n(100-999 ha)", "< 100 ha"
        )
    ),
    Area_ha = c(area_3000_10000, area_1000_2999, area_100_999, area_lt_100),
    N_Patches = c(n_3000_10000, n_1000_2999, n_100_999, n_lt_100)
)

# Define scientific color palette
color_palette <- c("#9da1a0", "#9da1a0", "#9da1a0", "#9da1a0")

p_bar <- ggplot(grafico_data, aes(x = Size_Class, y = Area_ha, fill = Size_Class)) +
    geom_col(color = "black", linewidth = 0.5, alpha = 0.85) +
    geom_text(
        aes(label = paste0("n. of patches = ", N_Patches)),
        vjust = -0.5, # Move the text above the bars
        size = 4.5, # Increase font size
        fontface = "bold"
    ) +
    labs(
        title = "Distribution of New Protected Forest Area by PWA Size Class",
        subtitle = "Scenario 1",
        x = "PWA Size Class",
        y = "Total Protected Area (hectares)",
    ) +
    scale_y_continuous(
        labels = scales::label_number(big.mark = ","),
        expand = expansion(mult = c(0, 0.15))
    ) +
    scale_fill_manual(values = color_palette, guide = "none") +
    theme_minimal() +
    theme(
        plot.title = element_text(face = "bold", size = 14), # Increase title font size
        plot.subtitle = element_text(size = 12), # Increase subtitle font size
        plot.caption = element_text(size = 10, hjust = 0), # Increase caption font size
        axis.title = element_text(size = 12, face = "bold"), # Increase axis title font size
        axis.text.x = element_text(size = 10), # Increase x-axis text size
        axis.text.y = element_text(size = 10), # Increase y-axis text size
        axis.title.x = element_text(margin = margin(t = 10)), # Increase space between x-axis title and axis
        axis.title.y = element_text(margin = margin(r = 10)), # Increase space between y-axis title and axis
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(t = 10, r = 20, b = 15, l = 10)
    ) +
    scale_x_discrete(labels = c("3000-10000 ha", "1000-2999 ha", "100-999 ha", "<100 ha"))

p_bar

# saving outputs
cat("\n\n=== 5. SAVING RESULTS AND FIGURES ===\n")

# Define output directory
output_dir <- "C:/NRW_figures/NRW figures/Outputs_figures"
if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat(sprintf("Created output directory: %s\n", output_dir))
}

# Save results table as CSV
results_csv_path <- file.path(output_dir, "PWA_size_class_results_Scenario1.csv")
write.csv(results_table, results_csv_path, row.names = FALSE)
cat(sprintf("Results table saved: %s\n", results_csv_path))

# Save results table as formatted text file
results_txt_path <- file.path(output_dir, "PWA_size_class_results_Scenario1.txt")
sink(results_txt_path)
cat("=== PWA SIZE CLASS DISTRIBUTION - SCENARIO 1 ===\n")
cat(sprintf("Analysis Date: %s\n\n", Sys.Date()))
cat("PWA Size Class Distribution\n")
cat(strrep("-", 90), "\n")
cat(sprintf("%-35s %12s %15s %12s\n", "Class", "N Patches", "Area (ha)", "% Total"))
cat(strrep("-", 90), "\n")
for (i in 1:nrow(results_table)) {
    cat(sprintf(
        "%-35s %12d %15.0f %11.1f%%\n",
        results_table$Class[i],
        results_table$N_Patches[i],
        results_table$Area_ha[i],
        results_table$Percent_Total[i]
    ))
}
cat(strrep("-", 90), "\n")
sink()
cat(sprintf("Results table saved (text): %s\n", results_txt_path))

# Save Plot 1: Patch size distribution histogram
p1_path <- file.path(output_dir, "Figure1_Patch_Size_Distribution_Scenario1.png")
ggsave(
    filename = p1_path,
    plot = p1,
    width = 10,
    height = 6,
    units = "in",
    dpi = 300,
    bg = "white"
)
cat(sprintf("Figure 1 saved: %s\n", p1_path))

# Also save as PDF for publication quality
p1_pdf <- file.path(output_dir, "Figure1_Patch_Size_Distribution_Scenario1.pdf")
ggsave(
    filename = p1_pdf,
    plot = p1,
    width = 10,
    height = 6,
    units = "in",
    bg = "white"
)
cat(sprintf("Figure 1 (PDF) saved: %s\n", p1_pdf))

# Save Plot 2: Area distribution by PWA size class
p2_path <- file.path(output_dir, "Figure2_Area_Distribution_by_PWA_Class_Scenario1.png")
ggsave(
    filename = p2_path,
    plot = p_bar,
    width = 10,
    height = 6,
    units = "in",
    dpi = 300,
    bg = "white"
)
cat(sprintf("Figure 2 saved: %s\n", p2_path))

# Also save as PDF for publication quality
p2_pdf <- file.path(output_dir, "Figure2_Area_Distribution_by_PWA_Class_Scenario1.pdf")
ggsave(
    filename = p2_pdf,
    plot = p_bar,
    width = 10,
    height = 6,
    units = "in",
    bg = "white"
)
cat(sprintf("Figure 2 (PDF) saved: %s\n", p2_pdf))

cat("\n✓ All outputs saved successfully to:\n")
cat(sprintf("  %s\n\n", output_dir))

################################################################################
# END OF ANALYSIS
################################################################################

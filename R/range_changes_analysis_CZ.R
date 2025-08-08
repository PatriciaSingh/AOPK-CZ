# Script Name: range_changes_analysis_CZ.R
# Purpose: Analyze TIFF files for range changes, calculate pixel counts by category
# Author: Generated for AOPK collaboration analysis
# Date: 2025-08-08

# Load required libraries
library(terra)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

# Set working directory and define paths
repo_path <- "C:/Users/patricia/PATA/SCIENCE/RESEARCH/AOPK_spoluprace/AOPK-CZ"
tiff_folder <- file.path(repo_path, "Range_Changes_EU")
cz_folder <- file.path(repo_path, "CZ")

# Set working directory
setwd(repo_path)

# Create output directories
output_dir <- file.path(repo_path, "Analysis_Results")
plots_dir <- file.path(output_dir, "Plots")
data_dir <- file.path(output_dir, "Data")

# Create directories if they don't exist
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

cat("Output will be saved to:", output_dir, "\n")

# Load Czech Republic boundary shapefile
# Adjust the shapefile name as needed (common names: CZ.shp, czech_republic.shp, etc.)
cz_boundary <- vect(file.path(cz_folder, "CZ.shp"))  # Adjust filename if needed

# Get list of TIFF files
tiff_files <- list.files(tiff_folder, pattern = "\\.tif$", full.names = TRUE)
print(paste("Found", length(tiff_files), "TIFF files:"))
print(basename(tiff_files))

# Initialize results data frames
results_full <- data.frame()
results_cz <- data.frame()

# Define category labels
category_labels <- c("-2" = "Decline", 
                     "-1" = "Stable", 
                     "0" = "Absence", 
                     "1" = "Increase")

# Function to calculate pixel counts
calculate_pixel_counts <- function(raster_data, file_name) {
  # Get frequency table
  freq_table <- freq(raster_data)
  
  # Convert to data frame and add file info
  if (nrow(freq_table) > 0) {
    result <- data.frame(
      file = file_name,
      category = freq_table$value,
      count = freq_table$count,
      stringsAsFactors = FALSE
    )
  } else {
    result <- data.frame(
      file = file_name,
      category = numeric(0),
      count = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  
  return(result)
}

# Process each TIFF file
cat("\nProcessing TIFF files...\n")
for (i in seq_along(tiff_files)) {
  file_path <- tiff_files[i]
  file_name <- tools::file_path_sans_ext(basename(file_path))
  
  cat(paste("Processing:", file_name, "(",i,"/",length(tiff_files),")\n"))
  
  # Load raster
  raster_data <- rast(file_path)
  
  # Calculate pixel counts for full extent
  counts_full <- calculate_pixel_counts(raster_data, file_name)
  results_full <- rbind(results_full, counts_full)
  
  # Crop to Czech Republic and calculate pixel counts
  tryCatch({
    # Ensure same CRS
    if (crs(raster_data) != crs(cz_boundary)) {
      cz_boundary_proj <- project(cz_boundary, crs(raster_data))
    } else {
      cz_boundary_proj <- cz_boundary
    }
    
    # Crop and mask to Czech Republic
    raster_cz <- crop(raster_data, cz_boundary_proj)
    raster_cz <- mask(raster_cz, cz_boundary_proj)
    
    # Calculate pixel counts for Czech Republic
    counts_cz <- calculate_pixel_counts(raster_cz, file_name)
    results_cz <- rbind(results_cz, counts_cz)
    
  }, error = function(e) {
    cat(paste("Error processing", file_name, "for Czech Republic:", e$message, "\n"))
  })
}

# Add category labels and calculate percentages
add_labels_and_percentages <- function(df) {
  df$category_label <- category_labels[as.character(df$category)]
  df$category_label[is.na(df$category_label)] <- paste("Category", df$category[is.na(df$category_label)])
  
  # Calculate percentages within each file
  df <- df %>%
    group_by(file) %>%
    mutate(
      total_pixels = sum(count, na.rm = TRUE),
      percentage = round(count / total_pixels * 100, 2)
    ) %>%
    ungroup()
  
  return(df)
}

# Process results
results_full <- add_labels_and_percentages(results_full)
results_cz <- add_labels_and_percentages(results_cz)

# Print summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")
cat("\nFull Extent Results:\n")
print(results_full)

cat("\nCzech Republic Results:\n")
print(results_cz)

# Create visualizations
cat("\nCreating visualizations...\n")

# 1. Stacked bar chart for full extent
p1 <- ggplot(results_full, aes(x = file, y = count, fill = category_label)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(name = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Pixel Counts by Category - Full Extent",
       x = "File", y = "Pixel Count") +
  scale_y_continuous(labels = scales::comma)

# 2. Stacked bar chart for Czech Republic
p2 <- ggplot(results_cz, aes(x = file, y = count, fill = category_label)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(name = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Pixel Counts by Category - Czech Republic",
       x = "File", y = "Pixel Count") +
  scale_y_continuous(labels = scales::comma)

# 3. Percentage comparison
comparison_data <- bind_rows(
  results_full %>% mutate(extent = "Full Extent"),
  results_cz %>% mutate(extent = "Czech Republic")
)

p3 <- ggplot(comparison_data, aes(x = file, y = percentage, fill = category_label)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~extent, ncol = 1) +
  scale_fill_viridis_d(name = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Percentage Distribution Comparison",
       x = "File", y = "Percentage") +
  ylim(0, 100)

# 4. Side-by-side comparison of categories
p4 <- ggplot(comparison_data, aes(x = category_label, y = percentage, fill = extent)) +
  geom_boxplot() +
  scale_fill_viridis_d(name = "Extent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Category Distribution Comparison Across All Files",
       x = "Category", y = "Percentage")

# Display plots
print(p1)
print(p2)
print(p3)
print(p4)

# Save results to CSV files
write.csv(results_full, file.path(data_dir, "pixel_counts_full_extent.csv"), row.names = FALSE)
write.csv(results_cz, file.path(data_dir, "pixel_counts_czech_republic.csv"), row.names = FALSE)

# Save plots
ggsave(file.path(plots_dir, "pixel_counts_full_extent.png"), p1, width = 12, height = 8, dpi = 300)
ggsave(file.path(plots_dir, "pixel_counts_czech_republic.png"), p2, width = 12, height = 8, dpi = 300)
ggsave(file.path(plots_dir, "percentage_comparison.png"), p3, width = 12, height = 10, dpi = 300)
ggsave(file.path(plots_dir, "category_distribution_comparison.png"), p4, width = 10, height = 8, dpi = 300)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Results saved to:\n")
cat("- Data files:", data_dir, "\n")
cat("- Plot files:", plots_dir, "\n")
cat("Check the Analysis_Results folder for all output files.\n")

# Print final summary
cat("\nFinal Summary:\n")
cat("Files processed:", length(tiff_files), "\n")
cat("Total categories found in full extent:", length(unique(results_full$category)), "\n")
cat("Total categories found in Czech Republic:", length(unique(results_cz$category)), "\n")


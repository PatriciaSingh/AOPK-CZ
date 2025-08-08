# Script Name: percentage_within_recent_occurrence_CZ.R
# Purpose: Calculate percentages of Decline vs Stable within recent occurrences 
#          (categories -2 and -1 treated as 100% baseline)
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
output_dir <- file.path(repo_path, "Percentage_Within_Recent_Occurrence")
plots_dir <- file.path(output_dir, "Plots")
data_dir <- file.path(output_dir, "Data")

# Create directories if they don't exist
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

cat("Percentage within recent occurrence analysis output will be saved to:", output_dir, "\n")

# Load Czech Republic boundary shapefile
cz_boundary <- vect(file.path(cz_folder, "CZ.shp"))  # Adjust filename if needed

# Get list of TIFF files
tiff_files <- list.files(tiff_folder, pattern = "\\.tif$", full.names = TRUE)
print(paste("Found", length(tiff_files), "TIFF files:"))
print(basename(tiff_files))

# Initialize results data frames
recent_occurrence_full <- data.frame()
recent_occurrence_cz <- data.frame()

# Define category labels for recent occurrences only
recent_labels <- c("-2" = "Decline", 
                   "-1" = "Stable")

# Function to calculate recent occurrence percentages
calculate_recent_occurrence <- function(raster_data, file_name) {
  # Get frequency table
  freq_table <- freq(raster_data)
  
  if (nrow(freq_table) > 0) {
    # Filter only recent occurrences (-2 and -1)
    recent_data <- freq_table[freq_table$value %in% c(-2, -1), ]
    
    if (nrow(recent_data) > 0) {
      # Calculate total recent occurrences
      total_recent <- sum(recent_data$count)
      
      # Calculate percentages within recent occurrences
      result <- data.frame(
        file = file_name,
        category = recent_data$value,
        count = recent_data$count,
        total_recent_occurrences = total_recent,
        percentage_of_recent = round((recent_data$count / total_recent) * 100, 2),
        stringsAsFactors = FALSE
      )
    } else {
      # No recent occurrences found
      result <- data.frame(
        file = file_name,
        category = numeric(0),
        count = numeric(0),
        total_recent_occurrences = 0,
        percentage_of_recent = numeric(0),
        stringsAsFactors = FALSE
      )
    }
  } else {
    result <- data.frame(
      file = file_name,
      category = numeric(0),
      count = numeric(0),
      total_recent_occurrences = 0,
      percentage_of_recent = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  
  return(result)
}

# Process each TIFF file
cat("\nProcessing TIFF files for recent occurrence analysis...\n")
for (i in seq_along(tiff_files)) {
  file_path <- tiff_files[i]
  file_name <- tools::file_path_sans_ext(basename(file_path))
  
  cat(paste("Processing:", file_name, "(",i,"/",length(tiff_files),")\n"))
  
  # Load raster
  raster_data <- rast(file_path)
  
  # Calculate recent occurrence percentages for full extent
  recent_full <- calculate_recent_occurrence(raster_data, file_name)
  recent_occurrence_full <- rbind(recent_occurrence_full, recent_full)
  
  # Crop to Czech Republic and calculate recent occurrence percentages
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
    
    # Calculate recent occurrence percentages for Czech Republic
    recent_cz <- calculate_recent_occurrence(raster_cz, file_name)
    recent_occurrence_cz <- rbind(recent_occurrence_cz, recent_cz)
    
  }, error = function(e) {
    cat(paste("Error processing", file_name, "for Czech Republic:", e$message, "\n"))
  })
}

# Add category labels
add_recent_labels <- function(df) {
  df$category_label <- recent_labels[as.character(df$category)]
  df$category_label[is.na(df$category_label)] <- paste("Category", df$category[is.na(df$category_label)])
  return(df)
}

# Process results
recent_occurrence_full <- add_recent_labels(recent_occurrence_full)
recent_occurrence_cz <- add_recent_labels(recent_occurrence_cz)

# Print summary statistics
cat("\n=== RECENT OCCURRENCE ANALYSIS SUMMARY ===\n")
cat("\nFull Extent - Recent Occurrences Analysis:\n")
print(recent_occurrence_full)

cat("\nCzech Republic - Recent Occurrences Analysis:\n")
print(recent_occurrence_cz)

# Create summary tables
create_summary_table <- function(df, extent_name) {
  summary_df <- df %>%
    group_by(file) %>%
    summarise(
      total_recent_pixels = first(total_recent_occurrences),
      decline_pixels = sum(count[category == -2], na.rm = TRUE),
      stable_pixels = sum(count[category == -1], na.rm = TRUE),
      decline_percentage = round(sum(count[category == -2], na.rm = TRUE) / first(total_recent_occurrences) * 100, 2),
      stable_percentage = round(sum(count[category == -1], na.rm = TRUE) / first(total_recent_occurrences) * 100, 2),
      extent = extent_name,
      .groups = "drop"
    )
  return(summary_df)
}

summary_full <- create_summary_table(recent_occurrence_full, "Full Extent")
summary_cz <- create_summary_table(recent_occurrence_cz, "Czech Republic")

# Print summaries
cat("\n=== SUMMARY TABLES ===\n")
cat("\nFull Extent Summary:\n")
print(summary_full)

cat("\nCzech Republic Summary:\n")
print(summary_cz)

# Create visualizations
cat("\nCreating visualizations...\n")

# 1. Percentage of Decline vs Stable within recent occurrences - Full Extent
p1 <- ggplot(recent_occurrence_full, aes(x = file, y = percentage_of_recent, fill = category_label)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Decline" = "#d32f2f", "Stable" = "#388e3c"), name = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Recent Occurrences: Decline vs Stable Percentages - Full Extent",
       subtitle = "Based on pixels with recent occurrence (categories -2 and -1 = 100%)",
       x = "File", y = "Percentage of Recent Occurrences") +
  ylim(0, 100)

# 2. Percentage of Decline vs Stable within recent occurrences - Czech Republic
p2 <- ggplot(recent_occurrence_cz, aes(x = file, y = percentage_of_recent, fill = category_label)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Decline" = "#d32f2f", "Stable" = "#388e3c"), name = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Recent Occurrences: Decline vs Stable Percentages - Czech Republic",
       subtitle = "Based on pixels with recent occurrence (categories -2 and -1 = 100%)",
       x = "File", y = "Percentage of Recent Occurrences") +
  ylim(0, 100)

# 3. Comparison between Full Extent and Czech Republic
comparison_recent <- bind_rows(summary_full, summary_cz)

p3 <- ggplot(comparison_recent, aes(x = file, y = decline_percentage, fill = extent)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(name = "Extent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Decline Percentage Comparison: Full Extent vs Czech Republic",
       subtitle = "Percentage of decline within recent occurrences",
       x = "File", y = "Decline Percentage") +
  ylim(0, 100)

# 4. Total recent occurrence pixels comparison
p4 <- ggplot(comparison_recent, aes(x = file, y = total_recent_pixels, fill = extent)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(name = "Extent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Total Recent Occurrence Pixels: Full Extent vs Czech Republic",
       subtitle = "Total pixels with recent occurrence (categories -2 and -1)",
       x = "File", y = "Total Recent Occurrence Pixels") +
  scale_y_continuous(labels = scales::comma)

# Display plots
print(p1)
print(p2)
print(p3)
print(p4)

# Save detailed results
write.csv(recent_occurrence_full, file.path(data_dir, "recent_occurrence_analysis_full_extent.csv"), row.names = FALSE)
write.csv(recent_occurrence_cz, file.path(data_dir, "recent_occurrence_analysis_czech_republic.csv"), row.names = FALSE)

# Save summary tables
write.csv(summary_full, file.path(data_dir, "summary_recent_occurrence_full_extent.csv"), row.names = FALSE)
write.csv(summary_cz, file.path(data_dir, "summary_recent_occurrence_czech_republic.csv"), row.names = FALSE)
write.csv(comparison_recent, file.path(data_dir, "comparison_recent_occurrence_summary.csv"), row.names = FALSE)

# Save plots
ggsave(file.path(plots_dir, "recent_occurrence_percentages_full_extent.png"), p1, width = 12, height = 8, dpi = 300)
ggsave(file.path(plots_dir, "recent_occurrence_percentages_czech_republic.png"), p2, width = 12, height = 8, dpi = 300)
ggsave(file.path(plots_dir, "decline_percentage_comparison.png"), p3, width = 12, height = 8, dpi = 300)
ggsave(file.path(plots_dir, "total_recent_pixels_comparison.png"), p4, width = 12, height = 8, dpi = 300)

cat("\n=== PERCENTAGE WITHIN RECENT OCCURRENCE ANALYSIS COMPLETE ===\n")
cat("Results saved to:\n")
cat("- Data files:", data_dir, "\n")
cat("- Plot files:", plots_dir, "\n")
cat("Check the Percentage_Within_Recent_Occurrence folder for all output files.\n")

# Print final summary
cat("\nFinal Summary:\n")
cat("Files processed:", length(tiff_files), "\n")
cat("Analysis focuses on recent occurrences (categories -2 and -1 only)\n")
cat("Results show percentages of Decline vs Stable within recent occurrences\n")
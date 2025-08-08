# Script Name: comprehensive_eunis_habitat_analysis_CZ.R
# Purpose: Complete EUNIS habitat analysis including:
#          1. Overall pixel counts and percentages (all categories)
#          2. Recent occurrence analysis (categories -2 and -1 as 100% baseline)
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

# Create comprehensive output directory structure
main_output_dir <- file.path(repo_path, "EUNIS_Habitat_Analysis")
overall_dir <- file.path(main_output_dir, "Overall_Analysis")
recent_dir <- file.path(main_output_dir, "Recent_Occurrence_Analysis")

# Create all subdirectories
for (analysis_dir in c(overall_dir, recent_dir)) {
  dir.create(file.path(analysis_dir, "Data"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(analysis_dir, "Plots"), showWarnings = FALSE, recursive = TRUE)
}

cat("Comprehensive EUNIS habitat analysis output will be saved to:", main_output_dir, "\n")
cat("- Overall analysis:", overall_dir, "\n")
cat("- Recent occurrence analysis:", recent_dir, "\n")

# Load Czech Republic boundary shapefile
cz_boundary <- vect(file.path(cz_folder, "CZ.shp"))  # Adjust filename if needed

# Get list of TIFF files
tiff_files <- list.files(tiff_folder, pattern = "\\.tif$", full.names = TRUE)
print(paste("Found", length(tiff_files), "TIFF files:"))
print(basename(tiff_files))

# Define EUNIS habitat labels based on Q codes
eunis_labels <- c("Q22" = "Poor fen",
                  "Q24" = "Intermediate fen and soft-water spring mire",
                  "Q25" = "Non-calcareous quaking mire", 
                  "Q41" = "Alkaline, calcareous, carbonate-rich small-sedge spring fen",
                  "Q42" = "Extremely rich moss–sedge fen",
                  "Q43" = "Tall-sedge base-rich fen",
                  "Q44" = "Calcareous quaking mire",
                  "Q45" = "Arctic–alpine rich fen")

# Define category labels
category_labels <- c("-2" = "Decline", 
                     "-1" = "Stable", 
                     "0" = "Absence", 
                     "1" = "Increase")

recent_labels <- c("-2" = "Decline", 
                   "-1" = "Stable")

# Function to extract Q code from filename
extract_q_code <- function(filename) {
  q_match <- regmatches(filename, regexpr("Q[0-9]{2}", filename))
  if (length(q_match) > 0) {
    return(q_match[1])
  } else {
    return("Unknown")
  }
}

# Initialize results data frames
# Overall analysis
results_full <- data.frame()
results_cz <- data.frame()

# Recent occurrence analysis
recent_occurrence_full <- data.frame()
recent_occurrence_cz <- data.frame()

# Function to calculate overall pixel counts (all categories)
calculate_overall_counts <- function(raster_data, file_name) {
  # Get frequency table
  freq_table <- freq(raster_data)
  
  # Extract Q code and get habitat name
  q_code <- extract_q_code(file_name)
  habitat_name <- eunis_labels[q_code]
  if (is.na(habitat_name)) {
    habitat_name <- paste("Unknown habitat (", q_code, ")", sep = "")
  }
  
  # Convert to data frame and add file info
  if (nrow(freq_table) > 0) {
    result <- data.frame(
      file = file_name,
      q_code = q_code,
      habitat_name = habitat_name,
      category = freq_table$value,
      count = freq_table$count,
      stringsAsFactors = FALSE
    )
  } else {
    result <- data.frame(
      file = file_name,
      q_code = q_code,
      habitat_name = habitat_name,
      category = numeric(0),
      count = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  
  return(result)
}

# Function to calculate recent occurrence percentages (only -2 and -1)
calculate_recent_occurrence <- function(raster_data, file_name) {
  # Get frequency table
  freq_table <- freq(raster_data)
  
  # Extract Q code and get habitat name
  q_code <- extract_q_code(file_name)
  habitat_name <- eunis_labels[q_code]
  if (is.na(habitat_name)) {
    habitat_name <- paste("Unknown habitat (", q_code, ")", sep = "")
  }
  
  if (nrow(freq_table) > 0) {
    # Filter only recent occurrences (-2 and -1)
    recent_data <- freq_table[freq_table$value %in% c(-2, -1), ]
    
    if (nrow(recent_data) > 0) {
      # Calculate total recent occurrences
      total_recent <- sum(recent_data$count)
      
      # Calculate percentages within recent occurrences
      result <- data.frame(
        file = file_name,
        q_code = q_code,
        habitat_name = habitat_name,
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
        q_code = q_code,
        habitat_name = habitat_name,
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
      q_code = q_code,
      habitat_name = habitat_name,
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
cat("\nProcessing TIFF files for comprehensive analysis...\n")
for (i in seq_along(tiff_files)) {
  file_path <- tiff_files[i]
  file_name <- tools::file_path_sans_ext(basename(file_path))
  
  cat(paste("Processing:", file_name, "(",i,"/",length(tiff_files),")\n"))
  
  # Load raster
  raster_data <- rast(file_path)
  
  # === OVERALL ANALYSIS ===
  # Calculate overall pixel counts for full extent
  counts_full <- calculate_overall_counts(raster_data, file_name)
  results_full <- rbind(results_full, counts_full)
  
  # === RECENT OCCURRENCE ANALYSIS ===
  # Calculate recent occurrence percentages for full extent
  recent_full <- calculate_recent_occurrence(raster_data, file_name)
  recent_occurrence_full <- rbind(recent_occurrence_full, recent_full)
  
  # === PROCESS CZECH REPUBLIC DATA ===
  # Crop to Czech Republic and calculate both analyses
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
    
    # Overall analysis for Czech Republic
    counts_cz <- calculate_overall_counts(raster_cz, file_name)
    results_cz <- rbind(results_cz, counts_cz)
    
    # Recent occurrence analysis for Czech Republic
    recent_cz <- calculate_recent_occurrence(raster_cz, file_name)
    recent_occurrence_cz <- rbind(recent_occurrence_cz, recent_cz)
    
  }, error = function(e) {
    cat(paste("Error processing", file_name, "for Czech Republic:", e$message, "\n"))
  })
}

# === PROCESS OVERALL ANALYSIS RESULTS ===
cat("\n=== PROCESSING OVERALL ANALYSIS ===\n")

# Add labels and calculate percentages for overall analysis
add_overall_labels_and_percentages <- function(df) {
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

results_full <- add_overall_labels_and_percentages(results_full)
results_cz <- add_overall_labels_and_percentages(results_cz)

# === PROCESS RECENT OCCURRENCE ANALYSIS RESULTS ===
cat("=== PROCESSING RECENT OCCURRENCE ANALYSIS ===\n")

# Add labels for recent occurrence analysis
add_recent_labels <- function(df) {
  df$category_label <- recent_labels[as.character(df$category)]
  df$category_label[is.na(df$category_label)] <- paste("Category", df$category[is.na(df$category_label)])
  return(df)
}

recent_occurrence_full <- add_recent_labels(recent_occurrence_full)
recent_occurrence_cz <- add_recent_labels(recent_occurrence_cz)

# Create summary tables for recent occurrence analysis
create_recent_summary_table <- function(df, extent_name) {
  summary_df <- df %>%
    group_by(file, q_code, habitat_name) %>%
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

summary_recent_full <- create_recent_summary_table(recent_occurrence_full, "Full Extent")
summary_recent_cz <- create_recent_summary_table(recent_occurrence_cz, "Czech Republic")

# === CREATE OVERALL ANALYSIS VISUALIZATIONS ===
cat("Creating overall analysis visualizations...\n")

# 1. Overall - Stacked bar chart for full extent
p1_overall <- ggplot(results_full, aes(x = habitat_name, y = count, fill = category_label)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(name = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(title = "Overall Analysis: Pixel Counts by Category - Full Extent",
       x = "EUNIS Habitat Type", y = "Pixel Count") +
  scale_y_continuous(labels = scales::comma)

# 2. Overall - Stacked bar chart for Czech Republic
p2_overall <- ggplot(results_cz, aes(x = habitat_name, y = count, fill = category_label)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(name = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(title = "Overall Analysis: Pixel Counts by Category - Czech Republic",
       x = "EUNIS Habitat Type", y = "Pixel Count") +
  scale_y_continuous(labels = scales::comma)

# 3. Overall - Percentage comparison
comparison_overall <- bind_rows(
  results_full %>% mutate(extent = "Full Extent"),
  results_cz %>% mutate(extent = "Czech Republic")
)

p3_overall <- ggplot(comparison_overall, aes(x = habitat_name, y = percentage, fill = category_label)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~extent, ncol = 1) +
  scale_fill_viridis_d(name = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(title = "Overall Analysis: Percentage Distribution Comparison",
       x = "EUNIS Habitat Type", y = "Percentage") +
  ylim(0, 100)

# === CREATE RECENT OCCURRENCE ANALYSIS VISUALIZATIONS ===
cat("Creating recent occurrence analysis visualizations...\n")

# 1. Recent - Percentage within recent occurrences - Full Extent
p1_recent <- ggplot(recent_occurrence_full, aes(x = habitat_name, y = percentage_of_recent, fill = category_label)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Decline" = "#d32f2f", "Stable" = "#388e3c"), name = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(title = "Recent Occurrence Analysis: Decline vs Stable Percentages - Full Extent",
       subtitle = "Based on pixels with recent occurrence (categories -2 and -1 = 100%)",
       x = "EUNIS Habitat Type", y = "Percentage of Recent Occurrences") +
  ylim(0, 100)

# 2. Recent - Percentage within recent occurrences - Czech Republic
p2_recent <- ggplot(recent_occurrence_cz, aes(x = habitat_name, y = percentage_of_recent, fill = category_label)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Decline" = "#d32f2f", "Stable" = "#388e3c"), name = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(title = "Recent Occurrence Analysis: Decline vs Stable Percentages - Czech Republic",
       subtitle = "Based on pixels with recent occurrence (categories -2 and -1 = 100%)",
       x = "EUNIS Habitat Type", y = "Percentage of Recent Occurrences") +
  ylim(0, 100)

# 3. Recent - Decline percentage comparison
comparison_recent <- bind_rows(summary_recent_full, summary_recent_cz)

p3_recent <- ggplot(comparison_recent, aes(x = habitat_name, y = decline_percentage, fill = extent)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(name = "Extent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(title = "Recent Occurrence Analysis: Decline Percentage Comparison",
       subtitle = "Percentage of decline within recent occurrences",
       x = "EUNIS Habitat Type", y = "Decline Percentage") +
  ylim(0, 100)

# 4. Recent - Total recent occurrence pixels comparison
p4_recent <- ggplot(comparison_recent, aes(x = habitat_name, y = total_recent_pixels, fill = extent)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(name = "Extent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(title = "Recent Occurrence Analysis: Total Recent Occurrence Pixels",
       subtitle = "Total pixels with recent occurrence (categories -2 and -1)",
       x = "EUNIS Habitat Type", y = "Total Recent Occurrence Pixels") +
  scale_y_continuous(labels = scales::comma)

# Display all plots
cat("Displaying plots...\n")
print(p1_overall)
print(p2_overall)
print(p3_overall)
print(p1_recent)
print(p2_recent)
print(p3_recent)
print(p4_recent)

# === SAVE ALL RESULTS ===
cat("Saving results...\n")

# Save overall analysis results
write.csv(results_full, file.path(overall_dir, "Data", "pixel_counts_full_extent.csv"), row.names = FALSE)
write.csv(results_cz, file.path(overall_dir, "Data", "pixel_counts_czech_republic.csv"), row.names = FALSE)

# Save recent occurrence analysis results
write.csv(recent_occurrence_full, file.path(recent_dir, "Data", "recent_occurrence_analysis_full_extent.csv"), row.names = FALSE)
write.csv(recent_occurrence_cz, file.path(recent_dir, "Data", "recent_occurrence_analysis_czech_republic.csv"), row.names = FALSE)
write.csv(summary_recent_full, file.path(recent_dir, "Data", "summary_recent_occurrence_full_extent.csv"), row.names = FALSE)
write.csv(summary_recent_cz, file.path(recent_dir, "Data", "summary_recent_occurrence_czech_republic.csv"), row.names = FALSE)
write.csv(comparison_recent, file.path(recent_dir, "Data", "comparison_recent_occurrence_summary.csv"), row.names = FALSE)

# Save overall analysis plots
ggsave(file.path(overall_dir, "Plots", "pixel_counts_full_extent.png"), p1_overall, width = 14, height = 8, dpi = 300)
ggsave(file.path(overall_dir, "Plots", "pixel_counts_czech_republic.png"), p2_overall, width = 14, height = 8, dpi = 300)
ggsave(file.path(overall_dir, "Plots", "percentage_comparison.png"), p3_overall, width = 14, height = 10, dpi = 300)

# Save recent occurrence analysis plots
ggsave(file.path(recent_dir, "Plots", "recent_occurrence_percentages_full_extent.png"), p1_recent, width = 14, height = 8, dpi = 300)
ggsave(file.path(recent_dir, "Plots", "recent_occurrence_percentages_czech_republic.png"), p2_recent, width = 14, height = 8, dpi = 300)
ggsave(file.path(recent_dir, "Plots", "decline_percentage_comparison.png"), p3_recent, width = 14, height = 8, dpi = 300)
ggsave(file.path(recent_dir, "Plots", "total_recent_pixels_comparison.png"), p4_recent, width = 14, height = 8, dpi = 300)

# === PRINT SUMMARY STATISTICS ===
cat("\n" + paste(rep("=", 50), collapse="") + "\n")
cat("COMPREHENSIVE EUNIS HABITAT ANALYSIS COMPLETE\n")
cat(paste(rep("=", 50), collapse="") + "\n")

cat("\nOVERALL ANALYSIS SUMMARY:\n")
cat("- Files processed:", length(tiff_files), "\n")
cat("- Total categories found in full extent:", length(unique(results_full$category)), "\n")
cat("- Total categories found in Czech Republic:", length(unique(results_cz$category)), "\n")

cat("\nRECENT OCCURRENCE ANALYSIS SUMMARY:\n")
cat("- Focuses on recent occurrences (categories -2 and -1 only)\n")
cat("- Results show percentages of Decline vs Stable within recent occurrences\n")
cat("- Total habitats with recent occurrence data (full extent):", nrow(summary_recent_full), "\n")
cat("- Total habitats with recent occurrence data (Czech Republic):", nrow(summary_recent_cz), "\n")

cat("\nRESULTS SAVED TO:\n")
cat("- Main folder:", main_output_dir, "\n")
cat("- Overall analysis:", overall_dir, "\n")
cat("- Recent occurrence analysis:", recent_dir, "\n")

cat("\nFOLDER STRUCTURE CREATED:\n")
cat("EUNIS_Habitat_Analysis/\n")
cat("├── Overall_Analysis/\n")
cat("│   ├── Data/ (CSV files)\n")
cat("│   └── Plots/ (PNG files)\n")
cat("└── Recent_Occurrence_Analysis/\n")
cat("    ├── Data/ (CSV files)\n")
cat("    └── Plots/ (PNG files)\n")

cat(paste("\n" + paste(rep("=", 50), collapse="") + "\n"))
cat("Analysis complete! Check the folders above for all results.\n")
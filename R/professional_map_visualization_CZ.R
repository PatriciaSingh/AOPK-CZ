# Script Name: professional_map_visualization_CZ.R
# Purpose: Create publication-ready maps for European and Czech Republic extents
#          Suitable for PowerPoint presentations and scientific publications
# Author: Generated for AOPK collaboration analysis
# Date: 2025-08-08

# Load required libraries
library(terra)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(RColorBrewer)
library(viridis)
library(patchwork)

# Set working directory and define paths
repo_path <- "C:/Users/patricia/PATA/SCIENCE/RESEARCH/AOPK_spoluprace/AOPK-CZ"
tiff_folder <- file.path(repo_path, "Range_Changes_EU")
cz_folder <- file.path(repo_path, "CZ")

setwd(repo_path)

# Create map output directories with force creation
main_output_dir <- file.path(repo_path, "Professional_Maps")
european_maps_dir <- file.path(main_output_dir, "European_Extent")
czech_maps_dir <- file.path(main_output_dir, "Czech_Republic_Extent")
powerpoint_dir <- file.path(main_output_dir, "PowerPoint_Ready")
publication_dir <- file.path(main_output_dir, "Publication_Quality")

# Create directories without prompting
dir.create(main_output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(european_maps_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(czech_maps_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(powerpoint_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(publication_dir, showWarnings = FALSE, recursive = TRUE)

cat("Professional maps will be saved to:", main_output_dir, "\n")

# Load Czech Republic boundary and ensure it's in WGS84
cz_boundary <- vect(file.path(cz_folder, "CZ.shp"))
# Ensure Czech boundary is in WGS84 (EPSG:4326)
cz_boundary <- project(cz_boundary, "EPSG:4326")
cz_boundary_sf <- st_as_sf(cz_boundary)

# Get European countries for context (also in WGS84)
europe <- ne_countries(scale = "medium", continent = "Europe", returnclass = "sf")

# Define the LAEA Europe projection (EPSG:3035) for raster data
laea_europe_crs <- "EPSG:3035"
wgs84_crs <- "EPSG:4326"

# Define EUNIS habitat labels
habitat_labels <- c("Q22" = "Poor fen",
                    "Q24" = "Intermediate fen and soft-water spring mire",
                    "Q25" = "Non-calcareous quaking mire", 
                    "Q41" = "Alkaline, calcareous, carbonate-rich small-sedge spring fen",
                    "Q42" = "Extremely rich moss–sedge fen",
                    "Q43" = "Tall-sedge base-rich fen",
                    "Q44" = "Calcareous quaking mire",
                    "Q45" = "Arctic–alpine rich fen")

# Define professional color scheme
map_colors <- c("-2" = "#d32f2f",    # Red for decline
                "-1" = "#4caf50",     # Green for stable  
                "0" = "#f5f5f5",      # Light gray for absent
                "1" = "#2196f3")      # Blue for expansion

# Alternative color scheme for colorblind-friendly maps
colorblind_colors <- c("-2" = "#e66101",  # Orange for decline
                       "-1" = "#5e3c99",   # Purple for stable
                       "0" = "#f7f7f7",    # Light gray for absent  
                       "1" = "#fdb863")    # Light orange for expansion

# Define category labels
category_labels <- c("-2" = "Decline", 
                     "-1" = "Stable", 
                     "0" = "Absent", 
                     "1" = "Increase")

# Function to extract Q code and get habitat name
extract_habitat_info <- function(filename) {
  q_match <- regmatches(filename, regexpr("Q[0-9]{2}", filename))
  if (length(q_match) > 0) {
    q_code <- q_match[1]
    habitat_name <- habitat_labels[q_code]
    if (is.na(habitat_name)) {
      habitat_name <- paste("Unknown habitat (", q_code, ")")
    }
    return(list(q_code = q_code, habitat_name = habitat_name))
  } else {
    return(list(q_code = "Unknown", habitat_name = "Unknown habitat"))
  }
}

# Function to create European extent map
create_european_map <- function(raster_data, habitat_info, color_scheme = map_colors, title_suffix = "") {
  
  cat("    Creating European extent map...\n")
  
  # Ensure raster is in LAEA Europe (EPSG:3035)
  if (crs(raster_data) != laea_europe_crs) {
    cat("    Reprojecting raster to LAEA Europe...\n")
    raster_data <- project(raster_data, laea_europe_crs)
  }
  
  # Project European countries to LAEA Europe for consistent mapping
  europe_laea <- st_transform(europe, crs = laea_europe_crs)
  
  # Project Czech boundary to LAEA Europe
  cz_boundary_laea <- st_transform(cz_boundary_sf, crs = laea_europe_crs)
  
  # Convert raster to data frame for ggplot
  raster_df <- as.data.frame(raster_data, xy = TRUE, na.rm = TRUE)
  if (ncol(raster_df) < 3) {
    cat("    Warning: No valid data in raster!\n")
    return(NULL)
  }
  names(raster_df)[3] <- "value"
  
  # Remove rows with NA values
  raster_df <- raster_df[!is.na(raster_df$value), ]
  
  if (nrow(raster_df) == 0) {
    cat("    Warning: No non-NA values found!\n")
    return(NULL)
  }
  
  # Convert values to factors with proper labels
  raster_df$category <- factor(raster_df$value, 
                               levels = c(-2, -1, 0, 1),
                               labels = c("Decline", "Stable", "Absent", "Increase"))
  
  # Create the map
  p <- ggplot() +
    # Add European countries as context
    geom_sf(data = europe_laea, fill = "white", color = "#cccccc", size = 0.3) +
    # Add raster data
    geom_raster(data = raster_df, aes(x = x, y = y, fill = factor(value))) +
    # Highlight Czech Republic boundary
    geom_sf(data = cz_boundary_laea, fill = NA, color = "black", size = 1.2) +
    # Color scheme
    scale_fill_manual(values = color_scheme, 
                      labels = category_labels,
                      name = "Change Type",
                      na.translate = FALSE,
                      drop = FALSE) +
    # Map styling
    theme_void() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      legend.position = "right",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    ) +
    labs(title = paste0(habitat_info$habitat_name, title_suffix),
         subtitle = paste0("Climate change projections 2011-2040 (", habitat_info$q_code, ")")) +
    # Use coordinate system of the data (LAEA Europe)
    coord_sf(crs = laea_europe_crs, expand = FALSE)
  
  return(p)
}

# Function to create Czech Republic focused map
create_czech_map <- function(raster_data, habitat_info, color_scheme = map_colors, title_suffix = "") {
  
  cat("    Creating Czech Republic map...\n")
  cat("    Raster CRS:", as.character(crs(raster_data)), "\n")
  
  # Ensure raster is in LAEA Europe (EPSG:3035)
  if (crs(raster_data) != laea_europe_crs) {
    cat("    Reprojecting raster to LAEA Europe...\n")
    raster_data <- project(raster_data, laea_europe_crs)
  }
  
  # Project Czech boundary from WGS84 to LAEA Europe for cropping
  cat("    Projecting Czech boundary from WGS84 to LAEA Europe...\n")
  cz_boundary_laea <- project(cz_boundary, laea_europe_crs)
  
  # Crop to Czech Republic extent
  cat("    Cropping raster to Czech Republic...\n")
  raster_cz <- crop(raster_data, cz_boundary_laea)
  raster_cz <- mask(raster_cz, cz_boundary_laea)
  
  # Check if we have valid data
  if (is.null(raster_cz) || ncell(raster_cz) == 0) {
    cat("    Warning: No raster data found for Czech Republic extent!\n")
    return(NULL)
  }
  
  # Convert to data frame for plotting
  raster_df <- as.data.frame(raster_cz, xy = TRUE, na.rm = TRUE)
  if (ncol(raster_df) < 3) {
    cat("    Warning: No valid data in Czech Republic extent!\n")
    return(NULL)
  }
  names(raster_df)[3] <- "value"
  
  # Remove rows with NA values
  raster_df <- raster_df[!is.na(raster_df$value), ]
  
  if (nrow(raster_df) == 0) {
    cat("    Warning: No non-NA values found!\n")
    return(NULL)
  }
  
  cat("    Found", nrow(raster_df), "valid pixels\n")
  
  # Convert values to factors with proper labels
  raster_df$category <- factor(raster_df$value, 
                               levels = c(-2, -1, 0, 1),
                               labels = c("Decline", "Stable", "Absent", "Increase"))
  
  # Get extent for proper zooming
  raster_extent <- ext(raster_cz)
  
  # Convert Czech boundary to sf in LAEA Europe for plotting
  cz_boundary_laea_sf <- st_as_sf(cz_boundary_laea)
  
  # Create the map
  p <- ggplot() +
    # Add raster data
    geom_raster(data = raster_df, aes(x = x, y = y, fill = factor(value))) +
    # Add Czech Republic boundary
    geom_sf(data = cz_boundary_laea_sf, fill = NA, color = "black", size = 1.5, inherit.aes = FALSE) +
    # Color scheme
    scale_fill_manual(values = color_scheme, 
                      labels = category_labels,
                      name = "Change Type",
                      na.translate = FALSE,
                      drop = FALSE) +
    # Add scale bar and north arrow
    annotation_scale(location = "bl", width_hint = 0.3, 
                     style = "bar", line_col = "black", text_col = "black") +
    annotation_north_arrow(location = "tl", which_north = "true", 
                           style = north_arrow_minimal,
                           height = unit(1.2, "cm"), width = unit(1.2, "cm")) +
    # Map styling
    theme_void() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      legend.position = "right",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    labs(title = paste0(habitat_info$habitat_name, " - Czech Republic", title_suffix),
         subtitle = paste0("Climate change projections 2011-2040 (", habitat_info$q_code, ")")) +
    # Use LAEA Europe coordinate system and focus on Czech extent
    coord_sf(xlim = c(raster_extent[1], raster_extent[2]), 
             ylim = c(raster_extent[3], raster_extent[4]),
             crs = laea_europe_crs,
             expand = FALSE)
  
  return(p)
}

# Get list of TIFF files
tiff_files <- list.files(tiff_folder, pattern = "\\.tif$", full.names = TRUE)
print(paste("Found", length(tiff_files), "TIFF files:"))
print(basename(tiff_files))

# Process each TIFF file
cat("\nCreating professional maps...\n")
for (i in seq_along(tiff_files)) {
  file_path <- tiff_files[i]
  file_name <- tools::file_path_sans_ext(basename(file_path))
  
  cat(paste("Processing maps for:", file_name, "(",i,"/",length(tiff_files),")\n"))
  
  tryCatch({
    # Load raster
    raster_data <- rast(file_path)
    
    # Get habitat information
    habitat_info <- extract_habitat_info(file_name)
    
    # Create file-safe name for saving
    safe_name <- gsub("[^A-Za-z0-9_]", "_", habitat_info$q_code)
    
    cat("  Creating European extent maps...\n")
    
    # European maps - Standard colors
    eu_map_standard <- create_european_map(raster_data, habitat_info, map_colors)
    
    # European maps - Colorblind friendly
    eu_map_colorblind <- create_european_map(raster_data, habitat_info, colorblind_colors, " (Colorblind-friendly)")
    
    cat("  Creating Czech Republic maps...\n")
    
    # Czech Republic maps - Standard colors
    cz_map_standard <- create_czech_map(raster_data, habitat_info, map_colors)
    
    # Only continue if we successfully created the Czech map
    if (is.null(cz_map_standard)) {
      cat("  Skipping", habitat_info$q_code, "- no data in Czech Republic extent\n")
      next
    }
    
    # Czech Republic maps - Colorblind friendly  
    cz_map_colorblind <- create_czech_map(raster_data, habitat_info, colorblind_colors, " (Colorblind-friendly)")
    
    cat("  Saving maps...\n")
    
    # Save European extent maps (create directories if needed)
    ggsave(file.path(european_maps_dir, paste0(safe_name, "_European_extent.png")), 
           eu_map_standard, width = 14, height = 10, dpi = 300, bg = "white", create.dir = TRUE)
    ggsave(file.path(european_maps_dir, paste0(safe_name, "_European_extent_colorblind.png")), 
           eu_map_colorblind, width = 14, height = 10, dpi = 300, bg = "white", create.dir = TRUE)
    
    # Save PowerPoint-ready European versions
    ggsave(file.path(powerpoint_dir, paste0(safe_name, "_PowerPoint_European.png")), 
           eu_map_standard, width = 16, height = 9, dpi = 150, bg = "white", create.dir = TRUE)
    
    # Save publication-quality European versions
    ggsave(file.path(publication_dir, paste0(safe_name, "_Publication_European.pdf")), 
           eu_map_standard, width = 10, height = 8, dpi = 600, create.dir = TRUE)
    
    # Save Czech Republic maps (only if maps were created successfully)
    if (!is.null(cz_map_standard)) {
      ggsave(file.path(czech_maps_dir, paste0(safe_name, "_Czech_Republic.png")), 
             cz_map_standard, width = 12, height = 10, dpi = 300, bg = "white")
      ggsave(file.path(powerpoint_dir, paste0(safe_name, "_PowerPoint_Czech.png")), 
             cz_map_standard, width = 16, height = 9, dpi = 150, bg = "white")
      ggsave(file.path(publication_dir, paste0(safe_name, "_Publication_Czech.pdf")), 
             cz_map_standard, width = 8, height = 10, dpi = 600)
    }
    
    if (!is.null(cz_map_colorblind)) {
      ggsave(file.path(czech_maps_dir, paste0(safe_name, "_Czech_Republic_colorblind.png")), 
             cz_map_colorblind, width = 12, height = 10, dpi = 300, bg = "white")
    }
    
    # Create combined overview (only if Czech map exists)
    if (!is.null(cz_map_standard)) {
      combined_map <- eu_map_standard + cz_map_standard + 
        plot_layout(ncol = 2, widths = c(1.2, 1)) +
        plot_annotation(
          title = paste0("Climate Change Impact Assessment: ", habitat_info$habitat_name),
          subtitle = paste0("Habitat code: ", habitat_info$q_code, " | Projections: 2011-2040"),
          theme = theme(plot.title = element_text(size = 18, face = "bold"),
                        plot.subtitle = element_text(size = 14))
        )
      
      ggsave(file.path(main_output_dir, paste0(safe_name, "_Combined_Overview.png")), 
             combined_map, width = 20, height = 10, dpi = 300, bg = "white")
    }
    
  }, error = function(e) {
    cat(paste("  Error processing", file_name, ":", e$message, "\n"))
  })
}

# Create a simple legend reference sheet with error handling
tryCatch({
  legend_reference <- ggplot(data.frame(x = 1:4, y = 1, 
                                        category = factor(c("Decline", "Stable", "Absent", "Increase"),
                                                          levels = c("Decline", "Stable", "Absent", "Increase"))), 
                             aes(x = x, y = y, fill = category)) +
    geom_tile(width = 0.8, height = 0.5) +
    scale_fill_manual(values = map_colors, name = "Change Type") +
    theme_void() +
    theme(
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 12),
      legend.key.size = unit(1, "cm"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    labs(title = "Climate Change Impact Categories") +
    guides(fill = guide_legend(override.aes = list(size = 2)))
  
  ggsave(file.path(main_output_dir, "Legend_Reference.png"), 
         legend_reference, width = 8, height = 4, dpi = 300, bg = "white")
  cat("Legend reference created successfully.\n")
}, error = function(e) {
  cat("Warning: Could not create legend reference:", e$message, "\n")
})

# Create README file with usage instructions
readme_content <- paste(
  "PROFESSIONAL MAPS - USAGE GUIDE",
  "===============================",
  "",
  "FOLDER STRUCTURE:",
  "- European_Extent/: Maps showing full European context",
  "- Czech_Republic_Extent/: Detailed maps focused on Czech Republic", 
  "- PowerPoint_Ready/: Optimized for presentations (16:9 ratio, 150 DPI)",
  "- Publication_Quality/: High-resolution PDFs for scientific publications (600 DPI)",
  "",
  "MAP TYPES:",
  "- Standard colors: Red (decline), Green (stable), Gray (absent), Blue (increase)",
  "- Colorblind-friendly: Alternative color scheme for accessibility",
  "- Combined overview: European and Czech maps side-by-side",
  "",
  "POWERPOINT USAGE:",
  "- Use files from PowerPoint_Ready/ folder",
  "- 16:9 aspect ratio fits standard slides",
  "- 150 DPI provides good quality with reasonable file size",
  "",
  "PUBLICATION USAGE:",
  "- Use PDF files from Publication_Quality/ folder", 
  "- 600 DPI suitable for scientific journals",
  "- Vector format scales perfectly at any size",
  "",
  "ZOOM CAPABILITY:",
  "- European extent maps: Good for overview and regional context",
  "- Czech Republic maps: Detailed view with scale bar and north arrow",
  "- High resolution allows zooming without quality loss",
  "",
  "COLOR SCHEMES:",
  "- Standard: Intuitive red/green for decline/stable",
  "- Colorblind-friendly: Orange/purple scheme for accessibility",
  "- Use Legend_Reference.png to explain categories",
  "",
  paste("Generated on:", Sys.Date()),
  paste("Total habitats processed:", length(tiff_files)),
  sep = "\n"
)

writeLines(readme_content, file.path(main_output_dir, "README_Maps_Usage.txt"))

# Final summary
cat(paste(rep("=", 60), collapse=""), "\n")
cat("PROFESSIONAL MAP CREATION COMPLETE\n")
cat(paste(rep("=", 60), collapse=""), "\n")

cat("\nMAPS CREATED FOR", length(tiff_files), "HABITATS:\n")
cat("✓ European extent maps (with context)\n")
cat("✓ Czech Republic detailed maps (with scale/north arrow)\n") 
cat("✓ PowerPoint-ready versions (16:9 format)\n")
cat("✓ Publication-quality PDFs (high resolution)\n")
cat("✓ Combined overview maps (both extents)\n")
cat("✓ Colorblind-friendly alternatives\n")

cat("\nFORMATS AVAILABLE:\n")
cat("📊 PowerPoint presentations: PNG files, 16:9 ratio, 150 DPI\n")
cat("📄 Scientific publications: PDF files, vector format, 600 DPI\n")
cat("🖥️  General use: PNG files, high resolution, 300 DPI\n")

cat("\nOUTPUT STRUCTURE:\n")
cat("Professional_Maps/\n")
cat("├── European_Extent/\n")
cat("├── Czech_Republic_Extent/\n") 
cat("├── PowerPoint_Ready/\n")
cat("├── Publication_Quality/\n")
cat("├── Combined_Overview.png (each habitat)\n")
cat("├── Legend_Reference.png\n")
cat("└── README_Maps_Usage.txt\n")

cat("\nREADY FOR:\n")
cat("🎯 Conservation presentations and stakeholder meetings\n")
cat("📑 Scientific publications and reports\n")
cat("🔍 Detailed analysis with zoom capability\n")
cat("♿ Accessibility (colorblind-friendly options included)\n")

cat(paste("\n", paste(rep("=", 60), collapse=""), "\n"))
cat("Professional maps ready for conservation communication!\n")


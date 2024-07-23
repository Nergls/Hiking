# 22.07.2024
# Written by Nargiz Safaraliyeva
# The Analysis of Hiking route preferences of a couple in Switzerland

#### Load (or; and install) Libraries
# install.packages(c("sf", "sp", "rgdal", "rnaturalearth", "rnaturalearthdata",
#                    "ggplot2", "leaflet","dplyr", "htmlwidgets", "webshot2",
#                     "ggmap", "rstudioapi", "plotly", "data.table", "units",
#                     "viridis", "viridisLite", "elevatr", "geosphere", "raster",
#                     "ggridges","hrbrthemes", "stringr", "patchwork"))
list.of.packages <- c("sf", "sp", "rgdal", "rnaturalearth", "rnaturalearthdata",
                      "ggplot2", "leaflet", "dplyr", "htmlwidgets", "webshot2",
                      "ggmap", "rstudioapi", "plotly", "data.table", "units",
                      "viridis", "viridisLite", "elevatr", "geosphere", "raster",
                      "ggridges","hrbrthemes", "stringr", "patchwork")
invisible(lapply(list.of.packages, library, character.only = TRUE))

#### Set Directory and define the path for outputs
setwd("C:/.../Hiking routes") #UPDATE IT 
pati_main <- "./"
# Avoid scientific notation globally
options(scipen = 999)

# Load natural earth data for countries
world <- ne_countries(scale = "medium", returnclass = "sf")
# Check the structure of the data
str(world)
# Filter for Switzerland
switzerland <- world[world$name_long == "Switzerland", ]
# Verify the filtering
print(switzerland)

# Data Cleaning or Reshaping
# Read all of the GPX files at once
gpx_file <- list.files("./", pattern = ".gpx*") 
gpx_file <- data.frame(File_Name = gpx_file)
gpx_file$Route_Name <- apply(gpx_file, 1, function(x) sub(".gpx*", "", x[1]))
# Initialize an empty list to store all GPX data frames
gpx_data_list <- list()
coords_df_list <- list()
projected_crs <- 32632

coords_df <- matrix(data=NA)

# Loop through each GPX file, read it, and assign it to a dynamically named data frame
for (g in seq(nrow(gpx_file))) { #g=1 #Debug when is needed
  
  gpx_file_DF <- st_read(paste0(pati_main, gpx_file[g,1]), layer = "tracks")
  gpx_file_DF$name <- gpx_file[g,2]
  if("name" %in% names(gpx_file_DF)){
    names(gpx_file_DF)[names(gpx_file_DF) == "name"] <- "Names"
  }
  gpx_file_DF$Longitude <- st_coordinates(st_centroid(st_union(gpx_file_DF[1,])))[1]
  gpx_file_DF$Latitude <- st_coordinates(st_centroid(st_union(gpx_file_DF[1,])))[2]
  gpx_file_DF$Centroid <- st_centroid(st_union(gpx_file_DF[1,]))
  gpx_file_DF$DistanceKM <- round(as.numeric(st_length(st_transform(gpx_file_DF, crs = projected_crs)))/1000, 1)
  coords_df <- st_coordinates(gpx_file_DF)
  coords_df <- as.data.frame(coords_df)
  coords_df[3] <- 1:nrow(coords_df)
  coords_df[4] <- 1:nrow(coords_df)
  names(coords_df) <- c("lon", "lat", "id", "order")
  coords_df$id <- paste0(coords_df$id,"_",gpx_file_DF$Names)
  coords_df$id_name <- paste0(gpx_file_DF$Names)
  
  # Convert coordinates data frame to spatial points
  coords_sf <- st_as_sf(coords_df, coords = c("lon", "lat"), crs = 4326)
  # Get elevation data using SRTM source
  elevation_data <- get_elev_raster(locations = coords_sf, z = 12)
  
  # Extract elevation values
  elevation_values <- extract(elevation_data, coords_sf)
  coords_df$elevation <- elevation_values
  
  # Calculate distances between consecutive points
  coords_df <- coords_df %>%
    mutate(distance = distHaversine(cbind(lon, lat), cbind(lag(lon, default = first(lon)), lag(lat, default = first(lat))))) %>%
    mutate(cumulative_distance = cumsum(distance))
  
  # Calculate the total elevation gain
  coords_df_elev <- coords_df %>%
    mutate(elevation_diff = elevation - lag(elevation, default = first(elevation))) %>%
    filter(elevation_diff > 0)
  total_elevation_gain <- sum(coords_df_elev$elevation_diff, na.rm = TRUE)
  coords_df$total_elevation_gain <- total_elevation_gain
  
  # Replace NA in the first distance with 0
  coords_df$cumulative_distance[is.na(coords_df$cumulative_distance)] <- 0
  
  # Calculate height difference from start to end
  start_elevation <- coords_df$elevation[1]
  #height_diff_text <- paste("Height Difference: ", round(total_elevation_gain, 2), "m", sep = "")
  
  # Create the elevation profile plot with gradient fill
  elevation_profile <-   ggplot(coords_df, aes(x = cumulative_distance / 1000, y = elevation, group = id_name, fill = id_name)) +
    geom_line(color = "black", linewidth =2, alpha = 0.7) +
    geom_area(alpha = 0.6, size = 0.5, colour = "white") +
    scale_fill_viridis(discrete = TRUE) +
    labs(title = paste0("Elevation Profile of ", gpx_file_DF$Names, " hike"),
         subtitle = paste("Total elevation gain: ", round(total_elevation_gain, 2), "m", sep = ""),  # Adding the subtitle
         x = "Distance (km)", y = "Elevation (m)") +
    theme_minimal() +
    theme(legend.position = 'none', 
          panel.spacing = unit(0.1, "lines"),
          strip.text.x = element_text(size = rel(0.9))) +
    coord_cartesian(ylim = c(min(coords_df$elevation, na.rm = TRUE), NA))  # Set y-axis limit starting from the minimum elevation
  #print(elevation_profile)
  
  # # Assign the graph to a dynamically named data frame
  # assign(paste0("elevation_profile_", g), elevation_profile)
  
  gpx_file_DF$start_elevation <- coords_df$elevation[1]
  gpx_file_DF$max_elevation <- max(coords_df$elevation)
  gpx_file_DF$total_elevation_gain <- total_elevation_gain
  
  # Append the data frame to the list
  coords_df_list[[g]] <- coords_df
  
  # Append the data frame to the list
  gpx_data_list[[g]] <- gpx_file_DF
  
}

# Combine all GPX data frames into a single sf object
all_routes <- do.call(rbind, gpx_data_list)

# Combine all elevation data frames into a single sf object
all_elevation <- do.call(rbind, coords_df_list)

# Find the maximum elevation for each unique id_name
max_elevations <- all_elevation %>%
  group_by(id_name) %>%
  summarize(max_elevation = max(elevation, na.rm = TRUE)) %>%
  arrange(max_elevation)

ggsf <- ggplot(all_routes) +
  geom_sf(data = switzerland, fill = "gray", color = "darkgray", size = 2, alpha=0.4) +
  geom_sf(aes(color = DistanceKM)) +
  scale_color_viridis_c() +
  labs(title = "Hiking Routes in Switzerland")
#print(ggsf)
#ggplotly(ggsf)


# Create the elevation profile plot with gradient fill
# Preprocess facet labels to wrap text
all_elevation$id_name <- str_wrap(all_elevation$id_name, width = 50)  # Adjust width as needed

elevation_proffi <- ggplot(all_elevation, aes(x = cumulative_distance / 1000, y = elevation, group = id_name, fill = id_name)) +
  geom_line(color = "black", linewidth =2, alpha = 0.7) +
  geom_area(alpha = 0.6, size = 0.5, colour = "white") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Elevation Profile of all hikes",
       x = "Distance (km)", y = "Elevation (m)") +
  theme_minimal() +
  theme(legend.position = 'none', 
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = rel(0.9))) +
  facet_wrap(~id_name, scales = 'free')+
  coord_cartesian(ylim = c(min(all_elevation$elevation, na.rm = TRUE), NA))  # Set y-axis limit starting from the minimum elevation #xlim = c(0,10)
#elevation_proffi
#ggplotly(elevation_proffi)


# Create the elevation profile plot with gradient fill
elevation_profffi <- ggplot(all_elevation, aes(x = cumulative_distance / 1000, y = elevation, group = id_name, fill = id_name)) +
  geom_area(alpha = 0.6, size = 0.5, colour = "white") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Elevation Profile of all hikes",
       x = "Distance (km)", y = "Elevation (m)") +
  theme_minimal() +
  theme(legend.position = 'right', 
        panel.spacing = unit(0.1, "lines")) + #strip.text.y = element_text(size = rel(0.9), face = "bold")
  facet_grid(cols = vars(id), scales = 'free') +  # Stack vertically with free y-axis scaling
  coord_cartesian(ylim = c(min(all_elevation$elevation, na.rm = TRUE), NA))  # Set y-axis limit starting from the minimum elevation
# Display the plot
#print(elevation_profffi)


# Create individual plots
plots <- all_elevation %>%
  split(.$id_name) %>%
  lapply(function(df) {
    ggplot(df, aes(x = cumulative_distance / 1000, y = elevation, group = id_name, fill = id_name)) +
      geom_area(alpha = 0.6, size = 0.5, colour = "white") +
      scale_fill_viridis(discrete = TRUE) +
      labs(title = unique(df$id_name),
           x = "Distance (km)", y = "Elevation (m)",
           subtitle = paste("Total elevation gain: ", round(unique(df$total_elevation_gain), 2), " m", sep = "")) +
      theme_minimal() +
      theme(legend.position = 'none', 
            panel.spacing = unit(0.1, "lines"),
            strip.text.x = element_text(size = rel(0.9))) +
      coord_cartesian(ylim = c(min(all_elevation$elevation, na.rm = TRUE), NA))  # Set y-axis limit starting from the minimum elevation
  })
# Combine all plots into one using patchwork
combined_plot <- wrap_plots(plots, ncol = 4)
# Display the combined plot
#print(combined_plot)


# Create the combined elevation profile plot with overlay
elevation_proffi_combined <- ggplot(all_elevation, aes(x = cumulative_distance / 1000, y = elevation, color = id_name)) +
  geom_line(size = 1) +  # Add lines on top of the area plots for better differentiation
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +  # Ensure lines have the same color scale
  labs(title = "Elevation Profile of All Hikes",
       x = "Distance (km)", y = "Elevation (m)") +
  theme_minimal() +
  theme(legend.position = 'right', 
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = rel(0.9))) +
  coord_cartesian(ylim = c(min(all_elevation$elevation, na.rm = TRUE), NA))  # Set y-axis limit starting from the minimum elevation
# Display the combined plot
#print(elevation_proffi_combined)


# END

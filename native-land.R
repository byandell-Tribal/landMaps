# Converted by https://www.codeconvert.ai/python-to-r-converter
# https://cran.r-project.org/web/packages/geojsonR/vignettes/the_geojsonR_package.html
# See https://native-land.ca/resources/api-docs/#Maps

# Install necessary libraries
#install.packages("sf")
#install.packages("ggplot2")
#install.packages("httr")
#install.packages("dplyr")

#library(sf)
#library(ggplot2)
#library(httr)
#library(dplyr)

##### Oceti Sakowin and Lakota Lands ######
# Send GET request to the API
response <- httr::GET(
  "https://native-land.ca/api/index.php?maps=languages,territories")

# The API returns a list of features
features <- httr::content(response, "parsed")

## *** For now restrict to 3 features. ***
features <- features[1:3]

## *** The following leads to an empty list object.
# Filter out features with inconsistent coordinate dimensionality
filtered_features <- list()
for (feature in features) {
  tryCatch({
    # Attempt to construct a shapely shape from the geometry
    # *** This line does not work ***.
    shape <- sf::st_as_sfc(feature$geometry)
    
    # *** This line ignores the 
    # filtered_features <- append(filtered_features, list(feature))
    filtered_features <- append(filtered_features, list(shape))
  }, error = function(e) {
    # If the shape construction fails, skip this feature
  })
}

## *** Following is an attempt to do something. Not working. ***
data_geojson <- list(type = "FeatureCollection", features = features)
data_geojson <- geojsonio::geojson_list(data_geojson)
data_gpd <- geojsonio::geojson_sf(data_geojson)

## Below are more attempts, but no success.

data_geojson <- list(type = "FeatureCollection", features = features)
class(data_geojson) <- c("sf", "data.frame")

# Convert the GeoJSON to a GeoDataFrame
data_gpd <- sf::st_as_sf(data_geojson)

data_geojson <- jsonlite::toJSON(data_geojson)

data_geojson <- geojsonsf::geojson_sf(as.character(data_geojson))

# Wrap filtered features in a list to create a valid GeoJSON
data_geojson <- list(type = "FeatureCollection", features = filtered_features)

# Convert the GeoJSON to a GeoDataFrame
ocheti_sakowin <- sf::st_as_sf(data_geojson)

# Filter for Ocheti Sakowin
oss_gdf <- ocheti_sakowin[ocheti_sakowin$Name == "Očhéthi Šakówiŋ", ]
print(oss_gdf$description)

# Convert the GeoJSON to a GeoDataFrame
lakota_lands <- sf::st_as_sf(data_geojson)

# Filter data for Lakȟótiyapi (Lakota)
lak_gdf <- lakota_lands[lakota_lands$Name == "Lakȟótiyapi (Lakota)", ]
print(lak_gdf$description)

# Calculate the bounding box
bbox <- sf::st_bbox(oss_gdf)
xmin <- bbox["xmin"]
ymin <- bbox["ymin"]
xmax <- bbox["xmax"]
ymax <- bbox["ymax"]

# Plot the data
ggplot2::ggplot() +
  ggplot2::geom_sf(data = oss_gdf, fill = "blue", alpha = 0.5,
                   ggplot2::aes(label = 'Očhéthi Šakówiŋ Territory')) +
  ggplot2::geom_sf(data = lak_gdf, fill = "green", alpha = 0.5,
                   ggplot2::aes(label = 'Lakȟótiyapi (Lakota) Territory')) +
  ggplot2::coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  ggplot2::labs(title =
    "Territories of Očhéthi Šakówiŋ and Lakȟótiyapi (Lakota)") +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::scale_fill_manual(
    values = c("Očhéthi Šakówiŋ Territory" = "cornflowerblue",
               "Lakȟótiyapi (Lakota) Territory" = "palegreen")) +
  ggplot2::guides(fill = ggplot2::guide_legend(title = "Territory"))


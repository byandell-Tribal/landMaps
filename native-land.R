# Converted by https://www.codeconvert.ai/python-to-r-converter
# https://cran.r-project.org/web/packages/geojsonR/vignettes/the_geojsonR_package.html
# See https://native-land.ca/resources/api-docs/#Maps

# See also https://tmieno2.github.io/R-as-GIS-for-Economists/simple-feature-geometry-simple-feature-geometry-list-column-and-simple-feature.html

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
featurex <- features[1:3]

#################
features_reform <- function(features) {
  out <- tibble::as_tibble(
    lapply(purrr::transpose(
      purrr::transpose(features)$properties),
      unlist))
  
  # Set up geometry coordinates
  tmpfn <- function(feature) {
    out <- lapply(feature$geometry$coordinates,
                  function(x) {
                    matrix(unlist(x),
                           ncol = 2, byrow = TRUE)
                  })
    # Split by polygon or multipolygon
    # If it fails, set to NA
    tryCatch(
      if(length(out) == 1)
        sf::st_polygon(out)
      else
        sf::st_multipolygon(list(out)),
      error = function(e) NA)
  }
  geometry <- lapply(features, tmpfn)
  
  out <- dplyr::filter(out, !is.na(geometry)) |>
    dplyr::mutate(geometry = geometry[!is.na(geometry)])
  
  sf::st_as_sf(out)
}
features <- features_reform(features)

oceti <- dplyr::filter(features, Name == "Očhéthi Šakówiŋ")
lakota <- dplyr::filter(features, grepl("Lakota", Name))

ggplot2::ggplot() +
  ggplot2::geom_sf(data = oceti, fill = "blue", alpha = 0.25,
                   ggplot2::aes(label = 'Očhéthi Šakówiŋ Territory')) +
  ggplot2::geom_sf(data = lakota, fill = "green", alpha = 0.25,
                   ggplot2::aes(label = 'Lakȟótiyapi (Lakota) Territory')) +
  ggplot2::labs(title =
                  "Territories of Očhéthi Šakówiŋ and Lakȟótiyapi (Lakota)") +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::scale_fill_manual(
    values = c("Očhéthi Šakówiŋ Territory" = "cornflowerblue",
               "Lakȟótiyapi (Lakota) Territory" = "palegreen")) +
  ggplot2::guides(fill = ggplot2::guide_legend(title = "Territory"))

# Calculate the bounding box
bbox <- sf::st_bbox(oceti, lakota)

# Plot the data
ggplot2::ggplot() +
  ggplot2::geom_sf(data = oceti, fill = "blue", alpha = 0.1,
                   ggplot2::aes(label = 'Očhéthi Šakówiŋ Territory')) +
  ggplot2::geom_sf(data = lakota, fill = "green", alpha = 0.1,
                   ggplot2::aes(label = 'Lakȟótiyapi (Lakota) Territory')) +
  ggplot2::coord_sf(xlim = bbox[c("xmin","xmax")],
                    ylim = bbox[c("ymin","ymax")]) +
  ggplot2::labs(title =
    "Territories of Očhéthi Šakówiŋ and Lakȟótiyapi (Lakota)") +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::scale_fill_manual(
    values = c("Očhéthi Šakówiŋ Territory" = "cornflowerblue",
               "Lakȟótiyapi (Lakota) Territory" = "palegreen")) +
  ggplot2::guides(fill = ggplot2::guide_legend(title = "Territory"))


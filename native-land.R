# Converted by https://www.codeconvert.ai/python-to-r-converter
# https://cran.r-project.org/web/packages/geojsonR/vignettes/the_geojsonR_package.html
# See https://native-land.ca/resources/api-docs/#Maps

# See also https://tmieno2.github.io/R-as-GIS-for-Economists/simple-feature-geometry-simple-feature-geometry-list-column-and-simple-feature.html

# ***Labels are not working properly.***
# ***Search using `Slug` column (lower case, with `-` for spaces)
# ***Languages and Territories not distinguished in `response`.

# Install necessary libraries
#install.packages("sf")
#install.packages("ggplot2")
#install.packages("httr")
#install.packages("dplyr")

#library(sf)
#library(ggplot2)
#library(httr)
#library(dplyr)

#################
features_reform <- function(features) {
  out <- tibble::as_tibble(
    lapply(purrr::transpose(
      purrr::transpose(features)$properties),
      unlist))
  
  # Set up geometry coordinates
  tran2 <- function(x) {
    # Some geometry$coordinates have a 3rd element (of 0) for points.
    # Take first two elements of every list element.
    # Transpose do make column matrix
    t(sapply(x, function(y) unlist(y)[1:2]))
  }
  tran2l <- function(x) tran2(x[[1]])
  
  # Main routine
  tran_feature <- function(feature) {
    type <- feature$geometry$type
    out <- switch(type,
      Polygon = {
        # Polygon has node dimension in second list level.
        # Return as list of matrices.
        lapply(feature$geometry$coordinates, tran2)
      },
      MultiPolygon = {
        # MultiPolygon has node dimension in third level.
        # Return as list of lists of matrices.
        list(lapply(feature$geometry$coordinates, tran2l))
      })
  
    # Split by polygon or multipolygon
    # If it fails, set to NA
    tryCatch(
      switch(type,
        Polygon = sf::st_polygon(out),
        MultiPolygon = sf::st_multipolygon(out)),
      error = function(e) NA)
  }
  geometry <- lapply(features, tran_feature)
  
  # Check of is.na takes care of issues not caught.
  # All issues currently taken care of.
  out <- dplyr::filter(out, !is.na(geometry)) |>
    dplyr::mutate(geometry = geometry[!is.na(geometry)])
  
  sf::st_as_sf(out)
}
###########################

##### Oceti Sakowin and Lakota Lands ######
# Send GET request to the API
response <- httr::GET(
  "https://native-land.ca/api/index.php?maps=languages,territories")
features <- httr::content(response, "parsed")
featurex <- features_reform(features)

oceti <- dplyr::filter(featurex, Name == "Očhéthi Šakówiŋ")
lakota <- dplyr::filter(featurex, grepl("Lakota", Name))

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

#########################################################
menominee <- dplyr::filter(featurex, Name == "Menominee")
menominee_lang <- dplyr::filter(featurex, Slug == "menominee")

ggplot2::ggplot() +
  ggplot2::geom_sf(data = menominee, fill = "blue", alpha = 0.25) +
  ggplot2::geom_sf(data = menominee_lang, fill = "green", alpha = 0.25) +
  ggplot2::labs(title =
                  "Territories of Menominee and Oma͞eqnomenew-ahkew") +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")# +
#  ggplot2::scale_fill_manual(
#    values = c("Menominee Territory" = "cornflowerblue",
#               "Oma͞eqnomenew-ahke Territory" = "palegreen")) +
#  ggplot2::guides(fill = ggplot2::guide_legend(title = "Territory"))

#########################################################
# Below is testing to figure out strange structure of object
# returned from native-land API.

# Various useful summaries.
types <- sapply(features, function(x) x$geometry$type)
lens <- sapply(features, function(x) length(x$geometry$coordinates))
lists <- sapply(features, function(x) length(x$geometry$coordinates[[1]]))
dims <- sapply(features, function(x) length(x$geometry$coordinates[[1]][[1]]))
dimmin <- sapply(features, function(x)
  min(sapply(x$geometry$coordinates[[1]], length)))
dimmax <- sapply(features, function(x)
  max(sapply(x$geometry$coordinates[[1]], length)))
mdims <- sapply(features, function(x) length(x$geometry$coordinates[[1]][[1]][[1]]))
mdimmin <- sapply(features, function(x)
  min(sapply(x$geometry$coordinates[[1]][[1]], length)))
mdimmax <- sapply(features, function(x)
  max(sapply(x$geometry$coordinates[[1]][[1]], length)))

# Organized summaries in dataframe to check issues.
polys <- tibble::as_tibble(data.frame(types,lens,lists,dims,dimmin, dimmax, mdims)) |>
  dplyr::mutate(entry = dplyr::row_number()) |>
  dplyr::filter(types == "Polygon") |>
# So for Polygon, need to pick up dimension
multis <- tibble::as_tibble(data.frame(types,lens,lists,dims,mdims, mdimmin, mdimmax)) |>
  dplyr::mutate(entry = dplyr::row_number()) |>
  dplyr::filter(types != "Polygon")
table(polys$dims)
table(multis$mdims)


# Following shows that 3rd coordinate is either missing or 0, so drop.
# Find values of 3rd coordinate for Polygon
p3 <- lapply(features, function(x) {
  out <- unlist(sapply(x$geometry$coordinates[[1]], function(y) ifelse(length(y) == 3, y[3], NA)))
  z = c(na = sum(is.na(out)), ze = sum(!is.na(out) & out == 0))
  c(z, ot = length(out) - sum(z))
})
p3 <- dplyr::bind_rows(p3) |>
  dplyr::mutate(row = dplyr::row_number()) |>
  dplyr::filter(types == "Polygon" & dimmax == 3)

# Find values of 3rd coordinate for MultiPolygon
m3 <- lapply(features, function(x) {
  out <- unlist(sapply(x$geometry$coordinates[[1]][[1]], function(y) ifelse(length(y) == 3, y[3], NA)))
  z = c(na = sum(is.na(out)), ze = sum(!is.na(out) & out == 0))
  c(z, ot = length(out) - sum(z))
})
m3 <- dplyr::bind_rows(m3) |>
  dplyr::mutate(row = dplyr::row_number()) |>
  dplyr::filter(types == "MultiPolygon" & mdimmax == 3)

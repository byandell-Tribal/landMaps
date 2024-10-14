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
source("R/features_reform.R")
###########################

###########################
# The native-land.ca just reorganized their API.
# I have put my API key in folder `data` local to my computer.
nativeLandAPI <- readRDS("data/nativeLandAPI.rds")
# Get full downloads at `https://api-docs.native-land.ca/full-geojsons`
# using your own key. I have done that and put entries in my `data` folder.

territories <- features_reform(rjson::fromJSON(file = "data/territories")$features)
languages <- features_reform(rjson::fromJSON(file = "data/languages")$features)
treaties <- features_reform(rjson::fromJSON(file = "data/treaties")$features)

nativeLand <- dplyr::bind_rows(
  territory = territories,
  language  = languages,
  treaty    = treaties,
  .id = "type")
nativeLand$geometry <- NULL
nativeLand$id <- NULL
nativeLand$color <- NULL
saveRDS(nativeLand, "data/NativeLand.rds")


## Carry on from below.

nativeLand <- readRDS("data/NativeLand.rds")
##### Oceti Sakowin and Lakota Lands ######
oceti <- dplyr::filter(territories, grepl("oceti-sakowin", Slug))
lakota <- dplyr::filter(languages, grepl("lakota", Slug))

grep("oceti", territories$Slug)
grep("oceti-sakowin", territories$Slug)
grep("oceti-sakowin-sioux", territories$Slug)
grep("sioux", territories$Slug)

slug <- territories$Slug[grep("oceti", territories$Slug)]
slug <- dplyr::filter(nativeLand,
                      grepl("oceti", .data$Slug),
                      .data$type == "territory")$Slug

#Direct request. Challenge is you need to know the Slug exactly
response <- features_reform(httr::content(httr::GET( 
  paste0("https://native-land.ca/api/index.php?maps=territories",
         "&name=", slug, # This is the Slug
         "&key=", nativeLandAPI)), "parsed"))

ggplot2::ggplot() +
  ggplot2::geom_sf(data = oceti, fill = "blue", alpha = 0.25,
                   ggplot2::aes(label = paste(.data$Name, 'Territory'))) +
  ggplot2::geom_sf(data = lakota, fill = "green", alpha = 0.25,
                   ggplot2::aes(label = paste(.data$Name, 'Language'))) +
  ggplot2::labs(title =
    paste(oceti$Name, "Territory and", lakota$Name, "Language")) +
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
# Menominee

menominee <- dplyr::filter(treaties, Name == "Menominee")
menominee_lang <- dplyr::filter(languages, Slug == "menominee")

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
# The latest API removes the spurious 3rd dimension
# but still has (multi)polygons as list of list rather than matrix.

features <- rjson::fromJSON(file = "data/territories")$features

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
  dplyr::filter(types == "Polygon")
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

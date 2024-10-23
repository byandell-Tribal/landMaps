# landMaps

Read land maps as simple features and display in various ways.
This includes

- Native maps for [native-land.ca](https://native-land.ca)
- [US Census TIGER](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html)
maps for states, counties, and 
[American Indian/Alaska Native/Native Hawaiian (AIANNH)](https://www.aiannhcaucus.com/) areas
- [OpenStreetMap (OSM)](https://www.openstreetmap.org/)

It uses the
[Native Land API](https://api-docs.native-land.ca/)
and the
[tidycensus](https://walker-data.com/tidycensus/) package,
as well as ideas from
[First Map Template](https://github.com/byandell-Tribal/first-map-template/).
Static maps are plotted using `ggplot2` with `sf` bridging
with `geom_sf` for polygon and point layers.
Instances include static maps in Rmarkdown and Shiny documents.

OSM data will be accessed via `osmdata`, with the OSM base map layer is added with 
`annotation_map_tile` from `ggspatial` package.
Interactive maps can be produces with package `tmap`.
Coming are interactive and embedded maps.

To install this package:

```
devtools::install_github("byandell-Tribal/landMaps")
```

The
[native-land.ca](https://native-land.ca/resources/api-docs/)
just reorganized their API, and now requires registration,
and subsequent use of a key.
There is also a
[US Census Key](https://walker-data.com/tidycensus/reference/census_api_key.html)
that can improve performance.
However, I was not able to obtain a key yet.

This package includes some shiny apps that can be used to
visualize selected territories, languages and treaties.

```
landMaps::nativeLandApp()
landMaps::censusApp()
```

You will need to paste in your `native-land` key to use,
as this repo does not include any data from
[native-land.ca](https://native-land.ca/resources/api-docs/).

For more details, see

- [nativeLand.Rmd)](https://github.com/byandell-Tribal/landMaps/blob/main/nativeLand.Rmd)
- [tidycensus.Rmd)](https://github.com/byandell-Tribal/landMaps/blob/main/tidycensus.Rmd)

for some ideas on use of keys with this package,
and for package function examples.
For instance, the `nativeLand` code constructs a `slug` data frame
that is used for name lookup.
The table `nativeDataSlug` expected to be found in
`data/nativeDataSlug.rds` for the `nativeLandApp()` shiny app.

## Issues

The `ggplot_layer_name` uses `ggrepel::geom_text_repel`,
which generates a warning

```
Warning: st_point_on_surface may not give correct results
for longitude/latitude data
```

There is a StackOverflow entry on 
[geom_sf mapping points/shapes](https://stackoverflow.com/questions/58676661/geom-sf-mapping-points-shapes)
and a
[discussion and pull request](https://github.com/tidyverse/ggplot2/pull/2761)
of a fix with `ggplot2::geom_sf_text`
but the warning seems to persist.

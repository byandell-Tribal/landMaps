# landMaps

Read land maps as simple features and display in various ways.
This repo includes an R package `landMaps` that was inspired by the
python-based
[Earth Data Analytics](https://github.com/byandell-envsys/EarthDataAnalytics)
course from
[Environmental Data Science Innovation & Inclusion Lab (ESIIL)](https://esiil.org/)
and
[Earth Lab](https://earthlab.colorado.edu/),
and my meetings with students and faculty of the
[Oglala Lakota College Department of Math, Science and Technology](https://www.olc.edu/current-students/degree-programs-areas-of-study/math-science-tech/).

The goal is to make land mapping using R for a variety of purposes
accessible and usable with minimal R experience.
This is (aspirationally) done by developing useful functions and
[modular Shiny apps](https://mastering-shiny.org/scaling-modules.html)
that encapsulate multiple technical steps,
which can be combined together to overlay different types of maps.
My intent at this point is not to provide a finished package and repo,
but to offer templates and suggestions that might inspire others to carry this
work forward.

This repo includes (or plans to include)

- Native maps for territories, languages and treaties from
[native-land.ca](https://native-land.ca)
- [US Census TIGER](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html)
maps for states, counties, and 
[American Indian/Alaska Native/Native Hawaiian (AIANNH)](https://www.aiannhcaucus.com/) areas
- [OpenStreetMap (OSM)](https://www.openstreetmap.org/)
- Crowd-sourced migration maps using the
[Global Biodiversity Information Facility (GBIF)](https://www.gbif.org/).
- Historical maps of redlining from
[Mapping Inequality](https://dsl.richmond.edu/panorama/redlining/).
- [NASA Earth Observation Data](https://www.earthdata.nasa.gov/), including
multispectral imaging.

## Use of this Repo and Package

To install this package:

```
devtools::install_github("byandell-envsys/landMaps")
```

It uses the
[Native Land API](https://api-docs.native-land.ca/)
and the
[tidycensus](https://walker-data.com/tidycensus/) package,
as well as ideas from
[First Map Template](https://github.com/byandell-envsys/first-map-template/)
and other repos you can find via my repo
[Earth Data Analytics](https://github.com/byandell-envsys/EarthDataAnalytics).
Static maps are plotted using `ggplot2` with `sf` bridging
with `geom_sf` for polygon and point layers.
Other packages will be pulled in as needed.
Instances include static maps in Rmarkdown and Shiny documents.

OSM data will be accessed via `osmdata`, with the OSM base map layer is added
with `annotation_map_tile` from `ggspatial` package.
Interactive maps can be produces with package `tmap`.
Coming are interactive and embedded maps.
See prototypes in
[first-map.Rmd](https://github.com/byandell-envsys/first-map-template/blob/main/first-map.Rmd).

## Native Land and Census Access

The
[native-land.ca](https://native-land.ca/resources/api-docs/)
just reorganized their API, and now requires registration,
and subsequent use of a key.
There is also a
[US Census Key](https://walker-data.com/tidycensus/reference/census_api_key.html)
that can improve performance.
However, I was not able to obtain a key yet.

This package includes some shiny apps that can be used to
visualize selected Native Land territories, languages and treaties,
and US Census AIANNH sites, states and counties.
The last app `landMapsApp()` combines the other two, with additional
table and thematic maps options.

```
landMaps::nativeLandApp()
landMaps::censusApp()
landMaps::landMapsApp()
```

Alternatively, there is a `shinyApp` folder in `inst` enabling one to do

```
runApp('inst/shinyApp')
```

However, one first has to create a `data` folder in that directory with
the needed files or (at Terminal) do the following once:

```
cd inst/shinyApp
ln -s ../../data .
```

## Data Setup

You will need to paste in your `native-land` key to use,
as this repo does not include any data from
[native-land.ca](https://native-land.ca/resources/api-docs/).
You will also need to construct some `data` files (stored in your local `data`
folder) to respect the sovereignty of these data.

For more details on data setup, see

- [nativeLandSetup.Rmd](https://github.com/byandell-envsys/landMaps/blob/main/nativeLandSetup.Rmd)
- [censusSetup.Rmd](https://github.com/byandell-envsys/landMaps/blob/main/censusSetup.Rmd)

The additional Rmarkdown files 

- [nativeLand.Rmd)](https://github.com/byandell-envsys/landMaps/blob/main/nativeLand.Rmd)
- [census.Rmd)](https://github.com/byandell-envsys/landMaps/blob/main/census.Rmd)

include some package function examples.

## Other Approaches

The 
[Earth Lab](https://earthlab.colorado.edu/)
course under ongoing developent,
[Earth Data Analytics](https://github.com/earthlab-education/Earth-Analytics-AY24),
is an excellent guide to using python to access land maps.
It will give one a detailed experience of the technical ins and outs of
building useful maps.
Its model is to teach one how to write in python and how to craft readable
and shareable renditions of maps that are relevant to one's interests.

There are other, more high-level, approaches available, including

- [Story Maps](https://storymaps.com/) and 
[ArcGIS Story Maps](https://storymaps.arcgis.com/)
- [Social Pinpoint community engagement platform](https://www.socialpinpoint.com/)
  - [Engage DeForest](https://www.engagedeforest.com/)
- [GIS Software Applications](https://gisgeography.com/best-gis-software/)

The challenge comes in whether one's approach is sufficiently accessible
and 
[extensible](https://en.wikipedia.org/wiki/Extensibility)
to be relevant for needs as they evolve over time.
That is, will a Story Map give one the ability to overlay maps and provide
external links to other resources?
The ideal is to have a set of tools that enable a diverse, multifaceted team
to quickly pull together desired maps and overlays, and other resources,
during a 2-hour meeting to make progress.
The pitfall to avoid is spending weeks or months (or years!) to develop a
specialized tool that is only used by a few individuals.

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

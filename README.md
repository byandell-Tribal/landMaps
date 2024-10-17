# landMaps

Read land maps as simple features and display in various ways.
This include Native maps for
[native-land.ca](https://native-land.ca)
as well as 
[US Census TIGER](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html).
It uses the
[Native Land API]
and the
[tidycensus](https://walker-data.com/tidycensus/) package.
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

This package includes some shiny apps that can be used to
visualize selected territories, languages and treaties.

```
landMaps::nativeLandApp()
```

You will need to paste in your `native-land` key to use,
as this app and this package do not include any data
from
[native-land.ca](https://native-land.ca/resources/api-docs/).

For more details, see
[nativeLand.Rmd)](https://github.com/byandell-Tribal/landMaps/blob/main/nativeLand.Rmd)
for some ideas on use of such a key with this package,
and for package function examples.
For instance, the code constructs a `slug` data frame
that can be used for name lookup.

## Issues

The `ggplot_layer_name` uses `ggrepel::geom_text_repel`,
which generates a warning

```
Warning: st_point_on_surface may not give correct results
for longitude/latitude data
```

There is some 
[discussion and pull request](https://github.com/tidyverse/ggplot2/pull/2761)
of a fix with `ggplot2::geom_sf_text`
but the warning seems to persist.

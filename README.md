# nativeLand

Package build around API for
[native-land.ca](https://native-land.ca).
To install this (small) package:

```
devtools::install_github("byandell-Tribal/nativeLand")
```

The
[native-land.ca](https://native-land.ca/resources/api-docs/)
just reorganized their API, and now requires registration,
and subsequent use of a key.

This package includes a shiny app that can be used to
visualize selected territories, languages and treaties.

```
nativeLand::nativeLandApp()
```

You will need to paste in your `native-land` key to use,
as this app and this package do not include any data
from
[native-land.ca](https://native-land.ca/resources/api-docs/).

For more details, see
[nativeLand.Rmd)](https://github.com/byandell-Tribal/nativeLand/blob/main/nativeLand.Rmd)
for some ideas on use of such a key with this package,
and for package function examples.
For instance, the code constructs a `slug` data frame
that can be used for name lookup.




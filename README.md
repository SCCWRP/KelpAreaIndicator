
# KelpAreaIndicator

The goal of the KelpAreaIndicator R package is to explore the Landsat kelp area data along the west coast of the US.

## Installation

You can install the development version of KelpAreaIndicator with the `install_github` function from the `devtools` package like so:

``` r
devtools::install_github(
  "SCCWRP/KelpAreaIndicator", 
  build_vignettes = TRUE, 
  build_manual = TRUE
)
```

## Help

After installation, all functions have help pages accessible through the `?` operator.

``` r
library(KelpAreaIndicator)
?segment_landsat_data
```

To view a simple example of using the package:
``` r
vignette("KelpAreaIndicator", package = "KelpAreaIndicator")
```

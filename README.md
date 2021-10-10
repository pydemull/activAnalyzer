
# activAnalyzer <img src="inst/app/www/favicon.png" align="right" height="138.5" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

# Overview

activAnalyzer is a shinny app that was developed to analyse ActiGraph
accelerometer data (.agd files) recorded at the hip in adults. Once
analysis is completed, the app allows to export results to .csv files
and to generate a report of the measurement (.pdf file). All the
configured inputs relevant for interpreting the results are recorded in
the report. Be sure that the inputs that are configured when generating
the report correspond to the analysis that was actually performed (in
other words, avoid modifying the inputs after generating satisfactory
results). Please read the user’s guide for details about how the app
works.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("pydemull/activAnalyzer")
```

## Example

To launch the app:

``` r
library(activAnalyzer)
run_app()
```

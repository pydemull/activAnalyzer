
# activAnalyzer <img src="inst/app/www/favicon.png" align="right" height="138.5" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/pydemull/activAnalyzer/branch/master/graph/badge.svg)](https://codecov.io/gh/pydemull/activAnalyzer?branch=master)
[![R-CMD-check](https://github.com/pydemull/activAnalyzer/workflows/R-CMD-check/badge.svg)](https://github.com/pydemull/activAnalyzer/actions)
<!-- badges: end -->

# Overview

activAnalyzer is a shinny app that was developed to analyze daily
physical behavior data recorded at the hip in adults using an ActiGraph
accelerometer (.agd files). Once analysis is completed, the app allows
to export results (summarized by day and averaged over valid days) to
.csv files and to generate a report of the measurement (.pdf file). All
the configured inputs relevant for interpreting the results are recorded
in the report. Be sure that the inputs that are configured when
generating the report correspond to the analysis that was actually
performed (in other words, avoid modifying the inputs after generating
satisfactory results). Please read the [user’s
guide](https://github.com/pydemull/activAnalyzer/blob/master/inst/guide/user_guide_en.pdf)
for details about how the app works.

# Usage

You can use a stable release of the app via a [shinyapps.io
plateform](https://pydemull.shinyapps.io/activAnalyzer/). You can also
use the development version on your PC after installing [R
software](https://cran.rstudio.com/) and the
[activAnalyzer](https://github.com/pydemull/activAnalyzer) package from
[GitHub](https://github.com/):

``` r
# install.packages("devtools")
devtools::install_github("pydemull/activAnalyzer")
```

## Example

To launch the app using R:

``` r
library(activAnalyzer)
activAnalyzer::run_app()
```

## Code of Conduct

Please note that the activAnalyzer project is released with a
[Contributor Code of
Conduct](https://pydemull.github.io/activAnalyzer/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

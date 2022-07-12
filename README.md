
# activAnalyzer

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/pydemull/activAnalyzer/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pydemull/activAnalyzer?branch=master)
[![R-CMD-check](https://github.com/pydemull/activAnalyzer/workflows/R-CMD-check/badge.svg)](https://github.com/pydemull/activAnalyzer/actions)
<!-- badges: end -->

activAnalyzer is a Shiny app that has been developed to analyze daily
physical behavior data recorded at the hip in adults using an ActiGraph
accelerometer (.agd files). Once analysis is completed, the app allows
exporting results (summarized by day with means or medians of the valid
days) to .csv files and generating a report of the measurement (.pdf
file). All the configured inputs relevant for interpreting the results
are recorded in the report. Be sure that the inputs that are configured
when generating the report correspond to the analysis that was actually
performed (in other words, avoid modifying the inputs after generating
satisfactory results). In addition to a general analysis of physical
behavior, the app also allows to implement the [Daily- and Clinical
visit-PROactive Physical Activity in COPD (D-PPAC and C-PPAC)
instruments](https://erj.ersjournals.com/content/early/2015/05/28/09031936.00183014).
Please read the [user’s
guide](https://github.com/pydemull/activAnalyzer/blob/master/inst/guide/user_guide_en.pdf)
for details about how the app works.

## Usage

There are three different ways to use the activAnalyzer app:

-   [On the web via a shinyapps.io
    plateform](https://pydemull.shinyapps.io/activAnalyzer/) (stable
    version). For information, as indicated by
    [RStudio](https://www.shinyapps.io/), “*shinyapps.io is
    secure-by-design. Each Shiny application runs in its own protected
    environment and access is always SSL encrypted*”. Importantly, the
    app is hosted using a free account that allows to run apps for 25
    hours per month. Thus, the availability of the app on the web is
    very dependent on the number of users as well as the time spent by
    each user on the app. Moreover, as computations when using the app
    can be quite intensive, it is possible that speed and stability of
    this online version of the app become sometimes compromised. For
    these reasons, this option should be considered as a way to have a
    quick look at how the app works. The other available options (please
    see below) will be more appropriate for working with the app on a
    regular basis. Of note, Google Chrome and Microsoft Edge browsers
    allow the app to work as expected but Mozilla Firefox does not seem
    to allow resetting all the inputs when required.
-   [On your machine via a standalone desktop
    app](https://sourceforge.net/projects/activanalyzer) that is
    downloadable from the SourceForge website (stable version, for
    Windows machines only). The standalone app has been developed using
    the framework *DesktopDeployR* made available by [W. Lee
    Pang](https://github.com/wleepang). Explanations related to this
    framework can be retrieved from [a dedicated GitHub
    repository](https://github.com/wleepang/DesktopDeployR). Once the
    app is installed on your PC, you will have to double-click on the
    desktop app icon (if you chose this option during the installation
    process), which will run the R-portable version embedded in the app
    and then will launch the app in your default web browser with
    127.0.0.1 as the value for the host parameter. This means that only
    your current machine will can access the app. You will can open only
    one session at a time. As written above, Google Chrome and Microsoft
    Edge browsers allow the app to work as expected but Mozilla Firefox
    does not seem to allow resetting all the inputs when required.
-   On your machine via [R software](https://CRAN.R-project.org/), the
    [RStudio environment](https://www.rstudio.com/), and the
    [activAnalyzer package](https://github.com/pydemull/activAnalyzer)
    installable from GitHub (development version). To be able to
    generate the .pdf reports, you will have to install the [TinyTeX
    distribution](https://yihui.org/tinytex/). In short, after
    installing R and RStudio, you can run the following command lines in
    the RStudio console:

``` r
# Code for installing the activAnalyzer package
install.packages("devtools")
devtools::install_github("pydemull/activAnalyzer")

# Code for installing the TinyTex distribution
install.packages("tinytex")
tinytex::install_tinytex()
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

## Resubmission
This is a resubmission. In this version I have:

* Moved https://codecov.io/gh/pydemull/activAnalyzer?branch=master to https://app.codecov.io/gh/pydemull/activAnalyzer?branch=master in the README.md file.

* Moved https://pydemull.github.io/activAnalyzer to https://pydemull.github.io/activAnalyzer/ in the DESCRIPTION file.

* Moved https://cran.rstudio.com/ to https://CRAN.R-project.org/ in the README.md file.

* Added single quotes to the term 'ActiGraph' in the Description field.

* Modified the plot_data_with_intensity.R file so that the example is not run because it randomly generated a NOTE causing CRAN pre-tests failure during package resubmission.



## R CMD check results
There were no ERRORs or WARNINGs.

There were 2 NOTEs:

* checking package dependencies ... NOTE
  Imports includes 24 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.

  Unfortunately I cannot use less packages. This number is related to the fact 
  that in addition to packages used for building functions, several packages are 
  here to propose a good user experience when using the package (that is a Shiny
  app).
  
* checking dependencies in R code ... NOTE
  Dépendance d'espace de nom dans le champ Imports non importé depuis : 'patchwork'
    All declared Imports should be used.
    
  Actually 'patchwork' is used, but only when the user of the app clicks on a button
  to generate a report of their measurement. More precisely, 'patchwork' package
  is only used in an .Rmd file that is called to build a figure when generating 
  the report. Thus, it is not called in any function of the package, but it is
  necessary if the user want to be able to generate the final report.

## Downstream dependencies
There are currently no downstream dependencies for this package.

## Resubmission 3 (current)
This is a resubmission. In this version I have:

* Added single quotes to the term 'Shiny' in the Title section of the DESCRIPTION file.
* Added single quotes to the term 'R' in the Description section of the DESCRIPTION file.
* Added an explanation of the acronym 'COPD' in the Description section of the DESCRIPTION file.
* Updated the README file to define the term 'COPD'.


## Resubmission 2
This is a resubmission. In this version I have:

* Updated the content of the 'Description' field in the DESCRIPTION file. I have 
changed the first terms of the description and added the mention to the author of the
'actigraph.sleepr' package, that is not imported with the app (only a few functions
from this package have been manually added into the structure of the present app).

* Indicated in the run_app.R file that the function does not return an object and 
that it is used for its side effects.

* Indicated in the tbl_agd.R file that the function returns a tibble with attributes
related to the measurement settings. I have also removed from this file the 
add_magnitude() function that is not used in the package.

* Replaced \dontrun{} by \donttest{} in the plot_data_with_intensity.R file (the single
problem with this function is that it is too long to be tested when submitting
on CRAN).

* Ensured that package functions, examples, tests, and vignettes do not modify the
Global Environement. Initially, functions that modified the Global Environment were
located in files created for testing. My apologies, I thought the CRAN policy regarding
the modification of the Global Environment concerned only the functions of the
package placed in the R/ repository. I did that because during testing, for unknown reasons,
shinytest::ShinyDriver$new() failed to locate global variables in relevant environments, 
and defining variables in the Global Environment was the solution I had found.
For this resubmission, I have solved the problem by attaching a new environment (containing
all the variables having to be seen as 'global') when the concerned test_that() 
function in called. This new environment, called 'test_activAnalyzer_env' is detached 
at the end of the test_that() function (cf. tests/testthat/test-golem-recommended.R file). 
Thus, after testing, the workspace and the list of the environments that are reached by 
the function search() are not modified.

## Resubmission 1
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

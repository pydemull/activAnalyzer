## Resubmission
This is a resubmission. In this version I have:

* Converted the DESCRIPTION title to title case.

* Corrected my ORCID ID.

* Included more plateforms in my R-CMD-check workflow on github. Following the last 
push of my package to github, all the workflows passed with the following configuration:
          - {os: macOS-latest,   r: 'release'}
          - {os: windows-latest, r: 'release'}
          - {os: ubuntu-latest,   r: 'release'}
          - {os: macOS-latest,   r: 'devel'}
          - {os: windows-latest, r: 'devel'}
          - {os: ubuntu-latest, r: 'devel'}
It may be worth noting that the error thrown during my first submission and seen for 
the platform x86_64-pc-linux-gnu (64-bit) is similar to one I regularly encounter
(but not systematically) with the mac-OS plateform during R-CMD check on github.
This error is related to the fail of the tested plateform to find PhantomJS, which
is required for testing server actions from my app thanks to the 'shinytest' package.
Thus, I guess this is not a problem from my side, but from the side of the 
plateform being tested (sorry in advance if I'm wrong, but the behavior of 
R-CMD-check on github supports this idea).

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

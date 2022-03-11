# activAnalyzer (development version)

# activAnalyzer 0.4.0

* Updated UI appearance.
* Updated documentation: it is now more clear that metrics are computed using valid wear time only.
* Updated graphics with activity data shown by day (the order of the dates was not systematically respected).
* Added the possibility to select the days to analyze.
* Updated feedbacks to be provided when inputs are not properly set.

# activAnalyzer 0.3.0

* There is now only one information about wear time, that is, wear time related to the period of the day considered by the user (before, there was also wear time over the 24-h cycle).
* Updated the graphics of the reports (those showing hazard ratios) which now provide more appropriate shapes for HR curves in comparison with the figures from the original articles (thanks to a better configuration of the R function to fit the data).
* Added daily medians into the final results.
* Added definitions of abbreviations into the report.
* Added references on the figures of the report.
* Added daily axis1 counts per minute to the list of the computed metrics.
* Added daily vm counts per minute to the list of the computed metrics.
* Added PROactive monitor-based physical activity score to the report.
* Updated the users guides.
* Updated the title in the DESCRIPTION file.
* Updated plot functions.
* Converted blocks of code of the reports to R functions.
* Changed the algorithm to provide the PROactive scores (the previous algorithm was actually suited for the use of the D-PPAC, not for the use of the C-PPAC as expected).
* Minor changes in the ui.

# activAnalyzer 0.2.0

* Added the tinytex package to the list of the packages imported during the installation of the activAnalyzer package.
* Updated the table showing the description of the studies related to the MET equations (for the study by Sasaki et al. (2011), the number of 50 participants was replaced by the number 36, that is the number of participants that actually completed the study).
* Added the possibility to use Freedson MVPA cut-points.
* When using the Sasaki et al. and the Freedson et al. equations, the kilocalories are at present computed by multiplying MET values by weight and 1/60 to better correspond to the context of the original studies (where METs were computed by dividing VO2 by 3.5, i.e., the default BMR value in ml/min/kg; this assumes that 1 MET  = 1 kcal/kg/min). For the Santos-Lozano et al. equations, kilocalories are still computed by multiplying  MET values by BMR (in kcal/min) because in the original study, METs were obtained by dividing VO2 by measured resting metabolic rate, i.e., a personalized BMR value in ml/min/kg.
* Added the possibility to set the period of the day (e.g., from 07:00:00 to 20:00:00) during which a given duration of wear time should be obtained to validate a day.

# activAnalyzer 0.1.3

* Added pieces of advice into the guides about caution when interpreting patient's scores in relation to hazard ratios.
* Updated DESCRIPTION file to set minimum version numbers for the packages and to place appropriate packages names into Suggests section.

# activAnalyzer 0.1.2

* Provided the default value of 0 for age and weight inputs as it caused bugs during the validation process when no value (initial state of the app) were provided.

# activAnalyzer 0.1.1

* Replaced the term "metric" by "variable" or "data" in the ui and the guides of the app.

# activAnalyzer 0.1.0

* Added a new block of code to stop the app when the last user leaves the app.
* Modified the plot for visualizing data with wear time so that data related to each day of measurement be on one line.
* Added step-based metrics to the results (only in the app, not in the report).
* Added a graph to visualize time spent in the different activity intensity categories.
* Updated user's guide with step-based metrics information.

# activAnalyzer 0.0.3

* Added alert message when closing the web window.
* Finally deleted the block of code initially introduced to stop the websocket server when the app is used as a desktop application. Indeed, this code led to close the app for all users when one user left the app on the shinyapps.io plateform.

# activAnalyzer 0.0.2

* Added code to stop process when closing app when used on desktop.
* Rounded wear time result averaged over valid days.
* Rounded PA/SB ratio and MET-hr results with 2 digits into the summary table.
* Added a line for the ratio MVPA/SB into the table of the report.

# activAnalyzer 0.0.1

* Updated version number to indicate that all important functions (those giving results) have been tested and that the app has now a finalized user interface.

# activAnalyzer 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.

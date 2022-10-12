# activAnalyzer 1.0.5

* Updated the version numbers of the package dependencies.
* Replaced `as.character()` by `format()` in the `mark_wear_time()` function so that there is no more error when checking for R dev versions.
* Added the argument `verify_fa = FALSE` to `icon()` functions in the UI to remove an error message that appeared when running the app.
* Updated the README by indicating the minimum versions to install to use the app.
* Updated the `mark_intensity()` function: the intensity category numbers associated to the Nonwear, SED, LPA, and MVPA categories (that are present only in the exported marked whole dataset when using the app) were not as expected because they were obtained by converting a factor vector to a numeric vector. Now the conversion is done from a character vector to a numeric vector, which keeps the numerical order as expected. This error had no impact on the results, nor on the figures provided by the package/app. The exported marked dataset has now the corrected intensity category numbers, that is: 0 for Nonwear, 1 for SED, 2 for LPA, and 4 for MVPA.

# activAnalyzer 1.0.4

* Added a block of code into the plot_data_with_intensity() function that was erroneously removed during a previous commit. This block of code allows to show grey bands on the figure with intensity data to indicate what periods will not be considered for analysis.
* This version has been accepted on CRAN. From now on, future shinyapps.io and desktop versions will be updated after new CRAN releases to keep consistency across the different ways to use the app.

# activAnalyzer 1.0.3

* Added an alert message with box when validating intensity analysis while all inputs are not correctly defined (before the user could not quickly see
that there were some problems if any).
* Improved UI reactivity depending on size screen.
* Added auto-fill button for patient information and intensity analysis.
* Added tests related to default values configuration when clicking on corresponding buttons.

# activAnalyzer 1.0.2

* Adjusted size of alert messages on report figures when scores are out of bounds.
* Improved warning messages design in the UI interface.
* Added the possibility to automatically upload a demo file.
* Added information in the guides on the content of .agd files that can be analyzed.
* Removed inappropriate options to set the wearing position in the UI (wrist, thigh, ankle).
* Added constraints to prevent the app to shut down when downloading invalid .agd file.

# activAnalyzer 1.0.1

* Updated UI buttons colors.
* The figures of the report now indicate that figures have been modified from original versions.
* Added a Full Screen button.
* Modified the figures of the report: the figures now respect the X range of the original figures. 95% CI zones have been deleted for more simplicity.
* Removed the comment related to the MVPA/SED ratio from the reports because it was finally appropriate only for 50-79 yr people (ie, the age category assessed in the concerned study).
* Updated guides.

# activAnalyzer 1.0.0

* Allowed the possibility to work with epochs shorter than 60 s. A warning is provided when the set epoch is <10 s to indicate that figures will not be created with such epochs to save time.
* The user can now set "23:59:59" rather than "23:59:00" to set the upper limit of the period of the day to be considered for analysis.
* Increased the size of the files that the app can manage.
* Corrected bug for the message that should appear when a file with incorrect extension is uploaded.
* Added feedbacks to guide the user for the choice of the values to set to configure the desired epoch for analysis (the ratio between the desired epoch and the current epoch in the file can only be an integer).
* Replaced for loop by vectorization to detect bouts in the data file with the mark_intensity() function.
* Removed actigraph.sleepr from the DESCRIPTION file and added the read_agd.R file (from the actigraph.sleepr package) as additional file into the activAnalyzer package.
* The Reset button is now at the top of the app.
* Removed two dependencies (RColorBrewer, tinytex).
* Vector magnitude is now computed when using the prepare_dataset() function, not the mark_wear_time() function, that should be used after the prepare_dataset() function.
* Added tests for questionnaires scores computations.
* Updated guides.
* Added reports for the C-PPAC and D-PPAC questionnaires.

# activAnalyzer 0.4.1

* Updated calculations for PROactive accelerometer scores (previous calculations actually were appropriate when using the means of the days of the measurement, not the medians). Now the app allows to compute scores from both medians and means of the days of the measurement (see references in the documentation for further details).
* Updated plots algorithms so that plots are correctly displayed in the app when a few days were recorded.
* Hidden the download buttons for the reports when there is not enough valid days.

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

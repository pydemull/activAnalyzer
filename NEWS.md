# activAnalyzer 2.0.2
* Corrected figure in the vignette (a step-based metrics image was used instead of a figure related IG metrics).
* Replaced ifelse() functions by dplyr::if_else() to increase speed.
* Updated read_agd(): ticks_to_dttm function within dplyr::across() functions has be rewritten so that it fits the requirements of the dplyr::across() function as of dplyr 1.1.0.
* Updated recap_by_day() function. The separation of the timestamp column in two columns (date and time) generated NAs for rows with 00:00:00. This has been solved by formating the timestamp column before separating it.
* Launching the app in the default web brower is now the default setting of the run_app() function to prevent the problems that appear when closing the app after a use in the RStudio window.

# activAnalyzer 2.0.1

* Increased speed of the `compute_accumulation_metrics()` function.
* Added progress bar for waiting for initialization of graphics related to final results.
* Corrected a bug that appeared when the width of the intensity bin set by the user to compute intensity gradient is too large, leading to time spent in only one bin during a day and thus leading to the impossibility to compute the slope of the log-log model used to get intensity gradient.

# activAnalyzer 2.0.0

* Total kcal and PAL are now correctly computed when the user does not analyse the whole day. There was no problem when the entire day was considered. However, when the user wanted to analyse a given period of the day, BMR information used in the calculations of kcal and PAL was still related to the whole day, not the period of the day to analyse. Total kcal and PAL were thus wrong when the period of the day to be analysed was not the entire day. This has been corrected ([#28](https://github.com/pydemull/activAnalyzer/issues/28)).
* Updated arguments passed to `dplyr::left_join()` function following the release of dplyr package v1.1.0.
* The app now provides intensity distribution metrics.
* The app now provides activity accumulation metrics for sedentary behaviour and physical activity: alpha coefficient, median bout duration, usual bout duration, Gini index.
* The addition of new metrics led to a breaking change in the package: the `recap_by_day()` function now returns a list, not a dataframe.
* The buttons and tables related to PROactive instruments panels now correctly disappear when the user runs an new analysis that leads to no valid days for the accelerometer.
* Some variable names in the source code are no longer hard coded to provide more flexibility when analysing personal datasets outside the app. Default variable names remain the same. 
* The app now allows to export all results to .html reports.
* The package does not export the following function anymore: `tbl_agd`.
* Now the Y labels of the graphics showing counts and steps data include the epoch duration for better data vizualisation context.
* The function `create_fig_res_by_day()` now allows to visualise all metrics by day (activity volume, step accumulation, intensity distribution).
* Added functions to get a radar plot showing MX metrics by day of measurement and for the mean or median of the considered days.
* The app now shows all results (previously several results were shown only in the report).
* Updated the vignette.


# activAnalyzer 1.1.0
 
* Added the `intersex` and `prefer not to say` categories to provide a more inclusive classification of sex. As it seems there is no scientific study about what should be the calculation of resting and activity energy expenditures for intersex people, the values provided for Basal metabolic rate (BMR) and METs are the averages of two values: the value that would be computed for a male, and the value that would be computed for a female. For people reporting `prefer not to say`, computations for females are used by default.
* Updated in the guide the description of the computation of BMR: "If the patient considers their sex as `undefined` or chooses the `prefer not to say` option, then an equation for females is used. If the patient falls into the `intersex` category, then the average of the results for a male and for a female of the considered age is used (WARNING: At the time of writing this guide, there is no scientific data to justify any calculation for intersex people).".
* Updated in the guide the description of the computation of METs: "METs, by using the MET equation provided by the user (if the patient considers their sex as `undefined` or chooses the `prefer not to say` option, then equations including sex information, when selected, are used as if the patient were a female; when the `intersex` category is used, an average of the METs related respectively to a male and to a female is used with the equations using sex information; of note, at the time of writing this guide, there is no scientific data to justify any calculation for intersex people);".
* The `compute_bmr()` and `compute_mets()` functions now use the `dplyr::case_when()` function to determine the appropriate value of BMR and METs, respectively.
* Corrected typos ("MPVA" -->"MVPA") in the English version of the guide.
* Added a class "icon-widget" to the sidebar icons to keep control on the appropriate size of the icons.
* Now the `prepare_dataset()` function imports data using the `read_agd()` function (instead of the `PhysicalActivity::readActigraph()` function). This modification now allows to import data from the GT3X device (previously only data from GT3X+ and newer devices could be used). This was not possible before because the structure of the .agd file obtained with a GT3X device is not accepted by the `PhysicalActivity::readActigraph()` function. 
* There is no more constraints about the necessity to have inclinometer information in the .agd file. This information is not used anymore in the app.
* Added a file info message (measurement information) when loading the data file.
* Changed the `size` arguments of the internal `geom_line()`, `geom_segment()` and `geom_rect()` functions by `linewidth` arguments in relation to the v3.4.0 `{ggplot2}` update.
* Added the possibility to zoom in on the figures of the app.
* Added the possibility to provide information about potential relevant physical activity periods that would have modified physical activity level but that could not be recorded due to accelerometer removal.
* Added an alert message for the figure with steps (cf. report) when the score is beyond the upper limit of the x axis.
* Corrected a bug: the app crashed when changing age or weight inputs after a data file was loaded. This is now fixed.

# activAnalyzer 1.0.5

* Updated the version numbers of the package dependencies.
* Replaced `as.character()` by `format()` in the `mark_wear_time()` function so that there is no more error when checking for R dev versions.
* Added the argument `verify_fa = FALSE` to `icon()` functions in the UI to remove an error message that appeared when running the app.
* Updated the README by indicating the minimum versions to install to use the app.
* Updated the `mark_intensity()` function: the intensity category numbers associated to the Nonwear, SED, LPA, and MVPA categories (that are present only in the exported marked whole dataset when using the app) were not as expected because they were obtained by converting a factor vector to a numeric vector. Now the conversion is done from a character vector to a numeric vector, which keeps the numerical order as expected. This error had no impact on the results, nor on the figures provided by the package/app. The exported marked dataset has now the corrected intensity category numbers, that is: 0 for Nonwear, 1 for SED, 2 for LPA, and 3 for MVPA.

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
* Finally deleted the block of code initially introduced to stop the websocket server when the app is used as a desktop application. Indeed, this code led to close the app for all users when one user left the app on the shinyapps.io platform.

# activAnalyzer 0.0.2

* Added code to stop process when closing app when used on desktop.
* Rounded wear time result averaged over valid days.
* Rounded PA/SB ratio and MET-hr results with 2 digits into the summary table.
* Added a line for the ratio MVPA/SB into the table of the report.

# activAnalyzer 0.0.1

* Updated version number to indicate that all important functions (those giving results) have been tested and that the app has now a finalized user interface.

# activAnalyzer 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.

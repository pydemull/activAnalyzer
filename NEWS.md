# activAnalyzer 0.1.3

* Added pieces of advice into the guides about caution when interpreting patient's scores in relation to hazard ratios.
* Updated DESCRIPTION file to set minimum version numbers for the packages and to place appropriate packages names into Suggest section.

# activAnalyzer 0.1.2

* Provided the default value of 0 for age and weight inputs as it caused bugs during the validation process when no value (initial state of the app) were provided.

# activAnalyzer 0.1.1

* Replaced the term "metric" by "variable" or "data" in the ui and the guides of the app.

# activAnalyzer 0.1.0

* Added a new block of code to stop the app when the last user leaves the app.
* Modified the plot for visualizing data with wear time so that data related to each day of measurement be on one line.
* Added step-based metrics to the results (only in the app, not in the report).
* Added a graph to visualize time spent in the different activity intensity categories.
* Update users guides with step-based metrics information.

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

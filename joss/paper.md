---
title: 'activAnalyzer: An R Shiny app to analyze ActiGraph accelerometer data and to implement the use of the PROactive Physical Activity in COPD instruments'
tags:
  - physical activity
  - sedentary behaviour
  - accelerometer
  - actigraph
  - COPD
  - PROactive instruments
  - R
  - Shiny
authors:
  - name: Pierre-Yves de Müllenheim
    orcid: 0000-0001-9157-7371
    affiliation: 1
  - name: Arnaud Chambellan
    orcid: 0000-0002-7860-1880
    affiliation: 2
affiliations:
 - name: Institute of Physical Education and Sport Sciences, Les Ponts-de-Cé, France
   index: 1
 - name: Hôpital Saint Philibert, GHICL, France
   index: 2
date: 05 July 2022
bibliography: references.bib
---

# Summary
Physical activity is a behaviour related to many health outcomes [@whoWHOGuidelinesPhysical2020]. Accelerometry has become a method to be prefered to questionnaires when available to measure physical activity, at least because: (i) it allows avoiding psychosocial bias related to questionnaire use (e.g., recall bias) [@ekelundPhysicalActivityMortality2020]; (ii) it allows capturing all activities, while questionnaires capture physical activity of moderate-to-vigorous intensity only [@ekelundPhysicalActivityMortality2020]; (iii) accelerometry is a more valid method than questionnaires to estimate total energy expenditure [@hallalEnergyExpenditureCompared2013; @colbertComparativeValidityPhysical2011;@gardnerAssessmentFreelivingDaily1998]; (iv) and accelerometry is now a way to access a diversity of metrics that cannot be easily (or not at all) obtained using questionnaires [@keadleEvaluationAccelerometerderivedMetrics2017; @backesAdvancedAnalyticalMethods2022].

ActiGraph devices (ActiGraph LLC, Pensacola, FL) have been the most used accelerometers in scientific literature [@bassettAccelerometerbasedPhysicalActivity2015a; @miguelesAccelerometerDataCollection2017]. These devices, along with their software companion ActiLife (ActiGraph LLC, Pensacola, FL), allow to get movement data expressed in either *activity counts* or *G-force* units. Activity counts represent the amount of acceleration produced over a given epoch of time at the wearing position of the device. While analytic methods based on G-force data are developping, the use of activity counts to assess physical activity and sedentary behaviours remains common [@miguelesAccelerometerDataCollection2017].

While the protocol for measuring physical activity itself may be relatively simple to implement for a given individual, the way to get the final results of the assessment is not straightforward. Indeed, several steps of data analysis must be completed, with one or several choices to be made at each step. These steps and choices could be described as follows [@heilModelingPhysicalActivity2012]:

* Controlling quality of data: the choices to be made concern the threshold above wich data should be flagged as anormal, and also the algorithm to be implemented to quantify nonwear time; 
* Controlling quantity of data that can be used for characterizing physical behaviour: the choices to be made concern the minimum wear time to be obtained in a day to consider the day as valid and also concern the minimum number of valid days to be obtained to consider the whole measurement as reliable.
* Converting activity counts into physiological meaningful units: the choices to be made concern the most appropriate algorithm to characterize the nature and the intensity of the activity performed at each epoch, such as an algorithm for classifying intensity of activity as sedentary, light, moderate, or vigorous.
* Summarising data using relevant metrics: the choices to be made concern the most appropriate method to accumulate data depending on the objective of the measurement (e.g., physical activity volume, time spent in different intensities of physical activity).

Beyond these general steps, additional steps may be required to get the final results in some research or clinical frameworks that use accelerometer data. A good example is the use of the PROactive Physical Activity in chronic obstructive pulmonary disease (COPD) instruments [@dobbelsPROactiveInnovativeConceptual2014; @gimeno-santosPROactiveInstrumentsMeasure2015; @garcia-aymerichValidityResponsivenessDaily2021]. Such a framework requires to combine scores related to  answers to questionnaire items and scores related to accelerometer metrics (daily mean vector magnitude and total steps count) obtained from a week of measurement. Of note, ActiGraph devices are among the accelerometers that can be used to implement this framework in COPD patients.

In view of the interest of using accelerometry to measure physical activity, in particular using activity counts from ActiGraph accelerometers, there is a need to both train students, future healthcare providers, and clinicians, to implement this method, and also a need to have the possibility to use a simple data analysis process to favor the implementation of this method routinely.

# The need of a simple app to analyse ActiGraph accelerometer counts and to get the PROactive instruments results
Due to the large size of data files to analyse when using accelerometry and due to the relative complexity of the implementation of some algorithms (e.g., to detect nonwear time), a simple spreadsheet does not appear as a feasible tool to perfom all the data analysis process in one place. The full version of ActiLife allows all the general steps of the data analysis process described above with activity counts but the cost of this ActiLife version may prevent teaching a wide audience to implement the data analysis process and working with large teams on the data. Moreover, there is no solution in ActiLife software to fully implement the PROactive framework for COPD patients described above.

Other free ways than ActiLife software to analyse activity counts include using programming languages. R [@rcoreteamLanguageEnvironmentStatistical2022] and Python [@pythonsoftwarefoundationPython] have been programming languages commonly used by scientists to build tools aiming at fostering physical activity data analysis. In R, the 'accelerometry' and 'nhanesaccel' packages by Van Domelen and Pittard [-@vandomelenFlexibleFunctionsProcessing2014], the 'actigraph.sleepr' package by Petkova [-@petkovaActigraphSleeprDetect2021], and the 'pawacc' package by Geraci [-@geraciPawaccPhysicalActivity2017], provide several functions to perform analyses of interest with activity counts. In Python, the 'pyActigraphy' by library Hammad and Reuy [-@hammadPyActigraphy2020] is also able to handle actigraph counts among various other features. While useful for research settings, these ressources may be of a little interest for other settings where people have no programming skills, because these pieces of software do not propose a GUI to help people who do not code to use the software. Van Domelen proposed a Shiny app to analyse NHANES data (https://jhubiostatistics.shinyapps.io/process_nhanes_app/), but this application is too restricted to be usefull to assess new people or patients. Beyond the lack of a free and simple interface to analyse ActiGraph activity counts data, there is, to our knowledge, no application to allow and easy implementation of the PROactive framework that is partly based on ActiGraph activity counts analysis. That is why we have developed an application, called 'activAnalyzer'. For now, a first main interest of this application is to allow teaching large groups of students, who have no programming skills, to analyse activity counts for assessing physical activity. A second main interest is to allow an easy implementation of the PROactive framework with COPD patients, by clinicians and health care providers, when working with an ActiGraph accelerometer.

# Use of the activAnalyzer app
activAnalyzer is an app built using R programming language. The app can be used according to three different frameworks as explained elsewhere (https://pydemull.github.io/activAnalyzer/).

When the user opens the app, he/she has to deal with four sections. The first section allows the user to complete information related to the measurement setup (patient's characteristics, device position, etc.). In the second section, the user must upload an .agd data file ('.agd' being the extension of the file generated by ActiLife when the user wants to work with activity counts data). Then, the user has to configure the app to detect nonwear time. The results from this first analysis can be visualised, as shown in the figure \autoref{fig:nonwear} below.

![Example of nonwear time detection analysis.\label{fig:nonwear}](nonwear.png)

In a third section, the user has to select an equation to estimate METs (an indicator of energy expendiure) and values to define cut-points, thare the values defining physical activity intensity categories (sedentary, light, moderate, vigourous). Once the user has completed the configuration, he/she can run analysis and then see a figure showing time spent in the different categories of intensities (see figure \autoref{fig:intensity} below), tables showing the results of the measurement for each day, and tables with daily means and daily medians, respectively, showing summarised scores from valid days. 

![Example of pattern of time spent in categories of intensity of physical activity. \label{fig:intensity}](intensity.png)

Once analysis finished, the user can either generate a report of the measurement, download .csv files containing data viewed used by the app (i.e., the whole dataset, or tables with summaries of results), and go to the questionnaires related to the PROactive framework. This last part consists simply of completing the chosen questionnaire, and downloading a report once analysis is completed.


# Acknowledgements
The authors thank Florian Congnard (IFEPSA-UCO) and Bénédicte Noury-Desveaux (IFEPSA-UCO) for their valuable feedbacks.

# References

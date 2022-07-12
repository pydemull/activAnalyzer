---
title: 'activAnalyzer: An app to analyze ActiGraph accelerometer data and to implement the use of the PROactive Physical Activity in COPD instruments'
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
Physical activity is a behaviour related to many health outcomes [@whoWHOGuidelinesPhysical2020]. To measure physical activity, researchers have increasingly used accelerometry during the past two decades to limit the influence of psychosocial bias in outcomes related to questionnaires use [@baumannPitfallsAccelerometerbasedMeasurement2018] and thus to use more valid methods for getting results [@hallalEnergyExpenditureCompared2013; @colbertComparativeValidityPhysical2011;@gardnerAssessmentFreelivingDaily1998]. Accelerometry has also become a way to access a diversity of metrics that cannot be easily (or not at all) obtained using questionnaires [@keadleEvaluationAccelerometerderivedMetrics2017; @backesAdvancedAnalyticalMethods2022].

ActiGraph devices have been the most used accelerometers in the scientific literature [@bassettAccelerometerbasedPhysicalActivity2015a; @miguelesAccelerometerDataCollection2017]. These devices, along with their software companion (ActiLife), allow to get movement data expressed in either *physical activity counts* or in *G-force* units. Physical activity counts represent the amount of acceleration produced over a given epoch of time at the wearing position of the device. While analytic methods based on G-force data are developping, the use of physical activity counts to study physical activity and sedentary behaviour remains common [@miguelesAccelerometerDataCollection2017]. In people with chronic pulmonary obstructive conditions, physical activity counts related to the vector magnitude, along with steps count, have been considered by

When working accelerometer data, a recommended workflow can be the following (based on Heil et al. [-@heilModelingPhysicalActivity2012]):
* Control the quality of the data (presence of anormal data, nonwear time duration).
* Control the quantity of data that can be use for characterizing physical behaviour.
* Convert physical activity counts into physiological meaningful units.
* Summarise data using relevant metrics (e.g., time spent in moderate-to-vigorous physical activity).

This workflow can be implemented using ActiLife software (full versions) or using programming languages, as R.




Numerous threshold values of counts per minute or per second have been published to estimate time spent in various categories of physical activity intensity.

Moreover, some research groups have developed specific frameworks to assess patients with specific conditions, as th D-PPAC and C-PPAC instruments developped by the PROactive consortium group for assessing physical activity in COPD patients.


- Importance of actigraph and actilife sofware
- problems for wide dissemination and use by clinicians, students, and some researchers; existence of programming interface but not accessible for non skilled users
-

# Statement of need
several packages : ggir, other...

# A description of how this software compares to other commonly-used packages in this research area.

# A list of key references including a link to the software archive.

# Usage



# Acknowledgements

# References

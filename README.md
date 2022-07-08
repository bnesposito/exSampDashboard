exSampDashboard: Exploration Sampling Dashboard
================

This repository contains a [dashboard](https://bnesposito.shinyapps.io/exsampdashboard/) where the user can 
model and analize a dataset from an adaptive experiment. The user can upload a dataset or a test set displayed 
in the Upload data tab. There are three models available: (i) Bernoulli, (ii) Binomial - logit GLM and 
(iii) Linear model. In the current version of the app, it is not possible to include random effects on the 
model specficiation.


## Structure and folders

  - **app.R**: main script containing the dashboard's UI and Server functions.
  - **Rtemp**: main functions of the used in the dashboard. All functions in this folder are imported to the app.
  - **data**: example datsets for the user test.

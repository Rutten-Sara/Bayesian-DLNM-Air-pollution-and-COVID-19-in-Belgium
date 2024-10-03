# Bayesian distributed lag non-linear models (DLNM) to describe the association between air pollution and COVID-19 in Belgium
## About this repository
This repository contains Rcodes used to generate the results from the paper "Bayesian distributed lag non-linear models (DLNM) to describe the association between air pollution and COVID-19 in Belgium.

## Data
The raw data are not deposited due to their large size. Data can be downloaded or made available on request. Following datasets are necessary to run the code:

| Dataset | Description | Downloaded from |
| --- | --- | --- |
| COVID-19 cases | Number of COVID-19 cases in Belgium between 2020 and 2023 | https://epistat.sciensano.be/covid/ |
| Air pollution | Daily pollution concentrations in Belgium | http://ftp.irceline.be/rio4x4/gemeente/ |
| Vaccination data | Weekly cumulative number of people vaccinated in Belgium | https://epistat.sciensano.be/covid/ |
| Population size | Population size of every Belgian municipality | https://statbel.fgov.be/en |

A shape file of Belgium is available in the data folder.

## Rcode
The Rcode used in this paper is structured in several files:

| File | Description |
| --- | ---|
| 00_covid19_exploration.Rmd | Exploration of Covid-19 and covariates data |
| 01_data_preparation.Rmd | Preparation of COVID-19 and pollution data |
| 02_visualise_data.Rmd | Data visualisation |
| 03_sensitivity_analysis.Rmd | Sensitivity analysis of COVID-19 cases imputation |
| 04_crossbasis_definition.Rmd | Preparation of crossbasis variables of pollution data for INLA models |
| 05_run_models.Rmd | Running and comparing INLA models |
| 06_noint_model_lagplots.Rmd | Model output and plots |
| 07_noint_model_random_effects_plots.Rmd | Plots of random effects (spatial and temporal) |

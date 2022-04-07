#' Run the Entire Project
#'
#' This script runs the entire project and produces all figures present in the
#' Langlois, Mouquet _et al._ paper submitted to PLoS Biology.
#'
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr}
#'
#' @date 2021/06/18
#' 
#' @note
#' **WARNING:** Running this script calls all the scripts and takes days so if 
#' you want to work on one or a few scripts, you should run lines 17-42 of this 
#' script and then go to the other one.


## Clean environment ----

rm(list = ls())


## Install required dependencies ----

if (!("remotes" %in% utils::installed.packages())) 
  install.packages("remotes")

if (!("SDMTools" %in% utils::installed.packages()))
  install.packages("SDMTools", repos = "https://rforge.net", 
                   dependencies = FALSE)

remotes::install_deps(upgrade = "never")


## Load packages & functions + Setup project ----

devtools::load_all(here::here())


## Define colors ----

colors <- c('#a50026', '#d73027', '#f46d43', '#fdae61', '#fee090', '#e0f3f8',
            '#abd9e9', '#74add1', '#4575b4', '#313695')


## Run analyses ----

source(here::here("analyses", "features", "features_analysis.R"))

source(here::here("analyses", "elo", "01_group_effect.R"))

source(here::here("analyses", "elo", "02_elo_scores.R"))

source(here::here("analyses", "deep", "01_size_effect.R"))

source(here::here("analyses", "deep", "02_prediction_performance.R"))

source(here::here("analyses", "deep", "03_aggregate_species_level.R"))

source(here::here("analyses", "biodiversity", "01_phylogeny.R"))

source(here::here("analyses", "biodiversity", "01_functional_div.R"))

source(here::here("analyses", "conservation", "01_conservation_status.R"))

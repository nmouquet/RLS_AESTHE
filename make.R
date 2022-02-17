###################################################################################################
#' Run the Entire Project
#'
#' This script runs the entire project and produces all figures present in the
#' Langlois et al. & Mouquet 2021 paper.
#'
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr}
#'
#' @date 2021/06/18
##################################################################################################

# WARNING running this script calls all the scripts and takes days so if you want to work on one 
# or a few scripts, you should run lines 14-21 of this script and then go to the other one.

rm(list = ls())

if (!("here" %in% installed.packages())) install.packages("here")

source(here::here("R", "setup.R"))

colors <- c('#a50026', '#d73027', '#f46d43', '#fdae61', '#fee090', '#e0f3f8',
            '#abd9e9', '#74add1', '#4575b4', '#313695')

source(here::here("analyses", "features", "features_analysis.R"))

source(here::here("analyses", "elo", "01_group_effect.R"))

source(here::here("analyses", "elo", "02_elo_scores.R"))

source(here::here("analyses", "deep", "01_size_effect.R"))

source(here::here("analyses", "deep", "02_prediction_performance.R"))

source(here::here("analyses", "deep", "03_aggregate_species_level.R"))

source(here::here("analyses", "biodiversity", "01_phylogeny.R"))

source(here::here("analyses", "biodiversity", "01_functional_div.R"))

source(here::here("analyses", "conservation", "01_conservation_status.R"))







 
 
 

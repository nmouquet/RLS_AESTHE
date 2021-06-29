RLS_AESTHE

Research compendium to reproduce analyses and figures of the following article:

R code and data to reproduce figures and tables of main text and appendices of Langlois et al.'s &amp; Mouquet XXXX article 

General

This repository is structured as follow:

    data/: contains all data required to reproduce figures and tables
    R/: contains R functions developed for this project
    analyses/: contains folders organized theme.Each folder contains R scripts
    results/: follows the structure of analyses. Contains intermediate results and the numeric results used to produce the figures.
    

Notes

    All required packages will be installed (if necessary) and loaded.
    
    Figures and tables will be stored in figures_tables/
    
    The file results/management/02_sptable_fishery.csv contains al the information used and procuded in this study at the species level for the 2417 species concerned
    
    The following Figures and Tables can be reproduced with the script indicated in brackets (all in analyses/):
    
      Figure 1b (deep/02_prediction_performances.R)
      Figure 2
      Figure 3 (biodiversity/01_phylogeny.R must be run to have data for panel a and biodiversity/02_functional_div.R generates data for panel b and the entire figure)
      Figure 4 (biodiversity/01_phylogeny.R produces the tree)
      Figure 5 (management/01_iucn_status.R)
      
      Figure S2
      Figure S4
      Figure S5
      Figure S7  (elo/01_group_effect.R)
      Figure S8  (elo/01_group_effect.R)
      Figure S9  (elo/02_elo_scores.R)
      Figure S10 (elo/02_elo_scores.R)
      Figure S11 (deep/01_size_effect.R)
      Figure S12
      Figure S13 (deep/02_prediction_perfomrance.R)
      Figure S14 (biodiversity/01_phylogeny.R)
      Figure S15 (biodiversity/01_phylogeny.R)
      Figure S16 (biodiversity/02_functional_div.R)
      Figure S17 (management/02_fishery_importance.R)
      
      Table S1 (elo/01_group_effect.R)
      Table S2 (biodiversity/01_phylogeny.R)
      Table S4
      
      Extended Table 2 (deep/03_aggreagte_scpecies_level.R)

Usage

Clone the repository and run this command in R/RStudio:

source("make.R")

WARNING: running make.R calls all the scripts and takes days so if you want to work on one 
or a few scripts, you should run lines 14-21 of make.R and then go to the other one.

Enjoy!



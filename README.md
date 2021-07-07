# RLS_AESTHE

Research compendium to reproduce analyses and figures of the following article:

R code and data to reproduce figures and tables of main text and appendices of Langlois et al.'s &amp; Mouquet XXXX article 

## General

This repository is structured as follow:

- `data/`: contains data required to reproduce figures and tables
- `R/`: contains R functions developed for this project
- `analyses/`: contains folders organized theme.Each folder contains R scripts
- `results/`: follows the structure of analyses. Contains intermediate results and the numeric results used to produce the figures.
    
## Storage

Not all fish images are free of copyrights and thousands of images are too heavy to be stored on github. We thus provide a example sample of images in `data/example_imgaes/`. The codes calling the images will show errors but you can use the example images to run them.

The script `analyses/elo/01_group_effect.R` normally generates and use a large file: 01_first_model.RData . It takes several days to generate this file but you can instead download it from https://zenodo.org/record/5052745#.YN2DpW46_LY (see lines 257-258 of script).

## File with all results  

The file results/conservation/01_sptable_fishery.csv contains all the information used and produced in this study at the species level for the 2417 species concerned
    
## Figures and Tables

Figures and Tables will be stored in `figures_tables/`

The following Figures and Tables can be reproduced with the script indicated in brackets (all in `analyses/`):
    
- Figure 1b (`deep/02_prediction_performances.R`)
- Figure 2
- Figure 3 (`biodiversity/01_phylogeny.R` must be run to have data for panel a and `biodiversity/02_functional_div.R` generates data for panel b and the entire figure)
- Figure 4 (`biodiversity/01_phylogeny.R` produces the tree)
- Figure 5 (`conservation/01_iucn_status.R`)

- Figure S2
- Figure S4
- Figure S5
- Figure S7  (`elo/01_group_effect.R`)
- Figure S8  (`elo/01_group_effect.R`)
- Figure S9  (`elo/02_elo_scores.R`)
- Figure S10 (`elo/02_elo_scores.R`)
- Figure S11 (`deep/01_size_effect.R`)
- Figure S12
- Figure S13 (`biodiversity/01_phylogeny.R`)
- Figure S14 (`biodiversity/01_phylogeny.R`)
- Figure S15 (`biodiversity/02_functional_div.R`)
- Figure S16 (`deep/03_aggregate_species_level.R`)
- Figure S17 (`deep/03_aggregate_species_level.R`)
- Figure S18 (`biodiversity/02_functional_div.R`)
- Figure S19 (`conservation/01_conservation_status.R`)
      
- Table S1 (`elo/01_group_effect.R`)
- Table S2 (`biodiversity/01_phylogeny.R`)
- Table S4

- Extended Table 2 (`deep/03_aggregate_species_level.R`)

## Usage

Clone the repository and run this command in R/RStudio:

```r 
source("make.R")
```
All required packages will be installed (if necessary) and loaded.
> :boom: WARNING: running `make.R` calls all the scripts and takes days so if you want to work on one or a few scripts, you should run lines 14-21 of `make.R` and then go to the other script.

Enjoy!



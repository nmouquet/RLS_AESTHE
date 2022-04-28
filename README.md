# RLS_AESTHE

Research compendium to reproduce analyses and figures of the article: 
_Global mismatch between the aesthetic value of reef fishes and their conservation priorities_ 
by Langlois, Mouquet _et al._ published in PLOS Biology.


## General

This repository is structured as follow:

- `data/`: contains data required to reproduce figures and tables
- `R/`: contains R functions developed for this project
- `analyses/`: contains folders organized by theme. Each folder contains R scripts
to run specific analysis
- `results/`: follows the structure of analyses. Contains intermediate results 
and the numeric results used to produce the figures.



## Storage

Not all fish images are free of copyrights. We thus provide an example sample of 
images in `data/example_images/`. The codes calling all images will show errors 
but you can use the example images set to run them. See Extended Table 1 
(provided as a supplementary information) for copyrights. 

The script `analyses/elo/01_group_effect.R` normally generates and use a large 
file: `01_first_model.RData`. It takes several days to generate this file but 
you can instead download it from https://zenodo.org/record/6325532#.YiDG99_jLoA
(see lines 257-258 of script).



## File with all results  

The file `results/conservation/01_sptable_all.csv` contains all the information 
used and produced in this study at the species level for the 2,415 species 
concerned. You are welcome to use it by citing properly our work, but even more 
welcome to contact us (nicolas.mouquet@cnrs.fr) if you want to collaborate :smiley:



## Figures and Tables

Figures and Tables will be stored in `figures_tables/`.

The following Figures and Tables can be reproduced with the script indicated in 
brackets (all in `analyses/`):
    
- Figure 1b (`deep/06_prediction_performances.R`)
- Figure 2a (`features/features_analysis.R`)
- Figure 2b (`features/features_analysis.R`) 
- Figure 3 (`biodiversity/01_phylogeny.R` must be run to have data for panel a 
and `biodiversity/02_functional_div.R` generates data for panel b and the entire figure)
- Figure 4 (`biodiversity/01_phylogeny.R` produces the tree)
- Figure 5 (`conservation/01_iucn_status.R`)
- Figure S1 A (`features/cluster.R`)
- Figure S1 B (`features/cluster.R`) 
- Figure S1 C modified from Wikipedia source (see legend) 
- Figure S1 D  (`features/momocs.R`)
- Figure S1 E  (`features/features_analysis.R`)
- Figure S1 G  (`elo/01_group_effect.R`)
- Figure S1 H  (`elo/01_group_effect.R`)
- Figure S1 I  (`elo/02_elo_scores.R`)
- Figure S1 J (`elo/02_elo_scores.R`)
- Figure S1 K (`deep/05_size_effect.R`)
- Figure S1 M (`deep/06_prediction_performances.R`)
- Figure S1 N (`biodiversity/01_phylogeny.R`)
- Figure S1 O (`biodiversity/01_phylogeny.R`)
- Figure S1 P (`biodiversity/02_functional_div.R`)
- Figure S1 Q (`deep/03_aggregate_species_level.R`)
- Figure S1 R (`deep/03_aggregate_species_level.R`)
- Figure S1 S (`biodiversity/02_functional_div.R`)
- Figure S1 T (`conservation/01_conservation_status.R`)
      
- Table S1 A  (`elo/01_group_effect.R`)
- Table S1 B  (`biodiversity/01_phylogeny.R`)
- Table S1 D  (`biodiversity/02_functional_div.R`)

- Extended Table 2 (`deep/03_aggregate_species_level.R`)



## Usage

Clone the repository and run this command in R/RStudio:

```r
source("make.R")
```


All required packages will be installed (if necessary) and loaded.

> :boom: WARNING: running `make.R` calls all the scripts and takes days so if 
you want to work on one or a few scripts, you should run lines 17-42 of 
`make.R` and then go to the other script.

Enjoy!

#' Set up
#'
#' This script loads the packages needed for the project
#' And 
#' 

# ---- Packages ----

cran_packages <- c(
  
  "ade4", "ape", "colorspace", "colorRamps",
  
  "EBImage", "EloChoice", "devtools", "dplyr", "doParallel",
  
  "FactoMineR", "factoextra", "fisheyeR", "fishtree", "forcats", "funrar", "FSA",
  
  "ggplot2", "ggpubr", "gplots", "graphics", "gridExtra", "GGally",  "ggtree",
  
  "harrypotter", "here", "Hmisc",
  
  "imager", "jpeg", "jtools", "lme4", "lmerTest", "moments", "Momocs", "missForest",
  
  "multcompView","numbers", "parallel", "picante", "plyr", "pdp",
  
  "remotes", "relaimpo", "reshape2", "rlang", "rworldmap",
  
  "SDMTools", "splancs", "tidyr", "tidyjson", "tidytree", "treeio",
  
  "usethis", "venn", "viridis", "wesanderson", "worms", "xlsx","phylolm",'harmonicmeanp','RhpcBLASctl')

n_i_p <- cran_packages[!(cran_packages %in% installed.packages())]

lapply(n_i_p, install.packages, dependencies = TRUE)

# NON CRAN packages

if (!("taxize" %in% installed.packages())) {remotes::install_github("ropensci/taxize")}

#Check CRAN packages
if (sum(unlist(lapply(cran_packages, require, character.only = TRUE))) == length(cran_packages)) {
  
  cat("\n", ">>> All packages loaded !\n")
  
} else {
  
  cat("\n", ">>> Some packages failed to load !\n")
  
}

# ----

# ---- Create results folders ----

# create result folders following the analysis scripts
ana_folders <- list.dirs(path = here::here("analyses"), full.names = FALSE, recursive = FALSE)

#create folders & folder names variables
sapply(1:length(ana_folders), function(i) {
  
  dir.create(
    path          = here::here("results", ana_folders[i]),
    showWarnings  = FALSE,
    recursive     = TRUE
  )
  
  script_folders <- list.files(path = here::here("analyses", ana_folders[i]), full.names = FALSE, recursive = FALSE)
  script_names   <- gsub("\\.R", "", script_folders)
  script_vars    <- paste0("res_dir_", ana_folders[i])
  
  
  sapply(1:length(script_vars), function(j) {
    assign(
      x      = script_vars[j],
      value  = here::here("results", ana_folders[i]),
      envir  = .GlobalEnv
    )})
  
})

cat("\n", emo::ji("folder"), ">>> All folders created !\n")

#clean temporary variables
rm(ana_folders, cran_packages, n_i_p)

# ----
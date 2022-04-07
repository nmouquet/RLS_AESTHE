#' Setup the whole project


## List analyses scripts ----

ana_folders <- list.dirs(path = here::here("analyses"), full.names = FALSE, 
                         recursive = FALSE)


## Create results sub-folders ----

sapply(1:length(ana_folders), function(i) {
  
  dir.create(here::here("results", ana_folders[i]), showWarnings = FALSE,
             recursive = TRUE)
  
  script_folders <- list.files(here::here("analyses", ana_folders[i]), 
                               full.names = FALSE, recursive = FALSE)
  
  script_names   <- gsub("\\.R", "", script_folders)
  script_vars    <- paste0("res_dir_", ana_folders[i])
  
  sapply(1:length(script_vars), function(j) {
    assign(x      = script_vars[j],
           value  = here::here("results", ana_folders[i]),
           envir  = .GlobalEnv
  )})
})

cat(">>> All folders created !\n")


## Clean temporary variables ----

rm(ana_folders)

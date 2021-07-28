###################################################################################################
#' Functions used in the scripts of the folder analysis/elo
#' 
#'
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         François Guilhaumon, \email{francois.guilhaumon@@ird.fr},
#'         Alienor Stahl, \email{a.stahl67@@gmail.com}
#'
#' @date 2021/02/17
##################################################################################################

#' booting_elo
#' function to calculate the elo score every step of x matches 
#'
#' @param data a dataframe with four columns at least: challenger_1 and challenger_2, 
#' Winner and Loser
#' @param startvalue the value at which all challengers start to calculate the elo score
#' @param runs the number of time to bootstrap the function
#' @param stepelo the threshold at which to calculate the elo score
#'
#' @return
#' @export
#'
booting_elo <- function(data, startvalue = 1500, runs = 1, stepelo = 100) {
  
  # initialization of the dataframe to record the elo scores
  elo_match_nb           <- as.data.frame(matrix(NA,
                                                 nrow = 1,
                                                 ncol = length(unique(c(data$challenger_1,
                                                                        data$challenger_2))) + 1))
  species                <- sort(unique(c(as.character(data$challenger_1),
                                          as.character(data$challenger_2))), 
                                 decreasing = FALSE)
  colnames(elo_match_nb) <- c('match_nb', species)
  
  # Start at match 1
  elo_match_nb[1,]         <- startvalue
  elo_match_nb$match_nb[1] <- 1
  
  # checking how many matches minimum 
  minmatch <-  opti_minmatch(data)
  
  cat("min nb match:",minmatch,"\n")
  
  res                      <- EloChoice::elochoice(winner     = data$Winner[1:minmatch], 
                                                   loser      = data$Loser[1:minmatch],
                                                   startvalue = startvalue,
                                                   runs       = runs)
  elo                      <- EloChoice::ratings(res, drawplot = F)
  elo_match_nb             <- tidyjson::bind_rows(elo_match_nb, elo[sort(names(elo))])
  elo_match_nb$match_nb[2] <-  minmatch
  match_current            <-  minmatch
  
  maxmatch <-  opti_maxmatch(data, minmatch, stepelo)[1]
  extra    <-  opti_maxmatch(data, minmatch, stepelo)[2]
  
  # loop for the rest of the matchs
  pos <- 3
  
  while(match_current < maxmatch + extra) {
    
    match_current              <-  match_current + stepelo
    res                        <- EloChoice::elochoice(winner     = data$Winner[1:match_current], 
                                                       loser      = data$Loser[1:match_current], 
                                                       startvalue = startvalue, 
                                                       runs       = runs)
    elo                        <- EloChoice::ratings(res, drawplot = F)
    elo_match_nb               <- tidyjson::bind_rows(elo_match_nb, elo[sort(names(elo))])
    elo_match_nb$match_nb[pos] <- match_current
    
    cat("match:",match_current,"\n")
    pos     <-  pos + 1
    stepelo <-  ifelse(match_current < maxmatch, stepelo, extra)
  }
  return(elo_match_nb)
  
}

#' jpeg_name
#' add the name of the photo at the bottom of the photo and save in out_photo rep
#' @param path_photo where to find the original jpg photos
#' @param out_photo where to save the named photos
#' @param x x coordinate of the name on the photo
#' @param y y coordinate of the name on the photo
#' @param size size of the police  
#'
#' @return
#' @export
jpeg_name <- function(path_photo, out_photo, x, y, size){
  
  files <- dir(path = path_photo, pattern = ".jpg")
  
  for (i in 1:length(files)){
    
    #read file
    img <- jpeg::readJPEG(file.path(path_photo,files[i]))
    
    #get size
    h <- dim(img)[1]
    w <- dim(img)[2]
    
    #open new file for output
    jpeg(file.path(out_photo,files[i]), width = w, height = h)
    
    par(mar = c(0,0,0,0), xpd = NA, mgp = c(0,0,0), oma = c(0,0,0,0), ann = F)
    plot.new()
    plot.window(0:1, 0:1)
    
    #fill plot with image
    usr <- par("usr")
    rasterImage(img, usr[1], usr[3], usr[2], usr[4])
    
    #add text
    text(x, y, gsub(".jpg", "", files[i]), cex = size, col = "black")
    
    #close image
    dev.off()
  } # eo for
} # eo jpeg_name


#' look_dev
#' This function orders the variables according to their individual effect on the response variable
#' @param dat 
#' @param explvar 
#' @param respvar 
#' @param randomvar 
#' @param ord 
#' @param model_ini
#' @param proc
#'
#' @return
#' @export
#'
look_dev <- function(dat, explvar, respvar, randomvar, ord, model_ini, proc){
  
  # dat = table_elo_judge; explvar = list_var; respvar = "wins"; randomvar = "challenger_1";
  # ord = "minusvar"; model_ini = first_model ; proc = 4
  
  # Prepare the character chain for random effects
  randomform <- paste0("(1|", randomvar)
  randomform <- paste0(randomform, ")")
  randomform <- paste0(randomform, collapse = "+")
  
  # linear regression between the target variable "respvar" and 
  # all the explaining variables "all_id"
  sumall <- summary(model_ini)
  
  # get the % of deviance lost/gained when removing variable i from the model 
  # and the % of deviance lost/gained with the variable alone 
  filist <- list.files(here::here(res_dir_elo))
  
  out_data <- as.data.frame(
    do.call(
      rbind, parallel::mclapply(1:length(explvar),function(i){
        # get the % of deviance varying between the full model and the model 
        # without variable i
        if(length(grep(pattern = paste0("modminus_", explvar[i]), x = filist)) != 0){
          modminus <- readRDS(here::here(res_dir_elo, paste0("modminus_", explvar[i], ".rds")))
        }else{
          modminus <- lme4::glmer(as.formula(paste(respvar," ~ ", 
                                                   paste(explvar[-i], collapse = "+"),
                                                   "+", randomform)),
                                  family = binomial, data = dat, na.action = na.fail)
          modname  <- paste0("modminus_", explvar[i])
          assign(x = modname, value  = modminus)
          saveRDS(modminus, file = here::here(res_dir_elo, paste0(modname, ".rds")))
        } # eo if
      
        summin       <- summary(modminus)
        dev_minusvar <- round(100 - (summin$AICtab[["deviance"]] /
                                       sumall$AICtab[["deviance"]] * 100), 3) 
        # round because of floatting zeros
      
        # get the deviance of the model with var i alone explaining the response variable
        if(length(grep(pattern = paste0("modalone_", explvar[i]), x = filist)) !=0){
          modalone <- readRDS(here::here(res_dir_elo, paste0("modalone_", explvar[i], ".rds")))
        }else{
          modalone <- lme4::glmer(as.formula(paste(respvar," ~ ", explvar[i], "+", randomform)), 
                                  family = binomial, data = dat, na.action = na.fail)
          modname  <- paste0("modalone_", explvar[i])
          assign(x = modname, value  = modalone)
          saveRDS(modalone, file = here::here(res_dir_elo, paste0(modname, ".rds")))
        } # eo if
        
        sumalone     <- summary(modalone)
        dev_varalone <- round(100 - (sumalone$AICtab[["deviance"]] /
                                       sumall$AICtab[["deviance"]] * 100), 3)

    cbind(explvar[i], dev_minusvar, dev_varalone)
    
  },
  mc.cores = proc))) # eo mclapply
  
  colnames(out_data) <- c("var","dev_minusvar", "dev_varalone")
  
  # We want the deviance of the model to be as small as possible ie
  # we want the biggest lost of deviance ie decreasing order
  
    if (ord == "varalone") corres <- out_data[order(out_data$dev_varalone, decreasing = TRUE), ]
    if (ord == "minusvar") corres <- out_data[order(out_data$dev_minusvar, decreasing = TRUE), ]
  
  return(corres)
  
} # eo look_dev


#' opti_maxmatch
#'function to find maxmatch
#' @param data dataframe with two columns at least: Winner and Loser
#' @param minmatch the number of matches needed to have at least one match per challenger
#' @param stepelo the step of at which to search for the answer
#'
#' @return
#' @export
#'
#' @examples
opti_maxmatch <- function(data, minmatch, stepelo = 100) {
  length_id = length(data$Winner)
  maxmatch  = floor((length_id - minmatch)/stepelo) * stepelo + minmatch
  extra     = length_id - maxmatch
  return(c(maxmatch, extra))
}

#' opti_minmatch
#' find how many match to use so that each competitor has faced another
#' @param data dataframe with two columns at least: Winner and Loser
#' @param match_base the minimum number of matches expected to have at least a match per challenger
#'
#' @return
#' @export
opti_minmatch <- function(data, match_base = 500) {
  
  liste_id = unique(c(data$Winner, data$Loser))
  match    = length(unique(c(data[0:match_base, c("Winner")], data[0:match_base, c("Loser")])))
  
  while (length(liste_id) != match) {
    match_base = match_base + 100
    match      = length(unique(c(data[0:match_base, c("Winner")], data[0:match_base, c("Loser")])))
  }
  return(match_base)
}


#' reduce_mod
#' reduces a model by removing the non-significant variables
#' @param sp_list 
#' @param respvar 
#' @param dat_cor 
#' @param thr 
#'
#' @return
#' @export
#'
reduce_mod <- function(var_list, respvar, randomvar, dat, thr){
  
  # var_list = var_order; respvar = "wins"; randomvar = "challenger_1";
  # dat = table_elo_judge; thr = 0.05
  
  if(length(var_list) == 0) stop("No more variables")
  
  # Prepare the character chain for random effects
  randomform <- paste0("(1|", randomvar)
  randomform <- paste0(randomform, ")")
  randomform <- paste0(randomform, collapse = "+")
  
  # Model with all the variable of var_list
  mod_full                   <- lme4::glmer(as.formula(
    paste(respvar," ~ ", paste(var_list, collapse = "+"), "+", randomform)),
    family = binomial, data = dat, na.action = na.fail) 
  
  saveRDS(mod_full, here::here(res_dir_elo, "02_temp_fullmod.rds"))
  mod_full <- readRDS(here::here(res_dir_elo, "02_temp_fullmod.rds"))
  
  mod_full_anov              <- anova(mod_full)
  mod_full_anov_coeff_sorted <- mod_full_anov[order(mod_full_anov$`F value`, decreasing = TRUE),]
  
  # Which variable has the highest F value?
  var_test <- rownames(mod_full_anov_coeff_sorted)[1]
  
  # test if the model without var_test is significantly different from the previous one.
  # anova (mod2, mod1, test = "Chisq") ==> H0 = mod2 is significantly different from mod1. 
  # Use only if mod1 and mod2 are nested models
  # if p-value > 0.05 H0 is rejected at the risk 5%
  modminus <- lme4::glmer(as.formula(
    paste(respvar," ~ ", 
          paste(var_list[-which(var_list == var_test)], collapse = "+"),
          "+", randomform)), family = binomial, data = dat, na.action = na.fail) 
  chi_test <- anova(modminus, mod_full, test = "Chisq")
  
  if(chi_test$`Pr(>Chisq)`[2] > thr) {
    cat("removing ",var_test, "\n")
    
    var_list <- var_list[!(var_list %in% var_test)]
    
    Recall(var_list = var_list, respvar = respvar, randomvar, dat, thr)
    
  } else {return(var_list)}
  
} # eo reduce_mod


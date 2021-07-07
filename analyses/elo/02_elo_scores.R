###################################################################################################
#' Elo score
#'
#'This script computes the Elo scores for the photographs of the survey of 2019
#' and adujst the scores of the survey of Tribot
#' 
#' Produces Figure S9 and Figure S10 of the Langlois et al. & Mouquet 2021 paper.
#'
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         François Guilhaumon, \email{francois.guilhaumon@@ird.fr}
#'         Alienor
#'
#' @date 2021/06/29
##################################################################################################

# Load data and functions ----
  
  source(here::here("R", "functions_elo.R"))

  table_survey <- read.csv(file = here::here(res_dir_elo, "01_nogroupeffect.csv"))
  esthe_focus  <- read.csv(here::here("data", "image_table.csv"))

# ----
  
# Vizualize the stabilization of the scores 
# (FIGURE S9) ----

# Takes several days to run on multiple cores so it is better take a subset to have an overview
  data       <- table_survey[1-50000]
  elo_scores <- booting_elo(data, stepelo = 500, runs = 100)

  write.csv(elo_scores, file = here::here(res_dir_elo, "02_stabilization_elo.csv"), row.names = FALSE)

#  Extract the last score to select examples
  elo_scores <- read.csv(here::here(res_dir_elo, "02_stabilization_elo.csv"))
  scores     <- tail(elo_scores, n = 1)
  scores     <- scores[colnames(scores) != "match_nb"]
  scores     <- tidyr::gather(scores, key = "name", value = "elo_score")
  max_name   <- scores$name[which(scores$elo_score == max(scores$elo_score))] # Pomacanthus_imperator_J_1 
  min_name   <- scores$name[which(scores$elo_score == min(scores$elo_score))] # Gerres_subfasciatus_A_2
  med_name   <- scores$name[which(median(scores$elo_score) - 5 <= scores$elo_score &
                                    scores$elo_score <= median(scores$elo_score) + 5)]

  med_name <- "Pseudanthias_huchtii_M_1"
  
  max_pict <- png::readPNG(here::here("data", "images", "png", paste0(max_name, ".png")))
  min_pict <- png::readPNG(here::here("data", "images", "png", paste0(min_name, ".png")))
  med_pict <- png::readPNG(here::here("data", "images", "png", paste0(med_name, ".png")))

# Plot
  png(here::here("figures_tables", "FIGURE_S9.png"),
      width = 800, height = 500, units  = "px", family = "serif")
  par(family = "serif")
  plot(elo_scores[,colnames(elo_scores) == c(med_name)], type = "l", ylim = c(900, 2250),
       x = elo_scores$match_nb,xlab = "Number of matches", ylab = "Elo score")
  lines(elo_scores[,colnames(elo_scores) == c(min_name)],col = "red", x = elo_scores$match_nb)
  lines(elo_scores[,colnames(elo_scores) == c(max_name)],col = "green", x = elo_scores$match_nb)
  graphics::rasterImage(max_pict, xleft = 270000, xright = 320000, ybottom = 1870, ytop = 2370)
  graphics::rasterImage(med_pict, xleft = 270000, xright = 320000, ybottom = 1400, ytop = 1900)
  graphics::rasterImage(min_pict, xleft = 270000, xright = 320000, ybottom = 1000, ytop = 1500)
  dev.off()

# ----

# Compute the final scores ----

  res    <- EloChoice::elochoice(winner = table_survey$Winner, loser = table_survey$Loser,
                                 startvalue = 1500, runs = 1000)
  scores <- EloChoice::ratings(res, drawplot = F)
  scores <- scores[sort(names(scores))]

  # sort by name
  species <- sort(unique(c(as.character(table_survey$challenger_1),
                           as.character(table_survey$challenger_2))), decreasing = FALSE)
  scores <- data.frame(name = species, elo_score = scores[sort(names(scores))])
  
  # # saving the results
  # write.csv(scores, file = here::here(res_dir_elo, "02_eloscores_fisheyes.csv"), row.names = F)
  # 
  # # add the fisheyes scores to esthe_rls_images
  scores                     <- read.csv(here::here(res_dir_elo, "02_eloscores_fisheyes.csv"))
  
  esthe_focus$esthe_fisheyes <- vector(length = nrow(esthe_focus))

  for(i in 1:nrow(esthe_focus)){
    if(esthe_focus$fisheyes_campgn[i] == 1){
      esthe_focus$esthe_fisheyes[i] <- scores$elo_score[
        which(scores$name == as.character(esthe_focus$name_worms[i]))]
    } # eo if
  } # eo for i

# ----

# Correlation between the survey of Tribot et al. 2018 (mayo) and our survey (fisheyes)
# (Figure S10)----

# Identify the pictures which are in fisheyes and mayo
  mayo_fisheyes_common <- esthe_focus[which(esthe_focus$mayo_campgn == 1 &
                                              esthe_focus$fisheyes_campgn == 1),]
  
# Correct the values of the pictures in Tribot et al but not in our survey 
# if the picture was in fisheyes esthe_all = esthe_fisheyes
# if the picture was only in mayo esthe_all = intercept(lm) + esthe_mayo * slope(lm)
  reg_lin_scores        <- lm(esthe_fisheyes ~ esthe_mayo, data = mayo_fisheyes_common)
  esthe_focus$esthe_all <- vector(length = nrow(esthe_focus))
  for(i in 1:nrow(esthe_focus)){
    ifelse(esthe_focus$esthe_fisheyes[i] == 0,
           esthe_focus$esthe_all[i] <-
           (as.numeric(reg_lin_scores$coefficients[2]) * esthe_focus$esthe_mayo[i]) +
           as.numeric(reg_lin_scores$coefficients[1]),
           esthe_focus$esthe_all[i] <- esthe_focus$esthe_fisheyes[i]
    ) # eo ifelse
  } # eo for i


# Correlation between the two estimated values of the 21 images present in our study and
# in Tribot et al. 2018's
  
  public_eval <- esthe_focus[which(is.na(esthe_focus$esthe_all) == FALSE), 
                             c("name_worms", "esthe_mayo", "esthe_all")]
  colnames(public_eval) <- c("Photograph_name","Elo_score_Tribot", "Elo_score_corrected")
  public_eval$diff      <- public_eval$Elo_score_Tribot - public_eval$Elo_score_corrected

  commons <- public_eval[which(public_eval$Photograph_name %in% mayo_fisheyes_common$name_worms),
                         c("Photograph_name", "Elo_score_Tribot", "Elo_score_corrected")]
  
# FIGURE S10 8x8 cm 600 dpi
  minxy <- 1200
  maxxy <- 1800
  
  corr_elo_plot <- ggplot2::ggplot(data = commons,
                                   ggplot2::aes(y = Elo_score_corrected,  x = Elo_score_Tribot)) +
    ggplot2::geom_point(size = 4, shape = 20,
                        ggplot2::aes(color = diff),
                        col = colors[7]) +
    ggplot2::theme_light() +
    ggplot2::xlim(minxy, maxxy) +
    ggplot2::ylim(minxy, maxxy) +
    ggplot2::theme(axis.title = ggplot2::element_text(size = 10, family = "serif"),
                   axis.text = ggplot2::element_text(size = 8, family = "serif"),
                   panel.grid = ggplot2::element_blank(),
                   legend.position = "none") +
    ggplot2::scale_x_continuous(breaks = c(1300, 1400, 1500, 1600, 1700),
                              labels = c("1300", "1400", "1500", "1600", "1700")) +
    ggplot2::scale_y_continuous(breaks = c(1300, 1400, 1500, 1600, 1700),
                                labels = c("1300", "1400", "1500", "1600", "1700")) +
    ggplot2::geom_abline(slope = reg_lin_scores$coefficients[2],
                         intercept = reg_lin_scores$coefficients[1],
                         col = colors[9],
                          lty = "dashed") +
    ggplot2::geom_abline(slope = 1,
                         intercept = 0,
                         col = colors[2],
                         lty = "dotted") +
    ggplot2::xlab("Elo score from Tribot et al. (2018)") +
    ggplot2::ylab("Elo score from the new survey")

  ggplot2::ggsave(plot = corr_elo_plot,
                  filename = here::here("figures_tables", "FIGURE_S10.jpg"),
                  width = 8, height = 8, units = "cm", dpi = 600, family = "serif")

  # Save
  write.csv(esthe_focus, file = here::here(res_dir_elo, "02_esthe_focus.csv"), row.names = F)
  
# ----

# File for deep ----

  deep_train            <- esthe_focus[which(is.na(esthe_focus$esthe_all) == FALSE),
                                       c("sp_worms", "name_worms", "esthe_all")]
  deep_train$name_worms <- droplevels(deep_train$name_worms)
  
# Prepare 5 folds for the cross-validation
  # Define groups so all the pictures of a single species are in the same set
  
  splist <- unique(deep_train$species)
  nbsp   <- trunc(0.2*length(splist))
  
  spa     <- sample(x = splist, size = nbsp+1)
  splft   <- setdiff(splist, spa)
  
  spb     <- sample(x = splft, size = nbsp)
  splft   <- setdiff(splft, spb)
  
  spc     <- sample(x = splft, size = nbsp)
  splft   <- setdiff(splft, spc)
  
  spd     <- sample(x = splft, size = nbsp)
  splft   <- setdiff(splft, spd)
  
  spe     <- splft
  
  for(i in 1:nrow(deep_train)){
    deep_train$set[i] <- ifelse(deep_train$species[i] %in% spa, "a", 
                                ifelse(deep_train$species[i] %in% spb, "b",
                                       ifelse(deep_train$species[i] %in% spc, "c",
                                              ifelse(deep_train$species[i] %in% spd, "d","e"))))
  }
  
  deep_train <- deep_train[, -which(colnames(deep_train) == "sp_worms")]
  
  write.csv(deep_train, here::here(res_dir_elo, "02_deep_train.csv"), row.names = FALSE)
  
# # Move pictures in folder for deep (to change for new repo)
#   all_picts <- list.files(path = here::here("data", "images", "png"), full.names = FALSE)
#   imglist   <- paste0(as.character(deep_train$name_worms),".png")
#   setdiff(imglist, all_picts)
#   
#   file.copy(from = paste0(here::here("data", "images", "png"), "/", imglist),
#             to =  paste0(here::here("data", "images", "deep"), "/", imglist))
  
  rm(deep_train, mayo_fisheyes_common, reg_lin_scores, i, scores, public_eval, esthe_focus)

# -----

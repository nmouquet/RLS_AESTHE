###################################################################################################
#' Vizualize the performances of the deep learning model
#'
#'Produces Figure 1 panel b of the Langlois et al. & Mouquet 2021 paper.
#'
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         Valentine Fleure \email{valentine.fleure@@gmail.com},
#'         Julien Renoult \email{jurenoult@@gmail.com}
#'         
#'
#' @date 2021/06/29
##################################################################################################

# Load data and functions ----

  source(here::here("R", "functions_elo.R"))

  all_table   <- read.csv(here::here(res_dir_elo, "02_esthe_focus.csv"))
  learn_table <- read.csv(here::here(res_dir_elo, "02_deep_train.csv"))
  pred_table  <- read.csv(here::here(res_dir_deep, "04_esthe_pred50.csv"))
  
  # variation of the MSE across the epochs
  run1 <- read.csv(here::here(res_dir_deep, "training", "run1.csv"), header = FALSE)
  run2 <- read.csv(here::here(res_dir_deep, "training", "run2.csv"), header = FALSE)
  run3 <- read.csv(here::here(res_dir_deep, "training", "run3.csv"), header = FALSE)
  run4 <- read.csv(here::here(res_dir_deep, "training", "run4.csv"), header = FALSE)
  run5 <- read.csv(here::here(res_dir_deep, "training", "run5.csv"), header = FALSE)
  
  # performances on the validation sets
  val1 <- read.csv(here::here(res_dir_deep, "validation_score", "validation_score1.txt"))
  val2 <- read.csv(here::here(res_dir_deep, "validation_score", "validation_score2.txt"))
  val3 <- read.csv(here::here(res_dir_deep, "validation_score", "validation_score3.txt"))
  val4 <- read.csv(here::here(res_dir_deep, "validation_score", "validation_score4.txt"))
  val5 <- read.csv(here::here(res_dir_deep, "validation_score", "validation_score5.txt"))
  
# ----

# R squared on the validation sets ----
  
  colnames(val1)
  val1$image_name <- gsub(".png", "", val1$image_name)
  for(i in 1:nrow(val1)){
    val1$evaluated_score[i] <- learn_table$esthe_all[which(learn_table$name_worms %in% val1$image_name[i])]  
  }
  mod1 <- lm(val1$predicted_score~val1$evaluated_score)
  sum1 <- summary(mod1)
  Rsq1 <- sum1$r.squared
  
  val2$image_name <- gsub(".png", "", val2$image_name)
  for(i in 1:nrow(val2)){
  val2$evaluated_score[i] <- learn_table$esthe_all[which(learn_table$name_worms %in% val2$image_name[i])]
  }
  mod2 <- lm(predicted_score~evaluated_score, data = val2)
  sum2 <- summary(mod2)
  Rsq2 <- sum2$r.squared
  
  val3$image_name <- gsub(".png", "", val3$image_name)
  for(i in 1:nrow(val3)){
  val3$evaluated_score[i] <- learn_table$esthe_all[which(learn_table$name_worms %in% val3$image_name[i])]
  }
  mod3 <- lm(predicted_score~evaluated_score, data = val3)
  sum3 <- summary(mod3)
  Rsq3 <- sum3$r.squared
  
  val4$image_name <- gsub(".png", "", val4$image_name)
  for(i in 1:nrow(val4)){
  val4$evaluated_score[i] <- learn_table$esthe_all[which(learn_table$name_worms %in% val4$image_name[i])]
  }
  mod4 <- lm(predicted_score~evaluated_score, data = val4)
  sum4 <- summary(mod4)
  Rsq4 <- sum4$r.squared
  
  val5$image_name <- gsub(".png", "", val5$image_name)
  for(i in 1:nrow(val5)){
  val5$evaluated_score[i] <- learn_table$esthe_all[which(learn_table$name_worms %in% val5$image_name[i])]
  }
  mod5 <- lm(predicted_score~evaluated_score, data = val5)
  sum5 <- summary(mod5)
  Rsq5 <- sum5$r.squared
  
  mean(Rsq1, Rsq2, Rsq3, Rsq4, Rsq5)
  sd(x = c(Rsq1, Rsq2, Rsq3, Rsq4, Rsq5))
  
# ----
  
# FIGURE 1b side left ----
  
  val1$set <- "1"
  val2$set <- "2"
  val3$set <- "3"
  val4$set <- "4"
  val5$set <- "5"
  
  val_all <- rbind(val1, val2, val3, val4, val5)
  
  perfplot <-
    ggplot2::ggplot(data = val_all, ggplot2::aes(x = evaluated_score, y = predicted_score, col = set)) + 
    ggplot2::geom_point(size = 3.5, shape = 20, alpha = 0.8) + 
    ggplot2::theme_light() + 
    ggplot2::geom_smooth(method = "lm", se = TRUE, col = "black", lty = "solid", alpha = 0.80) + 
    ggplot2::expand_limits(x = range(val_all$predicted_score), y = range(val_all$predicted_score)) +
    ggplot2::xlab("Evaluated aesthetic values") + 
    ggplot2::ylab("Predicted aesthetic values") +
    ggplot2::scale_color_manual(values =  wes_palette("Darjeeling1")) +
    ggplot2::scale_x_continuous(breaks = c(1250, 1500, 1750, 2000),
                                labels = c("1250", "1500", "1750", "2000")) + 
    ggplot2::scale_y_continuous(breaks = c(1250, 1500, 1750, 2000),
                                labels = c("1250", "1500", "1750", "2000")) + 
    ggplot2::theme(axis.title = ggplot2::element_text(size = 14, family = "serif"),
                   axis.text  = ggplot2::element_text(size = 9, family = "serif"),
                   panel.grid = ggplot2::element_blank(),
                   legend.position = "none")
  
# ----
  
# Clean data ----
# remove path from image name for predicted values
  pred_table$image_name <- gsub(pattern     = "../../../../data/images/png/", replacement = "",
                              x           = pred_table$image_name)
  pred_table$image_name <- gsub(pattern     = ".png", replacement = "", x = pred_table$image_name)
  colnames(pred_table)  <- c("name_worms", "esthe_pred")

# change column names of table of evaluated values
  eval_table            <- learn_table[, c("name_worms", "esthe_all")]
  colnames(eval_table)  <- c("name_worms", "esthe_pred")
  eval_table$method     <- "evaluated"
  
# remove the images evaluated from the predicted table
  pred_table <- pred_table[-which(pred_table$name_worms %in% eval_table$name_worms),]
  range(pred_table$esthe_pred)
  pred_table$method <- "predicted"
  
# combine the evaluated and predicted tables
  scores_table <- rbind(pred_table, eval_table)
    
# Add predicted score to the table with everything and save
  all_table <- merge(all_table, scores_table, by = "name_worms")
  write.csv(all_table, here::here(res_dir_deep, "06_esthe_focus.csv"), row.names = FALSE)

# ----

# Distribution of the predicted score VS the evaluated ones
# for train+val+test (FIGURE 1b side right) ----
  
  # Plot the densities
  
  ecrtyp <- max(pred_table$esthe_pred)-min(pred_table$esthe_pred)
  
  points_toshow <- tibble::tibble(x = c(min(pred_table$esthe_pred), min(pred_table$esthe_pred)+ecrtyp/11,
                     min(pred_table$esthe_pred)+2*ecrtyp/11, min(pred_table$esthe_pred)+3*ecrtyp/11, 
                     min(pred_table$esthe_pred)+4*ecrtyp/11, min(pred_table$esthe_pred)+5*ecrtyp/11,
                     min(pred_table$esthe_pred)+6*ecrtyp/11, min(pred_table$esthe_pred)+7*ecrtyp/11,
                     min(pred_table$esthe_pred)+8*ecrtyp/11, min(pred_table$esthe_pred)+9*ecrtyp/11,
                     min(pred_table$esthe_pred)+10*ecrtyp/11, max(pred_table$esthe_pred)), y = 0)
  
  par(mar = c(0,0,0,0), family = "serif")
  densplot <-
    ggplot2::ggplot(all_table, ggplot2::aes(x = esthe_pred, color = method, fill = method)) +
    ggplot2::geom_density(alpha = 0.5) + 
    ggplot2::theme_light() + 
    ggplot2::xlab("Aesthetic values") + 
    ggplot2::ylab("Density")  +
    ggplot2::scale_x_continuous(breaks = c(1250, 1500, 1750, 2000)) + 
    ggplot2::scale_fill_manual(values = c(colors[10], colors[8])) +
    ggplot2::scale_color_manual(values = c(colors[10], colors[8])) +
    ggplot2::theme(axis.title      = ggplot2::element_text(size = 14, family = "serif"),
                   axis.text       = ggplot2::element_text(size = 9, family = "serif"), 
                   panel.grid      = ggplot2::element_blank(),
                   legend.position = "none") + 
    # Add the 12 points to show were the photos come from
    ggplot2::geom_point(data        = points_toshow, ggplot2::aes(x, y),
                        inherit.aes = FALSE,
                        size        = 3,
                        color       = colors[9]) +
    ggplot2::annotate(geom  = "text", label = paste0("Predicted (n=",length(which(all_table$method == "predicted")),")"),
                      x     = 1550  , y     = 0.00272, family = "serif",
                      color = colors[10], size = 4.5, hjust = 0) +
    ggplot2::annotate(geom  = "text", label = paste0("Evaluated (n=",length(which(all_table$method == "evaluated")),")"),
                      x     = 1550  , y     = 0.00248, family = "serif",
                      color = colors[8], size = 4.5, hjust = 0)
# ----

# Gather the two plots (FIGURE 1b) ----
  plot <- ggpubr::ggarrange(perfplot, densplot,
            ncol = 2, nrow = 1)
  # Save
  ggplot2::ggsave(plot  = plot, filename = here::here("figures_tables", "FIGURE_1b.jpg"),
                  width = 20, height = 10, units = "cm", dpi = 600, family = "serif")
  
  rm(densplot, all_table, eval, points_toshow, ecrtyp, learn_table, mod1, mod2, mod3, mod4, mod5, 
     perfplot, plot, pred_table, run1, run2, run3, run4, run5, scores_table, sum1, sum2, sum3,
     sum4, sum5, val_all, val1, val2, val3, val4, val5, i, Rsq1, Rsq2, Rsq3, Rsq4, Rsq5, eval_table)
# ---- 

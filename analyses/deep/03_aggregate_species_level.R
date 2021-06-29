###################################################################################################
#' Aggregate the scores at the species level
#'
#'Produce Extended Table 2 of the Langlois et al. & Mouquet 2021 paper. 
#'
#' @date 2021/06/29
##################################################################################################

# Load data ----

all_table  <- read.csv(here::here(res_dir_deep, "02_esthe_focus.csv"))

# ----

# Histogram of the number of pictures by species (Figure S16) ----
  # Number of pictures by species
  count <- as.data.frame(table(all_table$sp_worms))
  
  # Number of species that have each possible number of picture
  count_2 <- as.data.frame(table(count$Freq))
  
  plot_2  <- ggplot2::ggplot(count_2, ggplot2::aes(y = Freq, x = Var1)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity", fill = colors[4]) +
    ggplot2::theme_light() +
    ggplot2::theme(legend.position = "none",
                   panel.grid = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = 8),
                   axis.title = ggplot2::element_text(size = 10)) +
    ggplot2::geom_vline(xintercept = mean(count$Freq), lty = "dashed", col = "red") +
    ggplot2::labs(x = "Number of pictures by species", y = "Freq") 
  
  ggplot2::ggsave(plot = plot_2,
                  filename = here::here("figures_tables", "FIGURE_S16.jpg"), 
                  width = 10, height = 8, units = "cm", dpi = 320)
# ----

# Select the picture with the highest value per species ----

# take the higher score of all picts for the species
  species_table           <- aggregate(x  = all_table$esthe_pred, 
                                       by = list(name = all_table$sp_worms), FUN = max)
  colnames(species_table) <- c("sp_name", "esthe_score")
  
  # keep the name of the file that has the max score, the copyright status of this file
  # and the database
  for(i in 1:nrow(species_table)){
    species_table$file_name[i] <- as.character(all_table$name_worms[
      which(all_table$esthe_pred == species_table$esthe_score[i] &
              all_table$sp_worms   == species_table$sp_name[i])])
    
    species_table$method[i] <- as.character(all_table$method[
      which(all_table$name_worms == species_table$file_name[i])])
  } # eo for i
  
# ----
  
# Compute mean value of the species to compare with max ----
  
  # take the higher score of all picts for the species
  species_table_mean      <- aggregate(x  = all_table$esthe_pred, 
                                       by = list(name = all_table$sp_worms), FUN = mean)
  colnames(species_table_mean) <- c("sp_name", "esthe_score_mean")
  
  # add the mean score to the table
  species_table <- merge(species_table, species_table_mean, by = "sp_name")
  
# ----
  
# Plot max values vs mean values (Figure S17)----
  
  plot <-
    ggplot2::ggplot(species_table, 
                  ggplot2::aes(x = esthe_score, 
                               y = esthe_score_mean)) + 
    ggplot2::geom_point(color = colors[8], alpha = 0.85) + 
    ggplot2::theme_light() +
    ggplot2::theme(
      panel.grid        = ggplot2::element_blank(),
      axis.title.y      = ggplot2::element_text(color = "black", size = 10),
      axis.title.x      = ggplot2::element_text(color = "black", size = 10),
      axis.line.x       = ggplot2::element_line(linetype = "blank"),
      axis.text.x       = ggplot2::element_text(size = 6), 
      axis.text.y       = ggplot2::element_text(size = 6),
      axis.ticks.x      = ggplot2::element_blank()) +
    ggplot2::labs(x = "Max aesthetic value", y = "Mean aesthetic value") 

model <- lm(esthe_score_mean~esthe_score, data = species_table) 
summary(model) 

ggplot2::ggsave(plot = plot,
                filename = here::here("figures_tables", "FIGURE_S17.jpg"), 
                width = 10, height = 8, units = "cm", dpi = 320)
 
# ----
  
# EXTENDED TABLE 2 ----
  extended_table2           <- species_table[,c("sp_name", "esthe_score", "method")]
  colnames(extended_table2) <- c("Species", "Evaluated/Predicted Score", "Method")
  write.csv(extended_table2, here::here("figures_tables", "Extended_table_2.csv"), row.names = FALSE)
    
  species_table <- species_table[,c("sp_name", "esthe_score", "esthe_score_mean")]
  write.csv(x = species_table, file = here::here(res_dir_deep, "03_species_table.csv"),
            row.names = FALSE)
  
  rm(i, extended_table2, species_table, all_table)
  
# ----

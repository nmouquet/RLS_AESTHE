###################################################################################################
#' Conservation status
#'
#'
#'Produces Figure 5b of the Langlois et al. & Mouquet 2021 paper.
#'
#'
#' @date 2021/03/01
##################################################################################################

source(here::here("R", "functions_cons.R"))

# Load species table ----

species_table <- read.csv(here::here(res_dir_conservation, "01_sp_table_cons.csv"))

# ----

# Get information about fishery importance from fishbase ----

  sp_list <- as.character(unique(species_table$sp_name))
  sp_list <- gsub("_", " ", sp_list)

  ptm                <- proc.time()
  data_fishbase  <- do.call(rbind, lapply(sp_list, function(name){
    dataFB <- tryCatch({
      rfishbase::species(name, fields = "Importance")},
      error = function(e){data.frame(sp_names_fishbase = name,
                                     Importance        = NA)},
      warning = function(w){data.frame(sp_names_fishbase = name,
                                       Importance        = NA)})
    
    dataFB                   <- as.data.frame(dataFB)
    dataFB$sp_names_fishbase <- name
    
    dataFB
  }))
  proc.time() - ptm
  
  data_fishbase$sp_name <- gsub(" ", "_", data_fishbase$sp_names_fishbase)
  data_fishbase         <- data_fishbase[, c("sp_name", "Importance")]
  
  write.csv(x = data_fishbase, file = here::here(res_dir_conservation, "02_data_imp_raw.csv"), row.names = FALSE)
  
  data_fishbase <- read.csv(here::here(res_dir_conservation, "02_data_imp_raw.csv"))
  
  fishery_table <- merge(data_fishbase, species_table[, c("sp_name", "esthe_score")], by = "sp_name")
  
# ----
  
# Gather categories and create a new column ----
  
  kw   <- kruskal.test(esthe_score ~ Importance, data = fishery_table)
  dunn <- dunnTestExtra(esthe_score ~ as.factor(Importance), dat = fishery_table, metho = "bh")
  aggregate(esthe_score ~ Importance, data = fishery_table, FUN = mean)
  
  highcom   <- "highly commercial"
  com       <- "commercial"
  subsfish  <- "subsistence fisheries"
  nocom     <- c("minor commercial", "of no interest", "of potential interest")
  nodata    <- c("NA", NA) 
  
  fishery_table$fishery_importance <- vector(length = nrow(fishery_table))
  
  for(i in 1 : nrow(fishery_table)){
    if(fishery_table$Importance[i] %in% highcom) {fishery_table$fishery_importance[i] <- 4}
    if(fishery_table$Importance[i] %in% com)     {fishery_table$fishery_importance[i] <- 3}
    if(fishery_table$Importance[i] %in% subsfish){fishery_table$fishery_importance[i] <- 2}
    if(fishery_table$Importance[i] %in% nocom)   {fishery_table$fishery_importance[i] <- 1}
    if(fishery_table$Importance[i] %in% nodata)  {fishery_table$fishery_importance[i] <- 0}
  }
  
  kw   <- kruskal.test(esthe_score ~ as.factor(fishery_importance), data = fishery_table) 
  dunn <- dunnTestExtra(esthe_score ~ as.factor(fishery_importance), dat = fishery_table, metho = "bh")
  aggregate(esthe_score ~ fishery_importance, data = fishery_table, FUN = mean)
  
  # # create fishery_importance variable for the species without data for fisheries
  
  species_table <- merge(species_table, fishery_table[, c("sp_name", "Importance", "fishery_importance")], by = "sp_name")

  write.csv(species_table, file = here::here(res_dir_conservation, "02_sptable_fishery.csv"), row.names = FALSE)

  rm(highcom, com, nocom, i, subsfish)

# ----


# FIGURE5 panel b----

  fishery_table <- fishery_table[,c("sp_name", "esthe_score", "fishery_importance")]
  
  fishery_table$fishery_importance <- as.factor(fishery_table$fishery_importance)
  
  table(fishery_table$fishery_importance)
  
  model <- lm(esthe_score ~ fishery_importance, data = fishery_table)
  ANOVA <- aov(model)
  summary(ANOVA)
  
  fishery_importance_TUKEY        <- TukeyHSD(x = ANOVA, 'fishery_importance', conf.level = 0.95)
  fishery_importance_TUKEY.levels <- fishery_importance_TUKEY[["fishery_importance"]][,4]
  fishery_importance_TUKEY.labels <- data.frame(
    multcompView::multcompLetters(fishery_importance_TUKEY.levels, reversed = TRUE)['Letters'])

  plot <- ggplot2::ggplot(fishery_table, ggplot2::aes(x = fishery_importance, 
                                                      y = esthe_score, fill = fishery_importance)) + 
    ggplot2::geom_violin(trim = FALSE, color = "white", alpha = 0.95) +
    ggplot2::geom_boxplot(width = 0.30, fill = "white", outlier.shape = NA, alpha = 0.8) +
    ggplot2::geom_jitter(shape = 19, size = 0.05, position = ggplot2::position_jitter(0.2),
                         col = "#5b5b5b", alpha = 0.6) +
    ggplot2::scale_x_discrete(labels = c(paste0("Data\ndeficient\nn = ",
                                                nrow(fishery_table[fishery_table$fishery_importance == 0,])),
                                         paste0("Non \ncommercial\nn = ",
                                                nrow(fishery_table[fishery_table$fishery_importance == 1,])),
                                         paste0("Subsistence \nfisheries\nn = ",
                                                nrow(fishery_table[fishery_table$fishery_importance == 2,])),
                                         paste0("Importance\n\nn = ", 
                                                nrow(fishery_table[fishery_table$fishery_importance == 3,])),
                                         paste0("High \nimportance\nn = ",
                                                nrow(fishery_table[fishery_table$fishery_importance == 4,])))) +
    ggplot2::stat_summary(fun = mean, geom = "point", shape = 23, size = 2) +
    ggplot2::scale_fill_manual(values = c("gray", "tomato2", "darkgoldenrod1", "skyblue1", "mediumpurple2"),
                               guide = FALSE) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 1))) + 
    ggplot2::theme_light() +
    ggplot2::ylim(1095, 2105) +
    ggplot2::geom_abline(intercept = mean(fishery_table$esthe_score),
                         slope = 0, lty = "dashed", col = "black") +
    ggplot2::theme(panel.grid        = ggplot2::element_blank(),
                   axis.title.y      = ggplot2::element_text(color = "black", size = 10),
                   axis.title.x      = ggplot2::element_text(color = "black", size = 10),
                   axis.line.x       = ggplot2::element_line(linetype = "blank"),
                   axis.text.x       = ggplot2::element_text(size = 9), 
                   axis.text.y       = ggplot2::element_text(size = 8),
                   axis.ticks.x      = ggplot2::element_blank()) +
    ggplot2::labs(x = "Fishery importance", y = "Aesthetic values")+ 
    ggplot2::geom_text(x = "0", y = 2100, label = "a", col = "#5b5b5b") +
    ggplot2::geom_text(x = "1", y = 2100, label = "a", col = "#5b5b5b") +
    ggplot2::geom_text(x = "2", y = 2100, label = "a", col = "#5b5b5b") +
    ggplot2::geom_text(x = "3", y = 2100, label = "b", col = "#5b5b5b") +
    ggplot2::geom_text(x = "4", y = 2100, label = "c", col = "#5b5b5b")
  
  ggplot2::ggsave(plot = plot,
                  filename = here::here("figures_tables", "FIGURE_5b.jpg"), 
                  width = 10, height = 8, units = "cm", dpi = 320)
  
  model <- lm(esthe_score ~ threats+fishery_importance+threats*fishery_importance, data = species_table, na.action = na.omit)
  ANOVA <- aov(model)

# ----


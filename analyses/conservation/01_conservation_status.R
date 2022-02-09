###################################################################################################
#' Conservation status
#'
#'Produces Figure 5 and Figure S19 of the Langlois et al. & Mouquet 2021 paper.
#'
#'Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#'        Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'        François Guilhaumon, \email{francois.guilhaumon@@ird.fr},
#'        Nicolas Casajus, \email{nicolas.casajus@@fondationbiodiversite.fr},
#'        Nicolas Loiseau, \email{nicolas.loiseau1@@gmail.com}
#'
#' @date 2021/06/29
##################################################################################################

source(here::here("R", "functions_cons.R"))

# Load species table ----
  
  species_table <- read.csv(here::here(res_dir_biodiversity, "02_sptable_biodiv.csv"))
  
# ----

# Get IUCN status from fishbase ----
  
  sp_list          <- data.frame(worms = gsub("_", "-", species_table$sp_name))
  sp_list$fishbase <- as.character(sp_list$worms)
  sp_list$fishbase[which(as.character(sp_list$worms) =="Pseudocaranx-georgianus")]         <- "Pseudocaranx-dentex"
  sp_list$fishbase[which(as.character(sp_list$worms) =="Stichaeus-punctatus-punctatus")]   <- "Stichaeus-punctatus"
  sp_list$fishbase[which(as.character(sp_list$worms) =="Tylosurus-crocodilus-crocodilus")] <- "Tylosurus-crocodilus"
  sp_list$fishbase[which(as.character(sp_list$worms) =="Platybelone-argalus-argalus")]     <- "Platybelone-argalus"

  start.time <- Sys.time()
  data_iucn  <- do.call(rbind, lapply(sp_list$fishbase, get_iucn))
  end.time   <- Sys.time()
  time.taken <- end.time - start.time
  time.taken

  data_iucn$fishbase  <- rownames(data_iucn)
  colnames(data_iucn) <- c("iucn_code", "fishbase")
  data_iucn           <- merge(data_iucn, sp_list, by = "fishbase")
  data_iucn$sp_name   <- gsub("-", "_", data_iucn$worms)
  data_iucn           <- data_iucn[,c("sp_name", "iucn_code")]

  species_table       <- merge(species_table, data_iucn, by = "sp_name")
  
# ----
  
# Group IUCN classes into four groups ----
  
  # threatened, non threatened and not evaluated 
  species_table$iucn_code <- toupper(species_table$iucn_code)
  species_table$iucn_code[which(is.na(species_table$iucn_code))] <- "NE"
  
  kw   <- kruskal.test(esthe_score ~ iucn_code, data = species_table)
  dunn <- dunnTestExtra(esthe_score ~ as.factor(iucn_code), dat = species_table, metho = "bh")
  
  thr  <- c("CR", "VU", "EN")
  nthr <- c("LC", "LR/lc", "NT")
  #nev  <- c("NE", "DD")
  nev  <- "NE"
  dd  <- "DD"
  
  species_table$threats <- vector(length = nrow(species_table))
  
  # Regroup the IUCN statuts into three groups
  for(i in 1 : nrow(species_table)){
    if(species_table$iucn_code[i] %in% thr) {species_table$threats[i] <- "thr"}
    if(species_table$iucn_code[i] %in% nthr){species_table$threats[i] <- "nthr"}
    if(species_table$iucn_code[i] %in% nev) {species_table$threats[i] <- "nev"}
    if(species_table$iucn_code[i] %in% dd) {species_table$threats[i] <- "dd"}
  }
  
  rm(thr, nthr, nev, i)
  
  species_table$threats <- factor(species_table$threats,c("thr", "nthr", "nev", "dd"))
  
  write.csv(species_table, file = here::here(res_dir_conservation, "01_sp_table_cons.csv"), row.names = FALSE)
  
# ----
  
# Get information about fishery importance from fishbase ----
  
  species_table <- read.csv(here::here(res_dir_conservation, "01_sp_table_cons.csv"))
  
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
  
  write.csv(x = data_fishbase, file = here::here(res_dir_conservation, "01_data_imp_raw.csv"), row.names = FALSE)
  
  data_fishbase <- read.csv(here::here(res_dir_conservation, "01_data_imp_raw.csv"))
  
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
  
  write.csv(species_table, file = here::here(res_dir_conservation, "01_sptable_all.csv"), row.names = FALSE)
  
  rm(highcom, com, nocom, i, subsfish)
  
# ----
  
# FIGURE 5 panel a ----
  
  species_table <- read.csv(here::here(res_dir_conservation, "01_sptable_all.csv"))
  
  #remove the Datadeficient species
  species_table <- species_table[species_table$threats!='dd',]
  
# Ordering by esthe_score median
  new_order_fac    <- with(species_table, tapply(esthe_score, threats, median))
  new_label        <- names(new_order_fac)[order(new_order_fac, decreasing = FALSE)]
  species_table$threats <- factor(species_table$threats, as.factor(new_label))
  
# ANOVA model
  model <- lm(esthe_score ~ threats, data = species_table)
  ANOVA <- aov(model)
  
# Tukey 
  threats_TUKEY        <- TukeyHSD(x = ANOVA, 'threats', conf.level = 0.95)
  threats_TUKEY.levels <- threats_TUKEY[["threats"]][,4]
  threats_TUKEY.labels <- data.frame(multcompView::multcompLetters(threats_TUKEY.levels, reversed = TRUE)['Letters'])
  
# Plot
  a <- ggplot2::ggplot(species_table, 
                          ggplot2::aes(x = threats, y = esthe_score, fill = threats)) + 
    ggplot2::geom_violin(trim = FALSE, color = "white", alpha = 0.95) +
    ggplot2::geom_boxplot(width = 0.30, fill = "white", outlier.shape = NA, alpha = 0.8) +
    ggplot2::geom_jitter(shape = 19, size = 0.05, position = ggplot2::position_jitter(0.2), col = "#5b5b5b", alpha = 0.6) +
    ggplot2::scale_x_discrete(
      labels = c(paste0("TH\nn = ", nrow(species_table[species_table$threats == "thr",])),
                 paste0("NE\nn = ", nrow(species_table[species_table$threats == "nev",])),
                 paste0("LC\nn = ", nrow(species_table[species_table$threats == "nthr",]))
      )) +
    ggplot2::stat_summary(fun = mean, geom = "point", shape = 23, size = 2) +
    ggplot2::scale_fill_manual(values = c("tomato2", "darkgoldenrod1", "seagreen3"), guide = FALSE) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 1))) + 
    ggplot2::theme_light() +
    ggplot2::ylim(1095, 2105) +
    ggplot2::geom_abline(intercept = mean(species_table$esthe_score), slope = 0, lty = "dashed", col = "black") +
    ggplot2::theme(
      panel.grid        = ggplot2::element_blank(),
      axis.title.y      = ggplot2::element_text(color = "black", size = 10, family = "serif"),
      axis.title.x      = ggplot2::element_text(color = "black", size = 10, family = "serif"),
      axis.line.x       = ggplot2::element_line(linetype = "blank"),
      axis.text.x       = ggplot2::element_text(size = 8, family = "serif"), 
      axis.text.y       = ggplot2::element_text(size = 8, family = "serif"),
      axis.ticks.x      = ggplot2::element_blank()) +
    ggplot2::labs(x = "IUCN status", y = "Aesthetic values")+ 
    ggplot2::geom_text(x = "thr", y = 2100, label = "a", col = "#5b5b5b", family = "serif") +
    ggplot2::geom_text(x = "nev", y = 2100, label = "b", col = "#5b5b5b", family = "serif") +
    ggplot2::geom_text(x = "nthr", y = 2100, label = "c", col = "#5b5b5b", family = "serif")
  
#----
  
# Comparing evaluated and not evaluated IUCN categories----
  
  species_table <- read.csv(here::here(res_dir_conservation, "01_sptable_all.csv"))
  
  #remove the Datadeficient species
  species_table <- species_table[species_table$threats!='dd',]
  
  iucn_table <- species_table[,c("esthe_score","threats")]
  iucn_table$evaluated <- "eval"
  iucn_table$evaluated[iucn_table$threats=="nev"]="not_eval"
  
  model <- lm(esthe_score ~ evaluated, data = iucn_table)
  summary(model)
  summary(aov(model))
  
  
#----
  
# FIGURE5 panel b----
  
  species_table <- read.csv(here::here(res_dir_conservation, "01_sptable_all.csv"))
  
  fishery_table <- species_table[,c("sp_name", "esthe_score", "fishery_importance")]
  
  fishery_table$fishery_importance <- as.factor(fishery_table$fishery_importance)
  
  table(fishery_table$fishery_importance)
  
  model <- lm(esthe_score ~ fishery_importance, data = fishery_table)
  ANOVA <- aov(model)
  summary(ANOVA)
  
  fishery_importance_TUKEY        <- TukeyHSD(x = ANOVA, 'fishery_importance', conf.level = 0.95)
  fishery_importance_TUKEY.levels <- fishery_importance_TUKEY[["fishery_importance"]][,4]
  fishery_importance_TUKEY.labels <- data.frame(
    multcompView::multcompLetters(fishery_importance_TUKEY.levels, reversed = TRUE)['Letters'])
  
  b <- ggplot2::ggplot(fishery_table, ggplot2::aes(x = fishery_importance, 
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
                                         paste0("Commercial\n\nn = ", 
                                                nrow(fishery_table[fishery_table$fishery_importance == 3,])),
                                         paste0("Highly \ncommercial\nn = ",
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
                   axis.title.y      = ggplot2::element_text(color = "black", size = 10, family = "serif"),
                   axis.title.x      = ggplot2::element_text(color = "black", size = 10, family = "serif"),
                   axis.line.x       = ggplot2::element_line(linetype = "blank"),
                   axis.text.x       = ggplot2::element_text(size = 8, family = "serif"), 
                   axis.text.y       = ggplot2::element_text(size = 8, family = "serif"),
                   axis.ticks.x      = ggplot2::element_blank()) +
    ggplot2::labs(x = "Fishery importance", y = "Aesthetic values")+ 
    ggplot2::geom_text(x = "0", y = 2100, label = "a", col = "#5b5b5b", family = "serif") +
    ggplot2::geom_text(x = "1", y = 2100, label = "a", col = "#5b5b5b", family = "serif") +
    ggplot2::geom_text(x = "2", y = 2100, label = "a", col = "#5b5b5b", family = "serif") +
    ggplot2::geom_text(x = "3", y = 2100, label = "b", col = "#5b5b5b", family = "serif") +
    ggplot2::geom_text(x = "4", y = 2100, label = "c", col = "#5b5b5b", family = "serif")
  
  ggplot2::ggsave(filename = here::here("figures_tables", "FIGURE_5.jpg"),
                  plot = gridExtra::grid.arrange(a, b, nrow = 2), 
                  width = 10, height = 16, units = "cm", dpi = 600)
  
# ----
  
# SUPPLEMENTARY FIGURE 19 ----
  
  species_table <- read.csv(here::here(res_dir_conservation, "01_sptable_all.csv"))
  
  #remove the Datadeficient species
  species_table <- species_table[species_table$threats!='dd',]
  
# Panel a
  
  # Ordering by mean esthe_score median
  new_order_fac         <- with(species_table, tapply(esthe_score_mean, threats, median))
  new_label             <- names(new_order_fac)[order(new_order_fac, decreasing = FALSE)]
  species_table$threats <- factor(species_table$threats, as.factor(new_label))
  
  # ANOVA model
  model <- lm(esthe_score_mean ~ threats, data = species_table)
  ANOVA <- aov(model)
  
  # Tukey 
  threats_TUKEY        <- TukeyHSD(x = ANOVA, 'threats', conf.level = 0.95)
  threats_TUKEY.levels <- threats_TUKEY[["threats"]][,4]
  threats_TUKEY.labels <- data.frame(multcompView::multcompLetters(threats_TUKEY.levels, reversed = TRUE)['Letters'])
  
  # Plot
  a <- ggplot2::ggplot(species_table, 
                          ggplot2::aes(x = threats, y = esthe_score_mean, fill = threats)) + 
    ggplot2::geom_violin(trim = FALSE, color = "white", alpha = 0.95) +
    ggplot2::geom_boxplot(width = 0.30, fill = "white", outlier.shape = NA, alpha = 0.8) +
    ggplot2::geom_jitter(shape = 19, size = 0.05, position = ggplot2::position_jitter(0.2), 
                         col = "#5b5b5b", alpha = 0.6) +
    ggplot2::scale_x_discrete(
      labels = c(paste0("TH\nn = ", nrow(species_table[species_table$threats == "thr",])),
                 paste0("NE\nn = ", nrow(species_table[species_table$threats == "nev",])),
                 paste0("LC\nn = ", nrow(species_table[species_table$threats == "nthr",]))
      )) +
    ggplot2::stat_summary(fun = mean, geom = "point", shape = 23, size = 2) +
    ggplot2::scale_fill_manual(values = c("tomato2", "darkgoldenrod1", "seagreen3"), guide = FALSE) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 1))) + 
    ggplot2::theme_light() +
    ggplot2::ylim(1095, 2105) +
    ggplot2::geom_abline(intercept = mean(species_table$esthe_score_mean), slope = 0, lty = "dashed", col = "black") +
    ggplot2::theme(
      panel.grid        = ggplot2::element_blank(),
      axis.title.y      = ggplot2::element_text(color = "black", size = 10, family = "serif"),
      axis.title.x      = ggplot2::element_text(color = "black", size = 10, family = "serif"),
      axis.line.x       = ggplot2::element_line(linetype = "blank"),
      axis.text.x       = ggplot2::element_text(size = 8, family = "serif"), 
      axis.text.y       = ggplot2::element_text(size = 8, family = "serif"),
      axis.ticks.x      = ggplot2::element_blank()) +
    ggplot2::labs(x = "IUCN status", y = "Aesthetic value (mean)")+ 
    ggplot2::geom_text(x = "thr",  y = 2100, label = "a", col = "#5b5b5b", family = "serif") +
    ggplot2::geom_text(x = "nev",  y = 2100, label = "b", col = "#5b5b5b", family = "serif") +
    ggplot2::geom_text(x = "nthr", y = 2100, label = "c", col = "#5b5b5b", family = "serif")
  
# Panel b
  
  species_table <- read.csv(here::here(res_dir_conservation, "01_sptable_all.csv"))
  fishery_table <- species_table[,c("sp_name", "esthe_score_mean", "fishery_importance")]
  
  fishery_table$fishery_importance <- as.factor(fishery_table$fishery_importance)
  
  table(fishery_table$fishery_importance)
  
  model <- lm(esthe_score_mean ~ fishery_importance, data = fishery_table)
  ANOVA <- aov(model)
  summary(ANOVA)
  
  fishery_importance_TUKEY        <- TukeyHSD(x = ANOVA, 'fishery_importance', conf.level = 0.95)
  fishery_importance_TUKEY.levels <- fishery_importance_TUKEY[["fishery_importance"]][,4]
  fishery_importance_TUKEY.labels <- data.frame(
    multcompView::multcompLetters(fishery_importance_TUKEY.levels, reversed = TRUE)['Letters'])
  
  b <- ggplot2::ggplot(fishery_table, ggplot2::aes(x = fishery_importance, 
                                                   y = esthe_score_mean, fill = fishery_importance)) + 
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
                                         paste0("Commercial\n\nn = ", 
                                                nrow(fishery_table[fishery_table$fishery_importance == 3,])),
                                         paste0("Highly \ncommercial\nn = ",
                                                nrow(fishery_table[fishery_table$fishery_importance == 4,])))) +
    ggplot2::stat_summary(fun = mean, geom = "point", shape = 23, size = 2) +
    ggplot2::scale_fill_manual(values = c("gray", "tomato2", "darkgoldenrod1", "skyblue1", "mediumpurple2"),
                               guide = FALSE) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 1))) + 
    ggplot2::theme_light() +
    ggplot2::ylim(1095, 2105) +
    ggplot2::geom_abline(intercept = mean(fishery_table$esthe_score_mean),
                         slope = 0, lty = "dashed", col = "black") +
    ggplot2::theme(panel.grid        = ggplot2::element_blank(),
                   axis.title.y      = ggplot2::element_text(color = "black", size = 10, family = "serif"),
                   axis.title.x      = ggplot2::element_text(color = "black", size = 10, family = "serif"),
                   axis.line.x       = ggplot2::element_line(linetype = "blank"),
                   axis.text.x       = ggplot2::element_text(size = 8, family = "serif"), 
                   axis.text.y       = ggplot2::element_text(size = 8, family = "serif"),
                   axis.ticks.x      = ggplot2::element_blank()) +
    ggplot2::labs(x = "Fishery importance", y = "Aesthetic values (mean)")+ 
    ggplot2::geom_text(x = "0", y = 2100, label = "a", col = "#5b5b5b", family = "serif") +
    ggplot2::geom_text(x = "1", y = 2100, label = "a", col = "#5b5b5b", family = "serif") +
    ggplot2::geom_text(x = "2", y = 2100, label = "a", col = "#5b5b5b", family = "serif") +
    ggplot2::geom_text(x = "3", y = 2100, label = "b", col = "#5b5b5b", family = "serif") +
    ggplot2::geom_text(x = "4", y = 2100, label = "c", col = "#5b5b5b", family = "serif")
  
  ggplot2::ggsave(filename = here::here("figures_tables", "FIGURE_S19.jpg"),
                  plot = gridExtra::grid.arrange(a, b, nrow = 2), 
                  width = 10, height = 16, units = "cm", dpi = 600)
  
  
#----
  
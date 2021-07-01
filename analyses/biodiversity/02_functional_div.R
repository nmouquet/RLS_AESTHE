###################################################################################################
#' Functional diversity
#'
#'This script produces Figure 3, Figure S15 and Figure S18 of the 
#'Langlois et al. & Mouquet 2021 paper.
#'
#' @date 2021/02/17
##################################################################################################

# Load data ----

  require(dplyr) # for the pipe
  species_table <- read.csv(here::here("results","biodiversity", "01_sptable_phylo.csv"))
  funct_table   <- read.csv(here::here("data", "fonctio_table.csv"), sep = ";")
  
# ----
  
# Select columns and lines of funct_table ----
  
  funct_table$habitat       <- tolower(funct_table$habitat)
  funct_table$diel_activity <- tolower(funct_table$diel_activity)
  funct_table$water_column  <- tolower(funct_table$water_column)
  funct_table$trophic_group <- tolower(funct_table$trophic_group)
  
  rownames(funct_table) <- gsub(" ", "_", funct_table$sp_name)
  
  funct_table         <- funct_table[which(rownames(funct_table) %in% species_table$sp_name),]
  funct_table         <- funct_table[, c('max_length', 'trophic_level','thermal_mp_5min_95max',
                                        'thermal_95thmax', 'trophic_group','water_column',
                                        'diel_activity','habitat')]
  
# Remove the species for which we have none of the traits
  funct_table <- funct_table[rowSums(is.na(funct_table)) != ncol(funct_table), ]
  
# ---- 

# Predict missing value for traits ----
  
# Which percentage of NAs?
  nbna <- length(which(is.na(funct_table)))/(nrow(funct_table)*ncol(funct_table))*100
    if(nbna <=5){
      cat(round(nbna, digits = 2),"% of NAs \n", ">>> You can use missForest!\n")
      } # eo if

# Lines whith more than 50% NAs?
  temp <- funct_table[rowSums(is.na(funct_table)) <= ncol(funct_table)*0.5, ]
  ifelse(nrow(temp) == nrow(funct_table),
           cat("\n", "No line with more than 50% NAs\n"),
           cat("\n", "Lines to remove\n"))

# Test efficiency of missForest
# create artificial NAs and see how good it is to replace them
# No need to run every time, once is ok

# subset of no na
  nona <- funct_table %>% na.omit()

# create artificial NAs
  artna           <- nona

# the number of random values to replace
  artna_mis               <- missForest::prodNA(x = artna, noNA = 0.05)
  artna_mis$trophic_group <- as.factor(artna_mis$trophic_group)
  artna_mis$water_column  <- as.factor(artna_mis$water_column)
  artna_mis$diel_activity <- as.factor(artna_mis$diel_activity)
  artna_mis$habitat       <- as.factor(artna_mis$habitat)
  artna_recons <- missForest::missForest(xmis = artna_mis, verbose = TRUE)

# Check if the predictions are good with correlation
  perf <- diag(cor(x = artna[, c("max_length", "trophic_level", "thermal_mp_5min_95max",
                                 "thermal_95thmax")],
                   y = artna_recons$ximp[, c("max_length", "trophic_level",
                                             "thermal_mp_5min_95max", "thermal_95thmax")]))
  trophic_group <- (length(which(as.factor(artna$trophic_group) ==
                                   artna_recons$ximp$trophic_group)))/nrow(artna)
  water_column  <- (length(which(as.factor(artna$water_column) ==
                                   artna_recons$ximp$water_column)))/nrow(artna)
  diel_activity <- (length(which(as.factor(artna$diel_activity) ==
                                   artna_recons$ximp$diel_activity)))/nrow(artna)
  habitat       <- (length(which(as.factor(artna$habitat) ==
                                   artna_recons$ximp$habitat)))/nrow(artna)
  perf <- c(perf, trophic_group, water_column, diel_activity, habitat)
        # all between 0.98 and 0.999 ==> ok method accepted
  
rm(artna, nona, artna_recons, temp, nbna, artna_mis)
    
  if(length(which(is.na(funct_table))) !=0){
   funct_table               <- as.data.frame(funct_table)
   funct_table$trophic_group <- as.factor(funct_table$trophic_group)
   funct_table$water_column  <- as.factor(funct_table$water_column)
   funct_table$diel_activity <- as.factor(funct_table$diel_activity)
   funct_table$habitat       <- as.factor(funct_table$habitat)
   funct_table               <-  missForest::missForest(funct_table, verbose = TRUE)
   funct_table               <- funct_table$ximp
  } # eo if

  length(which(is.na(funct_table)))
  funct_table$sp_name <- rownames(funct_table)

  save(funct_table, file = here::here(res_dir_biodiversity, "02_funk_nona.RData"))
  
rm(funct_table)
  
  load(here::here("results","biodiversity", "02_funk_nona.RData"))
    
# ----
  
# Log and normalize numeric variables ----
    
# Correlogram
  GGally::ggpairs(funct_table[,c('max_length', 'trophic_level','thermal_mp_5min_95max',
                                 'thermal_95thmax')], title = "correlogram with ggpairs()") 

# Log if skewed
  log_var <- c('max_length', 'thermal_mp_5min_95max', 'thermal_95thmax')
  for (id in log_var) funct_table[,id] <- log(funct_table[,id])
    
# Normalize all
  normalize <- function(x){
      (x-min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))
    }
  
  funct_table$max_length            <- normalize(funct_table$max_length)
  funct_table$thermal_mp_5min_95max <- normalize(funct_table$thermal_mp_5min_95max)
  funct_table$thermal_95thmax      <- normalize(funct_table$thermal_95thmax)
  funct_table$trophic_level        <- normalize(funct_table$trophic_level)

  rm(id, log_var)
        
# ----
    
# Compute the distance matrix ----
# Group the variables according to their type
# Quantitative variables
  quant_var           <- cbind.data.frame(funct_table$max_length, funct_table$thermal_mp_5min_95max,
                                         funct_table$thermal_95thmax, funct_table$trophic_level)
  rownames(quant_var) <- funct_table$sp_name
# Nominal variables
  nom_var            <- cbind.data.frame(funct_table$trophic_group, funct_table$water_column,
                                         funct_table$diel_activity, funct_table$habitat)
  rownames(nom_var)  <- funct_table$sp_name
  
# Compute distance matrix
  disTraits <- ade4::dist.ktab(ade4::ktab.list.df(list(quant_var, nom_var)), c("Q","N"),
                               scan = FALSE) %>% as.matrix()
# Save
  save(disTraits, file = here::here(res_dir_biodiversity,"02_disTraits.RData"))
  
# ----    

# Compute the functional distinctiveness and uniqueness ----
  
  load(here::here("results","biodiversity", "02_disTraits.RData"))
  
# Build hypothetical community where all species are presents. 
  Sim_commu           <- matrix(1, 1, ncol(disTraits))
  colnames(Sim_commu) <- colnames(disTraits)
  
# Compute Ui & Di for each species in the hypothetical community
  Di           <- t(funrar::distinctiveness(Sim_commu, disTraits))
  colnames(Di) <- "Di"
  
  Ui           <- funrar::uniqueness(Sim_commu, disTraits)
  rownames(Ui) <- Ui$species
  
  FR           <- merge(Di, Ui, by = "row.names", all.x = TRUE)
  rownames(FR) <- FR$species
  FR           <- FR[,-c(1,3)]
  
# Merge traits and Di, Ui
  funct_table           <- merge(funct_table, FR, by = "row.names")
  funct_table$sp_name   <- funct_table$Row.names
  funct_table           <- funct_table[, -which(colnames(funct_table) == "Row.names")]
  funct_table           <- merge(funct_table, 
                                 species_table[,c("sp_name", "esthe_score", "esthe_score_mean")],
                                 by = "sp_name")
  
# Relation between Ui and Di
  fit_Ui_Di     <- lm(Di ~ log(Ui), data = funct_table)
  sum_fit_Ui_Di <- summary(fit_Ui_Di)
  
# Aesthetic ~ Dinstinctiveness
    # max
      fit_ELO_Di <- lm(esthe_score ~ Di, data = funct_table)
      sum_ELO_Di <- summary(fit_ELO_Di)
    # mean
      fit_ELO_Di_mean <- lm(esthe_score_mean ~ Di, data = funct_table)
      sum_ELO_Di_mean <- summary(fit_ELO_Di_mean)
  
# Aesthetic ~ Uniqueness
      # max
        fit_ELO_Ui <- lm(esthe_score ~ Ui, data = funct_table)
        sum_ELO_Ui <- summary(fit_ELO_Ui)
      # mean
        fit_ELO_Ui_mean <- lm(esthe_score_mean ~ Ui, data = funct_table)
        sum_ELO_Ui_mean <- summary(fit_ELO_Ui)
  
  rm(fit_ELO_Di, fit_ELO_Ui, sum_ELO_Di, sum_ELO_Ui, fit_Ui_Di, FR, Ui, Di, Sim_commu, disTraits,
     nom_var, quant_var, sum_fit_Ui_Di, temp, nbna, fit_ELO_Di_mean, fit_ELO_Ui_mean,
     sum_ELO_Di_mean, sum_ELO_Ui_mean)

# ----

# Add functional trait, Ui, Di in species table ----
  
  funct_table <- funct_table[, -which(colnames(funct_table) %in%
                                        c("esthe_score", "esthe_score_mean"))]
  toadd       <-
    data.frame(sp_name               = setdiff(as.character(species_table$sp_name),
                                               funct_table$sp_name),
               max_length            = rep(NA, length(setdiff(species_table$sp_name,
                                                              funct_table$sp_name))),
               trophic_level         = rep(NA,length(setdiff(species_table$sp_name,
                                                             funct_table$sp_name))),
               thermal_mp_5min_95max = rep(NA, length(setdiff(species_table$sp_name,
                                                          funct_table$sp_name))),
               thermal_95thmax       = rep(NA, length(setdiff(species_table$sp_name,
                                                              funct_table$sp_name))),
               trophic_group         =  rep(NA, length(setdiff(species_table$sp_name,
                                                               funct_table$sp_name))), 
               water_column          = rep(NA, length(setdiff(species_table$sp_name,
                                                              funct_table$sp_name))),
               diel_activity         = rep(NA, length(setdiff(species_table$sp_name,
                                                              funct_table$sp_name))),
               habitat               = rep(NA, length(setdiff(species_table$sp_name,
                                                              funct_table$sp_name))),
               Di                    = rep(NA, length(setdiff(species_table$sp_name,
                                                              funct_table$sp_name))),
               Ui                    = rep(NA, length(setdiff(species_table$sp_name,
                                                              funct_table$sp_name))))

  funct_table   <- rbind(funct_table, toadd)
  species_table <- merge(species_table, funct_table, by = "sp_name")
  
# Save 
  write.csv(species_table, 
            file = here::here(res_dir_biodiversity, "02_sptable_biodiv.csv"),
            row.names = FALSE)
  
  rm(toadd, funct_table, species_table)
  
# ----
  
# FIGURE 3 -----

  data <- read.csv(here::here(res_dir_biodiversity, "02_sptable_biodiv.csv"))
  
  data <- data[which(!is.na(data$ED)),]
  
  data <- data[order(data$ED, decreasing = TRUE),]
  rownames(data) <- NULL
  
  a <-
    ggplot2::ggplot(data, ggplot2::aes(y = esthe_score, x = ages_mean)) +
    ggplot2::geom_point(shape = 21, alpha = 1, size = 2 ,
                        ggplot2::aes(fill = log(ED), color = log(ED))
                        ) + 
    ggplot2::scale_fill_gradientn(colors = colors) +
    ggplot2::scale_color_gradientn(colors = colors) +
    ggplot2::geom_smooth(method = "glm", formula = y~x, col = "gray30",
                         method.args = list(family = gaussian(link = 'log'))) +
    ggplot2::scale_x_continuous(trans = 'log2') +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text  = ggplot2::element_text(size = 8),
                   axis.title = ggplot2::element_text(size = 10),
                   panel.grid = ggplot2::element_blank(),
                   legend.position = "none")+
    ggplot2::labs(x ="Age of the species (MY)", y = "Aesthetic values")
  
  b <- ggplot2::ggplot(data, ggplot2::aes(y = esthe_score, x = Di)) +
    ggplot2::geom_point(shape = 21, alpha = 1, size = 2,
                        ggplot2::aes(fill = log(ED), color = log(ED))) +
    ggplot2::scale_fill_gradientn(colours = colors) +
    ggplot2::scale_color_gradientn(colours = colors) +
    ggplot2::geom_smooth(method = "lm", formula = y~x, col = 'gray30') +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text  = ggplot2::element_text(size = 8),
                   axis.title = ggplot2::element_text(size = 10),
                   panel.grid = ggplot2::element_blank(),
                   legend.position = c(0.89, 0.72),
                   legend.background = ggplot2::element_blank(),
                   legend.title = ggplot2::element_text(size = 8),
                   legend.text = ggplot2::element_text(size = 8)
                   # legend.position = "none"
                   ) +
    ggplot2::labs(x ="Functional Distinctiveness", y = "Aesthetic values")
  
  
# Save FIGURE 3 18X9cm 600dpi
  ggplot2::ggsave(filename = here::here("figures_tables", "FIGURE_3.jpg"),
                  plot = gridExtra::grid.arrange(a, b, ncol = 2), 
                  width = 20, height = 8, units = "cm", dpi = 600, family = "sans")
  
# ----- 
  
# SUPPLEMENTARY FIGURE 18 -----
  
  data <- read.csv(here::here(res_dir_biodiversity, "02_sptable_biodiv.csv"))
  
  a <- ggplot2::ggplot(data, ggplot2::aes(y = esthe_score_mean, x = ages_mean)) +
    ggplot2::geom_point(shape = 21, alpha = 0.85,
                        ggplot2::aes(fill = log(ED)), color = "gray65") + 
    ggplot2::scale_fill_gradientn(colors = colors) +
    ggplot2::geom_smooth(method = "glm", formula = y~x, col = "gray30",
                         method.args = list(family = gaussian(link = 'log'))) +
    ggplot2::scale_x_continuous(trans = 'log2') +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text  = ggplot2::element_text(size = 8),
                   axis.title = ggplot2::element_text(size = 10),
                   panel.grid = ggplot2::element_blank(),
                   legend.position = "none")+
    ggplot2::labs(x ="Age of the species (MY)", y = "Aesthetic value (mean)")
  
  b <- ggplot2::ggplot(data, ggplot2::aes(y = esthe_score_mean, x = Di)) +
    ggplot2::geom_point(shape = 21, alpha = 0.8,
                        ggplot2::aes(fill = log(ED)), color = "gray65") +
    ggplot2::scale_fill_gradientn(colours = colors) +
    ggplot2::geom_smooth(method = "lm", formula = y~x, col = 'gray30') +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text  = ggplot2::element_text(size = 8),
                   axis.title = ggplot2::element_text(size = 10),
                   panel.grid = ggplot2::element_blank(),
                   legend.position = "none") +
    ggplot2::labs(x ="Functional Distinctivness", y = "Aesthetic value (mean)")
  
  
  # Save FIGURE S20 18X9cm 600dpi
  ggplot2::ggsave(filename = here::here("figures_tables", "FIGURE_S18.jpg"),
                  plot = gridExtra::grid.arrange(a, b, ncol = 2), 
                  width = 20, height = 8, units = "cm", dpi = 600, family = "sans")
  
  # ----- 

# SUPPLEMENTARY FIGURE 15 ----- 
  
  data <- read.csv(here::here(res_dir_biodiversity, "02_sptable_biodiv.csv"))
  
# Panels a to c
# Habitat   
  dataha <- data[!is.na(data[,"habitat"]),]
  
  # Ordering by esthe_score median
  new_order_fac  <- with(dataha, tapply(esthe_score, habitat, median))
  new_label      <- names(new_order_fac)[order(new_order_fac, decreasing = TRUE)]
  dataha$habitat <- factor(dataha$habitat, levels = new_label)
  
  # ANOVA model
  model <- lm(esthe_score ~ habitat, data = dataha)
  ANOVA <- aov(model)
  
  # Tukey post hoc test 
  hab_TUKEY        <- TukeyHSD(x = ANOVA, 'habitat', conf.level = 0.95)
  hab_Tukey.levels <- hab_TUKEY[["habitat"]][,4]
  hab_Tukey.labels <- data.frame(multcompView::multcompLetters(hab_Tukey.levels, reversed = FALSE)['Letters'])
  hab_Tukey.labels$treatment <- rownames(hab_Tukey.labels)
  hab_Tukey.labels           <- hab_Tukey.labels[names(new_order_fac)[order(new_order_fac, decreasing = TRUE)],]
  hab_Tukey.labels$pos       <- 1:length(hab_Tukey.labels$Letters)
  
# Water_column  
  # Merge pelagic non-site attached and pelagic site attached species for clarity
  datawc <- data[!is.na(data[,"water_column"]),]
  datawc$water_column <- as.character(datawc$water_column)
  datawc$water_column[datawc$water_column == "pelagic non-site attached"] <- "pelagic"
  datawc$water_column[datawc$water_column == "pelagic site attached"]     <- "pelagic"
  
  # Ordering by esthe_score median
  new_order_fac         <- with(datawc, tapply(esthe_score, water_column, median))
  new_label             <- names(new_order_fac)[order(new_order_fac, decreasing = TRUE)]
  datawc$water_column <- factor(datawc$water_column, as.factor(new_label))
  
  # ANOVA model
  model <- lm(esthe_score ~ water_column, data = datawc)
  ANOVA <- aov(model)
  
  # Tukey
  watcol_TUKEY        <- TukeyHSD(x = ANOVA, 'water_column', conf.level = 0.95)
  watcol_Tukey.levels <- watcol_TUKEY[["water_column"]][,4]
  watcol_Tukey.labels <- data.frame(multcompView::multcompLetters(watcol_Tukey.levels, reversed = TRUE)['Letters'])
  watcol_Tukey.labels$treatment                <- rownames(watcol_Tukey.labels)
  watcol_Tukey.labels     <- watcol_Tukey.labels[names(new_order_fac)[order(new_order_fac,decreasing = TRUE)],]
  watcol_Tukey.labels$pos <- 1:length(watcol_Tukey.labels$Letters)
  
# Diel_activity   
  datadia <- data[!is.na(data[,"diel_activity"]),]
  table(datadia[,"diel_activity"])
  
  model <- lm(datadia$esthe_score ~ datadia$diel_activity)
  ANOVA <- aov(model)

  LABELS <- c('a','b')
  
# Ordering by esthe_score median before drawing the boxplot
  new_order <- with(datadia, reorder(diel_activity, esthe_score, median, na.rm = T))
  
# Save panel a, b, c
  jpeg(here::here("figures_tables", "FIGURE_S15_abc.jpg"),
       width = 20, height = 7, units = "cm", res = 600)
  
  par(mar = c(4, 4, 1, 0.5)) 
  layout(matrix(c(1,2,3), 1, 3, byrow = TRUE))
  
  # habitat
    boxplot(dataha$esthe_score ~ dataha$habitat,
            ylim = c(min(dataha$esthe_score, na.rm = TRUE),
                     1.05*max(dataha$esthe_score, na.rm = TRUE)), 
            col = colors[7] , xlab = "Habitat" , main = "", ylab = "Aesthetic Values",
            outline = FALSE, cex.lab = 1.25, cex.axis = 1, xaxt = 'n') 
    axis(1, labels = c("coral", "rock", "sand", "water\ncolumn"), at = c(1,2,3,4), tick = FALSE)
    abline(h = mean(dataha$esthe_score, na.rm = TRUE), col = colors[2], lty = 2, lwd = 2)
    text(hab_Tukey.labels$pos, 2100, hab_Tukey.labels$Letters)
  
  # water column
    boxplot(datawc$esthe_score ~ datawc$water_column,
            ylim = c(min(datawc$esthe_score, na.rm = TRUE),
                     1.05*max(datawc$esthe_score, na.rm = TRUE)),
            col = colors[7], xlab = "Water column", main = "", ylab = "Aesthetic Values",
            outline = FALSE, cex.lab = 1.25, cex.axis = 1, xaxt = 'n') 
    axis(1, labels = c("demersal", "benthic", "pelagic"), at = c(1,2,3), tick = FALSE)
    abline(h = mean(datawc$esthe_score, na.rm = TRUE), col = colors[2], lty = 2, lwd = 2)
    text(watcol_Tukey.labels$pos,2100, watcol_Tukey.labels$Letters)
  
  # Diel activity
   boxplot(datadia$esthe_score ~ new_order,
            ylim = c(min(datadia$esthe_score, na.rm = TRUE),
                   1.05*max(datadia$esthe_score, na.rm = TRUE)),
            col = colors[7], xlab = "Diel activity", main = "", ylab = "Aesthetic Values",
           outline = FALSE, cex.lab = 1.25, cex.axis = 1, xaxt = 'n') 
    axis(1, labels = c("night", "day"), at = c(1,2), tick = FALSE)
    abline(h = mean(datadia$esthe_score, na.rm = TRUE), col = colors[2], lty = 2, lwd = 2)
    text(c(1:length(unique(datadia$diel_activity))), 2100, LABELS)
  
  dev.off()

  
# Panel d: Trophic_group 
  
  datatrg <- data[!is.na(data[,"trophic_group"]),]
  
  # Ordering by esthe_score median
    new_order_fac <- with(datatrg, tapply(esthe_score, trophic_group, median))
    new_label     <- names(new_order_fac)[order(new_order_fac, decreasing = FALSE)]
    datatrg$trophic_group <- factor(datatrg$trophic_group, as.factor(new_label))
  
  # ANOVA model
    model <- lm(esthe_score ~ trophic_group, data = datatrg)
    ANOVA <- aov(model)
    summary(ANOVA)
    
  # Tukey test
    TUKEY        <- TukeyHSD(x = ANOVA, 'trophic_group', conf.level = 0.95)
    Tukey.levels <- TUKEY[["trophic_group"]][,4]
    Tukey.labels <- data.frame(multcompView::multcompLetters(Tukey.levels, reversed = TRUE)['Letters'])
    Tukey.labels$treatment <- rownames(Tukey.labels)
    Tukey.labels           <- Tukey.labels[names(new_order_fac)[order(new_order_fac, decreasing = TRUE)],]
    Tukey.labels$pos       <- length(Tukey.labels$Letters):1
  
# Save panel d
    jpeg(here::here(res_dir_biodiversity, "FIGURE_S15_d.jpg"),
         width = 20, height = 10, units = "cm", res = 600)
    boxplot(datatrg$esthe_score ~ datatrg$trophic_group,
            ylim = c(min(datatrg$esthe_score, na.rm = TRUE),
                     1.2*max(datatrg$esthe_score, na.rm = TRUE)),
            col = colors[7], xlab = "Aesthetic Values", main = "",
            ylab = "Trophic group", outline = FALSE, horizontal = TRUE, yaxt = "n",
            cex.lab = 0.85, 
            cex.axis = 0.50) 
    abline(v = mean(datatrg$esthe_score, na.rm = TRUE), col = colors[2], lty = 2, lwd = 2)
    text(2400, c(length(unique(datatrg$trophic_group))):1, Tukey.labels$Letters)
    text(2000, Tukey.labels$pos, Tukey.labels$treatment, adj = 0)
    dev.off()
  
# Panels e, f, g, h: max_length, trophic_level, thermal_mp_5min_95max, thermal_95thmax 

  # Max_length 
    datanona <- data[!is.na(data[,"max_length"]),]
    model    <- lm(datanona$esthe_score ~ poly(datanona$max_length, 2))
    summary(model)
    
    a <- ggplot2::ggplot(datanona, ggplot2::aes(y = esthe_score, x = max_length)) +
      ggplot2::geom_point(shape = 19, alpha = 0.8, col = colors[7]) +
      ggplot2::stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1, col = colors[2]) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none",
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank()) +
      ggplot2::labs(x = "Max length", y = "Aesthetic value")
  
  # Trophic_level 
    datanona <- data[!is.na(data[,"trophic_level"]),]
    model    <- lm(datanona$esthe_score ~ datanona$trophic_level)
    summary(model)
    
    b <- ggplot2::ggplot(datanona, ggplot2::aes(y = esthe_score, x = trophic_level)) +
      ggplot2::geom_point(shape = 19, alpha = 0.8, col = colors[7]) +
      ggplot2::geom_smooth(method = "lm", formula = y~x, col = colors[2]) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none",
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank()) +
      ggplot2::labs(x ="Trophic level", y = "Aesthetic value")
  
  # Thermal_mp_5min_95max 
    datanona <- data[!is.na(data[,"thermal_mp_5min_95max"]),]
    model    <- lm(datanona$esthe_score ~ datanona$thermal_mp_5min_95max)
    summary(model)
    
    c <- ggplot2::ggplot(datanona, ggplot2::aes(y = esthe_score, x = thermal_mp_5min_95max)) +
      ggplot2::geom_point(shape = 19, alpha = 0.8, col = colors[7]) +
      ggplot2::geom_smooth(method = "lm", formula = y~x,col = colors[2]) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none",
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank()) +
      ggplot2::labs(x ="Thermal mp 5min 95max", y = "Aesthetic value")
  
  
  # Thermal_95thmax 
    datanona <- data[!is.na(data[,"thermal_95thmax"]),]
    model    <- lm(datanona$esthe_score ~ datanona$thermal_95thmax)
    summary(model)
    
    d <- ggplot2::ggplot(datanona, ggplot2::aes(y = esthe_score, x = thermal_95thmax)) +
      ggplot2::geom_point(shape = 19, alpha = 0.8, col = colors[7]) +
      ggplot2::geom_smooth(method = "lm", formula = y~x,col = colors[2]) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none",
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank())+
      ggplot2::labs(x ="Thermal 95thmax", y = "Aesthetic value")
  
# Save panels efgh
  ggplot2::ggsave(filename = here::here(res_dir_biodiversity, "FIGURE_S15_efgh.jpg"),
                  plot = gridExtra::grid.arrange(a, b, c, d, ncol = 2), 
                  width = 20, height = 12, units = "cm", dpi = 600, family = "sans")
# ----  
  
  
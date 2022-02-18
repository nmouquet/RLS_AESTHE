################################################################################################
#' Phylogenetic diversity
#'
#'This script produces the background of Fig. 4, Fig. S13, Fig. S14 and Table S2 of the 
#'Langlois et al. & Mouquet 2021 paper.
#'
#'@author Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#'        Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'        Alienor Stahl, \email{a.stahl67@@gmail.com},
#'        Florian Baletaud, \email{baletaudflorian@@gmail.com},
#'        François Guilhaumon, \email{francois.guilhaumon@@ird.fr}
#'
#'
#' @date 2021/06/29
################################################################################################

# Load ----
  
  require(dplyr) # for the pipe
  source(here::here("R", "functions_biodiv.R"))  
  species_table <- read.csv(here::here(res_dir_deep, "03_species_table.csv"))
  
# ----

# Classification -----

  # This gets the classification for all species in the bdd
  # The first line takes more than an hour whith interactions required (taxize)
  # So once it has been computed, if the bdd doesn't change, no need to re-run.

  classif       <- get_classif(as.character(species_table$sp_name))
  
  write.csv(x = classif, file = here::here(res_dir_biodiversity, "01_classification.csv"),
            row.names = FALSE)

  classif           <- read.csv(here::here(res_dir_biodiversity, "01_classification.csv"))
  colnames(classif) <- c("sp_name", colnames(classif[-1]))
  species_table     <- merge(species_table, classif, by = "sp_name")
  
  # Scaridae is a subfamily of the Labridae (F. Leprieur personal comment)
  species_table$family[species_table$family == "Scaridae"] <- "Labridae"
  
  rm(classif)

# ----

# Generate Phylogenetic tree ----

  phylo_table          <- species_table[, c("sp_name", "esthe_score", "family", "order")]
  phylo_table$family   <- factor(phylo_table$family)
  phylo_table$sp_name  <- as.character(phylo_table$sp_name)
  phylo_table$tip_name <- paste(phylo_table$family, phylo_table$sp_name, sep = "_")

# Download the phylogenetic tree for all fishes to compute the age
    set100_all  <- fishtree::fishtree_complete_phylogeny(phylo_table$sp_name)
    # Dropping names not in list
    set100 <- parallel::mclapply(set100_all,function(x){
      ape::drop.tip(x, x$tip.label[!is.element(x$tip.label,as.character(phylo_table$sp_name))])
    }, mc.cores = 7)

# Save
  save(set100, file =  here::here(res_dir_biodiversity, "01_set100.RData"))
  
  rm(set100_all)
  
# ----
  
# Families ----
  load(here::here(res_dir_biodiversity, "01_set100.RData"))
  
  temp        <- set100[[1]]$tip.label
  
  missing_all <- setdiff(phylo_table$sp_name, temp) # 41 species missing in the tree
  
# Remove the species not found by fishtree
  phylo_table        <- phylo_table[phylo_table$sp_name %in% set100[[1]]$tip.label,]
  phylo_table$family <- droplevels(phylo_table$family) 

# Number of species in family
# This step takes about one hour with interaction required so once runed once,
# so if the database does not change, you should not run it every time.

  nb_sp_fam <- data.frame(family  = levels(species_table$family),
                          totsp   = vector(length = length(levels(species_table$family)),
                                           mode   = "numeric"),
                          oursp   = vector(length = length(levels(species_table$family)),
                                           mode   = "numeric"),
                          percent = vector(length = length(levels(species_table$family)),
                                           mode   = "numeric"))
  # In the entire family and in our data set
  for(i in 1:nrow(nb_sp_fam)){
    nb_sp_fam$oursp[i] <- length(species_table$sp_name[which(species_table$family ==
                                                             nb_sp_fam$family[i])])
    tryCatch({
      taxofam              <- taxize::downstream(sci_id = as.character(nb_sp_fam$family[i]),
                                                 db     = "worms",
                                                   downto = "species")
        nb_sp_fam$totsp[i]   <- nrow(taxofam[[1]])
        nb_sp_fam$percent[i] <- (nb_sp_fam$oursp[i]/nb_sp_fam$totsp[i])*100
      }, # eo tryCatch
      error = function(e){
        nb_sp_fam$totsp[i]   <- "NA"
        nb_sp_fam$percent[i] <- "NA"
      })# eo error
    }# eo for i

  species <- species_table[,c("family", "sp_name", "esthe_score")]

  for (i in 1:length(unique(nb_sp_fam$family))){

    fam <- as.character(unique(nb_sp_fam$family)[i])

    nb_sp_fam$mean_esth[i] <- mean(species$esthe_score[which(species$family == fam)])
    nb_sp_fam$sd_esth[i] <- sd(species$esthe_score[which(species$family == fam)])
    nb_sp_fam$min_esth[i] <- min(species$esthe_score[which(species$family == fam)])
    nb_sp_fam$max_esth[i] <- max(species$esthe_score[which(species$family == fam)])

  }# eo for i

  write.csv(x         = nb_sp_fam,
            file      = here::here(res_dir_biodiversity, "01_nb_sp_fam.csv"),
            row.names = FALSE)

  nb_sp_fam  <- read.csv(here::here(res_dir_biodiversity, "01_nb_sp_fam.csv"))
  
  rm(temp, i, taxofam)
  
# ----

# Species ages ----

# Compute the age of all species found by fishtree (for each of the 100 trees)
  ages      <- do.call(cbind, lapply(set100, get_ages)) 
  ages_mean <- apply(ages, 1, mean, na.rm = T) # mean across the trees
  ages_mean <- ages_mean[match(phylo_table$sp_name, names(ages_mean))]
  phylo_table$ages_mean <- ages_mean

# Add age info to species table
  missing_all         <- data.frame(sp_name = missing_all, ages_mean = NA)
  ages_mean           <- data.frame(sp_name = names(ages_mean), ages_mean = ages_mean)
  rownames(ages_mean) <- NULL
  ages_all            <- rbind(ages_mean, missing_all)
  species_table       <- merge(species_table, ages_all, merge = "sp_name")

# Linear relationship between the aesthetic score and the age of the species (both log transfomed)
  modlog <- summary(lm(log(phylo_table$esthe_score) ~ log(phylo_table$ages_mean)))

  rm(ages, ages_mean, modlog, ages_all, missing_all)
  
# ----

# Pagel's lambda ----

  phylo_table_pagel <- phylo_table[phylo_table$sp_name %in% set100[[1]]$tip.label,]
  trait             <- phylo_table_pagel$esthe_score
  names(trait)      <- phylo_table_pagel$sp_name

  # Create empty matrix
  lambda_100 <- as.data.frame(matrix(nrow = length(unique(phylo_table_pagel$family)) + 1,
                                        ncol = 201))
  # First column is families name
  lambda_100[1:(length(unique(phylo_table_pagel$family))+1),1] <-
    c(as.character(unique(phylo_table_pagel$family)), "Tree")
  colnames(lambda_100) <- c("family", 1:200)
  
# Compute Pagel's lambda for the 100 trees
  start.time <- Sys.time()
  for (j in 2: (length(set100) + 1)) {
    cat(paste('tree number', j-1, "\n"))
    set_100_elague <- set100[[j-1]]
    
    # Pagel's lambda inside each family
    for (i in 1:length(unique(phylo_table_pagel$family))) {
      arbre <- ape::drop.tip(set_100_elague, set_100_elague$tip.label[!is.element(
        set_100_elague$tip.label, as.character(phylo_table_pagel$sp_name)[
          which(phylo_table_pagel$family == lambda_100$family[i])])])
      data  <- phylo_table_pagel
      
      # Aesthetic
      dataE        <- data$esthe_score[match(arbre$tip.label, data$sp_name)]
      names(dataE) <- data$sp_name[match(arbre$tip.label, data$sp_name)]
      SGNL         <- tryCatch({phytools::phylosig(tree = arbre, x = dataE,
                                                   method = "lambda", test = TRUE)},
                               error = function(e){
                                 NAres <- list(lambda = NA, P = NA)
                                 NAres
                               }) # eo tryCatch
      lambda_100[i,j]       <- SGNL$lambda
      lambda_100[i,(100+j)] <- SGNL$P
    } # eo for i
    
    # Pagel's lambda for the entire tree
    esthe        <- phylo_table_pagel$esthe_score
    names(esthe) <- phylo_table_pagel$sp_name
    SGNL         <- phytools::phylosig(set_100_elague, esthe, method = "lambda", test = TRUE)
    lambda_100[length(unique(phylo_table_pagel$family))+1,j]        <- SGNL$lambda
    lambda_100[length(unique(phylo_table_pagel$family))+1, (100+j)] <- SGNL$P
  } # eo for j
  
  end.time   <- Sys.time()
  time.taken <- end.time - start.time
  time.taken

# Final lambda (mean of the 100)
  pagel_lambda_E <- as.data.frame(matrix(nrow = length(unique(phylo_table_pagel$family)) + 1,
                                         ncol = 4))
  rownames(pagel_lambda_E) <- c(as.character(unique(phylo_table_pagel$family)), "Tree")
  names(pagel_lambda_E)    <- c("mean_lambda","se_lambda","mean_pval","se_pval")

  for (i in 1:nrow(lambda_100)){
    # Aesthetic
    lambda <- c()
    pvalue <- c()
    for (j in 2:101){
      lambda <- c(unlist(lambda),lambda_100[i,j])
      pvalue <- c(unlist(pvalue),lambda_100[i,(100+j)])
    } # eo for j
    
    pagel_lambda_E$mean_lambda[i] <- mean(lambda, na.rm = TRUE)
    pagel_lambda_E$se_lambda[i]   <- sd(lambda, na.rm = TRUE)
    pagel_lambda_E$mean_pval[i]   <- mean(pvalue, na.rm = TRUE)
    pagel_lambda_E$se_pval[i]     <- sd(pvalue, na.rm = TRUE)
  } # eo for i

  pagel_lambda_E$meanlambda_rounded <- round(pagel_lambda_E$mean_lambda, 3)
  pagel_lambda_E$selambda_rounded   <- round(pagel_lambda_E$se_lambda, 3)
  pagel_lambda_E$meanpval_rounded   <- round(pagel_lambda_E$mean_pval, 3)
  pagel_lambda_E$sepval_rounded     <- round(pagel_lambda_E$se_pval, 3)
  
  pagel_lambda_E$size_fam <- c(unlist(
    lapply(unique(phylo_table_pagel$family),function(x){
    l        <- length(phylo_table_pagel$tip_name[which(phylo_table_pagel$family == x)])
    names(l) <- x
    l})),
    length(phylo_table_pagel$tip_name))
  
  pagel_lambda_E$percent_fam <- c(unlist(
    lapply(unique(phylo_table_pagel$family), function(x){
      l        <- nb_sp_fam$percent[which(nb_sp_fam$family == x)]
      names(l) <- x
      l})),
    length(phylo_table_pagel$tip_name))

  # Names of the families
  pagel_lambda_E$family <- rownames(pagel_lambda_E)
  pagel_lambda_E        <- pagel_lambda_E[,c(ncol(pagel_lambda_E), 1:(ncol(pagel_lambda_E)-1))]

  # Deal with the NAs
  length(which(is.na(pagel_lambda_E$meanlambda_rounded)))
  unique(pagel_lambda_E$size_fam[which(is.na(pagel_lambda_E$meanlambda_rounded))])
  # pagel's lambda can not be computed for the families with only 1 or 2 species
  # remove those families from the table
  pagel_lambda_E <- pagel_lambda_E[-which(is.na(pagel_lambda_E$meanlambda_rounded)),]
  
  # Save the table
  write.csv(pagel_lambda_E, file = here::here(res_dir_biodiversity, "01_pagel_all.csv"),
            row.names = FALSE)
# ----
  
# TABLE S2 ----
  # Remove families with 5 or less species beacause they are too small samples
  # to have a correct measure
  pagel_all <- read.csv(here::here(res_dir_biodiversity, "01_pagel_all.csv"))
  pagelred <- pagel_all[which(pagel_all$size_fam >5),
                         c("family", "meanlambda_rounded", "selambda_rounded", "meanpval_rounded",
                           "sepval_rounded", "size_fam", "percent_fam")]

  write.csv(pagelred, file = here::here("figures_tables", "TABLE_S2.csv"),
            row.names = FALSE)

  rm(pagelred, pagel_lambda_E, i, j, end.time, time.taken, start.time, lambda_100,
     trait, phylo_table_pagel, nb_sp_fam)
  
# ----

# Evolutionary distinctiveness ----
# load the 100 trees
  load(here::here(res_dir_biodiversity, "01_set100.RData"))
  
# Compute the ED of each species on each of the 100 trees (Takes about 3 hours)
  start.time     <- Sys.time()
  evol_distc_100 <- do.call(cbind, parallel::mclapply(set100, function(tree){
    ed_tree <- picante::evol.distinct(tree = tree, type = "fair.proportion", scale = FALSE,
                                      use.branch.lengths = TRUE)
    res        <- ed_tree[,2]
    names(res) <- ed_tree$Species
    res
  }, mc.cores = 7))
  end.time       <- Sys.time()
  time.taken     <- end.time - start.time
  time.taken

# Get the mean and sd of  the 100 ed for each sp
  mean_ed <- apply(evol_distc_100, 1, mean)
  sd_ed   <- apply(evol_distc_100, 1, sd)
  
# Format the final table
  final_ed                <- data.frame(mean_ed, sd_ed)
  final_ed$sp_name        <- rownames(final_ed)
  colnames(final_ed)[1:2] <- c("ED", "ED_sd")
  final_ed                <- final_ed[,c(3,1,2)]
  
# Add NA for the species for which we don't have the phylogeny
  naed_names <- setdiff(species_table$sp_name, final_ed$sp_name)
  na_ed      <- data.frame(sp_name = naed_names,
                           ED      = rep(x = NA, times = length(naed_names)),
                           ED_sd   = rep(x = NA, times = length(naed_names)))
# Combine the two
  ed_table <- rbind(final_ed, na_ed)

# Save
  species_table <- merge(species_table, ed_table, merge = "sp_name")
  write.csv(x         = species_table,
            file      = here::here(res_dir_biodiversity, "01_sptable_phylo.csv"),
            row.names = FALSE)
  
  rm(na_ed, naed_names, final_ed, sd_ed, mean_ed, time.taken, end.time, start.time, evol_distc_100)
# ----
  
# Figure S1 N ----
  
  ed_table <- read.csv(here::here(res_dir_biodiversity, "01_sptable_phylo.csv"))
  
  table       <- ed_table[-which(is.na(ed_table$ED)),]
  rownames(table) <- table$sp_name
  modlog      <- summary(lm(table$esthe_score ~ log(table$ED), na.action = na.omit))
  
  slope_modlog=modlog$coefficients[2,1]
  intercept_modlog=modlog$coefficients[1,1]
  
  fit_esth_ED <- do.call(rbind,parallel::mclapply(1:100, function(id){
    fit_LB = phylolm(esthe_score~log(ED),data=table,phy=set100[[id]],model="lambda")
    res_fit_LB <- summary(fit_LB)
    cbind.data.frame(tree=id,p.value_LB=res_fit_LB$coefficients[2,4],intercept=res_fit_LB$coefficients[1,1],slope=res_fit_LB$coefficients[2,1])
    
  },mc.cores = 7))
  
  mean_p_fit_esth_ED =mean(fit_esth_ED$p.value_LB)
  sd_p_fit_esth_ED =sd(fit_esth_ED$p.value_LB)
  mean_intercept_fit_esth_ED =mean(fit_esth_ED$intercept)
  mean_slope_fit_esth_ED =mean(fit_esth_ED$slope)
  sd_slope_fit_esth_ED =sd(fit_esth_ED$slope)
  
  harmonicmeanp::hmp.stat(fit_esth_ED$p.value_LB)
  
  evol_dist   <-
    ggplot2::ggplot(table, ggplot2::aes(x = ED, y = esthe_score)) +
    ggplot2::geom_point(shape = 19, alpha = 0.8, col = colors[9]) +
    ggplot2::labs(y = "Aesthetic value", x = "Evolutionary Distinctiveness (MY)") +
    geom_abline(intercept=intercept_modlog, slope=slope_modlog,col="#8b8b8b")+
    geom_abline(intercept=mean_intercept_fit_esth_ED, slope=mean_slope_fit_esth_ED,size=0.5,linetype = "dashed",col="#8b8b8b")+
    ggplot2::scale_x_continuous(trans  ='log',
                                breaks = c(10, 50, 100, 150),
                                labels = c("10", "50", "100", "150")) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text  = ggplot2::element_text(size = 8, family = "serif"),
                   axis.title = ggplot2::element_text(size = 10, family = "serif"),
                   panel.grid = ggplot2::element_blank())

    ggplot2::ggsave(filename = here::here("figures_tables", "FIGURE_N.jpg"),
                    plot = evol_dist, 
                    width = 9, height = 9, units = "cm", dpi = 600)
    
  rm(evol_dist, modlog, table, esthe_table, ed_table, set100)

# ----
  
# Create Esthe classes and the matrix of species in Esthe class ----
  
  # order according to esthe score
  phylo_table       <- phylo_table[order(phylo_table$esthe_score),]
  nb_split          <- 10 # 10 classes
  
  # define the classes range
  splits            <- split(phylo_table$sp_name,
                             Hmisc::cut2(phylo_table$esthe_score, g = nb_split))
  phylo_table$split <- unlist(lapply(1:nb_split, function(x, spl = splits){
    split <- rep(x, length = length(spl[[x]]))
    split
  })) # attribute a class to each species
  
  rm(splits, nb_split)
  
# ---- 

# Phylo tree (FIGURE 4) ----

# Order tree
  load(here::here(res_dir_biodiversity, "01_set100.RData"))
  usedTree <- reorder(set100[[100]], order = "cladewise")

# Set the color bar from blue to red
    species       <- phylo_table[phylo_table$sp_name %in% set100[[100]]$tip.label,]
    trait         <- species$esthe_score
    trait2        <- order(trait)
    names(trait2) <- species$sp_name
    obj2          <- phytools::contMap(usedTree, trait2, plot = FALSE, sig = 2, res = 100)
    obj2          <- phytools::setMap(obj2, invert = TRUE) # invert color map

# Plot the tree
    jpeg(here::here("figures_tables", "FIGURE_4.jpg"),
         width = 20, height = 20, units = "cm", res = 500)
    plot(obj2, ftype = "off", type = "fan", outline = FALSE, legend = FALSE,
         fsize = c(0.2,1), lwd = 1.5, offset = 5, xlim = c(-280,280), ylim = c(-280, 280))
    phytools::add.color.bar(100, obj2$cols, title = "Rank",
                  lims = obj2$lims, digits = 3, prompt = FALSE, x = 150,
                  y = -250, lwd = 4, fsize = 1, subtitle = "")
    dev.off()

    rm(obj2, trait2, trait, usedTree, set100, species)

# ----
    
# Difference of aesthetic value between the families
# (FIGURE S1 O) ----
    
  species_table <- read.csv(here::here(res_dir_biodiversity, "01_sptable_phylo.csv"))
  nb_sp_fam     <- read.csv(here::here(res_dir_biodiversity, "01_nb_sp_fam.csv"))
  
  for(i in 1:nrow(species_table)){
      species_table$nbsp[i] <- 
        as.character(nb_sp_fam$oursp[
          which(nb_sp_fam$family == as.character(species_table$family)[i])])
  }
  species_table$famnb <- paste0(species_table$family, " (", species_table$nbsp, ")")
    
  # Order the families according to their median aesthetic value
  new_order_fac       <- with(species_table, tapply(esthe_score, famnb, median))
  new_label           <- names(new_order_fac)[order(new_order_fac, decreasing = FALSE)]
  species_table$famnb <- factor(species_table$famnb, as.factor(new_label))
  
  # Select the families with at least 10 species
  temp         <- table(species_table$famnb)
  fam_top      <- names(temp[which(as.numeric(temp) >= 10)])
  red_sp       <- species_table[which(species_table$famnb %in% fam_top),]
  red_sp$famnb <- droplevels(red_sp$famnb) # remove unused levels
  
  # Order again with only the selected levels
  new_order_fac <- with(red_sp, tapply(esthe_score, famnb, median))
  new_label     <- names(new_order_fac)[order(new_order_fac, decreasing = FALSE)]
  red_sp$famnb  <- factor(red_sp$famnb, as.factor(new_label))
  
# Anova
  model <- lm(esthe_score ~ famnb, data = red_sp)
  ANOVA <- aov(model)
    
# Post-hoc test
  TUKEY        <- TukeyHSD(x = ANOVA, 'famnb', conf.level = 0.95)
  Tukey.levels <- TUKEY[["famnb"]][,4]
  Tukey.labels <- data.frame(multcompView::multcompLetters(Tukey.levels, 
                                                           reversed = TRUE)['Letters'])
  Tukey.labels$treatment <- rownames(Tukey.labels)
  Tukey.labels           <- Tukey.labels[names(new_order_fac)[order(new_order_fac, 
                                                                    decreasing = TRUE)],]
  Tukey.labels$pos       <- length(Tukey.labels$Letters):1
  
# Plot
  esthe_fam_plot <- ggplot2::ggplot(red_sp, ggplot2::aes(x = famnb, y = esthe_score)) +
    ggplot2::geom_abline(intercept = mean(red_sp$esthe_score), 
                         slope = 0, col = "red", lty = "dashed") +
    ggplot2::geom_abline(intercept = 1250, slope = 0, col = "gray", lty = "dotted") +
    ggplot2::geom_abline(intercept = 1750, slope = 0, col = "gray", lty = "dotted") +
    ggplot2::geom_boxplot(fill = colors[7], alpha = 0.6) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(breaks = c(1250,1500, 1750), limits = c(1100, 2400)) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.title = ggplot2::element_text(family = "serif"),
                   axis.text  = ggplot2::element_text(family = "serif"),
                   axis.text.y = ggplot2::element_text(face   = "italic", 
                                                       family = "serif",
                                                       size   = 6.5)) +
    ggplot2::xlab("Families") +
    ggplot2::ylab("Aesthetic value") +
    ggplot2::geom_text(inherit.aes = FALSE, data = Tukey.labels, family = "serif",
                         ggplot2::aes(x = pos, y = 2050, label = Letters), hjust = 0, size = 3)
    
# Save plot
  ggplot2::ggsave(filename = here::here("figures_tables", "FIGURE_O.jpg"), 
                  plot = esthe_fam_plot, family = "serif", dpi = 600)
  
  rm(esthe_fam_plot, red_sp, Tukey.labels, Tukey.levels, TUKEY, ANOVA, model, new_label,
     new_order_fac, fam_top, temp, i)
# ----
  
# Compute Pagel's lambda with mean aesthetic values ----
  
  phylo_table <- read.csv(here::here("results","biodiversity", "01_sptable_phylo.csv"))
  load(here::here(res_dir_biodiversity, "01_set100.RData"))
  
  phylo_table_pagel <- phylo_table[phylo_table$sp_name %in% set100[[1]]$tip.label,]
  trait             <- phylo_table_pagel$esthe_score_mean
  names(trait)      <- phylo_table_pagel$sp_name
  
  # Create empty matrix
  lambda_100 <- as.data.frame(matrix(nrow = length(unique(phylo_table_pagel$family)) + 1,
                                     ncol = 201))
  # First column is families name
  lambda_100[1:(length(unique(phylo_table_pagel$family))+1),1] <-
    c(as.character(unique(phylo_table_pagel$family)), "Tree")
  colnames(lambda_100) <- c("family", 1:200)
  
  # Compute Pagel's lambda for the 100 trees
  start.time <- Sys.time()
  for (j in 2: (length(set100) + 1)) {
    cat(paste('tree number', j-1, "\n"))
    set_100_elague <- set100[[j-1]]
    
    # Pagel's lambda inside each family
    for (i in 1:length(unique(phylo_table_pagel$family))) {
      arbre <- ape::drop.tip(set_100_elague, set_100_elague$tip.label[!is.element(
        set_100_elague$tip.label, as.character(phylo_table_pagel$sp_name)[
          which(phylo_table_pagel$family == lambda_100$family[i])])])
      data  <- phylo_table_pagel
      
      # Aesthetic
      dataE        <- data$esthe_score[match(arbre$tip.label, data$sp_name)]
      names(dataE) <- data$sp_name[match(arbre$tip.label, data$sp_name)]
      SGNL         <- tryCatch({phytools::phylosig(tree = arbre, x = dataE,
                                                   method = "lambda", test = TRUE)},
                               error = function(e){
                                 NAres <- list(lambda = NA, P = NA)
                                 NAres
                               }) # eo tryCatch
      lambda_100[i,j]       <- SGNL$lambda
      lambda_100[i,(100+j)] <- SGNL$P
    } # eo for i
    
    # Pagel's lambda for the entire tree
    esthe        <- phylo_table_pagel$esthe_score_mean
    names(esthe) <- phylo_table_pagel$sp_name
    SGNL         <- phytools::phylosig(set_100_elague, esthe, method = "lambda", test = TRUE)
    lambda_100[length(unique(phylo_table_pagel$family))+1,j]        <- SGNL$lambda
    lambda_100[length(unique(phylo_table_pagel$family))+1, (100+j)] <- SGNL$P
  } # eo for j
  
  end.time   <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  
  # Final lambda (mean of the 100)
  pagel_lambda_E <- as.data.frame(matrix(nrow = length(unique(phylo_table_pagel$family)) + 1,
                                         ncol = 4))
  rownames(pagel_lambda_E) <- c(as.character(unique(phylo_table_pagel$family)), "Tree")
  names(pagel_lambda_E)    <- c("mean_lambda","se_lambda","mean_pval","se_pval")
  
  for (i in 1:nrow(lambda_100)){
    # Aesthetic
    lambda <- c()
    pvalue <- c()
    for (j in 2:101){
      lambda <- c(unlist(lambda),lambda_100[i,j])
      pvalue <- c(unlist(pvalue),lambda_100[i,(100+j)])
    } # eo for j
    
    pagel_lambda_E$mean_lambda[i] <- mean(lambda, na.rm = TRUE)
    pagel_lambda_E$se_lambda[i]   <- sd(lambda, na.rm = TRUE)
    pagel_lambda_E$mean_pval[i]   <- mean(pvalue, na.rm = TRUE)
    pagel_lambda_E$se_pval[i]     <- sd(pvalue, na.rm = TRUE)
  } # eo for i
  
  pagel_lambda_E$meanlambda_rounded <- round(pagel_lambda_E$mean_lambda, 3)
  pagel_lambda_E$selambda_rounded   <- round(pagel_lambda_E$se_lambda, 3)
  pagel_lambda_E$meanpval_rounded   <- round(pagel_lambda_E$mean_pval, 3)
  pagel_lambda_E$sepval_rounded     <- round(pagel_lambda_E$se_pval, 3)
  
  pagel_lambda_E$size_fam <- c(unlist(
    lapply(unique(phylo_table_pagel$family),function(x){
      l        <- length(phylo_table_pagel$tip_name[which(phylo_table_pagel$family == x)])
      names(l) <- x
      l})),
    length(phylo_table_pagel$tip_name))
  
  pagel_lambda_E$percent_fam <- c(unlist(
    lapply(unique(phylo_table_pagel$family), function(x){
      l        <- nb_sp_fam$percent[which(nb_sp_fam$family == x)]
      names(l) <- x
      l})),
    length(phylo_table_pagel$tip_name))
  
  # Names of the families
  pagel_lambda_E$family <- rownames(pagel_lambda_E)
  pagel_lambda_E        <- pagel_lambda_E[,c(ncol(pagel_lambda_E), 1:(ncol(pagel_lambda_E)-1))]
  
  # Deal with the NAs
  length(which(is.na(pagel_lambda_E$meanlambda_rounded)))
  unique(pagel_lambda_E$size_fam[which(is.na(pagel_lambda_E$meanlambda_rounded))])
  # pagel's lambda can not be computed for the families with only 1 or 2 species
  # remove those families from the table
  pagel_lambda_E <- pagel_lambda_E[-which(is.na(pagel_lambda_E$meanlambda_rounded)),]
  
  # Save the table
  write.csv(pagel_lambda_E, file = here::here(res_dir_biodiversity, "01_pagel_mean.csv"),
            row.names = FALSE)
  
# ----

# Linear relationship between the mean aesthetic score and the age of the species (both log transfomed)
  
  modlog <- summary(lm(log(phylo_table$esthe_score_mean) ~ log(phylo_table$ages_mean)))
  
  rm(ages, ages_mean, modlog, ages_all, missing_all)
  
# ----
    
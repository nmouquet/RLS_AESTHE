###################################################################################################
#' Conservation status
#'
#'Produces Figure 5a of the Langlois et al. & Mouquet 2021 paper.
#'
#'
#' @date 2021/03/01
##################################################################################################

source(here::here("R", "functions_cons.R"))

# Load species table ----
  
  species_table <- read.csv(here::here(res_dir_biodiversity, "02_sptable_biodiv.csv"))
  
# ----

# Get IUCN status from fishbase ----
  
  get_fishbase_data <- function(x = "Regalecus-glesne"){

    #url2 <- paste("http://www.fishbase.de/summary/",x,".html",sep="")
    url2      <- paste("http://www.fishbase.se/summary/",x,".html",sep="")
    c         <- XML::htmlParse(RCurl::getURLContent(url2, followlocation=TRUE))
    link_list <- XML::getHTMLLinks(c, externalOnly = TRUE, xpQuery = "//a/@href", baseURL = docName(c))

    if(length(link_list) == 0){
      stop(paste(x, " is not an accepted name in fishbase, check for spelling mistakes and/or synonyms", sep = ""))
    }

    a1 <- XML::getNodeSet(c, "//div ")
    a  <- XML::getNodeSet(c, "//span ")
    rm(c)

    if (length(a)!=0){
      d  <- XML::xmlValue(a[[which.max(sapply(lapply(a, XML::xmlValue), function(x){regexec(pattern="Ecology", x)[[1]][1]}))+2]])
      m  <- regmatches(d,gregexpr(pattern = "[-[:alpha:]]+;", d))
      m1 <- regmatches(d,gregexpr(pattern = "[[:alpha:]]+", d))[[1]]
      m  <- gsub(";", "", unlist(m))

      List_env1 <- c("Marine","Freshwater","brackish")
      List_env2 <- c("bathydemersal", "bathypelagic", "benthopelagic","benthopelagic.","demersal","demersal.",
                     "pelagic", "pelagic-neritic", "pelagic-oceanic", "reef-associated")
      clim      <- c("Tropical","Temperate","Boreal","Subtropical","Deep-water")

      env1 <- paste(m[which(is.element(m,List_env1)==T)],collapse="_")

      env2 <- m1[which(is.element(m1,List_env2)==T)]
      env2_1 <-  m[which(is.element(m,List_env2)==T)]

      w_IUCN  <- which(sapply(lapply(a1, XML::xmlValue), function(x){regexec(pattern="IUCN", x)[[1]][1]})>0)
      if(length(w_IUCN) == 0){
        IUCN_status = NA
      } else {
        d1_IUCN     <- XML::xmlValue(a1[[w_IUCN[length(w_IUCN)]]])
        IUCN        <- unlist(regmatches(d1_IUCN,gregexpr(pattern= "[[:alpha:]]+)", d1_IUCN)))
        IUCN_status <- sub(pattern="[[:punct:]]",replacement="",IUCN[1] )
      } # end of ifelse

      rm(a1)

      res <- data.frame(c(IUCN_status = IUCN_status))

      rownames(res) = x

    } else {

      IUCN_status = "A_verifier"

      res <- data.frame(c(IUCN_status = IUCN_status))
      rownames(res) = x
    }

    return(res)

  }

  sp_list          <- data.frame(worms = gsub("_", "-", species_table$sp_name))
  sp_list$fishbase <- as.character(sp_list$worms)
  sp_list$fishbase[which(as.character(sp_list$worms) =="Pseudocaranx-georgianus")]         <- "Pseudocaranx-dentex"
  sp_list$fishbase[which(as.character(sp_list$worms) =="Stichaeus-punctatus-punctatus")]   <- "Stichaeus-punctatus"
  sp_list$fishbase[which(as.character(sp_list$worms) =="Tylosurus-crocodilus-crocodilus")] <- "Tylosurus-crocodilus"
  sp_list$fishbase[which(as.character(sp_list$worms) =="Platybelone-argalus-argalus")]     <- "Platybelone-argalus"

  start.time <- Sys.time()
  data_iucn  <- do.call(rbind, lapply(sp_list$fishbase, get_fishbase_data))
  end.time   <- Sys.time()
  time.taken <- end.time - start.time
  time.taken

  data_iucn$fishbase  <- rownames(data_iucn)
  colnames(data_iucn) <- c("iucn_code", "fishbase")
  data_iucn           <- merge(data_iucn, sp_list, by = "fishbase")
  data_iucn$sp_name   <- gsub("-", "_", data_iucn$worms)
  data_iucn           <- data_iucn[,c("sp_name", "iucn_code")]

  species_table       <- merge(species_table, data_iucn, by = "sp_name")

  write.csv(species_table, file = here::here(res_dir_conservation, "01_sp_table_cons.csv"), row.names = FALSE)

  species_table <- read.csv(here::here(res_dir_conservation, "01_sp_table_cons.csv"))
  
# ----
  
# Group IUCN classes into three groups ----
# threatened, non threatened and not evaluated 
  species_table$iucn_code <- toupper(species_table$iucn_code)
  species_table$iucn_code[which(is.na(species_table$iucn_code))] <- "NE"
  
  kw   <- kruskal.test(esthe_score ~ iucn_code, data = species_table)
  dunn <- dunnTestExtra(esthe_score ~ as.factor(iucn_code), dat = species_table, metho = "bh")
  
  thr  <- c("CR", "VU", "EN")
  nthr <- c("LC", "LR/lc", "NT")
  nev  <- c("NE", "DD")
  
  species_table$threats <- vector(length = nrow(species_table))
  
  # Regroup the IUCN statuts into three groups
  for(i in 1 : nrow(species_table)){
    if(species_table$iucn_code[i] %in% thr) {species_table$threats[i] <- "thr"}
    if(species_table$iucn_code[i] %in% nthr){species_table$threats[i] <- "nthr"}
    if(species_table$iucn_code[i] %in% nev) {species_table$threats[i] <- "nev"}
  }
  
  rm(thr, nthr, nev, i)
  
  species_table$threats <- factor(species_table$threats,c("thr", "nthr", "nev"))
  
# ----
  
# FIGURE 5 panel a ----
  
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
  plot <- ggplot2::ggplot(species_table, 
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
      axis.title.y      = ggplot2::element_text(color = "black", size = 10),
      axis.title.x      = ggplot2::element_text(color = "black", size = 10),
      axis.line.x       = ggplot2::element_line(linetype = "blank"),
      axis.text.x       = ggplot2::element_text(size = 9), 
      axis.text.y       = ggplot2::element_text(size = 8),
      axis.ticks.x      = ggplot2::element_blank()) +
    ggplot2::labs(x = "IUCN status", y = "Aesthetic values")+ 
    ggplot2::geom_text(x = "thr", y = 2100, label = "a", col = "#5b5b5b") +
    ggplot2::geom_text(x = "nev", y = 2100, label = "b", col = "#5b5b5b") +
    ggplot2::geom_text(x = "nthr", y = 2100, label = "c", col = "#5b5b5b")
  
  ggplot2::ggsave(plot = plot,
                  filename = here::here("figures_tables", "FIGURE_5a.jpg"), 
                  width = 10, height = 8, units = "cm", dpi = 320)
  
#----
  
# SUPPLEMENTARY FIGURE 19 ----
  
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
  plot <- ggplot2::ggplot(species_table, 
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
      axis.title.y      = ggplot2::element_text(color = "black", size = 8),
      axis.title.x      = ggplot2::element_text(color = "black", size = 8),
      axis.line.x       = ggplot2::element_line(linetype = "blank"),
      axis.text.x       = ggplot2::element_text(size = 8), 
      axis.text.y       = ggplot2::element_text(size = 6),
      axis.ticks.x      = ggplot2::element_blank()) +
    ggplot2::labs(x = "IUCN status", y = "Aesthetic value (mean)")+ 
    ggplot2::geom_text(x = "thr", y = 2100, label = "a", col = "#5b5b5b") +
    ggplot2::geom_text(x = "nev", y = 2100, label = "b", col = "#5b5b5b") +
    ggplot2::geom_text(x = "nthr", y = 2100, label = "c", col = "#5b5b5b")
  
  ggplot2::ggsave(plot = plot,
                  filename = here::here("figures_tables", "FIGURE_S19.jpg"), 
                  width = 10, height = 8, units = "cm", dpi = 320)
  
  #----
  
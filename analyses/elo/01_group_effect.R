###################################################################################################
#' Socio cultural background effect
#'
#'This script analyses if the scocio-cultural background pf the judges of the online survey has an
#'effect on the aesthetic score attribute to the photographs and if yes, quantifies this effect
#'
#'Produces Figure S7, Figure S8 and Table S1 of the Langlois et al. & Mouquet 2021 paper.
#'
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         François Guilhaumon, \email{francois.guilhaumon@@ird.fr}
#'
#' @date 2021/06/29
##################################################################################################

# Load function and data ----

  nbc <- 10
  require(dplyr)
  
  # load data
  load(file = here::here("data", "table_elo_judge.RData"))
  
  # source functions
  source(here::here("R", "functions_elo.R"))

# ----

# Replicate the answers of the children for the questions they did not answer ----

  start.time <- Sys.time()

  for(i in 1:nrow(table_elo_judge)){
    if(table_elo_judge$age[i] <= 14){
      table_elo_judge$distance_sea[i]      <- table_elo_judge$childhood_distance_sea[i]
      table_elo_judge$place[i]             <- table_elo_judge$childhood_place[i]
      table_elo_judge$frequency_nature[i]  <- table_elo_judge$childhood_frequency_nature[i]
      table_elo_judge$knowledge_biology[i] <- table_elo_judge$knowledge_fish[i]
      table_elo_judge$education[i]         <- "A" # The children did not answer the education
                                                  # question ==> they are "A" = "No qualification"
      } # eo if
    } # eo for i

  end.time   <- Sys.time()
  time.taken <- end.time - start.time
  time.taken

  rm(start.time, i, end.time, time.taken)

# ----

# World map to vizualize number of answers by country
# (FIGURE S7) ----

  judge <- table_elo_judge[,c("judge_id", "country")]
  judge <- unique(judge)

  count <- as.data.frame(table(judge[, "country"]))

  count_top <- count[which(count$Freq >= 100),]
  
# Histogram of the number of judges per country for the counrties with 100 judges or more
# Figure S7a
  hist_count <- ggplot2::ggplot(count_top, ggplot2::aes(x = reorder(Var1,-Freq),Freq)) +
    ggplot2::geom_bar(stat ="identity", position = "dodge", fill = "lightblue") +
    ggplot2::xlab("") +
    ggplot2::ylab("Number of judges") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none",
                   panel.grid = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = 8),
                   axis.title = ggplot2::element_text(size = 10))
  ggplot2::ggsave(hist_count, filename = here::here("figures_tables", "Figure_S7a.jpg"))

# Map of the number of judges per country (Figure S7b)
 
   #join data to a map to create a spatialPolygonsDataFrame
  count$freqlog <- log(count$Freq)
  sPDF          <- rworldmap::joinCountryData2Map(count, joinCode = 'ISO3', nameJoinColumn = 'Var1')

  jpeg(filename = here::here("figures_tables", "Figure_S7b.jpg"),width = 10, height = 10, units = "cm", res = 600)
  par(mar = c(1,1,1,1))
  rworldmap::mapCountryData(mapToPlot = sPDF, nameColumnToPlot = "freqlog",
                            catMethod = c(0:10), colourPalette = "heat",
                            missingCountryCol = 'gray90', addLegend = TRUE,
                            mapTitle = "", oceanCol = "lightblue")
  dev.off()

# ----

# Pool ages -----

  table_elo_judge$age_cat <- ifelse(table_elo_judge$age <= 14, "A",
                                    ifelse(table_elo_judge$age > 14 & table_elo_judge$age <= 25, "B",
                                           ifelse(table_elo_judge$age > 25 & 
                                                    table_elo_judge$age <= 40, "C",
                                                  ifelse(table_elo_judge$age > 40 &
                                                           table_elo_judge$age <= 60, "D", "E"))))
  table_elo_judge$age_cat <- factor(table_elo_judge$age_cat)

# ----

# Pool countries -----

  europe <- c("ALA", "ALB", "AND", "AUT", "BEL", "BGR", "BIH", "BLR", "CHE", "CYP", "CZE", "DEU",
              "DNK", "EST", "FIN", "FRO", "GBR", "GIB", "GRC", "HRV", "HUN", "IMN", "IRL", "ISL",
              "ITA", "JEY", "LIE", "LTU", "LUX", "LVA", "MCO", "MDA", "MKD", "MLT", "MNE", "NLD",
              "NOR", "POL", "ROU", "SJM", "SMR", "SRB", "SVK", "SVN", "SWE", "UKR", "VAT")
  
  table_elo_judge$country_cat <- ifelse(table_elo_judge$country == "FRA", "A",
                                    ifelse(table_elo_judge$country == "USA" , "B",
                                           ifelse(table_elo_judge$country %in% europe, "C", "D")))
  
  table_elo_judge$country_cat <- factor(table_elo_judge$country_cat)

# ----

# Number of aswers per category for each variable 
# (FIGURE S8) ----

  newjudge <- table_elo_judge[,c("judge_id", "gender", "age", "education", "scuba_diving",
                                 "fishing_spearing","aquarium", "place", "distance_sea",
                                 "frequency_nature", "knowledge_fish")]
  newjudge <- unique(newjudge)
  
  lplot <- list()
  j     <- 1
  
# Have the fisrt letter of a character string to uppercase
  firstup <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }
  
  for(i in 2:ncol(newjudge)){
    count <- as.data.frame(table(newjudge[,i]))
    name  <- colnames(newjudge)[i]
    par(mar = c(2,2,2,3))
    
    if(name == "age"){
      plot <- ggplot2::ggplot(count, ggplot2::aes(y = Freq, x = Var1)) +
        ggplot2::geom_bar(position = "dodge", stat = "identity", ggplot2::aes(fill = Freq)) +
        ggplot2::coord_flip() +
        ggplot2::scale_x_discrete(breaks = c( "10", "20", "30", "40", "50", "60", "70", "80", "90"),
                                  labels = c( "10", "20", "30", "40", "50", "60", "70", "80", "90")) +
        ggplot2::xlab(gsub("_", " ", firstup(name))) +
        viridis::scale_fill_viridis(alpha = 0.8) +
        ggplot2::theme(legend.position = "none",
                       axis.text = ggplot2::element_text(size = 8),
                       axis.title = ggplot2::element_text(size = 10))
        
    }else {
      if(nrow(count) <= 3){
        plot <- ggplot2::ggplot(count, ggplot2::aes(y = Freq, x = Var1)) +
          ggplot2::geom_bar(position = "dodge", stat = "identity", ggplot2::aes(fill = Freq)) +
          ggplot2::coord_flip() +
          ggplot2::xlab(gsub("_", " ", firstup(name))) +
          viridis::scale_fill_viridis(begin = 0.2, end = 0.6, alpha = 0.8) +
          ggplot2::theme(legend.position = "none",
                         axis.text = ggplot2::element_text(size = 8),
                         axis.title = ggplot2::element_text(size = 10))
      }else{
        plot <- ggplot2::ggplot(count, ggplot2::aes(y = Freq, x = Var1)) +
          ggplot2::geom_bar(position = "dodge", stat = "identity", ggplot2::aes(fill = Freq)) +
          ggplot2::coord_flip() +
          ggplot2::xlab(gsub("_", " ", firstup(name))) +
          viridis::scale_fill_viridis(begin = 0.2, end = 0.6, alpha = 0.8) +
          ggplot2::theme(legend.position = "none",
                         axis.text = ggplot2::element_text(size = 8),
                         axis.title = ggplot2::element_text(size = 10))
      } # eo else count
    } # else age
  
    lplot[[j]] <- plot
    j <- j+1
  
  } #eo for i
  
  ggplot2::ggsave(file = here::here("figures_tables", "FIGURE_S8.jpg"),
                  gridExtra::arrangeGrob(grobs = lplot, ncol = 5),
                  width = 26, height = 26, units = "cm")
  
  rm(plot, lplot, i, j, name, count, judge)

# -----

# Create response variable of the model ----
# challenger 1 becomes challenger 2
  matches           <- table_elo_judge
  colnames(matches) <- c(colnames(matches)[1:2], "challenger_2", "challenger_1",
                         colnames(matches)[5:ncol(matches)])
  matches           <- matches[,c(colnames(matches)[1:2], "challenger_1", "challenger_2",
                                  colnames(matches)[5:ncol(matches)])] # same order
# combine
  table_elo_judge <- rbind(table_elo_judge, matches)

# this column will be 1 if the target picture wins, 0 ifelse
  table_elo_judge$wins <-  ifelse(as.character(table_elo_judge$Winner) == as.character(
    table_elo_judge$challenger_1),table_elo_judge$wins <- 1, table_elo_judge$wins <- 0)

  rm(matches)

# ----

# Test difference between people under and over 14 ----
  
  for(i in 1:nrow(table_elo_judge)){
    ifelse(table_elo_judge$age[i] <= 14, 
    table_elo_judge$youth[i] <-  "under",
    table_elo_judge$youth[i] <- "over"
    )
    cat(paste("Line ", i, " ok\n"))
  }
  
  kw <- kruskal.test(wins ~ as.factor(youth), data = table_elo_judge)
  
  table        <- table_elo_judge[,c("wins", "youth")]
  youth_violin <- ggplot2::ggplot(table, ggplot2::aes(x = youth, y = wins)) + 
    ggplot2::geom_violin(trim = FALSE, color = "white", ggplot2::aes(fill = youth)) +
    ggplot2::geom_boxplot(width = 0.25, fill = "white") +
    ggplot2::stat_summary(fun.y = mean, geom = "point", shape = 23, size = 2) +
    viridis::scale_color_viridis(alpha = 0.65, begin = 0.4, end = 0.6) +
    ggplot2::theme_classic() +
    ggplot2::labs(y = "Probability of winning a match", x = "") + 
    ggplot2::theme(legend.position = "none",
          axis.title.y    = ggplot2::element_text(color = "black", size = 12),
          title           = ggplot2::element_text(color = "black", size = 14),
          axis.line.x     = ggplot2::element_line(linetype = "blank"),
          axis.text.x     = ggplot2::element_text(size = 10), 
          axis.ticks.x    = ggplot2::element_blank()) +
    ggsignif::geom_signif(comparisons = list(c("under", "over")), 
                map_signif_level = TRUE, y_position = 1.2, test = "wilcox.test",
                tip_length = 0.02, size = 0.4, textsize = 3)

# ----
  
# Order variables in the model according to their individual contribution to the % of explained
# variance (TABLE S1) ----

  list_var <- c("gender", "age_cat", "education", "scuba_diving", "country_cat", 
                "fishing_spearing", "aquarium", "place", "distance_sea", "frequency_nature",
                "knowledge_fish")

# Run the first model out of the function
  start.time  <- Sys.time()
  first_model <- lme4::glmer(as.formula(paste("wins ~", paste(list_var, collapse = "+"),
                                              " + (1|challenger_1)")), family = binomial,
                             data = table_elo_judge, na.action = na.fail)

  save(first_model, file = here::here(res_dir_elo, "01_first_model.RData"))
  end.time   <- Sys.time()
  time.taken <- end.time - start.time
  time.taken

# Effect of each variable in the glmm
  utils::download.file(url = "https://zenodo.org/record/5052745/files/01_first_model.RData?download=1",
                       destfile = here::here(res_dir_elo, "01_first_model.RData"))
  load(here::here(res_dir_elo, "01_first_model.RData"))
  
  sum_fm <- summary(first_model) # not a p-value but a z-value
  
# View summary of first model (TABLE S1)
  
  table_s1 <- car::Anova(first_model)
  write.csv(table_s1, here::here("figures_tables", "TABLE_S1.csv"), row.names = TRUE)
  
  rm(list_var, start.time, end.time, time.taken, pervar, first_model)

# ----

# Select the judges to keep ----

  list_judge           <- unique(as.character(table_elo_judge$judge_id))
  table_noeffect_focus <- table_elo_judge[which(as.character(table_elo_judge$judge_id) %in% list_judge),
                                     c("id", "challenger_1", "challenger_2", "Winner", "Loser")]

  write.csv(table_noeffect_focus, file = here::here(res_dir_elo, "01_nogroupeffect.csv"),
            row.names = FALSE)
  
  rm(list_judge, table_nona, table_noeffect_focus, newjudge, table_elo_judge)

# ----

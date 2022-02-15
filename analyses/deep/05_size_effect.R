###################################################################################################
#' Figure size effect
#'
#'Produces Figure S11 of the Langlois et al. & Mouquet 2021 paper.
#'
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         Valentine Fleure \email{valentine.fleure@@gmail.com}
#'
#' @date 2021/06/29
##################################################################################################

# Load ----

  resnet18 <- read.csv(here::here(res_dir_deep, "01_resnet18.csv"), sep = ";")
  resnet50 <- read.csv(here::here(res_dir_deep, "01_resnet50.csv"), sep = ";")

# ----

# Mean performance for each size (Figure 1 K) ----
  
  mean18 <- data.frame(Size = unique(resnet18$Size), Rsquared = vector(length = 6))
  for(i in 1:nrow(mean18)){
    size <- unique(resnet18$Size)[i]
    mean18$Rsquared[i]  <- mean(resnet18$Rsquared[which(resnet18$Size == mean18$Size[i])])
  }
  
  mean50 <- data.frame(Size = unique(resnet50$Size), Rsquared = vector(length = 6))
  for(i in 1:nrow(mean50)){
    size <- unique(resnet50$Size)[i]
    mean50$Rsquared[i]  <- mean(resnet50$Rsquared[which(resnet50$Size == size)])
  }
  
  pal <- viridis::viridis(n = 4, alpha = 0.8, begin = 0.25, end = 0.75)
  
  resnet18$Model <- "ResNet18"
  resnet50$Model <- "ResNet50"
  mean18$Model   <- "ResNet18"
  mean50$Model   <- "ResNet50"
  
  mean_perf <- rbind(mean18, mean50)
  
  plot <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = Size, y = Rsquared)) +
      ggplot2::theme_light() +
      ggplot2::theme(axis.title.y = ggplot2::element_text(color = "black", size = 14, family = "serif"),
                     axis.title.x = ggplot2::element_text(color = "black", size = 14, family = "serif"),
                     axis.text.x  = ggplot2::element_text(size = 12, family = "serif"),
                     axis.text.y  = ggplot2::element_text(size = 12, family = "serif"),
                     axis.ticks.x = ggplot2::element_blank(), 
                     axis.ticks.y = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank()) +
      # resnet18
      ggplot2::geom_line(data = mean18, col = pal[1], linetype = "dashed", alpha = 0.7, size = 1)  +
      ggplot2::geom_point(data = resnet18, col = pal[1], ggplot2::aes(group = Model), size = 2.5) +
      # resnet50
      ggplot2::geom_line(data = mean50, col = pal[3], linetype = "dashed", alpha = 0.7, size = 1)  +  
      ggplot2::geom_point(data = resnet50, col = pal[3], ggplot2::aes(group = Model), size = 2.5) +
      # legend
     ggplot2::annotate("rect", xmin = 309, xmax = 459, ymin = 0.525, ymax = 0.575, fill = "white") +
     ggplot2::annotate("segment", x = 314, y = 0.562, xend = 359, yend = 0.562, col = pal[1], size = 1) +
     ggplot2::annotate("segment", x = 314, y = 0.537, xend = 359, yend = 0.537, col = pal[3], size = 1) +
     ggplot2::annotate(geom = "text", x = 395, y = .565, label = "ResNet18", 
                       color = "black", size = 4, family = "serif") +
     ggplot2::annotate(geom = "text", x = 395, y = .538, label = "ResNet50", 
                       color = "black", size = 4, family = "serif") +
      # axis
     ggplot2::scale_x_continuous(breaks = c(32, 64, 128, 224, 256, 512),
                                labels = c("32", "64", "128", "224", "256", "512")) +
      ggplot2::labs(x = "Size of the pictures", y = "R2 on the test set")
  
# Save plot
  ggplot2::ggsave(plot = plot, family = "serif", units = "cm", width = 15, height = 15, dpi = 600,
                  filename = here::here("figures_tables", "FIGURE_K.jpg"))
  
  rm(resnet18, resnet50, mean18, mean50, pal, plot, mean_perf)
# ----
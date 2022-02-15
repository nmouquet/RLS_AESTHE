#' ################################################################################################
#' #' Features analysis
#' #'
#' #'Analysis the relationship between images features and aesthetic scores 
#' #'
#' #' @author Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#' #'         Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#' #'         François Guilhaumon, \email{francois.guilhaumon@@ird.fr}
#' #'         Nicolas Casajus, \email{nicolas.casajus@@fondationbiodiversite.fr}
#' #'
#' #' @date 2021/05/17
#' ################################################################################################

rm(list=ls(all=TRUE)) 

pathresults <- here::here("results","features")

####Load and combine the data----
  cluster <- read.csv(here::here("results","features", "cluster.csv"))
  cluster <- cluster[,-1]
  rownames(cluster) <- cluster$name
  lumsat <- read.csv(here::here("results","features", "lumsat.csv"))
  lumsat <- lumsat[,-1]
  rownames(lumsat) <- lumsat$name
  momocs <- read.csv(here::here("results","features", "momocs.csv"))
  momocs <- momocs[,-1]
  rownames(momocs) <- momocs$name
  
  esthe_focus_images <- read.csv(here::here("results","deep", "06_esthe_focus.csv"))
  rownames(esthe_focus_images) <- esthe_focus_images$name_worms
  
  tmp12 <- merge(cluster, lumsat, by=0, all=T)
  rownames(tmp12) <- tmp12$Row.names; tmp12$Row.names <- NULL; tmp12$name.x <- NULL; tmp12$name.y <- NULL
  tmp123 <- merge(tmp12, momocs, by=0, all=T)
  rownames(tmp123) <- tmp123$Row.names;tmp123$Row.names <- NULL;tmp123$name <- NULL
  
  datall <- merge(tmp123, esthe_focus_images, by=0, all=T)
  rownames(datall) <- datall$Row.names; datall$Row.names <- NULL;datall$image <- NULL
  
  #create a new colum to differenciat between the species who have a direct
  #evaluation of aesthetics from others 
  
  datall$campg <- "ALL"
  datall$campg[(datall$mayo_campgn==1)]="EVAL"
  datall$campg[(datall$fisheyes_campgn==1)]="EVAL"

  rm(tmp12)
  rm(tmp123)
  rm(cluster)
  rm(lumsat)
  rm(momocs)
  rm(esthe_focus_images)
  
  all_id <- c("CL_cie_d_mean","CL_cie_d_sd","CL_hullarea","SH_n.core.cell_mean","SH_n.core.cell_sd",
              "SH_perimeter_mean","SH_perimeter_sd","SH_perim.area.ratio_mean","SH_perim.area.ratio_sd",
              "SH_core.area.index_mean","SH_core.area.index_sd","LS_mean_satu","LS_sd_satu","LS_mean_light",
              "LS_sd_light","MO_Fourier_pc1","MO_Fourier_pc2")
  
#end----  
  
####Multiple linear regression FIGURE 2---- 

  varcor="esthe_pred"

  #subset the data for which we have esthe evaluation by humans 
    datacor <- datall[datall$focus==1,]

  #Look at the correlation between variables and remove the variable with pearson > 0.7 
  
    cormat <- function(idcor,data,thr){
      
      #idcor=all_id
      #data=data_fisheyes
      #thr=0.7 
      
      remove <- function(id_name,rem) {
        for (i in 1:length(rem)) id_name=id_name[id_name!=rem[i]]
        return(id_name)
      }     
  
      id_temp <- idcor
      
      for (i in 1:length(id_temp))  {
        if (length(id_temp)==1) break
        cor_mat <- cor(data[,id_temp])
        if (i>= dim(cor_mat)[1]) break
        rem <- names(which(abs(cor_mat[,i])>thr)[-1])
        if (length(rem)>0) id_temp <- remove(id_name=id_temp,rem=rem)
      }  
      return(id_temp)
    }
    
    id_tps <- cormat(idcor=all_id,data=datacor,thr=0.7)
  
  
  #Order the remaining variables in decreasing order of importance
    
    #varcor : variable to explain
    #all_id : vector of variable to use in the correlation
    #dat_cor: dataset to be used 
    #ord : stat to be used to sort the variable (R2, PERVAR percentage of total variance,PEARSON coefficient)
    
    look_cor <- function(all_id,dat_cor=datacor,varcor,ord){
    
     #all_id=all_id
     #dat_cor=data_fisheyes
     #varcor="esthe_fisheyes_all"
     #ord="R2"
    
    modall <- lm(as.formula(paste(varcor," ~ ", paste(all_id, collapse= "+"))),data=dat_cor, na.action=na.omit)
    
    out_data <- as.data.frame(matrix(NA,ncol=4,nrow=length(all_id)))
    colnames(out_data) <- c("var","pervar","R2","pearson.p")
    out_data$var <- all_id
    
    for (i in 1:length(all_id))
    {
      cort <- cor.test(dat_cor[,varcor],dat_cor[,all_id[i]])
      out_data$pearson.p[i] <- cort$p.value
      modminus <- lm(as.formula(paste(varcor," ~ ", paste(all_id[-i], collapse= "+"))),data=dat_cor, na.action=na.omit)
      modalone <- lm(as.formula(paste(varcor," ~ ", all_id[i])),data=dat_cor, na.action=na.omit)
      out_data$pervar[i] <- (100-(summary(modminus)[[8]]/summary(modall)[[8]])*100)
      out_data$R2[i] <- summary(modalone)[[8]]
    }
    
    if (ord=="R2") corres <- out_data[order(-out_data$R2),]
    if (ord=="PERVAR") corres <- out_data[order(-out_data$pervar),]
    if (ord=="PEARSON") corres <- out_data[order(-out_data$pearson.p),]
    
    return(corres)
    }
    
    ord <- look_cor(id_tps,dat_cor=datacor,varcor,ord="PERVAR")
    
    id_ord <- ord$var
    
  #build the full model 
    modall <- lm(as.formula(paste(varcor," ~ ", paste(id_ord, collapse= "+"))),data=datacor)
    summary(modall)
    
  #Keep the variables that are  significant
   
    id_final <- c("CL_cie_d_mean","LS_sd_light","LS_mean_satu",     
                  "MO_Fourier_pc1","MO_Fourier_pc2","LS_mean_light","SH_perimeter_mean","SH_core.area.index_sd",
                  "SH_perim.area.ratio_sd","SH_n.core.cell_mean")
    modfinal <- lm(as.formula(paste(varcor," ~ ", paste(id_final, collapse= "+"))),data=datacor)
    summary(modfinal)
    
    #remove SH_n.core.cell_mean which is not significant anymore 
    
    id_final <- c("CL_cie_d_mean","LS_sd_light","LS_mean_satu",     
                  "MO_Fourier_pc1","MO_Fourier_pc2","LS_mean_light","SH_perimeter_mean","SH_core.area.index_sd",
                  "SH_perim.area.ratio_sd")
    modfinal <- lm(as.formula(paste(varcor," ~ ", paste(id_final, collapse= "+"))),data=datacor)
    summary(modfinal)
    
  #Scale the variables in the final model & compare the Estimates (FIGURE 2a 600x900)
    
    data_scale <-  cbind.data.frame(esthe_pred=datacor$esthe_pred, scale(datacor[,id_final]))
    modfinal <- lm(as.formula(paste(varcor," ~ ", paste(id_final, collapse= "+"))),data=data_scale)
    sum_mod <- summary(modfinal)
      
    x <- rownames(sum_mod$coefficients)
    x <-x[-1]
    Estimate <- sum_mod$coefficients[-1,"Estimate"]
    sdev <- sum_mod$coefficients[-1,"Std. Error"]
      
    data_plot <- cbind.data.frame(name=x,Estimate=Estimate,sdev=sdev)
    data_plot <- data_plot[order(data_plot$Estimate,decreasing = FALSE),]
    data_plot$pos <- NA
    
    for (i in 1:length(data_plot$pos)) {if (data_plot$Estimate[i]<0) data_plot$pos[i]=10 else data_plot$pos[i]=-35}

    
    
  #FIGURE 2a 
    
    
    ##Change the names of the variables (see Text F in S1 File)
    
    data_plot$name <- c("Elongatedness","Pattern variation","Pattern asymmetry","Mean light","Morpho_2","Pattern repetition","SD light","Color saturation","Color heterogeneity")
    
    ##Draw the figure 
    library(ggplot2)
    theme_set(theme_bw())
    plot <-  ggplot(data_plot) +
          geom_bar( aes(x=name, y=Estimate), stat="identity",  fill=c("tomato1","tomato1","seagreen3","seagreen3","seagreen3","seagreen3","seagreen3","seagreen3","seagreen3"), alpha=0.8) +
          geom_errorbar( aes(x=name, ymin=Estimate-sdev, ymax=Estimate+sdev), width=0.3, colour="azure4", alpha=0.9, size=1) +
          scale_x_discrete(limits=data_plot$name)+
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.text.y = element_blank(),
                axis.text=element_text(size=10, family = "serif"),
                axis.title=element_text(size=18, family = "serif")
                ) +
          xlab("Images Features") +
          ylab("Estimates (scaled)") +
          coord_flip() + 
          geom_text(aes(x=name, y=pos,label = name),hjust = 0,size=5, family = "serif")
      
      ggplot2::ggsave(filename = here::here("figures_tables", "FIGURE_2a.jpg"),
                      plot, width = 14, height = 18, units = "cm", dpi = 600)

  #PCA ANALYSIS FIGURE 2b
    
    data_scale <-  cbind.data.frame(esthe_pred=datacor$esthe_pred, scale(datacor[,id_final]))
    
    ##Change the names of the variables (see Text F in S1 File)
    
    colnames(data_scale) <- c("esthe_pred","Color heterogeneity","SD light","Color saturation","Elongatedness",
                              "Morpho_2","Mean light","Pattern repetition","Pattern asymmetry","Pattern variation")
   
    pca_fish<-ade4::dudi.pca(data_scale[,-1], scannf=FALSE, nf = 5)
    
    factoextra::fviz_eig(pca_fish,main = "Eigenvalues")
    
    ##with ggplot (old version )
      # library(ggplot2)
      # factoextra::fviz_pca_biplot(pca_fish, 
      #                             col.ind       = data_scale$esthe_pred, 
      #                             geom          = "point",
      #                             gradient.cols = c("blue", "green","yellow","orange", "red" ),
      #                             repel         = TRUE,
      #                             col.var       = "darkblue",
      #                             geom.var      = c("arrow", "text"),
      #                             alpha.ind     = 0.9,
      #                             ggtheme       = theme_minimal(), 
      #                             ylim          = c(-5,6), 
      #                             xlim          = c(-7,5),
      #                             legend.position = c(0.2, 0.8)) + 
      #   labs(col="Aesthetic", title = " ") +
      #   theme(legend.position = c(0.95, 0.9))

    ## old fashion (thanks Nicolas_C !)
    
      source(here::here("R/shadow_text.R"))
      
      n_bins <- 255
      
      couleurs <- c('#a50026', '#d73027', '#f46d43', '#fdae61', '#fee090', '#e0f3f8',
                    '#abd9e9', '#74add1', '#4575b4', '#313695')
      couleurs <- colorRampPalette(couleurs)(n_bins)
      couleurs <- rev(couleurs)
      couleurs <- paste0(couleurs, "dd")
      
      z_range <- range(data_scale$"esthe_pred")
      z_bins  <- seq(z_range[1], z_range[2], length.out = n_bins + 1)
      z_colors <- rep(NA, length(data_scale$"esthe_pred"))
      
      for (i in 1:n_bins) {
        
        if (i == n_bins) {
          
          pos <- which(data_scale$"esthe_pred" >= z_bins[i] & 
                         data_scale$"esthe_pred" <= z_bins[i + 1])
        } else {
          
          pos <- which(data_scale$"esthe_pred" >= z_bins[i] & 
                         data_scale$"esthe_pred" <  z_bins[i + 1])
        }
        
        if (length(pos)) {
          z_colors[pos] <- couleurs[i]
        }
      }
      
      inds <- pca_fish$"li"
      eigens <- round(100 * pca_fish$eig / sum(pca_fish$eig) , 1)
      eigens <- eigens[1:2]
      
      ordre <- order(data_scale$"esthe_pred", decreasing = FALSE)
      
      png(filename = here::here("figures_tables", "FIGURE_2b.png"), width = 12, height = 12, res = 600, 
          units = "in", pointsize = 16)
      
        par(xaxs = "i", yaxs = "i", mar = c(2.5, 2.5, 1, 1), family = "serif", new = FALSE)
        
        plot(0, type = "n", xlim = c(-9, 7), ylim = c(-7, 8), ann = FALSE, bty = "n", 
             axes = FALSE)
        
        grid(lty = 1, lwd = 0.5, col = "#cccccc")
        
        par(mgp = c(3, 0.25, 0))
        axis(1, lwd = 0)
        axis(2, lwd = 0, las = 1)
        
        mtext(text = paste0("Dimension 1 (", eigens[1],"%)"), side = 1, line = 1.25,cex=1.8)
        mtext(text = paste0("Dimension 2 (", eigens[2],"%)"), side = 2, line = 1.25,cex=1.8)
        
        points(inds[ordre, 1:2], pch = 19, col = z_colors[ordre])
        
        par(new = TRUE)
        plot(0, type = "n", xlim = c(-1.29, 1), ylim = c(-0.87, 1), ann = FALSE, bty = "n", 
             axes = FALSE)

        vars <- pca_fish$co
        
        for (i in 1:nrow(vars)) {
          
          shape::Arrows(0, 0, vars[i, 1], vars[i, 2], arr.type = "triangle", lwd = 3.50,
                        arr.width = 0.15, arr.length = 0.15, col = "white")
          shape::Arrows(0, 0, vars[i, 1], vars[i, 2], arr.type = "triangle", lwd = 2.25,
                        arr.width = 0.15, arr.length = 0.15, col = "#111111")
        }
        
        abline(h = 0, v = 0, lwd = 1, lty = 1, col = "#333333")
        box(col = "#cccccc", lwd = 1)
        
        for (i in 1:nrow(vars)) {
          
          pos <- 1
          
          if (i %in% c(5,3,1,8,9)) {
            pos <- 2
          }
          
          if (i %in% c(2,7)) {
            pos <- 3
          }
          
          shadow_text(x = vars[i, 1], y = vars[i, 2], labels = rownames(vars)[i], 
                      pos = pos, cex = 0.95, font = 2, col = "black", bg = "#FFFFFF33",
                      radius = 0.1)
        }
        
        par(new = TRUE)
        
        y_bot <- 5.0
        
        plot(0, type = "n", xlim  = c(-9, 7), ylim = c(-7, 8), ann = FALSE, bty = "n", 
             axes = FALSE)
        
        inc <- 0.01
        
        for (i in 1:length(couleurs))
          rect(5, y_bot + (i - 1) * inc, 5.5, y_bot + i * inc, col = couleurs[i], 
               border = couleurs[i])
        
        llabels <- c(1250, 1500, 1750, 2000)
        
        for (i in llabels) {
          pos <- which(z_bins > i)[1]
          text(5.5, y_bot + pos * inc, i, pos = 4, cex = .85)
          
          lines(c(5.0, 5.1), rep(y_bot + pos * inc, 2), col = "white")
          lines(c(5.4, 5.5), rep(y_bot + pos * inc, 2), col = "white")
        }
        
        shadow_text(5.25, y_bot - 0.75, "Aesthetic", 
                    pos = 3, cex = 1.6, font = 1, col = "black", bg = "#FFFFFF33",
                    radius = 0.1)
        
      dev.off()
      
    ## find coordinates of fish to illustrate the PCA figure 
      res.ind <- factoextra::get_pca_ind(pca_fish)
      
      identifyPch <- function(x, y = NULL, n = length(x), plot = FALSE, pch = 19, ...)
      {
        xy <- xy.coords(x, y); x <- xy$x; y <- xy$y
        sel <- rep(FALSE, length(x))
        while(sum(sel) < n) {
          ans <- identify(x[!sel], y[!sel], labels = which(!sel), n = 1, plot = plot, ...)
          if(!length(ans)) break
          ans <- which(!sel)[ans]
          points(x[ans], y[ans], pch = pch)
          sel[ans] <- TRUE
        }
        ## return indices of selected points
        which(sel)
      }
     
      x <- res.ind$coord[,'Dim.1']
      y <- res.ind$coord[,'Dim.2']
      df <- data.frame(x=x, y=y,cop=datacor$copyright)
      rownames(df) <- rownames(res.ind$coor)
      plot(y ~ x, data=df,col=datacor$copyright)
      abline(v=0)
      abline(h=0)
      id_fish <- identifyPch(x=df$x,y=df$y)
      rownames(df)[id_fish]
   
#end----
      
####PCA analysis FIGURE S1 E ----
    
    pca_res <- prcomp(datall[,which(colnames(datall) %in% all_id)], center = TRUE,scale. = TRUE)
    summary(pca_res)
    
    pca_res.campg <- datall$campg
    str(pca_res)
    
    library(ggplot2)
    library(ggbiplot)
    
    
    plot <- ggbiplot(pca_res,var.axes=FALSE,groups=pca_res.campg,choices=c(1,2),alpha =0.5,ellipse = TRUE,ellipse.prob=0.99)+scale_colour_manual(values = c("grey", "red"))+ theme_bw()
    
    ggplot2::ggsave(filename = here::here("figures_tables", "FIGURE_E.jpg"),
                    plot = plot, 
                    width = 16, height = 16, units = "cm", dpi = 600)
    
    
#end----
    

  
    

  
  
  






#' ################################################################################################
#' #' Color cluster analysis
#' #'
#' #'This script extract information on color clusters heterogeneity in images 
#' #'
#' #' @author Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#' #'         Juliette Langlois, \email{juliette.a.langlois@@gmail.com}
#' #'         
#' #'
#' #' @date 2021/05/17 first created
#' ################################################################################################

#The whole analysis is made on a set of images for which we do not have copyrights for all of them
#A subdirectory here::here("data","exemple_images") provides a subset of images that can be used (see Extended table 1 for copyrights)

rm(list=ls(all=TRUE)) 

#pathdata <- here::here("data","images")
pathdata <- here::here("data","example_images")
  
pathresults <- here::here("results","features")
pathphoto_png <- here::here(pathdata,"png")
files <- dir(path=pathphoto_png,pattern=".png")
  
esthe_focus_images <- read.csv(here::here("data","image_table.csv"))
focus_images <- as.character(esthe_focus_images$name_worms)
files_focus <- paste0(focus_images,".png")

#BASIC FUNCTIONS----
  
  # load_clean : load the image, replace the background values with NA
  #              return a cimg object
  #              https://rdrr.io/cran/imager/man/cimg.html
    require(imager)
    load_clean <- function (file_image)
    {
      
      im <- imager::load.image(file_image)
      bdf <- as.data.frame(im)
      
      #remove the alpha channel (the png are coded in RGBA)
      newbdf <- bdf[bdf$cc %in% c(1,2,3),]
      
      #replace the 1 (white background) by NA
      
      c1 <- newbdf[newbdf$cc %in% 1,'value']
      c2 <- newbdf[newbdf$cc %in% 2,'value']
      c3 <- newbdf[newbdf$cc %in% 3,'value']
      te <- cbind.data.frame(newbdf[newbdf$cc %in% 1,c("x","y")],c1,c2,c3)
      te[(te$c1==te$c2 & te$c1==te$c3),c("c1","c2","c3")]=NA
      newbdf$value=c(te$c1,te$c2,te$c3)
      im_clean <- imager::as.cimg(newbdf$value,x=dim(im)[1],y=dim(im)[2],cc=3) #re-convert into cimg object
      
      return(im_clean)
    }
  
  # cluster : divide the image in cluster based on colorimetric similary using the kmeans function 
  #           based on the CIELAB color space (use only the a and b coordinates)
  #           https://en.wikipedia.org/wiki/CIELAB_color_space
    require(reshape2)
    require(SDMTools)
    cluster <- function (n_im,clust)
    {
      #n_im=1
      #clust=6
  
      name <- gsub(".png","",files[n_im])
      
      #load image and create a dataframe with cieLab values 
      
      file_image <- here::here(pathphoto_png,files[n_im])
      im_no_NA <- load_clean(file_image)
      
      bdf<-as.data.frame(im_no_NA,wide="c")
      bdf <- na.omit(bdf) # to remove the NA (white background)
      x <- colorspace::RGB(bdf$c.1,bdf$c.2,bdf$c.3)
      y <- as(x, "LAB")
      df <- cbind(bdf[,c("x","y")],as.data.frame(y@coords))
      colnames(df) <- c("x","y","c.1","c.2","c.3") #will use only "c.2","c.3" for the kmeans (a and b coordinates of the CIELAB space)
      
      #kmeans clustering analysis
      
      set.seed(1) #important to obtain always the same clusters
      kmeans   <- kmeans(na.omit(df[,c("c.2","c.3")]), centers = clust, iter.max = 100,nstart=1)
      
      kvalues <- as.numeric(names(kmeans$centers[kmeans$cluster,1]))
      rm(im_no_NA)
      
      #shape analysis (using the PatchStat function)
      
      patch <- cbind(na.omit(df)[,c(1,2)],kvalues)
      patch_mat <- reshape2::acast(patch, x~y, value.var="kvalues")
      ps.data = SDMTools::PatchStat(patch_mat)
      rm(kvalues,patch,df)
      
      #output
      
      cbind.data.frame(name,as.data.frame(kmeans$centers),ps.data)
      
    }
    
  # cluster.lab.plot : need to run colors first
    require(dplyr)
    # require(fisheyeR)
    cluster.lab.plot <- function (n_im,clust,lg_sz,ncol,FISH)
    {
        #n_im=2
        #clust=3
        #lg_sz=7
        #FISH=TRUE
      
        name <- gsub(".png","",files[n_im])
      
      #load image and create a dataframe with cieLab values 
        file_image <- here::here(pathphoto_png,files[n_im])
        im_no_NA <- load_clean(file_image)
        bdf<-as.data.frame(im_no_NA,wide="c")
        bdf <- na.omit(bdf) # to remove the white 
        x <- colorspace::RGB(bdf$c.1,bdf$c.2,bdf$c.3)
        y <- as(x, "LAB")
        df <- cbind(bdf[,c("x","y")],as.data.frame(y@coords))
        colnames(df) <- c("x","y","c.1","c.2","c.3")
        
        df0NA <- na.omit(as.data.frame(RGBtoLab(im_no_NA),wide="c"))
        df_rgb <-  na.omit(as.data.frame(im_no_NA,wide="c")) %>% mutate(rgb.val=rgb(c.1,c.2,c.3))
        
        set.seed(1) #important to obtain always the same clusters ! 
        kmeans   <- kmeans(na.omit(df[,c("c.1","c.2","c.3")]), centers = clust, iter.max = 100,nstart=1)
        kvalues <- as.numeric(names(kmeans$centers[kmeans$cluster,1]))
        
        
        cl_colours <- kmeans$centers[kmeans$cluster,]
        kColours <- rgb(convertColor(cl_colours, "Lab", "sRGB"))
        col_clust <- rgb(convertColor(kmeans$centers, "Lab", "sRGB"))
        
        dat <- as.data.frame(kmeans$centers)

      #plot
        library(ggplot2)
        
        p <- list()
        p[[1]] <-ggplot(df_rgb,aes(x,y))+geom_raster(aes(fill=rgb.val))+scale_fill_identity() + xlab("") + ylab("") + ggtitle(name) +scale_y_reverse() + theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.ticks = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())
        p[[2]] <-ggplot(df,aes(x,y))+geom_raster(aes(fill=kColours))+scale_fill_identity() + theme(panel.background = element_rect(fill = 'white', colour = 'red'), panel.border = element_rect(colour = "black", fill=NA, size=1), axis.ticks = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank()) + xlab("") + ylab("") + ggtitle(paste("nb_clust= ",clust )) + scale_y_reverse()
        
        if (min(kmeans$centers[,"c.2"])>10) minx <- 5 else minx <- min(kmeans$centers[,"c.2"])
        if (min(kmeans$centers[,"c.3"])>10) miny <- 5 else miny <- min(kmeans$centers[,"c.3"])
        
        x <- seq(minx-10,max(kmeans$centers[,"c.2"])+10,0.2)
        
        y <- seq(miny-10,max(kmeans$centers[,"c.3"])+10,0.2)
        
        
        #bkg <- cbind(kmeans$centers[clust_id,"c.1"],expand.grid(x,y))
        bkg <- cbind(70,expand.grid(x,y))
        colnames(bkg) <- c("c.1","c.2","c.3")
        bkg <-  cbind(bkg,col=rgb(convertColor(bkg, "Lab", "sRGB")))
        
        bg <- ggplot(bkg, aes(x=c.2, y=c.3)) + geom_point(size=1,shape=15,colour=bkg$col) + geom_hline(yintercept = 0,color="white") + geom_vline(xintercept = 0,color="white")
        
        p[[3]] <- bg+geom_point(data=dat,aes(x=dat$c.2,y=dat$c.3),size=3,shape=21,fill=col_clust,color="white")+geom_text(data=dat,aes(label=rownames(dat)),color="white",size=3,hjust=-1, vjust=0)+scale_fill_identity() + xlab("") + ylab("") + ggtitle("Kmeans clusters") + theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.ticks = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),plot.title = element_text(size = 10),axis.text.x = element_text(size=5),axis.text.y = element_text(size=5))
        
        g <- lapply(4:(clust+3), function(i) {

          kColours_cl=kColours
          kColours_cl[kColours_cl!=col_clust[i-3]]="#FFFFFF"
          name <-paste0("cluster #", i-3)
          ggplot(df,aes(x,y))+geom_raster(aes(fill=kColours_cl))+scale_fill_identity() + xlab("") + ylab("") + ggtitle(name) + 
            scale_y_reverse() + theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.ticks = element_blank(),
                                      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),
                                      axis.text.x = element_blank(),axis.text.y = element_blank(),plot.title = element_text(size = lg_sz))
        })
        
        if (FISH==TRUE) (do.call(gridExtra::grid.arrange,c(p, g))) else do.call(gridExtra::grid.arrange,c(g))
        
    }
  
  # cha : compute the area of the convex hull volume. Used in cluster_stat function
    require(splancs)
    cha<-function(x,y){
      chull(x,y)->i
      return(splancs::areapl(cbind(x[i],y[i])))
    }
  
  # cluster_stat : compute various statistics with the data produced by the cluster function 
    cluster_stat <- function(cluster_raw,n_im,clust,colors_data)
    {
       #n_im=1120
       #cluster_raw=raw_data #product of the cluster function
       #clust=6
       #colors_data=colors_data

      name <- gsub(".png","",files[n_im])
      
      #Variables that will be measured
      
        cie_id <- c("CL_cie_d_mean","CL_cie_d_sd","CL_hullarea")
    
        ## from the SDMTools::PatchStat function 
        ## https://www.rdocumentation.org/packages/SDMTools/versions/1.1-221/topics/PatchStat
        shape_id <- c("SH_n.core.cell_mean", "SH_n.core.cell_sd","SH_perimeter_mean", "SH_perimeter_sd", 
                      "SH_perim.area.ratio_mean", "SH_perim.area.ratio_sd","SH_core.area.index_mean", "SH_core.area.index_sd")
        
        id_name <- c("cie_id","shape_id")
      
      #subset the data produced by the cluster and colors functions 
        dat <- cluster_raw[cluster_raw$name %in% name,-1]
        rownames(dat) <- c(1:clust)
        
      
      #Clusters colors dist
        CL_cie_d_mean <- mean(dist(dat[,c("c.2","c.3")]))
        CL_cie_d_sd  <- sd(dist(dat[,c("c.2","c.3")]))
      
      #Convex hull area
        CL_hullarea <- cha(x=dat$c.2,y=dat$c.3)
      
      #Clusters size & shape 
      
        SH_n.core.cell_mean<- mean(dat[,"n.core.cell"])
        SH_n.core.cell_sd<- sd(dat[,"n.core.cell"])

        SH_perimeter_mean<- mean(dat[,"perimeter"])
        SH_perimeter_sd<- sd(dat[,"perimeter"])
        
        SH_perim.area.ratio_mean<- mean(dat[,"perim.area.ratio"])
        SH_perim.area.ratio_sd<- sd(dat[,"perim.area.ratio"])
        
        SH_core.area.index_mean<- mean(dat[,"core.area.index"])
        SH_core.area.index_sd<- sd(dat[,"core.area.index"])
        

      #Output
      
      varout <- cbind.data.frame(do.call(cbind,lapply(cie_id,function(i) get(i))),
                                 do.call(cbind,lapply(shape_id,function(i) get(i))))
      colnames(varout) <- c(cie_id,shape_id)
      
      cbind.data.frame(name,varout)
      
    }
    
  # get_id : Get the id number of a photo with the name.png in input
    get_id <- function(name,files) {
      which(files %in% paste0(name,".png"))
    }
  
#----  
  
#EXEMPLE WITH THE BASIC FUNCTIONS FIGURE S1 B----
  
  get_id(name="Holacanthus_ciliaris_J_1",files=files)
  
  n_im=30 
  clust=9
    
  raw_data <- cluster(n_im=n_im,clust=clust)

  cluster_stat(cluster_raw=raw_data,n_im=n_im,clust=clust)
  
  ##FIGURE S1 B
  plot <- cluster.lab.plot(n_im=n_im,clust=clust,lg_sz=8,FISH=TRUE)
  
  ggplot2::ggsave(filename = here::here("figures_tables", "FIGURE_B.jpg"),
                  plot = plot, 
                  width = 18, height = 24, units = "cm", dpi = 600)
  

#----
  
#ANALYSE ALL IMAGES----
  #Note that the number of clusters used in the cluster function (9) has been chosen 
  #to maximize the correlation between the aesthetics scores and the measured 
  #features using a subset of the fish (used in Tribot et al. 2018. Confronting 
  #species aesthetics with ecological functions of coral reef fishes. 
  #Scientific Reports, 8, 11733.). Data not shown.
  
  #WARNING Do not run as all images are not available; the output files in results/features have already been computed with all images
  
  pict <-length(files)
  
  raw_data <- as.data.frame(do.call(rbind,parallel::mclapply(1:pict, function(i) cluster(n_im=i,clust=9),mc.cores = 7 )))
  stat_data <- as.data.frame(do.call(rbind,parallel::mclapply(1:pict, function(i) cluster_stat(cluster_raw=raw_data,n_im=i,clust=9),mc.cores = 7 )))
  write.csv(stat_data,here::here("results","features","cluster.csv"))
  
#----
  
  
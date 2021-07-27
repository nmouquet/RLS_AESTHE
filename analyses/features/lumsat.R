#' ################################################################################################
#' #' Lightness and saturation analysis
#' #'
#' #'extract information on lightness and saturation in images https://en.wikipedia.org/wiki/HSL_and_HSV
#' #'
#' #' @author Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#' #'         Juliette Langlois, \email{juliette.a.langlois@@gmail.com}
#' #'         
#' #'
#' #' @date 2021/05/17
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
    
  # lumsat : extrat the lightness and saturation values from the HSV color space 
  #          https://en.wikipedia.org/wiki/HSL_and_HSV
    require(imager)
    require(colorspace)
    lumsat <- function(n_im)
    {
      
      #n_im=5
  
      name <- gsub(".png","",files[n_im])
      
      #load image and create a dataframe with SV values from the HSV color space
      
        file_image <- here::here(pathphoto_png,files[n_im])
        im_no_NA <- load_clean(file_image)
        bdf<-as.data.frame(im_no_NA,wide="c")
        bdf<-na.omit(bdf)
        x <- colorspace::RGB(bdf$c.1,bdf$c.2,bdf$c.3)
        y <- as(x, "HSV")
        bdf_SATURA <- cbind(bdf[,c("x","y")],as.data.frame(y@coords))
        colnames(bdf_SATURA) <- c("x","y","c.1","c.2","c.3") 
        rm(im_no_NA)
    
      #load image and create a dataframe with cieLab values 
        
        LS_mean_satu <- mean(na.omit(bdf_SATURA$c.2))
        LS_sd_satu <- sd(na.omit(bdf_SATURA$c.2))
        LS_mean_light <- mean(na.omit(bdf_SATURA$c.3))
        LS_sd_light <- sd(na.omit(bdf_SATURA$c.3))
       
        
        rm(bdf_SATURA)

      cbind.data.frame(name,LS_mean_satu,LS_sd_satu,LS_mean_light,LS_sd_light)
      
    }
#----

#EXEMPLE WITH THE BASIC FUNCTIONS----
    
  lumsat(n_im=2)
    
#----
    
#ANALYSE ALL IMAGES----
  
    #WARNING Do not run as all images are not available; the output files in results/features have already been computed with all images
    
    pict <-length(files)

    lum_data <- as.data.frame(do.call(rbind,parallel::mclapply(1:pict, function(i) lumsat(n_im=i),mc.cores = parallel::detectCores() )))
    write.csv(lum_data,here::here("results","features","lumsat.csv"))
    
#----



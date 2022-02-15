#' ################################################################################################
#' #' Outline shapes analysis
#' #'
#' #'extract information on the shape of the fish using the package https://cran.r-project.org/web/packages/Momocs/index.html
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
pathphoto_mask <- here::here(pathdata,"jpg_mask")
  
files <- dir(path=pathphoto_mask,pattern=".jpg")
files_path <- list.files(pathphoto_mask, full = T)

#BASE FUNCTIONS----

  library(Momocs)  
  
  load_mask <- function(mask.list){
    I <- Momocs::import_jpg(mask.list)
    out <- Out(I)
    inherits(out, "Coo")
    coo <- coo_center(out)
    return(coo)
  }
  
  visu <- function(coo,f,n_im,w_plot)
  {
    # f=10
    # n_im=1
    # w_plot="acp"
    
    dev.off()
    if (w_plot=="panel") panel(coo)
    if (w_plot=="stack") stack(coo)
    if (w_plot=="species") {
      coo_plot(coo[n_im])
      ef <- efourier(coo[n_im], f)
      efi <- efourier_i(ef)
      coo_draw(efi, border='red', col=NA)
    }
    if (w_plot=="acp") {
      coo.f <- efourier(coo, nb.h=f)
      coo.p <- PCA(coo.f)
      plot(coo.p,name=TRUE)
    }
    
  }
  
  fourier <- function (f,coo)
  {
    coo.f_acp <- efourier(coo, nb.h=f)
    PCA(coo.f_acp)
  }
  
  momocs <- function (n_im,f,coo,coo.p,files) # need to run fourier first to compute coo.p
  {
     #n_im <- 1
     #f=27
     #coo=coo
     #coo.p=coo.p
     
    #var that will be computed 
     
      momocs_id <- c("MO_Fourier_pc1","MO_Fourier_pc2")
      id_name <- c("momocs_id")
      
    #get the name and the rank of the species 
    
      pos <- which(files %in%  files[n_im])
      name <- gsub(".jpg","", files[n_im])
      
    #compute the momocs stats 
    
      cooc <- coo$coo[[name]]
      
      MO_Fourier_pc1=as.numeric(coo.p$x[,"PC1"])[pos]
      MO_Fourier_pc2=as.numeric(coo.p$x[,"PC2"])[pos]

      cbind.data.frame(name,MO_Fourier_pc1,MO_Fourier_pc2)
      
  }
  
  # get_id : Get the id number of a photo with the name.png in input
  get_id <- function(name,files) {
    which(files %in% paste0(name,".png"))
  }
  
#---- 
  
#EXEMPLE WITH THE BASIC FUNCTIONS FIGURE S1 D----
  
  #Returns shapes centered on the origin. 
    coo <- load_mask(mask.list=files_path)
  
  #Visualisation of species fourier transformation and species position
  #on the two first axis of a PCOA based on fourier transformation
  #FIGURE S1 D
  
    visu(coo=coo,f=15,n_im=3,w_plot="species")

    visu(coo=coo,f=15,n_im=length(files_path_tribot),w_plot="acp")

  #Compute the momocs stats for one species 

    coo.p <- fourier(f=15,coo=coo)
    momocs(n_im=2,f=15,coo=coo,coo.p=coo.p,files=files)
    
#----

#ANALYSE ALL IMAGES----
    
    #WARNING Do not run as all images are not available; the output files in results/features have already been computed with all images
    
    pict <-length(files)
    
    start.time <- Sys.time()
    coo <- load_mask(mask.list=files_path[1:pict])
    coo.p <- fourier(f=15,coo=coo)
    
    momocs_data <- as.data.frame(do.call(rbind,parallel::mclapply(1:pict, function(i) momocs(n_im=i,f=15,coo=coo,coo.p=coo.p,files=files),mc.cores = parallel::detectCores() )))
    
    
    write.csv(momocs_data,here::here("results","features","momocs.csv"))
    
#----



################################################################################
#' 
#' Functions used in the scripts of the folder `analysis/conservation`
#' 
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         François Guilhaumon, \email{francois.guilhaumon@@ird.fr},
#'         Florian Baletaud, \email{baletaudflorian@@gmail.com},
#'         Nicolas Loiseau, \email{nicolas.loiseau1@@gmail.com}
#'
#' @date 2021/02/17 first created
#' 
################################################################################



#' dunnTestExtra
#' 
#' @description
#' Adaptation of the `dunnTest` from package `FSA` but with signification 
#' symbols.
#'
#' @param x A numeric vector of data values or a formula of the form x~g.
#' @param dat A `data.frame` that minimally contains x and g.
#' @param metho A single string that identifies the method used to control the
#'  experiment wise error rate.
#'  See the list of methods in `p.adjustment.methods` 
#'  (documented with `dunn.test`) in dunn.test.
#'
#' @return A `list`. See [FSA::dunnTest()] for further detail.
#' 
#' @export

dunnTestExtra <- function(x, dat, metho) {
  
  dunn <- FSA::dunnTest(x, data = dat, method = metho)
  
  dunn$res$signif <- unlist(lapply(dunn$res$P.adj, function(x) {
    if (x > 0.05)  r <- "NS."
    if (x < 0.05)  r <- "*"
    if (x < 0.01)  r <- "**"
    if (x < 0.001) r <- "***"
    r
  }))
  
  dunn
}



#' Get IUCN
#' 
#' @description 
#' Extract the IUCN status of a species from **FishBase**.
#'
#' @param x The species of which we want the IUCN status. Must be written like 
#'   `"Genus-species"`.
#'
#' @return iucn status of species x
#' 
#' @export
#'
get_iucn <- function(x = "Regalecus-glesne") {

  url2      <- paste0("http://www.fishbase.se/summary/", x, ".html")
  
  c         <- XML::htmlParse(RCurl::getURLContent(url2, followlocation = TRUE))
  link_list <- XML::getHTMLLinks(c, externalOnly = TRUE, xpQuery = "//a/@href", 
                                 baseURL = docName(c))
  
  if (length(link_list) == 0) {
    stop(x, " is not an accepted name in Fishbase. Check for spelling mistakes",
         " and/or synonyms.")
  }
  
  a1 <- XML::getNodeSet(c, "//div ")
  a  <- XML::getNodeSet(c, "//span ")
  
  rm(c)
  
  if (length(a) != 0) {
    
    d  <- XML::xmlValue(a[[which.max(sapply(lapply(a, XML::xmlValue), function(x){
      regexec(pattern = "Ecology", x)[[1]][1] })) + 2]])
    
    m  <- regmatches(d, gregexpr(pattern = "[-[:alpha:]]+;", d))
    m1 <- regmatches(d, gregexpr(pattern = "[[:alpha:]]+", d))[[1]]
    m  <- gsub(";", "", unlist(m))
    
    List_env1 <- c("Marine","Freshwater","brackish")
    List_env2 <- c("bathydemersal", "bathypelagic", "benthopelagic",
                   "benthopelagic.", "demersal","demersal.", "pelagic", 
                   "pelagic-neritic", "pelagic-oceanic", "reef-associated")
    clim      <- c("Tropical", "Temperate", "Boreal", "Subtropical", "Deep-water")
    
    env1   <- paste(m[which(is.element(m, List_env1) == TRUE)], collapse = "_")
    
    env2   <- m1[which(is.element(m1,List_env2) == TRUE)]
    env2_1 <-  m[which(is.element(m,List_env2) == TRUE)]
    
    w_IUCN  <- which(sapply(lapply(a1, XML::xmlValue), function(x) {
      regexec(pattern="IUCN", x)[[1]][1] }) > 0)
    
    if (length(w_IUCN) == 0) {
      
      IUCN_status = NA
      
    } else {
      
      d1_IUCN     <- XML::xmlValue(a1[[w_IUCN[length(w_IUCN)]]])
      IUCN        <- unlist(regmatches(d1_IUCN,gregexpr(pattern= "[[:alpha:]]+)",
                                                        d1_IUCN)))
      
      IUCN_status <- sub(pattern = "[[:punct:]]", replacement = "", IUCN[1])
    }
    
    rm(a1)
    
    res <- data.frame(c("IUCN_status" = IUCN_status))
    
    rownames(res) = x
    
  } else {
    
    IUCN_status = "A_verifier"
    
    res <- data.frame(c("IUCN_status" = IUCN_status))
    rownames(res) = x
  }
  
  res
}

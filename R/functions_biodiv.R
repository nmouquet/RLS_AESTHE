###################################################################################################
#' Functions used in the sripts of the folder analysis/biodiversity
#'
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         François Guilhaumon, \email{francois.guilhaumon@@ird.fr}
#'
#' @date 2021/02/17
##################################################################################################

# GET CLASSIF
#' Title
#'
#' @param names 
#'
#' @return
#' @export
#'
get_classif <- function(names) {
  # names <- table$acc_sci_name[1:10]
  A <- data.frame(name = names, kingdom = NA, phylum = NA, 
                  # subphylum = NA, superclass = NA, 
                  class = NA, order = NA, family = NA, genus = NA, species = NA)
  for (i in 1:length(names)) {
    classi <- taxize::classification(names[i], db="worms")
    classi <- classi[[1]]
    if (length(classi)==1) {
      A[i, "kingdom"]    = NA
      A[i, "phylum"]     = NA
      # A[i, "subphylum"]  = NA
      # A[i, "superclass"] = NA
      A[i, "class"]      = NA
      A[i, "order"]      = NA
      A[i, "family"]     = NA
      A[i, "genus"]      = NA
      A[i, "species"]    = NA
    } else {
      kingdom    <- classi$name[classi$rank=="Kingdom"]
      phylum     <- classi$name[classi$rank=="Phylum"]
      # subphylum  <- classi$name[classi$rank=="Subphylum"]
      # superclass <- classi$name[classi$rank=="Superclass"]
      class      <- classi$name[classi$rank=="Class"]
      order      <- classi$name[classi$rank=="Order"]
      family     <- classi$name[classi$rank=="Family"]
      genus      <- classi$name[classi$rank=="Genus"]
      species    <- classi$name[classi$rank=="Species"]
      A[i, "kingdom"]    = kingdom
      A[i, "phylum"]     = phylum    
      # A[i, "subphylum"]  = subphylum 
      # A[i, "superclass"] = superclass
      A[i, "class"]      = class
      A[i, "order"]      = order     
      A[i, "family"]     = family    
      A[i, "genus"]      = genus     
      A[i, "species"]    = species   
    }
  }
  return(A)
}
#

# Get Ages
#' Title
#'
#' @param tree 
#'
#' @return
#' @export
#'
get_ages <- function(tree){
  
  nsp <- length(tree$tip.label) # number of species
  
  tips <- which(tree$edge[,2] <= nsp) # get the starting and ending nodes of each edge # security to take only the ones inferior to the number of species ie "leaves" 
  
  ages <- tree$edge.length[tips] # the age of a species is actually the length of the edges from the first node of the phylo tree to the species' leaf
  
  names(ages) <- tree$tip.label
  
  ages
  
}
#
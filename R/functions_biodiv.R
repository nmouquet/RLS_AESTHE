###################################################################################################
#' Functions used in the scripts of the folder analysis/biodiversity
#'
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         François Guilhaumon, \email{francois.guilhaumon@@ird.fr}
#'
#' @date 2021/02/17
##################################################################################################

# GET CLASSIF
#' Extract classification of a list of species from the worms database
#'
#' @param names species names
#'
#' @return
#' @export
#'
get_classif <- function(names) {
  A <- data.frame(name = names, kingdom = NA, phylum = NA, 
                  class = NA, order = NA, family = NA, genus = NA, species = NA)
  for (i in 1:length(names)) {
    classi <- taxize::classification(names[i], db="worms")
    classi <- classi[[1]]
    if (length(classi)==1) {
      A[i, "kingdom"]    = NA
      A[i, "phylum"]     = NA
      A[i, "class"]      = NA
      A[i, "order"]      = NA
      A[i, "family"]     = NA
      A[i, "genus"]      = NA
      A[i, "species"]    = NA
    } else {
      kingdom    <- classi$name[classi$rank=="Kingdom"]
      phylum     <- classi$name[classi$rank=="Phylum"]
      class      <- classi$name[classi$rank=="Class"]
      order      <- classi$name[classi$rank=="Order"]
      family     <- classi$name[classi$rank=="Family"]
      genus      <- classi$name[classi$rank=="Genus"]
      species    <- classi$name[classi$rank=="Species"]
      A[i, "kingdom"]    = kingdom
      A[i, "phylum"]     = phylum    
      A[i, "class"]      = class
      A[i, "order"]      = order     
      A[i, "family"]     = family    
      A[i, "genus"]      = genus     
      A[i, "species"]    = species   
    }
  }
  return(A)
}


# Get Ages
#' compute the age of all the species on the leaves of a phylgenetic tree
#'
#' @param treea phylogenetic object
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

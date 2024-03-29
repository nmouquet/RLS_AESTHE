################################################################################
#' 
#' #' Functions used in the scripts of the folder `analysis/biodiversity`
#'
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         François Guilhaumon, \email{francois.guilhaumon@@ird.fr},
#'         Alienor Stahl, \email{a.stahl67@@gmail.com},
#'         Florian Baletaud, \email{baletaudflorian@@gmail.com}
#'
#' @date 2021/02/17 first created
#' 
################################################################################



#' Extract classification of a list of species from the WORMS database
#'
#' @param names A character vector. Species names
#'
#' @return A `data.frame` with the following variables: `kingdom`, `phylum`, 
#'   `class`, `order`, `family`, `genus`, and `species`.
#'   
#' @export

get_classif <- function(names) {
  
  A <- data.frame(name = names, kingdom = NA, phylum = NA, class = NA, 
                  order = NA, family = NA, genus = NA, species = NA)
  
  for (i in 1:length(names)) {
    
    classi <- taxize::classification(names[i], db = "worms")
    classi <- classi[[1]]
    
    if (length(classi) == 1) {
      
      A[i, "kingdom"] <- NA
      A[i, "phylum"]  <- NA
      A[i, "class"]   <- NA
      A[i, "order"]   <- NA
      A[i, "family"]  <- NA
      A[i, "genus"]   <- NA
      A[i, "species"] <- NA
      
    } else {
      
      kingdom    <- classi$name[classi$rank == "Kingdom"]
      phylum     <- classi$name[classi$rank == "Phylum"]
      class      <- classi$name[classi$rank == "Class"]
      order      <- classi$name[classi$rank == "Order"]
      family     <- classi$name[classi$rank == "Family"]
      genus      <- classi$name[classi$rank == "Genus"]
      species    <- classi$name[classi$rank == "Species"]
      
      A[i, "kingdom"] <- kingdom
      A[i, "phylum"]  <- phylum    
      A[i, "class"]   <- class
      A[i, "order"]   <- order     
      A[i, "family"]  <- family    
      A[i, "genus"]   <- genus     
      A[i, "species"] <- species   
    }
  }
  
  A
}



#' Get Ages
#' 
#' @description 
#' Computes the age of all the species on the leaves of a phylgenetic tree.
#'
#' @param tree A phylogenetic object
#'
#' @return A vector of ages of the leaves of tree
#' 
#' @export

get_ages <- function(tree) {
  
  nsp <- length(tree$tip.label) # number of species
  
  tips <- which(tree$edge[ , 2] <= nsp) # get the starting and ending nodes of each edge
  # security to take only the ones inferior to the number of species ie "leaves" 
  
  ages <- tree$edge.length[tips] # the age of a species is actually the length of the edges from
  # the first node of the phylo tree to the species' leaf
  
  names(ages) <- tree$tip.label
  
  ages
}

###################################################################################################
#' Functions used in the sripts of the folder analysis/conservation
#' 
#'
#' @date 2021/02/17
##################################################################################################


# dunnTest from package FSA but with signification asterixeses's
dunnTestExtra <- function(x, dat, metho){
  dunn <- FSA::dunnTest(x, data = dat, method = metho)
  dunn$res$signif <- unlist(lapply(dunn$res$P.adj, function(x){
    if(x > 0.05) r  <- "NS."
    if(x < 0.05) r  <- "*"
    if(x < 0.01) r  <- "**"
    if(x < 0.001) r <- "***"
    r
  }))
  dunn
}

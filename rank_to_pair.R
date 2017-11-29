#' Convert rankings to pairwise comparisons (can't deal with ties allowed)
#'
#' @param ranking a matrix of rankings, as returned by ratings2ranking
#' 
#' @return a matrix W where w_{i,j} is the number of rankings in which i beat j
#' 
#' @export ranking2pairs
#' 

ranking2pairs <- function(ranking) {
  n.alts <- nrow(ranking)
  w <- matrix(NA, nrow=n.alts, ncol=n.alts)
  for (i in 1:n.alts) {
    for (j in 1:n.alts) {
      w[i,j] <- sum(ranking[i,]<ranking[j,])
    }
  }
  w
}
#' Convert ratings to pairwise comparisons
#'
#' @param rating a matrix of ratings, as returned by make.ratings
#' 
#' @return a matrix W where w_{i,j} is the number of rankings in which i beat j
#' 
#' @export ratings2pairs
#' 

ratings2pairs <- function(rating, allow.ties=FALSE, delta=NULL) {
  n.alts <- nrow(rating)
  w <- matrix(NA, nrow=n.alts, ncol=n.alts)
  for (i in 1:n.alts) {
    for (j in 1:n.alts) {
      if (allow.ties){
        if (!is.numeric(delta)) {stop("please provide threshold delta for tied ranks")}
        w[i,j] <- sum(abs(rating[i,]-rating[j,])<delta)
      }
      else{
        w[i,j] <- sum(rating[i,]>rating[j,])
      }
    }
  }
  w
}
#' Convert ratings to rankings
#' 
#' @param ratings The matrix of ratings as returned in the first list item from make.ratings
#'
#' @return matrix where each column contains the ranking by one rater. e.g. "2 3 1 4" means alternative 1 was ranked second, 2 was ranked third, etc.
#'
#' @export ratings2ranking
#' @export rank.tie

ratings2ranking <- function(ratings, allow.ties=FALSE, delta=NULL) {
  invrank <- apply(ratings, 2, order, decreasing=TRUE)
  rank <- apply(invrank, 2, order)
  if (allow.ties) {
    if (!is.numeric(delta)) {stop("please provide threshold delta for tied ranks")}
    rank <- apply(ratings, 2, rank.tie, delta=delta)
  }
  rank
}

rank.tie <- function(ratings, delta) {
  rank <- order(order(ratings, decreasing=TRUE))
  rank.tie <- rank
  for (k in 1:(length(ratings)-1)){
    which.tied <- which(rank>k & abs(ratings-ratings[rank==k])<delta)
    if (length(which.tied)==0) {next} # no ties with this one
    rank.tie[which.tied] <- rank.tie[rank==k]
  }
  rank.tie
}
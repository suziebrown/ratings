#' Convert ratings to rankings
#' 
#' @param ratings The matrix of ratings as returned in the first list item from make.ratings
#'
#' @return matrix where each column contains the ranking by one rater. e.g. "2 3 1 4" means alternative 1 was ranked second, 2 was ranked third, etc.
#'
#' @export ratings2ranking

ratings2ranking <- function(ratings) {
  invrank <- apply(ratings, 2, order, decreasing=TRUE)
  rank <- apply(invrank, 2, order)
  rank
}
#' Convert ratings to rankings
#' 
#' @param ratings The matrix of ratings as returned in the first list item from make.ratings
#'
#' @return matrix where each column contains the ranking by one rater. e.g. "2 3 1 4" means 2 was ranked first, 3 second etc.
#'
#' @export ratings2ranking

ratings2ranking <- function(ratings) {
  apply(ratings, 2, order, decreasing=TRUE)
}
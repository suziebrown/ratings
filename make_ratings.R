#' Simulate ratings data
#' 
#' @param n.alts number of alternatives to be rated
#' @param n.raters number of people assigning ratings
#' @param strengths a vector of length n.alts containing the underlying strengths; if NULL strengths are chosen at random
#' @param sd.strengths standard deviation for randomly assigning Normally-distributed strengths
#' @param sd.noise standard deviation of additive Normal noise
#' 
#' @return a list of two items; first the matrix where each column is the vector of ratings assigned by one rater, then the vector of random underlying strengths
#' 
#' @export make.ratings
#' 

make.ratings <- function(n.alts, n.raters, strengths=NULL, sd.strengths=1, sd.noise=0.1) {
  if (!is.null(strengths) && length(strengths)!=n.alts){stop("length of strengths vector is not equal to number of alternatives")}
  if (is.null(strengths)){strengths <- rnorm(n.alts, 0, sd.strengths)}
  ratings <- replicate(n.raters, strengths + rnorm(n.alts, 0, sd.noise))
  list(ratings=ratings, strengths=strengths)
}
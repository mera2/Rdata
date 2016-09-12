euclidean <- function(x,y) {
  r <- x%%y
  return(ifelse(r, euclidean(y, r), y))
}

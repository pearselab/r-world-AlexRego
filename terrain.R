# generate matrix with odd dimensions
mat <-matrix(NA)
genmat <- function(n){
  mat <- matrix(data=NA, nrow = (2*n)+1, ncol = (2*n)+1)
  mat[1,1] <- rnorm(1,0,10)
  mat[(2*n)+1,1] <- rnorm(1,0,10)
  mat[1,(2*n)+1] <- rnorm(1,0,10)
  mat[(2*n)+1,(2*n)+1] <- rnorm(1,0,10)
  return(mat)
}

# create diamond step function
d.s <- function(n){
  mat <- genmat(n)
  ## find corners
  corners <- c(mat[1,1],mat[nrow(mat),1],mat[1,ncol(mat)],mat[nrow(mat),ncol(mat)])
  ## calculate averaged
  avg <- mean(corners) + rnorm(1,0,5)
  ## input average into center of matrix
  mat[ceiling(nrow(mat)/2),ceiling(ncol(mat)/2)] <- avg
  return(mat)
}

## square-step
s.s <- function(n){
  mat <- d.s(n)
  middle <- mat[ceiling(nrow(mat)/2),ceiling(ncol(mat)/2)]
  ## Averages of points
  mat[1,ceiling(ncol(mat)/2)] <- mean(c(middle,mat[1,1],mat[nrow(mat),1]))
}

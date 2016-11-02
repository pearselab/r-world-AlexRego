# generate matrix with odd dimensions
genmat <- function(n){
  out.mat <- matrix(data=NA, nrow = (2*n)+1, ncol = (2*n)+1)
  out.mat[1,1] <- rnorm(1,0,10)
  out.mat[(2*n)+1,1] <- rnorm(1,0,10)
  out.mat[1,(2*n)+1] <- rnorm(1,0,10)
  out.mat[(2*n)+1,(2*n)+1] <- rnorm(1,0,10)
  return(out.mat)
}

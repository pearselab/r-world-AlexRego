# generate matrix with odd dimensions
genmat <- function(n){
  out.mat <- matrix(data=NA, nrow = (2*n)+1, ncol = (2*n)+1)
  out.mat[1,1] <- rnorm(1,0,10)
  out.mat[(2*n)+1,1] <- rnorm(1,0,10)
  out.mat[1,(2*n)+1] <- rnorm(1,0,10)
  out.mat[(2*n)+1,(2*n)+1] <- rnorm(1,0,10)
  return(out.mat)
}

mat <- genmat(5)
# create diamond step function
d.s <- function(mat){
  ## find corners
  ul <- mat[1,1]
  ur <- mat[1,ncol(mat)]
  ll <- mat[nrow(mat),1]
  lr <- mat[nrow(mat),ncol(mat)]
  corners <- c(ul,ur,ll,lr)
  ## calculate averaged
  avg <- mean(corners) + rnorm(1,0,1)
  ## input average into center of matrix
  mat[median(1:nrow(mat)),median(1:ncol(mat))] <- avg
  return(mat)
}
mat <- d.s(mat)

# square-step
s.s <- function(n){
  ## find center + corners
  center <- mat[median(1:nrow(mat)),median(1:ncol(mat))]
  ul <- mat[1,1]
  ur <- mat[1,ncol(mat)]
  ll <- mat[nrow(mat),1]
  lr <- mat[nrow(mat),ncol(mat)]

  ## placing averages
  # upper
  mat[1,median(1:ncol(mat))] <- mean(ul,center,ur) + rnorm(1,0,1)
  # right
  mat[median(1:nrow(mat)),ncol(mat)] <- mean(ur,center,lr) + rnorm(1,0,1)
  # lower
  mat[nrow(mat),median(1:ncol(mat))] <- mean(ll,center,lr) + rnorm(1,0,1)
  # left
  mat[median(1:nrow(mat)),1] <- mean(ul,center,ll) + rnorm(1,0,1)
  return(mat)
}
mat <- s.s(mat)
# diamond square step
dss <- function(mat){
  mat <- mat
  while(any(is.na(mat))){
    mat <- d.s(mat)
    mat <- s.s(mat)
    mat <- mat
  }
  return(mat)
}

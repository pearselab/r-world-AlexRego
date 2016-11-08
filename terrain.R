# generate matrix with odd dimensions
mat <-matrix(NA)
genmat <- function(n){
  mat <- matrix(data=NA, nrow = (2**n)+1, ncol = (2**n)+1)
  mat[1,1] <- rnorm(1,0,10)
  mat[(2**n)+1,1] <- rnorm(1,0,10)
  mat[1,(2**n)+1] <- rnorm(1,0,10)
  mat[(2**n)+1,(2**n)+1] <- rnorm(1,0,10)
  return(mat)
}

mat <- genmat(3)
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
  mat <- d.s(n)
  middle <- mat[ceiling(nrow(mat)/2),ceiling(ncol(mat)/2)]
  ## Averages of points
  mat[1,ceiling(ncol(mat)/2)] <- mean(c(middle,mat[1,1],mat[nrow(mat),1]))
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
  half <- median(1:nrow(mat))
  side <- nrow(mat)
  mat <- d.s(mat)
  mat <- s.s(mat)
  while(any(is.na(mat))){
    #quadrant one
    mat[1:half,1:half] <- d.s(mat[1:half,1:half])
    mat[1:half,1:half] <- s.s(mat[1:half,1:half])
    # quadrant two
    mat[median(1:side):side,1:half] <- d.s(mat[median(1:side):side,1:half])
    mat[median(1:side):side,1:half] <- s.s(mat[median(1:side):side,1:half])
    # quadrant three
    mat[1:half,half:side] <- d.s(mat[1:half,half:side])
    mat[1:half,half:side] <- s.s(mat[1:half,half:side])
    # quadrant four
    mat[side:half,side:half] <- d.s(mat[side:half,side:half])
    mat[side:half,side:half] <- s.s(mat[side:half,side:half])
    # new half to iterate over smaller square 
    half <- median(1:half)
    if(all(!is.na(mat))){
      return(mat)
    }
  }
}

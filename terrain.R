# generate matrix with odd dimensions
genmat <- function(n){
  side <- (2^n)+1
  mat <- matrix(data=NA, nrow = side, ncol = side)
  mat[1,1] <- rnorm(1,0,10)
  mat[side,1] <- rnorm(1,0,10)
  mat[1,side] <- rnorm(1,0,10)
  mat[side,side] <- rnorm(1,0,10)
  return(mat)
}

mat <- genmat(3)
# create diamond step function
d.s <- function(mat, sd){
  ## find corners
  ul <- mat[1,1]
  ur <- mat[1,ncol(mat)]
  ll <- mat[nrow(mat),1]
  lr <- mat[nrow(mat),ncol(mat)]
  corners <- c(ul,ur,ll,lr)
  ## calculate averaged
  avg <- mean(corners) + rnorm(1,0,sd)
  ## input average into center of matrix
  mat[median(1:nrow(mat)),median(1:ncol(mat))] <- avg
  return(mat)
}

# square-step
s.s <- function(mat, var){
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
  mat[1,median(1:ncol(mat))] <- mean(ul,center,ur) + rnorm(1,0,sd)
  # right
  mat[median(1:nrow(mat)),ncol(mat)] <- mean(ur,center,lr) + rnorm(1,0,sd)
  # lower
  mat[nrow(mat),median(1:ncol(mat))] <- mean(ll,center,lr) + rnorm(1,0,sd)
  # left
  mat[median(1:nrow(mat)),1] <- mean(ul,center,ll) + rnorm(1,0,sd)
  return(mat)
}

# diamond square step
dss <- function(n, sd){
  mat <- genmat(n)
  side <- (2^n)+1
  sd <- sd
  for(k in 2^(n:1)){
    for(i in seq(1,side-1,by=k)){
      for(j in seq(1,side-1,by=k)){
        mat[j:(j+k),i:(i+k)] <- d.s(mat[j:(j+k),i:(i+k)],sd)
        mat[j:(j+k),i:(i+k)] <- s.s(mat[j:(j+k),i:(i+k)],sd)
      }
    }
  sd <- abs(sd/1.5)
  }
  return(mat)
}

# wrapper function
terrain <- function(n, sd, lakes){
  mat <- dss(n, sd)
  if(lakes==TRUE){
    mat[mat<0] <- NA
  }
  return(mat)
}

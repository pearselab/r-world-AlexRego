# generate matrix with odd dimensions


#' Generates starting terrain matrix; includes starting values
#'
#' @param n Size of the matrix will be (2^n)+1, which will always produce a
#'   matrix that is subdivisible into matrices with odd sides
#' @return a terrain matrix


genmat <- function(n){
  side <- (2^n)+1
  mat <- matrix(data=NA, nrow = side, ncol = side)
  mat[1,1] <- rnorm(1,0,10)
  mat[side,1] <- rnorm(1,0,10)
  mat[1,side] <- rnorm(1,0,10)
  mat[side,side] <- rnorm(1,0,10)
  return(mat)
}

#' The diamond-step algorithm
#'
#' @param mat This inputs the matrix on which the diamond-step algorithm will act upon
#' @param sd This is the noise that will be added to each iteration of the DS algorithm
#' @return returns the matrix, now manipulated to have a center

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

#' The square-step algorithm
#'
#' @param mat This inputs the matrix on which the square-step algorithm will act upon
#' @param sd This is the noise that will be added to each iteration of the SS algorithm
#' @return returns the matrix, now manipulated to contain north/east/south/west points
#'    originating from the center produced by the diamond-step

# square-step
s.s <- function(mat, sd){
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

#' The diamond-square-step algorithm
#'
#' @param n Will create the matrix
#' @param sd This is the noise that will be added to each iteration of the DSS algorithm
#' @return returns the matrix, now manipulated by the DSS algorithm

# diamond square step
dss <- function(n, sd){
  mat <- genmat(n)
  side <- (2^n)+1
  sd <- sd
  ## I talked with Bodie and Spencer about how to implement the DSS on sub-matrices.
  ## k will run through half-sides of each sub-matrix within the original matrix
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

#' The wrapper function for the diamond-square-step algorithm
#'
#' @param n Generates original matrix with side lengths (2^n)+1
#' @param sd This is the noise that will be added to each iteration of the DSS algorithm
#' @param lakes If True, will induce values in the matrix less than 0 to be NA values, indicating water
#' @return returns the completed matrix

# wrapper function
terrain.gen <- function(n, sd, lakes){
  mat <- dss(n, sd)
  if(lakes==TRUE){
    mat[mat<0] <- NA
  }
  return(mat)
}

#setting up parameters
names <- c('lupin', 'alfalfa')
repro <- c(.5,.5)
survive <- c(.9,.9)
comp.mat <- matrix(1,2,2)
comp.mat[1,2] <- .7
comp.mat[2,1] <- .3
rownames(comp.mat) <- names
colnames(comp.mat) <- names

#' Setting up plant information
#'
#' @param repro
#' @param survive
#' @param comp.mat
#' @param names
#' @return returns a list containing all the parameters that will constitute the ecosystem

#plant set up
setup.plants <- function(repro,survive,comp.mat, names=NULL){
  if(is.null(names)){
    names <- letters[seq_along(repro)]
  }
  if(length(repro) != length(survive)){
    stop("Reproduction and survival parameters needed for all species")
  }
  repro <- setNames(repro,names)
  survive <- setNames(survive,names)
  return(list(repro=repro,survive=survive,comp.mat=comp.mat,names=names))
}

info <- setup.plants(repro,survive, comp.mat,names)

# set up ecosystem
eco <- array('',dim=c(dim(terrain),3))

for(i in 1:length(terrain))
  eco[sample(nrow(eco),1), sample(ncol(eco),1), 1] <- sample(info$names,1)

for(i in seq_len(dim(eco)[3])){
  eco[,,i][is.na(terrain)] <- NA
}

#' Survival function of plants in ecosystem
#'
#' @param cell This is a particular cell in the ecosystem at a particular time step
#' @param info Info is a list contianing all of the parameter information of the ecosystem
#' @return returns the cell to be input back into the larger ecosystem matrix

# survival function
survive <- function(cell,info){
  if(is.na(cell)){
    cell <- NA
  }
  else{
    if(cell==''){
      cell <- ''
    }
    else{
      if(runif(1) >= info$survive[cell]){
        cell <- ''
      }
    }
  }
  return(cell)
}

#' reproduction function of plants in ecosystem
#'
#' @param row The row position of the ecosystem array to be input
#' @param col The column position of the ecosystem array to be input
#' @param time Which timestep in the array that will be input
#' @param eco The ecosystem array
#' @param info Info is a list contianing all of the parameter information of the ecosystem
#' @return returns the ecosystem array, now with plants having reproduced and undergone competition from surrounding cells
#reproduction
#row and col are going to be position for certain iteration
reproduce <- function(row,col,time, eco,info){
  surrounding <- as.matrix(expand.grid(row+c(-1,0,1),col+c(-1,0,1)))
  surrounding[surrounding < 1] <- dim(eco)[1]
  surrounding[surrounding > dim(eco)[1]] <- 1
  for(r in 1:nrow(surrounding)){
    if(eco[row,col,time] == "alfalfa" | eco[row,col,time] == "lupin"){
      if(runif(1) <= info$repro[eco[row,col,time]]){
        if(!is.na(eco[surrounding[r,1],surrounding[r,2],time])){
          if(eco[surrounding[r,1],surrounding[r,2],time] == "lupin" | eco[surrounding[r,1],surrounding[r,2],time] == "alfalfa"){
            if(runif(1) <= info$comp.mat[eco[row,col,time],eco[surrounding[r,1],surrounding[r,2],time]]){
              eco[surrounding[r,1],surrounding[r,2],time] <- eco[row,col,time]
            }
          } else{
              eco[surrounding[r,1],surrounding[r,2],time] <- eco[row,col,time]
            }
        }
      }
    }
  }
  return(eco)
}

#' Timestep function of plants in ecosystem
#'
#' @param eco The ecosystem array containing information on plant position at each time step
#' @param info Info is a list contianing all of the parameter information of the ecosystem
#' @return returns the ecosystem array, now with plants at all timesteps having reproduced in adjacent cells

#timestep for plant
plant.timestep <- function(eco,info){
  for(k in 1:(dim(eco)[3]-1)){
    for(i in 1:dim(eco)[1]){
      for(j in 1:dim(eco)[2]){
        eco[i,j,k+1] <- survive(eco[i,j,k], info)
        eco <- reproduce(i,j,k,eco,info)
      }
    }
  }
  return(eco)
}

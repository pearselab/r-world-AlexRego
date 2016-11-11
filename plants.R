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
#setting up parameters
names <- c('lupin', 'alfalfa')
repro <- c(.5,.5)
survive <- c(.9,.9)
comp.mat <- matrix(1,2,2)
comp.mat[1,2] <- .7
comp.mat[2,1] <- .3
rownames(comp.mat) <- names
colnames(comp.mat) <- names

info <- setup.plants(repro,survive, comp.mat,names)

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

#timestep for plant
plant.timestep <- function(eco,info){
  for(k in 1:(dim(eco)[3]-1)){
    for(i in 1:dim(eco)[1]){
      for(j in 1:dim(eco)[2]){
        eco[i,j,k+1] <- survive(eco[i,j,k], info)
      }
    }
  }
  return(eco)
}

# set up ecosystem
eco <- array('',dim=c(dim(terrain),3))

for(i in 1:length(terrain))
  eco[sample(nrow(eco),1), sample(ncol(eco),1), 1] <- sample(info$names,1)

for(i in seq_len(dim(eco)[3])){
  eco[,,i][is.na(terrain)] <- NA
}

# run plant ecosystem
# run.eco <- function(eco=eco, info=info,timesteps)
# for(i in 1:timesteps){
#   plant.timestep(ecosystem, info)
# }

#reproduction
reproduce <- function(row,col,plants,info){
  possible.locations <- as.matrix(expand.grid(row+c(-1,0,1),col+c(-1,0,1)))
  for(i in possible.locations){
    for(j in possible.locations){
      if(plants[i,j,k] != NA){
        if(runif(1) <= info$reproduce[plant]){
          plants[i,j] <- info$names[plant]
      }
    }
  }
  return(plants)
}

plant <- reproduce(row,column,plants,info)

# competition
fight <- function(species_names,info){
  sample(species_names, 1, prob=comp.mat[row,column])
}

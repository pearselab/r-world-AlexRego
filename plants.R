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


# survival function
survive <- function(cell,info){
  if(!is.na(cell) | plant==''){
    if(runif(1) <= info$survive[plant]){
      return(plant)
    }
  }
}

#timestep for plant
plant.timestep <- function(plants,terrain,info){
  for(i in terrain){
    for(j in terrain){
      new.plants.matrix[i,j] <- survive(terrain[i,j])
    }
  }
  return(new.plants.matrix)
}

#run.plant.ecosystem
plants <- array('',dim=c(dim(terrain),timesteps+1))

for(i in seq_len(dim(plants)[3])){
  plants[,,i][is.na(terrain)] <- NA
}

#reproduction
reproduce <- function(row,col,plants,info){
  possible.locations <- as.matrix(expand.grid(row+c(-1,0,1),col+c(-1,0,1)))
  for(i in possible.locations){
    for(j in possible.locations){
      if(possible.locations[i,j] != NA){
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

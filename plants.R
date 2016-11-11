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
        reproduce(i,j,k,eco,info)
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
#row and col are going to be position for certain iteration
reproduce <- function(row,col,time, eco,info){
  possible.locations <- as.matrix(expand.grid(row+c(-1,0,1),col+c(-1,0,1)))
  possible.locations[possible.locations < 1] <- dim(eco)[1]
  possible.locations[possible.locations > dim(eco)[1]] <- 1
  for(r in 1:nrow(possible.locations)){
    if(eco[row,col,time] == "alfalfa" | eco[row,col,time] == "lupin"){
      if(runif(1) <= info$reproduce[eco[row,col,time]]){
        if(!is.na(eco[possible.locations[r,1],possible.locations[r,2],time])){
          eco[possible.locations[r,1],possible.locations[r,2],time] <- eco[row,column,time]
      return(eco)
        }
      }
    } else{
      return(eco)
      }
  }
}


# as.matrix(expand.grid(2:(nrow(terrain)-1)+c(-1,0,1),2:(ncol(terrain)-1)+c(-1,0,1)))

plant <- reproduce(row,column,plants,info)

# competition
fight <- function(species_names,info){
  sample(species_names, 1, prob=comp.mat[row,column])
}

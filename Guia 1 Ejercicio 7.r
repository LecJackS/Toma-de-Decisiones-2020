play <- function(policy=0){
  # Policy 0: Always stay
  # Policy 1: Always change
  
  doors <- c(1:3)
  prize <- sample(doors, 1)
  #free <- doors[doors!=prize]
  
  first_elec <- sample(doors, 1)
  sec_doors <- doors[doors!=first_elec]
  if(policy==0){
    if(prize == prim_elec){
      win=TRUE
    }else{
      win=FALSE
    }
  } else{
    # Se le ofrece cambiar y cambia
    if(prize %in% sec_doors){
      win=TRUE
    }else{
      win=FALSE
    }
  }
  return(win)
}

## Simulo Nrep juegos
count_wins_0 <- 0
count_wins_1 <- 0
Nrep <- 900000
for(i in 1:Nrep){
  win <- play(policy=0)
  
  if(win){
    # Gano
    count_wins_0 = count_wins_0 + 1
  }
}
for(i in 1:Nrep){
  win <- play(policy=1)
  
  if(win){
    # Gano
    count_wins_1 = count_wins_1 + 1
  }
}
p_win_0 <- count_wins_0/Nrep
p_win_1 <- count_wins_1/Nrep

p_win_0
p_win_1


?formula
  
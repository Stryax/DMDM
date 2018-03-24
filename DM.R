#Cell 15 only ########################
#Creation markov matrix ##############
modMatrix<- function(dice,trap,circle){
  M = matrix(0, ncol = 15, nrow = 15)
  #Safe dice
  if(dice=0){
    for(i in 1:15){
      for(j in 1:15){
        if(i == j || i == j-1){
          M[i,j] = 0.5
        }
      }
    }
    M[3,3] = 0.5
    M[3,4] = 0.25
    M[3,11] = 0.25
    M[15,15] = 1
  #Normal dice
  if(dice=1){
    for(i in 1:15){
      for(j in 1:15){
        if(i == j || i == j-1 || i == j-2){
          M[i,j] = 1/3
        }
      }
    }
    M[3,3] = 1/3
    M[3,4] = 1/6
    M[3,5] = 1/6
    M[3,11] = 1/6
    M[3,12] = 1/6
    M[15,15] = 1
    #Modifying circle probabilities
    if(circle==1){
      M[14,1] = 1/3
    }else if(circle==0){
      M[14,15] = 2/3
    }
    
    for(k in 1:length(trap)){
      if(k == 1){
        
      }
    }
    
    
    M = round(M, 2)
  }
  #Risky dice
  if(dice=2){
    for(i in 1:15){
      for(j in 1:15){
        if(i == j || i == j-1 || i == j-2 || i == j-3){
          M[i,j] = 1/4
        }
      }
    }
    M[3,3] = 1/4
    M[3,4] = 1/8
    M[3,5] = 1/8
    M[3,6] = 1/8
    M[3,11] = 1/8
    M[3,12] = 1/8
    M[3,13] = 1/8
    M[15,15] = 1
    M[13,1] = 1/4
    M[14,1] = 1/4
    M[14,2] = 1/4
    M = round(M, 2)
    
  }
}

cells = matrix(ncol = 15, nrow = )

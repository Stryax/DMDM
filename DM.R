#Trap on shortcut ???
######################################
#Creation markov matrix ##############
######################################
modMatrix<- function(dice,trap,circle){ #Must be used on a matrix full of 0's
  M = matrix(0, ncol = 15, nrow = 15)
  trapList1 = NULL #initiate list for traps
  trapList2 = NULL
  #Safe dice
  #basic matrix
  if(dice==0){
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
    M[10,15] = 0.5
    M[10,11]= 0
  }
  #Normal dice
  if(dice==1){
    #basic matrix
    for(i in 1:15){
      for(j in 1:15){
        if(i == j || i == j-1 || i == j-2){
          M[i,j] = 1/3
        }
      }
    }
    M[3,3] = 1/3 #Modifying probabilities
    M[3,4] = 1/6
    M[3,5] = 1/6
    M[3,11] = 1/6
    M[3,12] = 1/6
    M[9,11] = 0
    M[9,15] = 1/3
    M[10,11] = 0
    M[10,12] = 0
    M[15,15] = 1
    
    #Modifying trap probabilities
    trapList1 = NULL
    trapList2 = NULL
    for(i in 1:length(trap)){
      if(trap[i] == 1){
        trapList1[length(trapList1)+1] = i
      }else if(trap[i] == 2){
        trapList2[length(trapList2)+1] = i
      }
    }
    
    #Modifying circle probabilities
    
    if(circle==1){
      M[10,1] = 1/3
      M[10,15] = 1/3
      M[14,1] = 1/3
    }else if(circle==0){
      M[10,15] = 2/3
      M[14,15] = 2/3
    }

    
    for(j in trapList1){
      #Modifying trap 1 probabilities
      #This is supposing there can't be traps 1 in the first 3 cases.
      if(j==11){
        M[11,12]=M[11,12]/2
        M[11,13]= M[11,13]/2
        M[11,11] = M[11,11]/2
        M[11,1]= 1/2
      }else if(j==12){
        M[12,13]=M[12,13]/2
        M[12,14]= M[12,14]/2
        M[12,12] = M[12,12]/2
        M[12,2]= 1/2
      }else if(j==13){
        M[13,14]=M[13,14]/2
        M[13,15]= M[13,15]/2
        M[13,13] = M[13,13]/2
        M[13,3]= 1/2
      }else{
      M[j-2, j] = M[j-2, j]/2
      M[j-2, j-3] = M[j-2, j-3] + M[j-2, j]
      
      M[j-1, j] = M[j-1, j]/2
      M[j-1, j-3] = M[j-1, j-3] + M[j-1, j]
      
      M[j, j] = M[j, j]/2
      M[j, j-3] = M[j, j-3] + M[j, j]
      }
      
    }
    for(j in trapList2){
      #Modifying trap 2 probabilities

      M[j-2, j] = M[j-2, j]/2
      M[j-2, 1] = M[j-2, 1] + M[j-2, j]
      
      M[j-1, j] = M[j-1, j]/2
      M[j-1, 1] = M[j-1, 1] + M[j-1, j]
      
      M[j, j] = M[j, j]/2
      M[j, 1] = M[j, 1] + M[j, j]
      
    }
    M = round(M, 2)
  }
  #Risky dice
  if(dice==2){
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
    M[8,11] = 0
    M[8,15] = 1/4
    M[9,11] = 0
    M[9,12] = 0
    M[10,11] = 0
    M[10,12] = 0
    M[10,13] = 0
    M[15,15] = 1
    
    #Trap probabilities
    trapList1 = NULL
    trapList2 = NULL
    for(i in 1:length(trap)){
      if(trap[i] == 1){
        trapList1[length(trapList1)+1] = i
      }else if(trap[i] == 2){
        trapList2[length(trapList2)+1] = i
      }
    }
    
    #Circle probabilities
    if(circle==1){
      M[14,1] = 1/4
      M[14,2] = 1/4
      M[13,1] = 1/4
      M[9,1] = 1/4
      M[9,15] = 1/4
      M[10,1] = 1/4
      M[10,2] = 1/4
      M[10,15] = 1/4
    }else if(circle==0){
      M[14,15] = 3/4
      M[13,15] = 1/2
      M[10,15] = 3/4
      M[9,15] = 1/2
    }

    
    for(j in trapList1){
      #Modifying trap 1 probabilities
      #This is supposing there can't be traps 1 in the first 3 cases.
      if(j==11){
        M[11,12] = 0
        M[11,13] = 0
        M[11,14] = 0
        M[11,11] = 0
        M[11,1] = 1
      } else if(j==12){
        M[12,13] = 0
        M[12,14] = 0
        M[12,15] = 0
        M[12,12] = 0
        M[12,2] = 1
      } else if(j==13){
        M[13,14] = 0
        M[13,15] = 0
        M[13,1] = 0
        M[13,13] = 0
        M[13,3] = 1
      }else{
      M[j-3, j-3] = M[j-3, j-3] + M[j-3, j]
      M[j-3, j] = 0
      
      M[j-2, j-3] = M[j-2, j-3] + M[j-2, j]
      M[j-2, j] = 0
      
      M[j-1, j-3] = M[j-2, j-3] + M[j-1, j]
      M[j-1, j] = 0
      
      M[j, j-3] = M[j-2, j-3] + M[j, j]
      M[j, j] = 0
      }
    }
    for(j in trapList2){
      #Modifying trap 2 probabilities
      M[j-3, 1] = M[j-3, 1] + M[j-3, j]
      M[j-3, j] = 0
      
      M[j-2, 1] = M[j-2, 1] + M[j-2, j]
      M[j-2, j] = 0
      
      M[j-1, 1] = M[j-1, 1] + M[j-1, j]
      M[j-1, j] = 0
      
      M[j, 1] = M[j, 1] + M[j, j]
      M[j, j] = 0
      }
    M = round(M,2)
    
    
  }
  return(M) #return final matrix
}

trap = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) #trap list

##############################################
# Choice of policy ###########################
##############################################




VKF <- function(trap, circle){
  getProb <- function(dice, start){ #Getting prob
    if(dice == 0){
      return(safeM[start,])
    }else if(dice == 1){
      return(normM[start,])
    }else if(dice == 2){
      return(riskM[start,])
    }
  }
  #Init matrix for each dice
  safeM = modMatrix(0, trap, circle)
  normM = modMatrix(1, trap, circle)
  riskM = modMatrix(2, trap, circle)
  
  Rsa = matrix(1,15,1) #cost matrix
  Rsa[15] = 0 #cost at 15 is null
  VkT = matrix(0, 1001, 15)
  policy = matrix(0,15,1) #Init policy
  VkT[1,] = Rsa
  
  Vksa = NULL
  for(k in 1:1000){ #for k iterations (convergence)
    for(s in 1:15){ #for all states
      for(a in 0:2){ #for every possible action
        Vksa[a+1] = Rsa[s] + getProb(a, s) %*% VkT
      }
      VkT[s] = min(Vksa) #Getting min
      policy[s] = which.min(Vksa)-1
    }
  }
  return(list(VkT, policy))
}

listt = VKF(trap, 0)





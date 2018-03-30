#Cell 15 only ########################
#Trap on shortcut ???
#Creation markov matrix ##############
modMatrix<- function(dice,trap,circle){ #Must be used on a matrix full of 0's
  M = matrix(0, ncol = 15, nrow = 15)
  trapList1 = NULL
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
    M[3,3] = 1/3
    M[3,4] = 1/6
    M[3,5] = 1/6
    M[3,11] = 1/6
    M[3,12] = 1/6
    M[15,15] = 1
    #Modifying circle probabilities
    
    trapList1 = NULL
    trapList2 = NULL
    for(i in 1:length(trap)){
      if(trap[i] == 1){
        trapList1[length(trapList1)+1] = i
      }else if(trap[i] == 2){
        trapList2[length(trapList2)+1] = i
      }
    }
    
    
    if(circle==1){
      M[14,1] = 1/3
    }else if(circle==0){
      M[14,15] = 2/3
    }

    
    for(j in trapList1){
      #Modifying trap 1 probabilities
      #This is supposing there can't be traps 1 in the first 3 cases.
      M[j-2, j] = M[j-2, j]/2
      M[j-2, j-3] = M[j-2, j-3] + M[j-2, j]
      
      M[j-1, j] = M[j-1, j]/2
      M[j-1, j-3] = M[j-1, j-3] + M[j-1, j]
      
      M[j, j] = M[j, j]/2
      M[j, j-3] = M[j, j-3] + M[j, j]
      
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
    M = round(M,2)

    
    
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
    M[15,15] = 1
    M[13,1] = 1/4
    M[14,1] = 1/4
    M[14,2] = 1/4
    
    trapList1 = NULL
    trapList2 = NULL
    for(i in 1:length(trap)){
      if(trap[i] == 1){
        trapList1[length(trapList1)+1] = i
      }else if(trap[i] == 2){
        trapList2[length(trapList2)+1] = i
      }
    }
    
    
    if(circle==1){
      M[14,1] = 1/4
      M[14,2] = 1/4
      M[13,1] = 1/4
    }else if(circle==0){
      M[14,15] = 3/4
      M[13,15] = 1/2
    }

    
    for(j in trapList1){
      #Modifying trap 1 probabilities
      #This is supposing there can't be traps 1 in the first 3 cases.
      M[j-3, j-3] = M[j-3, j-3] + M[j-3, j]
      M[j-3, j] = 0
      
      M[j-2, j-3] = M[j-2, j-3] + M[j-2, j]
      M[j-2, j] = 0
      
      M[j-1, j-3] = M[j-2, j-3] + M[j-1, j]
      M[j-1, j] = 0
      
      M[j, j-3] = M[j-2, j-3] + M[j, j]
      M[j, j] = 0
      
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
  return(M)
}

cells = matrix(ncol = 15, nrow = )

trap = matrix(0, 1, 15)
trap = c(0,0,0,0,0,0,0,0,0,1,0,0,0,2,0)
matT = matrix(0,15,15)
matT = modMatrix(2, trap, 1)

##############################################
# Choice of policy ###########################
##############################################

rewM = matrix(1, 15, 15)
rewM[15,1:3] = 1000 #Cost of s15 = 0

circle = 1 #Can't go past case 15 (set to 1 if it is possible to do so)

iteM = matrix(0, 15, 3) #iteration matrix



Vk = matrix(0,15,1)
safeM = modMatrix(0, trap, circle)
normM = modMatrix(1, trap, circle)
riskM = modMatrix(2, trap, circle)


getProb <- function(dice, start){ #Getting prob
  if(dice == 0){
    return(safeM[start,])
  }else if(dice == 1){
    return(normM[start,])
  }else if(dice == 2){
    return(riskM[start,])
  }
}


VKF <- function(s, a, Vk){ #calculation of Vk
  M = NULL
  if(a == 0){
    M = safeM
  }else if(a == 1){
    M = normM
  }else if(a == 2){
    M = riskM
  }
  sum = 0
  for(i in 1:15){
    Vksa = Vksa + getProb(M, s, i) * (getCost(M, s, i) + gamma * VKF())
  }
  
}  

for(k in 1:15){ #for all states
  for(a in 1:3){ #for all policies
    
  }
}
Rsa = matrix(1,15,3)
Rsa[15, ] = 0
policy = matrix(0,15,1)
Vk = matrix(100,15,1)
Vk[15] = 0
for(k in 1:1000){
  for(s in 1:15){
    min = 10000
    for(a in 0:2){
      Vksa[a+1] = Rsa[s, a+1] + sum((getProb(a, s) * Vk[s]))
    }
    Vk[s] = min(Vksa)
    policy[s] = which.min(Vksa)-1
  }
}
policyTwo = matrix(0,15,1)
for(s in 1:15){
  sMin = 10000
  for(a in 0:2){
    arg[a+1] = sum(getProb(a, s) * Rsa[s, a+1] + Vk[s])
    policyTwo[s] = which.min(arg)-1
  }
}




#Cell 15 only ########################
#Creation markov matrix ##############
safeM = matrix(0, ncol = 15, nrow = 15)
for(i in 1:15){
  for(j in 1:15){
    if(i == j || i == j-1){
      safeM[i,j] = 0.5
    }
  }
}
safeM[3,3] = 0.5
safeM[3,4] = 0.25
safeM[3,11] = 0.25
safeM[15,15] = 1
######################################
#Creation markov matrix ##############
normM = matrix(0, ncol = 15, nrow = 15)
for(i in 1:15){
  for(j in 1:15){
    if(i == j || i == j-1 || i == j-2){
      normM[i,j] = 1/3
    }
  }
}
normM[3,3] = 1/3
normM[3,4] = 1/6
normM[3,5] = 1/6
normM[3,11] = 1/6
normM[3,12] = 1/6
normM[15,15] = 1
normM = round(normM, 2)
######################################
#Creation markov matrix ##############
riskM = matrix(0, ncol = 15, nrow = 15)
for(i in 1:15){
  for(j in 1:15){
    if(i == j || i == j-1 || i == j-2 || i == j-3){
      riskM[i,j] = 1/3
    }
  }
}
riskM[3,3] = 1/4
riskM[3,4] = 1/8
riskM[3,5] = 1/8
riskM[3,6] = 1/8
riskM[3,11] = 1/8
riskM[3,12] = 1/8
riskM[3,13] = 1/8
riskM[15,15] = 1
riskM = round(riskM, 2)
######################################
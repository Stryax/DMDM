#Creation markov matrix 
safeM = matrix(0, ncol = 15, nrow = 15)
for(i in 1:15){
  for(j in 1:15){
    if(i == j || i == j+1){
      safeM[i,j] = 0.5
    }
  }
}
safeM[3,3] = 0.5
safeM[3,4] = 0.25
safeM[3,11] = 0.25

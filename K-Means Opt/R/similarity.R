similarity <- function(l1, l2){
  # Computes Similarity Between Member Vectors Using Correlation 
  # Args:
  #     l1: Vector of cluster labels for subsample 1
  #     l2: Vector of cluster labels for subsample 2
  # Returns: Similarity (Correlation) Between l1 and l2
  
  q <- length(l1)
  dot_c1c2 <- 0
  dot_c1c1 <- 0
  dot_c2c2 <- 0
  
  #Idea: Don't store Matrix, just calculate dot products from vectors
  #Idea: Only need to calculate all i,j where i < j
  
  for(i in 1:q) {
    for(j in 1:q) {
      if(i < j) {
        l1_i <- l1[i]
        l1_j <- l1[j]
        l2_i <- l2[i]
        l2_j <- l2[j]
        
        #C1_ij
        l1_same <- as.numeric(l1_i == l1_j)
        
        #C2_ij
        l2_same <- as.numeric(l2_i == l2_j)
        
        
        #Use 2* to account for C_ij and c_ji
        dot_c1c1 <- dot_c1c1 + (2*l1_same)
        dot_c2c2 <- dot_c2c2 + (2*l2_same)
        
        dot_c1c2 <- dot_c1c2 + (2*l1_same*l2_same)
      }
    }
  }
  
  numer <- dot_c1c2
  denom <- max(1, ((dot_c1c1*dot_c2c2)**(1/2)))
  
  numer/denom
}









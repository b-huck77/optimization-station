#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double similarityRcpp(NumericVector l1, NumericVector l2) {
  // Computes Similarity Between Member Vectors Using Correlation 
  // Args:
  //     l1: Vector of cluster labels for subsample 1
  //     l2: Vector of cluster labels for subsample 2
  // Returns: Similarity (Correlation) Between l1 and l2
  
  // Create Variables to Store Running Sums
  // of necessary Dot Products for correlation
  
  // Dot Products for correlation do not require storing
  // The entire C matrices since they are just sums of
  // booleans
  int q = l1.length();
  double dot_c1c1 = 0;
  double dot_c2c2 = 0;
  double dot_c1c2 = 0;
  
  
  // Loop Through necessary element-wise comparisons
  // Only need those such that i < j (strictly)
  for(int i = 0; i < q; i++) {
    for(int j = i+1; j < q; j++) {
      
      // Extract necessary elements from l1, l2
      double l1_i = l1[i];
      double l1_j = l1[j];
      double l2_i = l2[i];
      double l2_j = l2[j];
      
      // Calculate C1_ij and C2_ij
      double l1_same = (l1_i == l1_j);
      double l2_same = (l2_i == l2_j);
      
      // Update Each Dot Product
      // Multiply term by 2 to account for 
      // symmetry of C matrices
      dot_c1c1 += 2*l1_same;
      dot_c2c2 += 2*l2_same;
      dot_c1c2 += 2*l1_same*l2_same;
    }
  }
  
  // Return correlation calculation as defined in Ben Hur (2004)
  return dot_c1c2/(std::max(1.0, pow(dot_c1c1*dot_c2c2, 0.5)));
}



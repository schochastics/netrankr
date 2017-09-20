#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix transreduct(NumericMatrix M) {
  NumericMatrix R = M;
  int n = R.rows();
  for(int j=0;j<n;++j){
    for(int i=0;i<n;++i){
      if(R(i,j)==1){
        for (int k=0; k<n;++k){
          if (R(j,k)==1){
            R(i,k)=0;
          } 
        }
      }
    }
  }
  return R;
}

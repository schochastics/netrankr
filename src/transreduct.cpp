#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix transreduct(IntegerMatrix M) {
  int n =M.rows();
  for(int j=0;j<n;++j){
    for(int i=0;i<n;++i){
      if(M(i,j)==1){
        for (int k=0; k<n;++k){
          if (M(j,k)==1){
            M(i,k)=0;
          } 
        }
      }
    }
  }
  return M;
}

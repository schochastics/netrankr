#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(rng = false)]]
NumericMatrix transreduct(NumericMatrix M) {
  NumericMatrix R(clone(M));
  int n = R.rows();
  // the reduction is irreflexive; a reflexive diagonal would erase whole rows
  for(int i=0;i<n;++i){
    R(i,i)=0;
  }
  for(int j=0;j<n;++j){
    for(int i=0;i<n;++i){
      if(R(i,j)==1){
        for (int k=0; k<n;++k){
          if (k!=i && R(j,k)==1){
            R(i,k)=0;
          } 
        }
      }
    }
  }
  return R;
}

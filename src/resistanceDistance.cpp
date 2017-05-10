#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix resistanceDistance(NumericMatrix C, int n) {
  NumericMatrix R(n,n);
  for(int i=0; i<n; ++i){
    for(int j=i; j<n; ++j){
      R(i,j)=C(i,i)+C(j,j)-2*C(i,j);
      R(j,i)=R(i,j);
    }
  }
  return R;
}


#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int preserve(IntegerMatrix P, NumericVector s,int n) {
  int test=0;
  for(int i=0;i<n;++i){
    for(int j=0;j<n;++j){
      if(P(i,j)==1){
        if(s[i]>s[j]){
          test=1;
          return test;
        }
      }
    }
  }
  return test;
}



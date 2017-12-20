#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix dependCurFlow(NumericMatrix Tmat,IntegerMatrix el, int m, int n) {
  NumericMatrix betmat(n,n);
  for(int e=0; e<m; ++e){
    int i = el(e,0);
    int j = el(e,1);
    for(int s=0;s<n;++s){
      for(int t=0;t<n;++t){
        if(i!=s & i!=t & s!=t){
          betmat(i,s) += 0.5*std::abs(Tmat(i,s)-Tmat(i,t)-Tmat(j,s)+Tmat(j,t));
        }
        if(j!=s & j!=t & s!=t){
          betmat(j,s) += 0.5*std::abs(Tmat(j,s)-Tmat(j,t)-Tmat(i,s)+Tmat(i,t));
        }
      }
    }
  }
  return betmat;
}
  

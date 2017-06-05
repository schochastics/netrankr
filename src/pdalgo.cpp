#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

arma::imat matdom(NumericMatrix A, bool map, bool benefit) {
  arma::mat B=as<arma::mat>(A);
  int n=B.n_rows;
  // int m=B.n_cols;
  bool status;
  arma::mat Asort(n,n,fill::zeros);
  arma:imat dom(n,n,fill::zeros);
  arma::rowvec tmpi(n);
  arma::rowvec tmpj(n);
  arma::rowvec check(n); 
  if(map){
    Asort=arma::sort(B,"descend",1);
  }
  int c=1;
  if(!benefit){
     c=-1;
  }
  for(int i = 0; i < n; ++i) {
    for(int j = 0; j < n; ++j) {
      if(i!=j){
        if(!map){
          tmpi=c*B.row(i);
          tmpj=c*B.row(j);
          tmpi[i]=c;
          tmpi[j]=c;
          tmpj[i]=c;
          tmpj[j]=c;
          check=tmpi-tmpj;
          status = arma::all(check<=0);
          if(status){
            dom(i,j)=1;
          }
        } else{
          tmpi=c*Asort.row(i);
          tmpj=c*Asort.row(j);
          check=tmpi-tmpj;
          status = arma::all(check<=0);
          if(status){
            dom(i,j)=1;
          }
        }

      }
    }
  }
  return dom;
}

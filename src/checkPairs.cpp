#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(rng = false)]]
Rcpp::List checkPairs(NumericVector x,NumericVector y) {
  // doubles: the number of pairs exceeds the int range for n > 65536
  double Con=0;
  double Dis=0;
  double Tie=0;
  double Left=0;
  double Right=0;
  
  int n=x.size();
  for(int i=0; i<n-1; ++i){
    Rcpp::checkUserInterrupt();
    for(int j=i+1; j<n; ++j){
      if(((x[i]>x[j]) && (y[i]>y[j])) || ((x[i]<x[j]) && (y[i]<y[j]))){
        Con+=+1;
      }
      else if(((x[i]>x[j]) && (y[i]<y[j])) || ((x[i]<x[j]) && (y[i]>y[j]))){
        Dis+=+1;
      }
      else if((x[i]==x[j]) && (y[i]==y[j])){
        Tie+=1;
      }
      else if((x[i]==x[j]) && (y[i]!=y[j])){
        Left+=1;
      }
      else{
        Right+=1;
      }
      
    }
  }
  return Rcpp::List::create(Rcpp::Named("concordant") = Con, 
                             Rcpp::Named("discordant") = Dis,
                             Rcpp::Named("ties")=Tie,
                             Rcpp::Named("left")=Left,
                             Rcpp::Named("right")=Right);
}



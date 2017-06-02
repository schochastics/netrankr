#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix approx_relative(NumericVector Nu, 
                              NumericVector Nd, 
                              IntegerMatrix P,
                              bool iterative,
                              int max_iter) {
  int n=Nu.size();
  NumericMatrix rrp(n,n);
  for(int x=0;x<(n-1);++x){
    for(int y=x+1;y<n;++y){
      if(P(y,x)==1){
        rrp(y,x)=1;
        rrp(x,y)=0;
      }
      else if(P(x,y)==1){
        rrp(y,x)=0;
        rrp(x,y)=1;
      }
      else{
        rrp(y,x)=double(Nu[y]+1)*(Nd[x]+1)/double((Nd[y]+1)*(Nu[x]+1)+(Nu[y]+1)*(Nd[x]+1));
        rrp(x,y)=1-rrp(y,x);
      }
    }
  }
  if(iterative){
    for(int it=0; it<max_iter-1; ++it){
      for(int i=0; i<n; ++i){
        Nu[i]=sum(rrp(i,_));
        Nd[i]=sum(rrp(_,i));
      }
      for(int x=0;x<(n-1);++x){
        for(int y=x+1;y<n;++y){
          if(P(y,x)==1){
            rrp(y,x)=1;
            rrp(x,y)=0;
          }
          else if(P(x,y)==1){
            rrp(y,x)=0;
            rrp(x,y)=1;
          }
          else{
            rrp(y,x)=double(Nu[y]+1)*(Nd[x]+1)/double((Nd[y]+1)*(Nu[x]+1)+(Nu[y]+1)*(Nd[x]+1));
            rrp(x,y)=1-rrp(y,x);
          }
        }
      }
    }
  }
  return rrp;
}

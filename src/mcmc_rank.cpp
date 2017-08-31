#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
List mcmc_rank(IntegerMatrix P, 
               IntegerVector init_rank,
               int rp) {
  int t = 1;
  int n = P.rows();
  NumericMatrix rrp(n,n);
  NumericVector expected(n);
  IntegerVector elements=seq(0,n-1);

  //rrp
  for(int i=0; i<(n-1); ++i){
    int j=init_rank[i];
    IntegerVector tmp=init_rank[elements>i];
    for(int k=0;k<tmp.length();++k){
      rrp(j,tmp[k])=1;
    }
  }
  //expected
  for(int i=0;i<n; ++i){
      expected[init_rank[i]] =i;
  }
  
  //MC
  while(t<=rp){
    int p = floor(R::runif(0,1)*(n-1));//rand() % (n-1);
    int c = round(R::runif(0,1));//rand() % 2;
    int a = init_rank[p];
    int b = init_rank[p+1];
    t+=1;
    if(c==1 & P(a,b)!=1){
      init_rank[p]=b;
      init_rank[p+1]=a;
      //expected update
      for(int i=0;i<n; ++i){
        expected[init_rank[i]]=double(expected[init_rank[i]]*(t-1)+i)/double(t);
      }
      //rrp update
      for(int i=0; i<(n-1); ++i){
        int j=init_rank[i];
        IntegerVector tmp=init_rank[elements>i];
        for(int k=0;k<tmp.length();++k){
          rrp(j,tmp[k])=double(rrp(j,tmp[k])*(t-1)+1)/double(t);
          rrp(tmp[k],j)=1-rrp(j,tmp[k]);
        }
      }
    }
  }
  
  return Rcpp::List::create(Rcpp::Named("expected") = expected,
                            Rcpp::Named("rrp")=rrp);
}


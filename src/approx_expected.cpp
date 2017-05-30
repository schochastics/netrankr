#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector approx_glpom(NumericMatrix P) {
  int n=P.ncol();
  NumericVector rapprox(n);
  IntegerVector elements = seq(0, rapprox.size()-1);
  IntegerVector Lxvec(n);
  IntegerVector Lyvec(n);
  IntegerVector Sxvec(n);
  std::vector<int> intersec;
  std::vector<int> diff;
  List Ix(n);
  List Sx(n); 
  for(int i=0;i<n;++i){
    rapprox[i]=sum(P(_,i))+1;
    Lxvec=elements[P(_,i)==0 & P(i,_)==0];
    Ix[i]=Lxvec[Lxvec!=i];
    Sx[i]=elements[P(_,i)==1];
  }
  for(int x=0;x<n;++x){
    Lxvec=Ix[x];
    Sxvec=Sx[x];
    for(int j=0; j<Lxvec.size();++j){
      std::vector<int> intersec;
      std::vector<int> diff;
      int y=Lxvec[j];
      Lyvec=Ix[y];
      
      set_intersection(Sxvec.begin(),Sxvec.end(),Lyvec.begin(),Lyvec.end(),std::back_inserter(intersec));
      
      set_difference(Lyvec.begin(), Lyvec.end(), Lxvec.begin(), Lxvec.end(),std::inserter(diff, diff.begin()));
      rapprox[x]+=double(intersec.size()+1)/double(diff.size()+1);
    }
  }
  return rapprox;
}



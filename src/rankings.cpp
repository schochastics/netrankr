#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
IntegerMatrix rankings(std::vector<std::vector<int> > paths,
                       std::vector<std::vector<int> > ideals,
                       int nRank,
                       int nElem) {
  
  IntegerMatrix rks(nElem,nRank);
  for(int i=0; i<nRank; ++i){
    std::vector<int> pths=paths[i];
    for(int j=0;j<nElem; ++j){
      int t=pths[j+1];
      int s=pths[j];
      int x;
      std::set_difference(ideals[t].begin(), ideals[t].end(),
                          ideals[s].begin(), ideals[s].end(), &x);
      rks(x,i)=j;
    }
  }
  
  
  return rks;
}


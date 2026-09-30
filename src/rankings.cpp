#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export(rng = false)]]
IntegerMatrix rankings(const std::vector<std::vector<int> >& paths,
                       const std::vector<std::vector<int> >& ideals,
                       int nRank,
                       int nElem) {
  
  if ((int) paths.size() < nRank) {
    Rcpp::stop("fewer paths through the lattice of ideals than rankings");
  }
  IntegerMatrix rks(nElem,nRank);
  for(int i=0; i<nRank; ++i){
    const std::vector<int>& pths=paths[i];
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


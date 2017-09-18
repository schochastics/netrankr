#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix nialgo(List adjList, IntegerVector deg) {
  int n=deg.size();
  IntegerVector marked(n);
  IntegerVector t(n);
  IntegerMatrix dom(n,n);
  for(int v = 0; v < n; ++v) {
    Rcpp::checkUserInterrupt();
    // IntegerVector Nv = as<IntegerVector>(adjList[v]);

    std::vector<int> Nv = as<std::vector<int> >(adjList[v]);
    //check if isolate
    if (Nv.empty()) {
      for(int j = 0; j < n; ++j){
        dom(v,j)=1;
      }
      dom(v,v)=0;
    }
    
    for(std::vector<int>::size_type j = 0; j!=Nv.size(); ++j){
    // for(int j = 0; j < NvSize; ++j) {
      int u = Nv[j];
      std::vector<int> Nu=adjList[u];
      Nu.push_back(u);
      for(std::vector<int>::size_type i = 0; i!=Nu.size(); ++i){
      // for(int i = 0; i<Nu.size(); ++i){
        int w=Nu[i];
        if(w!=v){
          if(marked[w]!=v){
            marked[w]=v;
            t[w]=0;
          }
          t[w]+=1;
          if(t[w]==deg[v]){
            dom(v,w)=1;
          }
        }
      }
    }
  }
  return dom;
}

#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

// [[Rcpp::export]]
std::vector<std::vector<int> > LatticeOfIdeals(std::vector<std::vector<int> > child,
                                               std::vector<int> parent,
                                               std::vector<std::vector<int> > Ek,
                                               int nElem, int nIdeals) {

  std::vector<std::vector<int> > ImPred(nIdeals);

  for(int i=0; i< child[0].size(); ++i){
    int tmp=child[0][i];
    ImPred[0].push_back(tmp);
  }
  for(int k=nElem-1;k>=0;--k){
    for(int i=0; i<Ek[k].size(); ++i){
      int v=Ek[k][i];
      int vPrime =ImPred[parent[v]][0];
      int j=0;

      while(vPrime!=v){
        int vStar=child[vPrime][0];
        ImPred[v].push_back(vStar);
        j+=1;
        vPrime=ImPred[parent[v]][j];
      }
      /* append children to ImPred*/
      for(int c=0;c<child[v].size();++c){
        ImPred[v].push_back(child[v][c]);
      }

    }
    /* erase v from parent children */
    for(int i=0; i<Ek[k].size(); ++i){
      int v=Ek[k][i];
      std::vector<int>::iterator position = std::find(child[parent[v]].begin(), child[parent[v]].end(), v);
      if (position != child[parent[v]].end())
        child[parent[v]].erase(position);
    }
  }
  
  return ImPred;
}



#include <Rcpp.h>
// #include <cstddef>
using namespace Rcpp;

// typedef int_least64_t linex;

double AssignTopDown(int v, 
                  NumericVector &lef, 
                  IntegerVector &visited,
                  std::vector<std::vector<int> > &ImSucc
                  ){
  visited[v]=1;
  double e=0;
  for(std::vector<int>::size_type i = 0; i!=ImSucc[v].size(); ++i){
  // for(int i=0; i<ImSucc[v].size(); ++i){
    int vPrime=ImSucc[v][i];
    if(vPrime==0){
      e+=1;
      lef[vPrime]=1;
    }
    else{
      if(visited[vPrime]==0){
        e=e+AssignTopDown(vPrime,lef,visited,ImSucc);
      }
      else{
        e=e+lef[vPrime];
      }
    }
  }
  lef[v]=e;
  return e;
}

void AssignBottomUp(int nElem,
                    NumericVector &lei, 
                    IntegerVector &visited,
                    std::vector<std::vector<int> > &ImSucc){
  std::vector<int> Q;

  lei(nElem)=1;

  for(std::vector<int>::size_type i = 0; i!=ImSucc[nElem].size();++i){
  // for(int i=0; i<ImSucc[nElem].size();++i){
    int vPrime=ImSucc[nElem][i];
    Q.push_back(vPrime);
    lei[vPrime]=1;
  }
  while(Q.size()!=0){
    Rcpp::checkUserInterrupt();
    int v=Q[0];
    Q.erase(Q.begin());

    for(std::vector<int>::size_type j = 0; j!=ImSucc[v].size();++j){
    // for(int j=0;j<ImSucc[v].size();++j){
      int vPrime=ImSucc[v][j];
      lei[vPrime]+=lei[v];
      if(visited[vPrime]==0){
        Q.push_back(vPrime);
        visited[vPrime]=1;
      }
    }
  }
}

void ComputeRankProb(int v,int h, NumericMatrix &rp,
                     std::vector<std::vector<int> > &ImSucc,
                     std::vector<std::vector<int> > &ideals,
                     IntegerVector &visited,
                     NumericVector &lei,
                     NumericVector &lef,
                     double &e){
  visited[v]=1;
  // std::vector<int>::size_type i = 0; i!=child[0].size(); ++i
  for(std::vector<int>::size_type j = 0;j!=ImSucc[v].size();++j){
    // for(int j=0;j<ImSucc[v].size();++j){
    int vPrime=ImSucc[v][j];
    int x;
    std::set_difference(ideals[vPrime].begin(), ideals[vPrime].end(),
                        ideals[v].begin(), ideals[v].end(), &x);
    rp(x,h)=rp(x,h)+double(lei[v])*double(lef[vPrime])/double(e);
    if((vPrime!=0) & (visited[vPrime]==0)){
      ComputeRankProb(vPrime,h+1,rp,ImSucc,ideals,visited,lei,lef,e);
    }
  }
  
}

void ComputeMutualRankProb(int v,int h, int &nElem,
                           NumericMatrix &mrp,
                           std::vector<std::vector<int> > &ImSucc,
                           std::vector<std::vector<int> > &ideals,
                           IntegerVector &visited,
                           IntegerVector &visitedElem,
                           NumericVector &lei,
                           NumericVector &lef,
                           double &e){
  visited[v]=1;
  for(std::vector<int>::size_type j = 0;j!=ImSucc[v].size();++j){
  // for(int j=0;j<ImSucc[v].size();++j){
    int vPrime=ImSucc[v][j];
    for(int y=0;y<nElem; ++y){
      if(visitedElem[y]==1){
        int x;
        std::set_difference(ideals[vPrime].begin(), ideals[vPrime].end(),
                            ideals[v].begin(), ideals[v].end(), &x);
        mrp(x,y)=mrp(x,y)+double(lei[v])*double(lef[vPrime])/double(e);
      }
      if((vPrime!=0) & (visited[vPrime]==0)){
        int x;
        std::set_difference(ideals[vPrime].begin(), ideals[vPrime].end(),
                            ideals[v].begin(), ideals[v].end(), &x);
        visitedElem[x]=1;
        ComputeMutualRankProb(vPrime,h+1,nElem,mrp,ImSucc,ideals,visited,visitedElem,lei,lef,e);
        visitedElem[x]=0;
      }
        
    }
  }
  
  
}

// [[Rcpp::export]]
Rcpp::List rankprobs(std::vector<std::vector<int> > ImPred,
                     std::vector<std::vector<int> > ideals,
                     int nElem,
                     int nIdeals){
  double e;
  NumericVector lei(nIdeals);
  NumericVector lef(nIdeals);
  IntegerVector visited(nIdeals);
  IntegerVector visitedElem(nElem);
  
  /* Turn ImPred to ImSucc*/
  std::vector<std::vector<int> > ImSucc(nIdeals);
  for(int i=0; i<nIdeals;++i){
    for(std::vector<int>::size_type j=0;j!=ImPred[i].size();++j){
    // for(int j=0;j<ImPred[i].size();++j){
      int idx=ImPred[i][j];
      ImSucc[idx].push_back(i);
    }
  }
  /* Sort increasingly*/
  for(int i=0;i<nIdeals;++i){
    std::sort(ImSucc[i].begin(), ImSucc[i].end());
  }
  /*calculate number of path*/
  e=AssignTopDown(nElem, lef,visited,ImSucc);
  std::fill(visited.begin(), visited.end(), 0);
  AssignBottomUp(nElem,lei,visited,ImSucc);

  /*rank probabilities*/
  std::fill(visited.begin(), visited.end(), 0);
  NumericMatrix rp(nElem,nElem);
  ComputeRankProb(nElem,0,rp,ImSucc,ideals,visited,lei,lef,e);
  
  /*mutual rank probabilities*/
  std::fill(visited.begin(), visited.end(), 0);
  NumericMatrix mrp(nElem,nElem);
  ComputeMutualRankProb(nElem,1,nElem,mrp,ImSucc,ideals,visited,visitedElem,lei,lef,e);
  return Rcpp::List::create(Rcpp::Named("linext") = e, 
                            Rcpp::Named("rp")=rp,
                            Rcpp::Named("mrp")=mrp);
}



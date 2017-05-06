#include <Rcpp.h>

using namespace Rcpp;
using namespace std;
// [[Rcpp::export]]
NumericMatrix dependency(std::vector<std::vector<int> > adj) {
  int n=adj.size();
  std::vector<std::vector<int> > Pred(n);
  std::vector<int> dist(n,-1);
  std::vector<int> sigma(n);
  std::vector<double> delta(n);
  NumericMatrix rel(n,n);
  std::vector<int> Q;
  List S;
  
  NumericVector bc(n);
  
  for(int s=0;s<n; ++s){
    /* SSP */
    for(int w=0;w<n; ++w){
      Pred[w].clear();
      dist[w]=-1;
      sigma[w]=0;
    }
    dist[s]=0;
    sigma[s]=1;
    Q.push_back(s);
    while(!Q.empty()){
      Rcpp::checkUserInterrupt();
      int v=Q[0];
      Q.erase(Q.begin());
      S.push_front(v);
      std::vector<int> Nv=adj[v];
      for(int i=0; i<Nv.size();++i){
        int w=Nv[i];
        /* path discovery */
        if(dist[w]<0){
          dist[w]=dist[v]+1;
          Q.push_back(w);
        }
        /* path counting */
        if(dist[w]==dist[v]+1){
          sigma[w]=sigma[w]+sigma[v];
          Pred[w].push_back(v);
        }
      }
    }
    /* accumulation */
    for(int v=0; v<n;++v){
      delta[v]=0;
    }
    while(S.size()>0){
      Rcpp::checkUserInterrupt();
      int w=S[0];
      S.erase(S.begin());
      for(int i=0;i<Pred[w].size(); ++i){
        int v=Pred[w][i];
        delta[v]+=double(sigma[v])/double(sigma[w])*(1+delta[w]);
      }
      if(w!=s){
        rel(w,s)+=delta[w];
      }
      
    }
  }
  return rel;
}



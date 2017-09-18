#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::vector<std::vector<int> > listingIdeals(std::vector<std::vector<int> > P, int nElem, int nIdeals) {
  std::vector<std::vector<int> > Ideals(nIdeals);
  std::vector< std::vector<int> > S(nElem+1);
  std::vector<int> S2;
  std::vector<std::vector<int> > Pwork=P;
  std::vector<std::vector<int> > Ptmp=Pwork;
  std::vector<int> I(nElem) ; 

  int ifound=0;
  /*Full Ideal*/
  std::iota (std::begin(I), std::end(I), 0);
  Ideals[0]=I;
  
  /* S(i)=max(P)*/  
  int i=0;
  for(int k=nElem-1; k>=0; --k){
    if(P[k].size()==0){
      S[0].push_back(k);  
    }
  }
  std::list<int> Code;
  while(i>=0){
    Rcpp::checkUserInterrupt();
    if(S[i].size()!=0){
      int x = S[i][0];
      Code.push_back(x);

      std::vector<int>::iterator position = std::find(I.begin(), I.end(), x);
      I.erase(position);
      S[i].erase(S[i].begin());
      ifound+=1;

      std::sort(I.begin(), I.end());
      Ideals.at(ifound)=I;

      /*Remove x from P*/
      Ptmp=Pwork;
      for(int k=0;k<nElem;k++){
        std::vector<int>::iterator position = std::find(Ptmp[k].begin(), Ptmp[k].end(), x);
        if (position != Ptmp[k].end())
          Ptmp[k].erase(position);
      }
      
      S2.clear();
      for(int k=nElem-1; k>=0; --k){
        if((Ptmp[k].size()==0) & (Pwork[k].size()!=0)){
          S2.push_back(k);  
        }
      }
      Pwork=Ptmp;
      S[i+1].clear();
      std::merge(S[i].begin(), S[i].end(), S2.begin(), S2.end(), std::back_inserter(S[i+1]), std::greater<int>());
      i+=1;
    }
    else{
      if(Code.size()!=0){
        int x=Code.back();
        Code.pop_back();
        I.push_back(x);
        
        /*Insert x from P*/
        for(int k=0;k<nElem;k++){
          if(P[k].size()>0){
            std::vector<int>::iterator position = std::find(P[k].begin(), P[k].end(), x);
            if (position != P[k].end())
              Pwork[k].insert(Pwork[k].begin() + (position - P[k].begin()),x);
          }
        }
      }
      i-=1;
      
    }
  }
  return Ideals;
}



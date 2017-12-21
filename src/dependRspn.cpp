#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat dependRspn(std::vector<std::vector<int> > A,arma::mat Z,
                     arma::mat Zdiv, arma::mat W, int n) {
  arma::mat betmat(n,n);
  betmat.fill(0);
  
  arma::colvec e(n);
  e.fill(1);
  for(int i=0;i<n; ++i){
    
    arma::colvec z_ci = Z.col(i);
    arma::rowvec z_ri = Z.row(i);
    for(std::vector<int>::size_type jiter = 0; jiter!=A[i].size(); ++jiter){
      int j = A[i][jiter];
      
      arma::colvec z_cj = Z.col(j);
      arma::rowvec z_rj = Z.row(j);
      
      arma::mat s1_ij = (z_ci*z_rj) % Zdiv;
      arma::mat s1_ji = (z_cj*z_ri) % Zdiv;
      
      arma::colvec s2_vec_ij = (z_ci % z_rj.t()) % Zdiv.diag();
      arma::mat s2_ij = e*s2_vec_ij.t();
      
      arma::colvec s2_vec_ji = (z_cj % z_ri.t()) % Zdiv.diag();
      arma::mat s2_ji = e * s2_vec_ji.t();
      
      arma::mat N_ij = arma::as_scalar(W(i,j)) * (s1_ij - s2_ij);
      arma::mat N_ji = arma::as_scalar(W(j,i)) * (s1_ji - s2_ji);
      
      arma::mat N = abs(N_ij - N_ji);
      arma::colvec e_i = e;
      e_i[i] = 0;
      arma::colvec e_j = e;
      e_j[j] = 0;
      
      betmat.row(i) += (N * e_i).t();
      betmat.row(j) += (N * e_j).t();
    }
  }
  betmat= betmat - arma::diagmat(betmat.diag());
  return betmat;
}
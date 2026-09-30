#include <RcppArmadillo.h>
using namespace Rcpp;

// Net randomized shortest path dependencies (Kivimaki et al., 2016).
// Each undirected edge {i,j} is visited once and, as for current flow,
// the net flow through a node is counted with weight 0.5.
// [[Rcpp::export(rng = false)]]
arma::mat dependRspn(const std::vector<std::vector<int> >& A, const arma::mat& Z,
                     const arma::mat& Zdiv, const arma::mat& W, int n) {
  arma::mat betmat(n, n, arma::fill::zeros);
  arma::vec zdiv_diag = Zdiv.diag();

  for (int i = 0; i < n; ++i) {
    Rcpp::checkUserInterrupt();
    for (size_t jiter = 0; jiter < A[i].size(); ++jiter) {
      int j = A[i][jiter];
      if (j <= i) {
        continue;
      }
      double wij = W(i, j);
      double wji = W(j, i);
      for (int t = 0; t < n; ++t) {
        double zjt = Z(j, t);
        double zit = Z(i, t);
        double s2_ij = Z(t, i) * zjt * zdiv_diag[t];
        double s2_ji = Z(t, j) * zit * zdiv_diag[t];
        for (int s = 0; s < n; ++s) {
          double zd = Zdiv(s, t);
          double N = std::abs(wij * (Z(s, i) * zjt * zd - s2_ij) -
                              wji * (Z(s, j) * zit * zd - s2_ji));
          if (t != i) {
            betmat(i, s) += N;
          }
          if (t != j) {
            betmat(j, s) += N;
          }
        }
      }
    }
  }
  betmat *= 0.5;
  betmat.diag().zeros();
  return betmat;
}

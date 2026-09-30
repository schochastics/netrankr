#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]

// Neighborhood-inclusion: dom(v, w) = 1 if N(v) is a subset of N[w].
// The entries are collected first and the sparse matrix is built in one go.
// [[Rcpp::export(rng = false)]]
arma::sp_mat nialgo(const std::vector<std::vector<int> >& adjList, IntegerVector deg) {
  int n = deg.size();
  std::vector<int> marked(n, -1);
  std::vector<int> t(n, 0);
  std::vector<unsigned int> rows, cols;
  for (int v = 0; v < n; ++v) {
    Rcpp::checkUserInterrupt();
    const std::vector<int>& Nv = adjList[v];

    // an isolate is dominated by all other nodes
    if (Nv.empty()) {
      for (int j = 0; j < n; ++j) {
        if (j != v) {
          rows.push_back(v);
          cols.push_back(j);
        }
      }
    }

    for (size_t j = 0; j < Nv.size(); ++j) {
      int u = Nv[j];
      const std::vector<int>& Nu = adjList[u];
      // closed neighborhood of u
      for (size_t i = 0; i <= Nu.size(); ++i) {
        int w = (i < Nu.size()) ? Nu[i] : u;
        if (w != v) {
          if (marked[w] != v) {
            marked[w] = v;
            t[w] = 0;
          }
          t[w] += 1;
          if (t[w] == deg[v]) {
            rows.push_back(v);
            cols.push_back(w);
          }
        }
      }
    }
  }
  arma::umat locations(2, rows.size());
  for (size_t k = 0; k < rows.size(); ++k) {
    locations(0, k) = rows[k];
    locations(1, k) = cols[k];
  }
  arma::vec values(rows.size(), arma::fill::ones);
  return arma::sp_mat(locations, values, n, n);
}

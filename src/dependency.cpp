#include <Rcpp.h>

using namespace Rcpp;

// Brandes' algorithm, returning the pairwise dependencies instead of their sum.
// rel(w, s) is the dependency of s on w.
// [[Rcpp::export(rng = false)]]
NumericMatrix dependency(const std::vector<std::vector<int> >& adj) {
  int n = adj.size();
  std::vector<std::vector<int> > Pred(n);
  std::vector<int> dist(n, -1);
  std::vector<double> sigma(n);
  std::vector<double> delta(n);
  std::vector<int> Q(n);
  std::vector<int> S;
  S.reserve(n);
  NumericMatrix rel(n, n);

  for (int s = 0; s < n; ++s) {
    Rcpp::checkUserInterrupt();
    /* SSP */
    for (int w = 0; w < n; ++w) {
      Pred[w].clear();
      dist[w] = -1;
      sigma[w] = 0;
      delta[w] = 0;
    }
    dist[s] = 0;
    sigma[s] = 1;
    int head = 0, tail = 0;
    Q[tail++] = s;
    while (head < tail) {
      int v = Q[head++];
      S.push_back(v);
      const std::vector<int>& Nv = adj[v];
      for (size_t i = 0; i < Nv.size(); ++i) {
        int w = Nv[i];
        /* path discovery */
        if (dist[w] < 0) {
          dist[w] = dist[v] + 1;
          Q[tail++] = w;
        }
        /* path counting */
        if (dist[w] == dist[v] + 1) {
          sigma[w] += sigma[v];
          Pred[w].push_back(v);
        }
      }
    }
    /* accumulation */
    while (!S.empty()) {
      int w = S.back();
      S.pop_back();
      for (size_t i = 0; i < Pred[w].size(); ++i) {
        int v = Pred[w][i];
        delta[v] += sigma[v] / sigma[w] * (1 + delta[w]);
      }
      if (w != s) {
        rel(w, s) += delta[w];
      }
    }
  }
  return rel;
}

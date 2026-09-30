#include <Rcpp.h>

using namespace Rcpp;

// Markov chain on the linear extensions of the partial order P (Bubley & Dyer).
// Every step yields one sample: rejected moves count the current state again.
// Accumulators are updated lazily, so a step costs O(1) instead of O(n^2).
// [[Rcpp::export(rng = true)]]
List mcmc_rank_dense(IntegerMatrix P,
                     IntegerVector init_rank,
                     double rp) {
  int n = init_rank.length();
  IntegerVector rank = clone(init_rank);
  std::vector<int> pos(n);
  for (int i = 0; i < n; ++i) {
    pos[rank[i]] = i;
  }
  // position sums and the step since which each element holds its position
  std::vector<double> pos_sum(n, 0.0), pos_since(n, 1.0);
  // time x is ranked before y and the step since which the pair keeps its order
  NumericMatrix before(n, n);
  NumericMatrix pair_since(n, n);
  std::fill(pair_since.begin(), pair_since.end(), 1.0);

  for (double s = 1; s <= rp; s += 1) {
    int p = floor(R::runif(0, 1) * (n - 1));
    int c = round(R::runif(0, 1));
    int a = rank[p];
    int b = rank[p + 1];
    if ((c == 1) && (P(a, b) != 1)) {
      pos_sum[a] += pos[a] * (s - pos_since[a]);
      pos_sum[b] += pos[b] * (s - pos_since[b]);
      pos_since[a] = pos_since[b] = s;
      before(a, b) += s - pair_since(a, b);
      pair_since(a, b) = pair_since(b, a) = s;
      rank[p] = b;
      rank[p + 1] = a;
      pos[a] = p + 1;
      pos[b] = p;
    }
    if (fmod(s, 1e6) == 0) {
      checkUserInterrupt();
    }
  }

  double end = rp + 1;
  NumericVector expected(n);
  NumericMatrix rrp(n, n);
  for (int x = 0; x < n; ++x) {
    expected[x] = (pos_sum[x] + pos[x] * (end - pos_since[x])) / rp;
    for (int y = 0; y < n; ++y) {
      if (x != y && pos[x] < pos[y]) {
        before(x, y) += end - pair_since(x, y);
      }
    }
  }
  for (int x = 0; x < n; ++x) {
    for (int y = 0; y < n; ++y) {
      if (x != y) {
        rrp(x, y) = before(x, y) / rp;
      }
    }
  }
  return List::create(Named("expected") = expected,
                      Named("rrp") = rrp);
}

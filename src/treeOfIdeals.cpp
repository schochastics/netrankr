#include <Rcpp.h>

#include <vector>
#include <algorithm>

namespace
{
	struct toi_data
	{
		std::vector<int> parent;
		std::vector<int> label;
		std::vector<std::vector<int> > children;
		// is_impred[i][val]: val is an immediate predecessor of element i (0-based)
		std::vector<std::vector<char> > is_impred;

		toi_data(const Rcpp::List &impred) : is_impred(impred.size())
		{
			int n = impred.size();
			for (int i = 0; i < n; ++i)
			{
				is_impred[i].assign(n + 1, 0);
				Rcpp::IntegerVector impredi = Rcpp::as<Rcpp::IntegerVector>(impred[i]);
				for (int k = 0; k < impredi.size(); ++k)
				{
					is_impred[i][impredi[k]] = 1;
				}
			}
		}
	};

	bool is_immediate_predecessor(int i, int val, const toi_data &d)
	{
		return d.is_impred[i-1][val];
	}

	void add_child(int parent, int child, toi_data &d)
	{
		d.children[parent].push_back(child);
	}

	void right(int i, int r, int root, toi_data &d)
	{
		// copy: d.children grows (and may reallocate) during the recursion
		const std::vector<int> range = d.children[r];
		for (const int child : range)
		{
			int l = d.label[child];

			if (!is_immediate_predecessor(i, l, d))
			{
				int t = d.parent.size();
				d.parent.push_back(root);
				d.label.push_back(l);
				d.children.push_back({});
				add_child(root, t, d);
				right(i, child, t, d);
			}
		}
	}

	int left(int i, toi_data &d)
	{
		int root = d.parent.size();
	  d.label.push_back(i);
	  d.parent.push_back(0);
	  d.children.push_back({});
	  
		if (i == 0)
		{
			return root;
		}

		int r = left(i - 1, d);

		d.parent[r] = root;

		add_child(root, r, d);
		right(i, r, root, d);
		

		return root;
	}
}

/**
* Computes the tree of ideals. Return values needs P to be sorted according to a topological sort!
**/

// [[Rcpp::export(rng = false)]]

Rcpp::List treeOfIdeals(Rcpp::List imPred)
{
	toi_data d(imPred);
	left(imPred.size(), d);
	return Rcpp::List::create(Rcpp::Named("label") = d.label, 
                           Rcpp::Named("parent") = d.parent,
                           Rcpp::Named("child")=d.children);
}

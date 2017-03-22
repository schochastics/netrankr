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
		const Rcpp::List &impred;


		toi_data(const Rcpp::List &impred) : impred(impred) {}
	};

	bool is_immediate_predecessor(int i, int val, const toi_data &d)
	{
		Rcpp::IntegerVector impredi = Rcpp::as<Rcpp::IntegerVector>(d.impred[i-1]);
		return std::find(impredi.begin(), impredi.end(), val) != impredi.end();
	}

	void add_child(int parent, int child, toi_data &d)
	{
		d.children[parent].push_back(child);
	}

	void right(int i, int r, int root, toi_data &d)
	{
		const auto &range = d.children[r];
		std::for_each(range.begin(), range.end(), [=,&d](const int child) {

		  int l = d.label[child];

			if (!is_immediate_predecessor(i, l, d))
			{
				int t = d.parent.size();
				d.parent.push_back(root);
				d.label.push_back(d.label[child]);
				d.children.push_back({});
				add_child(root, t, d);
				right(i, child, t, d);
			}
		});
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

		right(i, r, root, d);
		add_child(root, r, d);

		return root;
	}
}

/**
* Computes the tree of ideals. Return values needs P to be sorted according to a topological sort!
**/

// [[Rcpp::export]]

Rcpp::List treeOfIdeals(Rcpp::List imPred)
{
	toi_data d(imPred);
	left(imPred.size(), d);
	return Rcpp::List::create(Rcpp::Named("label") = d.label, 
                           Rcpp::Named("parent") = d.parent,
                           Rcpp::Named("child")=d.children);
}
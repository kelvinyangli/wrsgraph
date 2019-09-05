# wrsgraph
An r package to check if a given (undirected) graph is weakly recursively simplicial (wrs) https://en.wikipedia.org/wiki/Moral_graph#Weakly_recursively_simplicial.

A set of Markov blankets B={B1,..., Bn}, one for each variable in a Bayesian network, needs to satisfy both symmetric and consistent properties. Symmetric means if x is in the Markov blanket of y, then y is also in the Markov blanket of x. This can be efficiently checked. Consistent means there must exist a DAG, in which the Markov blankets of all variables is identical to B. It is not trivial to check consistency. 

A graph is wrs if and only if it is moral https://arxiv.org/pdf/1903.01707.pdf, which is equivalent to the set of Markov blankets being consistent. Hence, checking wrs is equivalent to checking Markov blanket consistency.

This package contains a backtracking algorithm to check wrs. Note deciding morality is NP-complete https://arxiv.org/pdf/1303.1501.pdf, so this algorithm can be extremely slow (superpolynomial) in some cases. But overall it is quite fast. 

There are two polynomial time algorithms for checking wrs on graphs with maximum degree 3 and 4. 

In addition to checking morality, the above algorithms also returns partially directed graphs correspond to the node order when checking wrs. 

The package also contains methods for moralizing a graph, if one wants to enforce morality on a set of learned Markov blanekts in order to ensure there is a causal interpretation of the learned graph https://escholarship.org/content/qt6ks2015k/qt6ks2015k.pdf. 

The package has dependency on the following packages: igraph and ...

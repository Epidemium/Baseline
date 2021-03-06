# Baseline MODELS GLM_Aggregate

A GLM is of the form f(Y)~u+v+w (also written f(Y) = a + bu + cv + dw + errorTerm) where X=[u|v|w]

"Aggregate" is the name chosen here for a way to handle the sparsity of the X matrix:
Let us for example consider that u is available for all rows but that v is available for a portion of rows and w for another portion of rows
One could do several GLM:
* f(Y)~u+v+w for the lines where u,v,w are all defined
* f(Y)~u+v for the lines where only u,v are defined
* etc.

With more variables this leads to an astronomical number of coefficients; if the number of data points is small such is totally inadequate.
Instead Aggreate considers that all "b" are the same, all "c" are the same, all "d" are the same. But each equation can have a different "a".
This leads to a single equation, GLM_Aggregate:
f(Y) = (a+ a' 1_v + a'' 1_w) + bu + cv + dw + errorTerm

It is not perfect because if the lines where all u,v,w are defined are sufficiently numerous / have sufficient weight then only those lines should count and "b", "c" and "d" should be the coefficients of f(Y)~u+v+w only. So implicitely the approach uses a very poor level of credibility theory.
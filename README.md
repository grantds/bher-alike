bher-alike
==========


A simple probabilistic programming language in the style of bher, using the "random database"
approach with Metropolis-Hastings independence sampling. This project is primarily intended
to be instructive to the author, **and is a work in progress**


Programming languages that include sampling from stochastic sources as elementary procedures
(_elementary random procedures_) induce probability distributions on program execution traces.
Programs written in probabilistic programing languages describe generative probabilistic models.
The point of a probabilistic program is not to execute the program, but rather to reason about
the conditional distribution defined by assigning values to a subset of the random variables.
A probabilistic model (the prior) may be specified by writing a program, and samples could automatically 
be drawn from the conditional posterior. 




To read about the ideas behind this project:
* http://www.mit.edu/~ast/papers/lightweight-mcmc-aistats2011.pdf




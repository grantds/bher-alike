bher-alike
==========


A simple probabilistic programming language in the style of bher, using the
"random database" approach with Metropolis-Hastings independence sampling. This
project is primarily intended to be instructive to the author, **and is a work
in progress**.


Programming languages that include sampling from stochastic sources as
elementary procedures (_elementary random procedures_) induce probability
distributions on program execution traces.  Programs written in probabilistic
programing languages describe generative probabilistic models.  The point of a
probabilistic program is not to execute the program, but rather to reason about
the conditional distribution defined by assigning values to a subset of the
random variables.  A probabilistic model (the prior) may be specified by
writing a program, and samples could automatically be drawn from the
conditional posterior. 


Primitives include a variety of of familiar scheme procedures: cons, car, cdr,
+, etc. as well as elementary random primitives corresponding to sampling from
random variables.

### Usage
To load a (bher-alike) repl, call `scheme --load repl.scm`. There is
currently no error handling in place, so any run-time errors will drop you out
of the bher-alike repl and back into the underlying scheme top-level. 

For example: 
```scheme 
$ scheme --load repl.scm
bher-alike>> (flip .5)
#t
```

Currently, the only elementary random primitives are `(flip p)` and `(geometric
p)`. This may change in the future.

To sample from a conditional distribution, use `(mh-query-general 'exp 'cond
iters)`, which returns a sample produced by evaluating `exp` conditioned on
`cond`, which can be any boolean expression. We use Metropolis-Hastings on
execution traces to produce a sample from the conditional distribution. In
order for this to work, we must first find some execution trace in which the
condition holds -- the current implementation uses rejection sampling, which
quickly becomes intractable when conditioning on unlikely events. Future
work may restrict conditioning to assigning values to random variables, which
would be more practical.

**Note:** a quirk of the current implementation requires `exp` and `cond` to be
quoted.  In the future, query will be treated as a special form and this will
unnecessary.

A trivial example:
```scheme
bher-alike>>  (mh-query-general `(begin (define x (geometric .5)) x) `(<= x 5) 5)
6
```




To read about the ideas behind this project:
* [Lightweight Implementations of Probabilistic Programming Languages Via Transformational Compilation, (Wingate _et al_, 2011)](http://www.mit.edu/~ast/papers/lightweight-mcmc-aistats2011.pdf)




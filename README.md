# Interpreter for Scheme
This is a term project for CSSE304 (Programming language concept).

It is essentially an interpreter for Scheme written in Scheme.
To simply basic operation, we use primitive procedure in Scheme, such as car, cdr etc.
However, we implemented many coreSyntax of Scheme, such as lambda, set!, define. We use Scheme's "if" to implement our if since we can't really write program without branches.

#API

To use and test this interpreter, you need to first obtain a scheme interpreter first, which is availabe at http://scheme.com/download/.

First of all, change the root directory to the folder containing all of those files.
You can use `(laod main.ss)` to load the main.ss, which contain the bulk of the interpreter.
You type `(rep)` to start the console, and type instruction into the console.
You can also use `(eval-one-exp '(<You code>))` use eval only one expression.

#define-syntax
The most interesting part of Scheme is customizable syntax. You can use define-syntax to define any kind of syntax you want to use. In fact, very few syntax are part of the core scheme. In our interpreter, we try to implement only the core syntax and use syntax expansion to convert other syntax to core form before evaluation.

We implemented define-syntax with the idential syntax of scheme. See http://scheme.com/tspl4/syntax.html#./syntax:s12 for more details on how to use define-syntax.


#lexical order
instead of stored each variable with their name, our interpreter would find its lexical order information, i.e. where to find that variable. This is more work for parse-exp, but less work at run time, since this interpreter can directly to go that level and grad data without a linear time search.

Each variable is represented as the following
free variable: (name)
bounded variable: (name depth . index)

#local define
This is some feature that standard scheme does not have. Starndard Scheme only support local define at the very beginnig of a lexical scope. This is legal since no ambiguity is introduced. It is essentailly a syntax-expansion for letrec*.

Arbitrary local define introduce uncertainly to execution, since the interpreter cannot determine whether certain variable is bounded or not at run time. For example,

``
(define example1
	(lambda (x)
		(if x
			(define a 1)
			(define b 1))
		a
		b))
``

In this exmaple, if x is true, a is defined, but if x is #f, b is defined. Whether a and b are bounded or not can only be determined at run time. This troubles efficient lexical order implementation, since interperter are not sure where to get the value.

In the following a bit more complex example, local defined variable can hide outer scope variable. If x is #f, a is not define in the inner scope, so the result is 1 + 2 = 3, but if x is true, the local define hides the outer a, so the result is 3 + 2 = 5.

``
(define example2 
	(lambda (x)
		(let ([a 1][b 2])
			(let ()
				(if x
					(define a 3))
				(+ a b)))))
``

Those undeterminstic is difficult to implement, but we find a way.
Instead of having a simple two kind of lexical order -- bounded and free, this interpreter has three.
free var: '(name)
	Go to global environment directly
bounded var: '(name depth . index)
	Go to depth up lexical scope's index-th element directly
uncertain free var: '(name d1 d2 d3 ...)
	Check d1 d2 d3 level, get the closet one, if exists. If none of them contain this variable, go to global environment
uncertain bounded var: '(name d1 d2 d3 ... depth . index)
	Check d1 d2 d3 level, get the closet one, if exists. If none of them contain this variable, go to the specified index.

This representation minimizes the levels to check for. If a is bounded at level 1, and posibly be redefined at level 10, and used at level 20. This representation help the interpreter to only look for the value in two levels.





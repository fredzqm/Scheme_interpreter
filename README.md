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




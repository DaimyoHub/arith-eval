# Arithmetical expression interpreter

## Abstract

This project is an arithmetical expression evaluator. It is not perfectly
working yet since priority of operations are not implemented, except for 
parentheses.

In other words :
  - this expression is well evaluated : `((3 * 4) / 2) + 5`
  - this expression is not : `3 * 4 / 2 + 5`

## How does it work ?

The evaluation of the expression is done through 3 phases :
- The lexical analyses, which transforms a string into a list of tokens that
can be manipulated by a program.
- The syntax analyses, which transforms the list of tokens into an Abstract
Syntax Tree. The shape of the tree determines the evaluation order of the 
expression.
- The evaluation of the AST. It is a simple reduction of the tree until it is
reduced to a number.
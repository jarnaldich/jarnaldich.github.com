---
layout: post
title: Blogging with Jekyll
tags: jekyll blogging
---

In the last post we saw how build an evaluator for the polish notation
calculator that we will grow into a Scheme-like language. In this post
we are going focus our attention into the _reader_ layer, so that we
can interact with the evaluator without using the OCaml top-level.
This will be the first of two posts and devoted mainly to the lexical
analysis (see below). Parsing will be covered in the next post. 

Remember that, in a lisp-like language, the task of the _reader_ is to
transform a stream of characters into symbolic expressions. The reader
step is the typical example of something you'd _snarf_ if you are
building a _meta-circular_ evaluator. I you are to implement the
reader layer explicitly, though, a common way to tackle the parsing
problem in general is to divide it in two steps:

1. _Lexical analysis_, which converts the stream of characters into a stream
of _meaningful_ tokens, getting rid of anything not really important
for the compiler (ie, comments, whitespace...), and performing some
simple abstraction operations (case conversion, getting numbers out of their
string representation, etc...) 
2. _Parsing_, which converts the stream of tokens into some sort of
abstract structure, most frequently an _Abstract Syntax Tree_ (AST).
In the lisp case, a symbolic expression can actually be seen as an _AST_.

Although the parsing process may actually be tackled in many ways and
with the help of different tools (some of which merge steps 1 and 2),
there is a kind of de-facto standard: using some helper tool for
building _DFA_ code for the _tokenizer_, and a _LALR_ parser
generator for the _parser_. This has become the standard probably due to the _(f)lex_
and _bison/yacc_ tools for C, which generate `.c` files for the
lexers/parser out of some description files. Many languages have
ported these tools, and OCaml is no exception: it comes bundled with
an `ocamllex` lexer generator and an `ocamlyacc` parser generator,
which take `.mll/.mly` files into `.ml` files for compilation.

For a language like the one we are going to implement, all that is
probably overkill, and we could just write our own recursive-descent
parser from scratch. I've resisted the temptation for the following
reasons. First, the code for an ad-hoc parser tends to be as ugly as
it gets, that is, tends to be harder to maintain and understand than
the `flex/bison` DSLs (no surprise, that's why they were invented in
the first place). The only advantage in not using them is not
depending on external tools... but the OCaml distribution already
comes with them, so we're not adding any dependency here. The make
process has one more step but we all know how to write makefiles,
right? 

The other reason is that we actually tend to underestimate the
complexity of Scheme in general... it _is_ one of the simplest
languages around, but in many ocasions is not as simple as one may
think... I guess even a simple addition like floating-point arithmetic
would be less than fun in an ad-hoc parser.

Our Tokens
==========

The tokens are the only data type the parser and lexer share. It is
defined in the `parser.mly` file.

{% highlight ocaml %}
%token <int> INT
%token <string> SYMBOL STRING 
%token LPAREN RPAREN LBRACKET RBRACKET DOT TRUE FALSE
{% endhighlight %}

These are the only meaningful bits we're interested in extracting from
the source code:

- Opening and closing parens (_?PAREN_, _?BRACKET_). We will allow for
  brackets because it is easy to implement and improves readability.
- The DOT token will help us parse improper lists.
- TRUE and FALSE tokens will read booleans.

All of the above bear no extra information with them. The remaining
ones have some sort of type, declared above between angle brackets:

- STRING represents a Scheme string and carries an OCaml string as
  extra information.
- SYMBOL represents a Scheme symbol and carries an OCaml string as
  extra information. 
- INT represents a Scheme INT and carries an OCaml int with it. 

The Lexer
=========

Any _lex_ clone works by defining a set of patterns and the code to
execute when the parser accepts one. With the above tokens,
lexing is just a matter of defining some significant character classes:

{% highlight ocaml %}
let spaces = ['\r' '\t' ' ']
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let arith_ops = ['*' '/' '+' '-' '=']
let punctuation = ['!' '#'  '$'  '%'  '&'  '|'  '*'  
	           '+' '-'  '/'  ':'  '<'  '='  '>'  '?'  
	           '@'  '^'  '_'  '~'  '\"']
let symbol = (digit|letter)+ (digit|punctuation|letter)*
let scheme_string = '"' (('\\' _ )|[^ '"'])* '"'
{% endhighlight %}

And then define the pattern- action pairs:

{% highlight ocaml %}
rule token = parse
| eof { raise Eof }
| '\n' 
    {
      incr_linenum lexbuf; 
      token lexbuf
    }
| scheme_string as s  { make_string s }
| spaces+ { token lexbuf }
| (digit+ as inum) { INT (int_of_string inum) }
| arith_ops as op { SYMBOL (Char.escaped op) }
| symbol as s { SYMBOL s }
| ';' [^ '\n']* { token lexbuf }	(* eat up one-line comments *)
| '[' { LBRACKET  }
| ']' { RBRACKET }
| '(' { LPAREN }
| ')' { RPAREN }
| "#t" { TRUE }
| "#f" { FALSE }
{% endhighlight %}

It is not really important yet because we're building a repl rather
than using input files, but our lexer tries to keep track of lines and
chars so that we could report errors properly. The Eof rule is used to
abort the whole repl without generationg an error (will make sense
when we get to the repl code)

Our lexer does not support Unicode. For a more modern lexer, take a
look at the (Ulex)[http://www.cduce.org/download.html#side] library. I
haven't used it here to keep the code free from external dependencies.

Final thougths
==============

You may have already realized that our Scheme interpreter is flawed in
many different ways:

- The INT representation is just wrong: we try to parse as an _ocaml_
  machine-dependent integer a string of digits, without caring about
  overflows and different integer semantics (ints in OCaml get 1 bit
  reserved).
- Strings support no escape sequences...

Of course, this is just to keep the code clean. As with other kinds of
programs, reality tends to spoil our beautiful ideas. Most real-world
parsers and lexers do all sort of tricks to make _lex_ and _yacc_
behave as they should. For example, lexical analysis tends to add some
sort of context information or global state in combination with
sub-parsers. Take a look at IronScheme's
[lexer](https://github.com/leppie/IronScheme/blob/master/IronScheme/IronScheme/Compiler/IronScheme.lex)
and
[parser](https://github.com/leppie/IronScheme/blob/master/IronScheme/IronScheme/Compiler/IronScheme.y)
for a real-world example of a parser for a Scheme language.

Resources
=========

- The
  [camllex/camlyacc manual](http://caml.inria.fr/pub/docs/manual-ocaml/manual026.html).
- A gentler
  [introductory tutorial](http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamllex-tutorial/)
  on ocamllex.
- A gentler
  [introductory tutorial](http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/)
  on ocamlyacc.
- For an example of using Parser combinators instead of YACC-like
  tools for parsing a Scheme-like language, see
  [here](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing).
- [Scheme From Scratch](http://michaux.ca/articles/scheme-from-scratch-introduction)
  uses an add-hoc, recursive-descent parser.
- Take a look at the [IronScheme implementation](https://github.com/leppie/IronScheme).




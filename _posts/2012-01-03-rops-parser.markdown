---
layout: post
title: ROPS v0.1 - The Parser
tags: rops parser
---

In the previous post we learned how to do lexical analysis for our toy
language using `ocamllex`. Remember that the result of that step is a
"cleaner" input stream (ie, only with meaningful information, in bits
called _tokens_). It is the task of the parser to give some sort of
structure to that stream. Since we are dealing with a lisp-like
language, that structure will be a representation of symbolic
expressions using OCaml data types.

The Parser
==========

All `yacc/bison`-style parsers work more or less the same: the grammar
is specified in a _BNF_-like language, with a set of _rules_ and
the callback code to execute each time the parser accepts a production.
Apart from the grammar itself, there's also a _header section_ with
extra information for the code-generation step. In our case, this
generation step will turn `.mly` grammar description files into `.ml/.mli`
ocaml source/header files.

The entry point in our grammar is the symbolic expression production,
which will generate a value of type `scheme_obj` we can later feed to
the evaluator (See [this previous post](/2011/12/30/rops-pncalc.html)
for an explanation on the `scheme_obj` type). We tell `ocamlyacc` about
this in the following header line:

{% highlight ocaml %}
%start sexp
%type <SchemeTypes.scheme_obj> sexp
{% endhighlight %}

{% highlight ocaml %}
sexp
    : list                                        {  $1; }
    | SYMBOL                                      {  SchemeTypes.Symbol $1 }
    | STRING                                      {  SchemeTypes.String $1 }
    | INT                                         {  SchemeTypes.Int $1 }
    | TRUE                                        {  SchemeTypes.True }
    | FALSE                                       {  SchemeTypes.False }                
    ;
{% endhighlight %}

The above should be almost self-evident: a symbolic expression is
either a list or an atom of some basic type. For a basic type, the
right `scheme_obj` constructor is called. The `list` is a recursive
production:

{% highlight ocaml %}
list
    : LPAREN exprlist RPAREN                      {  SchemeTypes.List (List.rev $2) }
    | LBRACKET exprlist RBRACKET                  {  SchemeTypes.List (List.rev $2) }
    | LPAREN exprlist sexp DOT sexp RPAREN        {  SchemeTypes.DottedList ((List.rev ($3::$2)), $5) }
    | LBRACKET exprlist sexp DOT sexp RBRACKET    {  SchemeTypes.DottedList ((List.rev ($3::$2)), $5) }
    ;
{% endhighlight %}

These four definitions handle the simple/dotted and paren/bracket
cases. Note that parens and brackets cannot be mixed in the same
sexp. Also note that non-dotted lists may be empty, while dotted ones
must have non-empty _car_ and _cdr_ fields. The `exprlist` rule above
will accept possibly empty `sexp` sequences:

{% highlight ocaml %}
exprlist
    :                                             {  []; }
    |  exprlist sexp                              {  $2::$1 }
    ;   
{% endhighlight %}

Note that, in this production, the _exprlist_ appears to the left of
the _sexp_; this is the preferred way for a LALR parser. Since the
accumulator will actually _prepend_ each sexp to the previous list,
the results must be reversed at the end (That's what the `List.rev`
calls are for in the `list` production).

There's really not much to it. Using `ocamlyacc` for such a simple
grammar may seem overkill, but bear in mind that there's a lot of
scheme we haven't covered at all (think of quoting and quasi-quoting,
for example). `ocamlyacc` really doesn't add that much complexity and
yet would make future additions easier.

The printer
===========

The final missing layer is the `Printer` module, which will just send
the result of an evaluation back to the user. At the moment, the
printer will just have a `write` function. Its output should be such
that, if `read` again, the value is `equal?` to the output. The code
is probably too boring to go over it here in detail. Just check the
[repo](https://github.com/jarnaldich/rops/tree/v1-rpn-calc).

The repl
========

Once we have the three layers complete, we can write the code that
ties them all together. Our main will just input buffer for the lexer
and call the repl. It will also catch the `Eof` exception thrown by
the lexer so it aborts without generating an "empty token" error.

{% highlight ocaml %}
let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "stdin" };
    repl lexbuf
  with
    Lexer.Eof ->
      flush stdout;
      exit 0
{% endhighlight %}

The the interesting code is in the `repl` function:

{% highlight ocaml %}
let rec repl lexbuf =
  print_prompt ();
  (try
     print_obj (Evaluator.eval (Parser.sexp Lexer.token lexbuf))
   with
    | SchemeLexerError (s, pos) ->
      Format.printf "Error in %s at line %d, char %d: %s@." pos.pos_fname pos.pos_lnum pos.pos_cnum s;
    | SchemeEvalError (s) -> Format.printf "Evaluation Error: %s@." s);
  repl lexbuf;;
{% endhighlight %}

Here you can see how we simply thread the results of the tree layers
(print_obj just calls the `Printer` module with the needed formatter).
The rest is just exception handling.

Final thougths
==============

You may have already realized that our Scheme interpreter is flawed in
many different ways:

- The INT representation is just wrong: we try to parse as an _ocaml_
  machine-dependent integer a string of digits, without caring about
  overflows and different integer semantics (ints in OCaml get 1 bit
  reserved).
- Strings support no escape sequences...

Of course, this is just to keep the code simple and easy to understand. As with other kinds of
programs, reality tends to spoil our beautiful ideas. Most real-world
parsers and lexers do all sort of tricks to make _lex_ and _yacc_
behave as they should. For example, lexical analysis tends to add some
sort of context information or global state in combination with
sub-parsers. Take a look at IronScheme's
[lexer](https://github.com/leppie/IronScheme/blob/master/IronScheme/IronScheme/Compiler/IronScheme.lex)
and
[parser](https://github.com/leppie/IronScheme/blob/master/IronScheme/IronScheme/Compiler/IronScheme.y)
for a real-world example of a parser for a Scheme language.

One of the main design decisions here is how much work each layer does
and how much work it defers to the next layer. In our case, for
example, it would make sense to capture some basic keywords in the
reader layer so that the evaluator doesn't need to do string
comparison. We have chosen to keep the reader layer thin so that we
can add all the missing features by just tweaking the evaluator
module. This also keeps most of the significant code in one place and
should be easier to follow.

Resources
=========
- The
  [camllex/camlyacc manual](http://caml.inria.fr/pub/docs/manual-ocaml/manual026.html).
- [IronScheme's parser](https://github.com/leppie/IronScheme/blob/master/IronScheme/IronScheme/Compiler/IronScheme.y)
- A gentler
  [introductory tutorial](http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/)
  on ocamlyacc.


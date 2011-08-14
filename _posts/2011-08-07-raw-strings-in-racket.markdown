---
layout: post
title: Raw Strings in Racket 
tags: racket scheme lisp
---

One of the features I missed from the outstanding lisp dialect
[Racket](http://www.racket-lang.org/) (formerly known as PLT Scheme),
especially when working on windows, was the ability to use some raw
string syntax. I this article I explain how easy it is for such a
feature to be implemented in Racket thanks to the language's ability
to extend its own syntax.  

__2011-08-14 UPDATE:__ This article can be used as a tutorial for
implementing readtable extensions to Racket. If you just want the
functionality, you can achieve it with the ``at-exp`` language,
already included in Racket's distribution. Just
skip to the <a href="#at-exp">at-exp</a> section at the end of this
tutorial to see how.

# The problem

This will probably sound familiar to any of you using windows paths or
regexes in a language with strings supporting backslash escape
sequences. Essentially, the problem is that instead of writing, for
example:

{% highlight scheme %}
(directory-list "\\MACHINE\Share\directory")
{% endhighlight %}

You need to write:

{% highlight scheme %}
(directory-list "\\\\MACHINE\\Share\\directory")
{% endhighlight %}

Not an awful amount of work, but if you got to appreciate python's
convenient ``r''`` and C# ``@""`` syntax, then you'd probably miss the
feature. 

# The plan

Racket is extensible by design. When we run a racket program, there
are several stages involved:

1. First, a _reader_ layer turns a stream of characters into a kind of
AST. Since racket is a lisp, this AST is, of course, made up of
s-expressions. But in Racket's case, they're a particular kind of
s-expressions which contain extra information on their lexical scope
and their source location, and are called _syntax objects_.

2. After that, those syntax objects are further expanded through the
macro layer.

The extension architecture in Racket will let you "plug" your
extension into existing languages, so in the process of creating a new
language you can (and usually will) build upon existing features.

Our purpose, then, is to build a new language on top of racket by
tuning the reader so that we can pass strings verbatim to the
expander layer.

# The reader extension

Racket provides a standard way to extend the reader by writing the so
called _reader extensions_. Those are modules that implement the
``read`` and ``read-syntax`` functions. Remember lisp is code-as-data?
``read`` is called when forms are to be used as data, and can return
any kind of value; ``read-syntax`` is called when forms are to be
interpreted as code, since its output is a syntax object. Apart from
the source location and scope, their behaviour should be equivalent
not to mislead the users, so you can implement ``read`` from
``read-syntax`` just by stripping the lexical information.

The implementation is listed below:

{% highlight scheme %}
(require syntax/readerr)
(provide read read-syntax)

(define (read in)
  (syntax->datum (read-syntax #f in)))

(define (read-syntax src in)
  
  (define opening-char (read-char in))
  (define closing-str (case opening-char
                         [(#\() "\\)"]
                         [(#\[) "\\]"]
                         [(#\{) "\\}"]
                         [else (string opening-char)]))
  
  (define regex (pregexp (string-append "^.*?" closing-str)))
  
  (define-values (line col pos) (port-next-location in))
  
  (define raw-str (regexp-match regex in))
  
  (unless raw-str 
    (raise-read-error "bad raw string syntax"
                      src line col pos
                      (- (file-position in) pos)))
  
  (define (strip-last-char s)
    (substring s 0 (- (string-length s) 1)))

  (define (to-syntax v)
    (datum->syntax #f ; lexical context. read-syntax should have no lexical context
                   v; Value
                   (vector src ; File, normally syntax-source
                           line  ; line
                           col  ; column
                           pos  ; character since beginning of source
                           (string-length v)  ; the span (width)
                           ))) ; check location info
  
  (to-syntax  (strip-last-char (bytes->string/locale (car raw-str)))))
{% endhighlight %}

If you name the above module as ``reader_extension.rkt``, then you can
pass a raw string to the expander by invoking the reader extension at
any moment in your code, just by prepending
``#reader"reader_extension.rkt"``. The reader extension, as you can
see in the previous listing, is written so that the first character
defines the extent of the string. If it's a pairing character (normal
or curly brackets and parenthesis), then it expects the appropiate
closing. Oherwise it looks for the same character.

See these rackunit tests for an example:

{% highlight scheme %}
(require rackunit)
(check-equal? #reader"reader_extension.rkt"(\\TEST\One) 
              "\\\\TEST\\One")

(check-equal? #reader"reader_extension.rkt"_\test\no\escape_ 
              "\\test\\no\\escape")
{% endhighlight %}

# The readtable hook

While the above fully works, It may not save much typing having to
write ``#reader"reader_extension.rkt"`` instead of manually backslashing the
string. It would be more convenient to just use one character, for
example ``$``, like this:

{% highlight scheme %}
(directory-list $(\\SERVER\Share))
{% endhighlight %}

Fortunately, that's pretty easy: the racket reader is implemented as a
recursive descent parser, and you can hook your own functions to call
back when the parser sees a character. This association between
characters and callbacks is known as the readtable. 

The readtable has a dynamic scope (it's a ``parameter``), and every
call to the ``read`` and ``read-syntax`` functions is performed in the
context of a readtable. There is, of course, a starting default
readtable in case the user didn't specify one.

# Using the extension as a language

The other drawback of using the ``#reader"reader_extension.rkt"`` prefix is that
you need to make the module available to each project, and use the
prefix each time you introduce a string. It would be both nicer and
more racketish to be able to use it any other language, like this:

{% highlight scheme %}
#lang with-raw-string racket #\%
(regexp-split (pregexp %'\s') "two fields")
{% endhighlight %}

Meaning that you add raw string syntax on top of the ``racket``
language with ``%`` as your readtable character.

# The syntax-module/reader language

Fortunately, both problems can be solved by using the
syntax/module-reader language, which is a helper language for
installing your own languages into a Racket distribution.

All you need to do is locate the collects dir
``(find-user-collects-dir)`` and place the "reader_extension.rkt" in a
subdirectory called ``with-raw-string/lang`` together with a
``reader.rkt`` with this contents:

{% highlight scheme %}
(module reader syntax/module-reader
  #:language read 
  #:wrapper2 (lambda (in rd)
               (parameterize ([current-readtable 
                               (make-raw-str-readtable (read in))])
                 (rd in)))
  
  (require syntax/readerr
           (prefix-in raw: "reader_extension.rkt"))
    
  (define readtable-hook
    (case-lambda
      [(ch in)
       (raw:read in)]
      [(ch in src line col pos)
       (raw:read-syntax src in)]))
  
  (define (make-raw-str-readtable c)
    (make-readtable (current-readtable)
                    c 'terminating-macro readtable-hook)))
{% endhighlight %}

The interesting points are:

- The ``make-raw-str-readtable`` will create a readtable that will
call the ``reader_extension`` functions with its character argument.

- The ``#:language`` keyword will let you specify the underlying
language. It can be a literal or a callback function. In this case we
use the ``read`` function as a callback, so that we read the
underlying language from the input stream.

- The ``#:wrapper2`` callback will parameterize both ``read`` and
``read-syntax`` with the quote-character enhanced readtable.  Note
that the quoting char is also read from the input stream first.

<a name="at-exp" > </a>
# UPDATE: The ``at-exp`` language 

After posting a link to this tutorial to the ``users@racket-lang.org``
mailing list (a very active and helpful list for Racket users), Eli
Barzilay (one of Racket's core developers) pointed out that the
[at-exp](http://docs.racket-lang.org/scribble/reader-internals.html?q=at-exp#(mod-path._at-exp)
language could be used to achieve the same results. This language acts
at the reader level and was originally developed for [scribble](http://docs.racket-lang.org/scribble/index.html?q=scribble)
 (a family of languages for writing textual content, such as
racket's documentation itself).

Basically, ``at-exp`` extends another language (passed in as a
parameter, like the one in this tutorial), so that expressions
of the form:

{% highlight scheme %}
@func{Text here}
{% endhighlight %}

make it to the expansion layer like

{% highlight scheme %}
(func "Text here")
{% endhighlight %}

Where text is read literally (no backslash substitution). So here's a
way to achieve the same functionality we expected just by what's
already provided by Racket:

{% highlight scheme %}
#lang at-exp racket
(define r string-append)
(display @r{...nearly free text here...})
{% endhighlight %}

When using __DrRacket__, you can press the _Macro Stepper_ button to
see how the above is read:

{% highlight scheme %}
(module anonymous-module racket
  (#%module-begin
   (define r string-append)
   (display (r "...nearly free text here..."))))
{% endhighlight %}

Of course, the ``r`` is there just to make the syntax shorter... you
could just use ``string-append`` each time. There is also another way
to pass parameters to the ``@`` functions, which is not relevant here,
through ``[]``. Check the
[docs](http://docs.racket-lang.org/scribble/reader-internals.html?q=at-exp#(mod-path._at-exp)
for the details.

# Conclusions

You see how easy it is to add new features on top of the Racket
language. How many languages do you know that you can modify to
match your needs in this way?

BTW, The above code is available at [github](http://github.com/jarnaldich/with-raw-string).

# Further Reading

Check out [the excellent Racket
documentation](http://docs.racket-lang.org/guide/languages.html) on
creating new languages for racket. Seriously, the racket documentation
system is as impressive as Racket itself.

Check out [This article](http://hashcollision.org/brainfudge/) for a
more complete example on how to design a Turing-complete (but maybe
not that useful) language in Racket.

Check out [This other
article](http://matt.might.net/articles/implementing-a-programming-language/)
to see how to develop an interpreter for two small languages, but
without using Racket's language extension mechanisms (evaluation is
performed at run-time, through an eval function).

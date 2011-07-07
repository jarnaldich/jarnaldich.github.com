---
layout: post
title: Blogging with Jekyll
tags: jekyll blogging
---

... or, "from static to dynamic and back again".

I've finally decided to set-up a new technical blog. I think one of
the main reasons it took me so long to start was that I couldn't find
an engine that matched my taste. Ok... as a coder, my taste is not
exactly that of a final user:

- I didn't want to pay. I woult have felt a bit dumb knowing I could
  be using something like [WordPress](http://www.wordpress.com) or
  [Blogger](http://www.blogger.com) for free even if they're not
  exactly what I'm looking for.
- I don't think storing pages in a relational DB is the smartest thing to
  do. Relational DBs are for structured data, not documents. 
- I want to be able to use the tools I learned to love as a coder in my
  posts. After all, It's all about text, right? I want versioning,
  regexp searches, and all that good old unix tools for text processing.
- I'd rather die than use a web-based editor.
- I want to be able to produce high quality, standards compliant and
  easily modifyable html... but authoring in a more human-friendly
  markup, of course... 
- I don't want to be stuck to a system once I've started. What I care
  most is content. I want to be able to migrate my posts whenever I want. 
- The system should be elegant, not a bunch of scripts tied together.

After reading [this
post](http://tom.preston-werner.com/2008/11/17/blogging-like-a-hacker.html),
it became clear that what I wanted was a *static* site generator.

Static? But that's soooo 1990s... well yes... and no. I've you've ever
tried to roll your own blog engine (a common web framework learning
exercice), and got to the point of optimization, you may have come to
realize that you can cache most, if not all, your
content... mmmmm... This is certainly oversimplifying, but you get the
point:

DYNAMIC + A WHOLE LOT OF CACHING = STATIC

The other, probably most relevant, difference from the 90s is the
mashups culture. Your web app can rely on third party services for its
dynamic parts. (IMHO, Even if you are able to serve dynamic content,
it's still interesting to use some of those services).

Now that you know the _whys_, let's talk about the _hows_.

## Jekyll
There are several static page generators out there (just Google for
them). I feel confident with Ruby, but the key point here was
[Github Pages](http://pages.github.com/) having automatic
[Jekyll](http://jekyllrb.com/) processing (no suprise: jekyll's author is
one of the Github founders). 

I must admit I am curious about [Hakyll](http://jaspervdj.be/hakyll/)
too... 

With jekyll, you get:

- A full templating system based on markup authoring run through
  [liquid](http://www.liquidmarkup.org/)-powered templates.
- Syntax highlighting (through) pygments. Just install it and run with
  the `--pygments` option.
- Tagging (not directly, but easy to add).
- A content-based related posts feature.

Plus, of course, the possibility to store your blog at GitHub.

## Comments system

There are some good reasons for using a third party comment system
instead of your own (security, merging different sources,
statistics...), even if you can serve dynamic content. For this blog I
chose [disqus](http://disqus.com/), but there are others...

## Github

Both the content and the templates for this blog are hosted at a
Github free account, which will even let you redirect your domain.
You can check the source code, fork the repo or whatever
[here](https://github.com/jarnaldich/jarnaldich.github.com).



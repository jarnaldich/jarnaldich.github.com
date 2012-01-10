---
layout: post
title: ROPS v0.1 - The Parser
tags: rops parser
---

In the [previous post](/2012/01/03/rops-parser.html) we ended up with
a fully working version of a polish-notation calculator. In our slow
journey towards a fully working Scheme interpreter, the next thing we
will add is a proper environment, so that any Object in our language
can be properly _named_ and _stored_ for later retrieval.

Our language will be a statically scoped one in the same way Scheme
is: 

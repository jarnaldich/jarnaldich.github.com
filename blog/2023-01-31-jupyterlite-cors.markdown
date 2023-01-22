---
title: Dealing with CORS in JupyterLite
date: 2023-01-31T00:00:00
tags: jupyterlite, CORS, data, data, webassembly
---

Following my [previous
post](blog/2022/12/08/data-manipulation-jupyterlite.html), I am intending to see
how far I can push JupyterLite as a platform for data analysis in the browser.
The convenience of having a full enviroment with a sensible default set of
libraries for dealing with data [one link
away](https://jupyterlite.github.io/demo/lab/index.html) is really something I could use.

But of course, for data analysis you need... well... data. There is certainly no
shortage of public datasets on the internet. The [Open
Data](https://data.europa.eu/en/publications/open-data-maturity/2022) initiative
in the EU is specially interesting 

But, as soon as you try to use JupyterLite to directly fetch data from those
sites, you find yourself stumping a wall named CORS.

## Cross Origin Request Policy

The Cross Origin Request Policiy is a protection system designed to prevent a
family of attacks known as Cross Site Scripting or XSS. Browsers implement this
protection by not allowing a page to perform requests to a server that is
different from where it wast downloaded unless this other server explicitly
allows for it.

This behaviour bites hard at any application involving third party data
analysis, and a lot of webassembly "ports" of existing applications have a hard
time with networking since the original desktop apps do not suffer from this
restriction and were not designed to deal with it[^webvm].

For example, if you are using the Jupyterlite at `jupyterlite.github.io`, you
will not be able to fetch any server beyond `github.io` that does not allow for
it specifically... which most data providers don't do. You will either need to
download yourself the data and upload it to JupyterLite, or self-host
jupyterlite and the data in your own server (using it as a proxy for data
requests), which kinda takes all the convenience out of it.

There are two ways in which a data provider can accept cross-site requests. The
main one (the "canonical" one) is by adding explicit permission in the HTTP
headers. Whenever this is not possible or practical (it needs access to the HTTP
server configuration, and some hosting providers may not allow it), there is a
second way: the JSONP callback.

## Implementing a JSONP Callback helper

The JSONP callback works this way:

1. The calling page defines a callback function, with a data parameter.
2. The calling page (JupyterLite) loads a script from the data provider, passing
   a the name of the callback function.
3. The data provider script calls the callback function with the requested data.
   
Since the script was downloaded from the data provider's domain, it can perform
requests to that domain, so CORS restrictions do not apply. 

This is not the recommended solution because it delegates to the application
something that belongs to another layer: both the server and the consuming
webpage have to modified. It may be the only way in older browsers, though.



![&nbsp;](/images/tiles5k.png "Orthophoto Tiling"){.center}

[^webvm]: If you are curious about the possible solutions to this problems, you
    may like to read how [WebVM](https://webvm.io/), a server-less virtual
    Debian, implements a general solution 
    [here](https://leaningtech.com/webvm-virtual-machine-with-networking-via-tailscale/).

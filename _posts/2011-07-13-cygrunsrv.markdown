---
layout: post
title: Run Legacy Apps as Windows Services
tags: windows service
---

# The problem #

Although the right thing to do when programming a windows service is
to use Microsoft's API from the very beginning, it may also be
desirable to run a regular executable as a service. A typical
candidate would be a legacy TCP server, so that you get:

- automatic startup without login.
- integration with third-party monitoring apps.
- the ability to define failure recovery policies.

Microsoft had already noticed this, so they provide the executable
[SRVANY](http://support.microsoft.com/kb/137890) as part of the
**Windows Resource Kit**; but sometimes installing the whole resource
kit may not be an option, specially in a production server. 

It would be nicer to have a small application you could just
xcopy-deploy that would allow you to install, remove and update the
properties of your service candidate.

# The solution #

Here's where [CygWin's](http://www.cygwin.com/) little {{cygrunsrv}}
comes handy. Notice that, although it behaves nicely with cygwin apps,
it can be used with **any** Win32 executable.

To use it, you just need to put the files `cygrunsrv.exe` and
`cygwin1.dll` in the same directory and go. Unfortunately, both
files lie in different packages and cannot, AFAIK, be downloaded on
their own, so the first time you'll have to download and uncompress the
files {{cygrunsrv-1.34.1.tar.bz2}} and {{cygwin-1.7.9-1.tar.bz2}},
(version may differ in the future) from whatever mirror is most suitable for you. 

The full documentation for this tool is
[here](http://web.mit.edu/cygwin/cygwin_v1.3.2/usr/doc/Cygwin/cygrunsrv.README).


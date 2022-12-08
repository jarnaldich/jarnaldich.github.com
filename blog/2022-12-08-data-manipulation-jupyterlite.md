---
title: Data Manipulation with JupyterLite
date: 2022-12-08T00:00:00
tags: data, tools, jupyterlite, data-manipulation, data-wrangling, data-munging, webassembly
---

![&nbsp;](/images/jupyterlite.png "JupyterLite screenshot"){ width=400px .center }

Data comes in all sizes, shapes and qualities. The process of getting data ready
for further analysis is equally crucial and tedious, as many data professionals
will [confirm](https://forbes.com/sites/gilpress/2016/03/23/data-preparation-most-time-consuming-least-enjoyable-data-science-task-survey-says/).

This process of many names (data wrangling/munging/cleaning) is often performed
by an unholy mix of command-line tools, one-shot scripts and whatever is at hand
depending on the data formats and computing environment.

I have been intending to share some of the tools I have found useful for this
task in a series of blog posts, especially if they are particularly
unexpected or lesser-known. I will always try to demonstrate the tool with some
common data processing application, and then finally highlight which
conditions the tool is most suitable under.

# JupyterLite

Jupyter/JupyterLab are the de-facto standard notebook environment, especially
among Python data scientists (although it was designed from the start to work
for multiple languages or _kernels_ as the [name
hints](https://blog.jupyter.org/i-python-you-r-we-julia-baf064ca1fb6)). The
frontend runs in a browser, and
setting up the backend often
requires a local installation, although some providers will let you spin a
backend in the cloud, see: [Google
Collab](https://colab.research.google.com/) or [The Binder
Project](https://mybinder.org/).

JupyterLite is simpler/cleaner solution for simple analysis if sharing is not
needed: it is a quite complete Jupyter environment where all its components
run in the browser via webassembly compilation. Just visit its
[Github](https://github.com/jupyterlite/jupyterlite) project page for the
details. Following some of the referenced projects and examples is a worthy
rabbit hole to enter.

Some things you might not expect from a webassembly solution:

- Comes with most data-science libraries ready to use: matplotlib, pandas, numpy.
- Can install third party packages via regular magic:
```
%pip install -q bqplot ipyleaflet
```

## Example: Not so Simple Excel Manipulation

Sometimes you need to perform some not so simple manipulation in an Excel sheet
that outgrows pivot tables but is kind of the bread and butter of pandas. Since
copying an excel table defaults to a tabbed-separated string, getting a pandas
dataframe is as easy as firing up jupyterlite by visiting [this
page](https://jupyterlite.github.io/demo/lab/index.html), then getting a python
notebook and evaluating this code in the first cell, pasting the excel table
between the multi-line string separator:

```python
import pandas as pd
import io

df = pd.read_table(io.StringIO("""
<PRESS C-V HERE>
"""))
df
```
# Highlights

- **Useful for:** The kind of analysis/manipulation one would use Pandas / Numpy
  for, especially if it involves visualizations or richer interaction.
- **Useful when:** You don't have access to a pre-installed Jupyter environment but
  have a modern browser and intenet connection at hand, or when you are dealing
  with sensitive data that should not leave your computer.

# Conclusion

JupyterLite is an amazing project: as with many webassembly based solutions, we
are just starting to see the possibilities. I encourage you to explore it
beyond data manipulation because you can easily find other applications for it,
from interactive dashboards to authoring diagrams...

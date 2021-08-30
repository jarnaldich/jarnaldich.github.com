---
title: Posershell tricks
date: 2020-12-30T:00:00
tags: postgres, metaprogramming
---

A fucnction stores information of the file where it was defined. Useful for getting de relative path of dot-sorced scripts

```powershell
${function.RoboC}.File
```

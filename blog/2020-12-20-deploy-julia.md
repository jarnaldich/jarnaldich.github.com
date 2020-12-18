---
title: Deploying Julia Apps
date: 2020-12-20T:00:00
tags: julia, deploy
---

A Julia application from zero to deploy

## Generate a package for the app

Packages are the main way to publish code for reuse in Julia. Packages may be
versioned, keep track of their own dependencies and may be itself
dependencies of larger projectes. The default package management utility for
Julia is the Pkg *package*. Packages live in a particular _environment_ and are
loaded through *import* or *using* statements. It is the responsability of the
active environment to know which code (version, file on disk) is loaded when a
particular name is imported.

Environments in Julia are similar to environments in other programming
languages: a set of packages (source code designed for reuse) and their
dependencies. While every Julia installation comes with a default environment,
it is wise to create a specific one for each project we develop, to avoid
generating installation-wide problems when a library for a particular project
needs to be updated, for example.

https://github.com/JuliaLang/PackageCompiler.jl#static-julia-compiler

add PackageCompiler to the Main environment

Then create a new env for the project:

julia.exe --% -e 'using Pkg; Pkg.generate("OrtoXpres")'

Enter julia with package: julia.exe --% --project=.

using PackageCompiler
create_sysimage(:OrtoXpres, sysimage_path="OrtoXpres.so")

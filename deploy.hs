#!/usr/bin/env stack
-- stack --package directory-tree --package Turtle .\deploy.hs
{-# LANGUAGE OverloadedStrings #-}
-- RUN with stack runghc deploy.hs
-- TODO: 
--    directory-tree not needed
--    add command-line parser and deploy to GitHub options 
import System.Directory.Tree
import Turtle
import System.FilePath

main = do
  dir <- pwd 
  when (filename dir /= "jarnaldich.github.com") (die "Must be run from git root")

  cptree "./_site" "."
  rmtree "./drafts"
  shell "git status" empty

  -- Copy the site blog
  -- To just print the structure for debugging
  -- (site :/ t) <- readDirectoryWith return "./_site/"
  -- print t
  
--  (site :/ t) <- readDirectory "./_site/"
--  res <- writeDirectory (".." :/ t)
--  return ()

--  let failed = anyFailed t
--  if not failed then

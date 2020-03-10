import System.Directory.Tree

main = do
  (site :/ t) <- readDirectory "./_site/blog"
  writeDirectory ("." :/ t)
--  let failed = anyFailed t
--  if not failed then

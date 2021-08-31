USAGE
=====

In the "source" branch

1. Add new file into "blog". (You can use drafts for ideas you do not want to process yet).
2. Run `stack run watch` to fire up a server for preview. Edit until happy.
3. Save work at any time by commiting/pushing to the source branch

The "contents" of the blog will be generated under the `_site` subdir.

When you are ready to publish:

1. Switch to the "master" branch: `git checkout master`
2. Execute `deploy.ps1`. This will copy the contents of the `_site` subdir into the root dir and add it to git.
3. Publish (commit/push) to the master branch.

References
==========
https://jip.dev/posts/the-switch-to-hakyll/

https://stackoverflow.com/questions/29868096/how-to-use-pandoc-filter-within-hakyll

http://pandoc.org/scripting.html

http://jaspervdj.be/hakyll/reference/Hakyll-Core-Compiler.html

http://jaspervdj.be/hakyll/reference/Hakyll-Web-Pandoc.html

https://ahti.space/git/hydraz/blag/raw/master/site.hs

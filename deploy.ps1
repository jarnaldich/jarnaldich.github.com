stack runghc  --package directory-tree .\deploy.hs
#copy -Force -Recurse .\_site\blog\*\*\*\*.* .\blog\ 
git add .\blog

("css images tags apps" -split " ") | % {
    copy -Force -Recurse .\_site\$_\*.* .\$_\ 
    git add $_
}

copy -Force .\_site\*.*ml . 
git add *.*ml
git status
echo "Check files before commiting and pushing..."

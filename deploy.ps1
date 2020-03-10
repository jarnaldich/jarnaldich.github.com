copy -Recurse .\_site\blog\*\*\*\*.* .\blog\ 
git add .\blog

("css images tags" -split " ") | % {
    copy -Recurse .\_site\$_\*.* .\$_\ 
    git add $_
}

copy .\_site\*.*ml . 
git add *.*ml
git status
echo "Check files before commiting and pushing..."

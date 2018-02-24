
#checks status of local repository with respect to remote repository
function checkstatus()
{
git fetch origin 
echo $(git status)
}

#Puts all uncommitted changes to changes.log
function redirectChanges()
{
#git stash pop >> changes.log
git diff >> changes.log
}
#Puts each line from every file of your project with the tag TODO into a file todo.log
function todo()
{
grep -r "#TODO" >> todo.log --exclude "todo.log" --exclude "ProjectAnalyze.sh"
}
#Checks all haskell files for syntax errors and puts the results into error.log
function checkHaskell()
{
find -name "*.hs" -exec  ghc -fno-code {} \;>>error.log 2>&1

}


function pagebypage()
{
find . -type f -name "*.txt" -exec  more {} \;
}

function firstfew()
{
find . -type f -name "*.txt" -exec   head {} \;
}

function lastfew()
{
find . -type f -name "*.txt" -exec  tail {} \;
}

checkstatus
redirectChanges
todo
checkHaskell
pagebypage
firstfew
lastfew


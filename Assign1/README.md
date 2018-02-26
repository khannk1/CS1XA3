1) The function checkstatus checks the status of the local repository with respect to the remote repository.The function uses git fetch and git status to obtain the result 

2)Function redirectChanges redirects all the changes we made to our local repository that have not been commited to file called changes.log using the git diff command

3)The function todo Puts each line from every file of the project with the tag #TODO into a file todo.log

4) Function checkHaskell redirects all the erros in a haskell file to a file error.log using the exec flag

5) Function pagebypage redirects  the content of each text file page by page using the exec flag and more command

6)Function firstfew redirects the first five lines of each text file using the exec flag and head command

7)Function lastfew redirects the last five lines of each text file using the exec flag and tail command 

REFERENCES

1) I would like to give credit to Sam Cymbaluk for informing that we need to use git fetch before comparing the status of the local and remote repositories 


2)I would like to give credit to Jessy De Leeuw as I took the feature of exclude flag from her project and used it in my todo function

3)I referred to the following link to gather insight on how to use find and exec which I have used in my script
https://stackoverflow.com/questions/5119946/find-exec-with-multiple-commands

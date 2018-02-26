## Implementation

1) The function `checkstatus` checks the status of the local repository with respect to the remote repository.The function uses git fetch and git status to obtain the result

2) The function `redirectChanges` redirects all the changes we made to our local repository that have not been committed to a file called changes.log using the git diff command

3) The function `todo` puts each line from every file of the project with the tag #TODO into a file todo.log

4) The function `checkHaskell` redirects all the errors in a Haskell file to a file error.log using the exec flag. I have created a testing file Haskell.hs to test this functionality. 

5) The function `pagebypage` redirects the content of each text file page by page using the exec flag and more command and returns the result in page.log

6) The function `firstfew` redirects the first five lines of each text file using the exec flag and head command and stores it in head.log

7) The function lastfew redirects the last five lines of each text file using the exec flag and tail command and stores it in tail.log


## Testing

1) Created 'Haskell.hs' which includes Haskell code to test 'checkHaskell' function and also has  `#TODO` to test `todo` function and return the results to `error.log` and `todo.log` respectively.

2) I also created two `.txt` files sample.txt and sample2.txt with some text from 'Lorem Ipsum' (https://www.lipsum.com) and some lines to specifically test for head and tail. I included two files instead of just one to make sure the content of both the files is returned in the desired log files. I have also tested by varying the number of lines to returned in the script.

    Some test cases:
    1) Test for head content
    2) Test for tail content
    3) Test for two files
    4) Test with variable number of lines to be returned
    5) Test for empty text file
    6) Test for test file with insufficient text


## Refrences

1) I would like to give credit to Sam Cymbaluk for informing that we need to use git fetch before comparing the status of the local and remote repositories

2) I would like to give credit to Jessy De Leeuw as I took the feature of exclude flag from her project and used it in my todo function

3) I referred to the following link to gather insight on how to use find and exec which I have used in my script https://stackoverflow.com/questions/5119946/find-exec-with-multiple-commands


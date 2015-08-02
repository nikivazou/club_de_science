Making emails to count tables
------------------------------

\begin{code}

module EmailNormalization where 

import Control.Applicative
import System.Directory (getDirectoryContents)


import BST as BST 
import MailProcessor
\end{code}


This code assumes the existence of two directories full of emails. 
The `Spam/spamEmails/` directory has spam emails, and 
the `Spam/hamEmails/` directory has ham (good) emails. 
Make these two directories manually!


\begin{code}
spamDir = "Spam/spamEmails/"
hamDir  = "Spam/hamEmails/"
\end{code}

`getDirectoryContents` is a function that returns all the files in a directory

< getDirectoryContents :: String -> IO [String]

_Caveat 1:_ Is the above the real type of the function? 
The library `System.Directory` that defines the above function uses the _type alias_


< type FilePath = String

for readability.

_Caveat 2:_ `getDirectoryContents` returns all the files of a directory. 
Sadly, '.' and '..' are treated by the filesystem as files too. 
Write a modified version of the function that throws away these files.

\begin{code}
getDirectoryContents' :: FilePath -> IO [FilePath]
getDirectoryContents' = undefined
  where
    noDots x = x /= "." && x /= ".."

\end{code}


Using your modified function, and the function 

< redFile :: FilePath -> IO String

write the function `getWordsFromDir` that for each file returns the "processed" list of words.
\begin{code}
getWordsFromDir :: String -> IO [String]
getWordsFromDir dir 
  = undefined 
\end{code}


Our goal is to transform emails into a `Map`, i.e., the BST that we builded earlier.
For that we create a type alias for the `WordCount`:

\begin{code}
type WordCount = Map String [Int]
\end{code}

`WordCount` maps words to a list of integers that always has two elements.
The first element counts how many times a word appears in spam 
and the second in hams.
For example, if "free" appears in spams 10 times and in hams 2, 
and "monad" appears in spams 0 times and in hams 3, 
then your word count should look like 

< wc = fromList [("free", [10, 2]), ("monad", [0, 3])]

Write two functions that update the `WordCount` according to a list of words:

\begin{code}
updateSpamWords :: WordCount -> [String] -> WordCount
updateSpamWords wc ss = undefined

updateHamWords :: WordCount -> [String] -> WordCount
updateHamWords wc ss = undefined
\end{code}

Now we can combine all the above, and complete the function `makeCountTable`
that according to the contents of the `Spam` directory constructs a `WordCount`:

\begin{code}
makeCountTable :: IO WordCount
makeCountTable = undefined 
\end{code}
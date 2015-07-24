\begin{code}

module EmailNormalization where 

import Control.Applicative
import System.Directory (getDirectoryContents)


import BST as BST 
import MailProcessor

clasifyEmail :: String -> IO ()
clasifyEmail path = do 
  emailwords     <- processEmail <$> readFile path
  wordCountTable <- makeCountTable 
  return ()


spamDir = "Spam/spamEmails/"
hamDir = "Spam/hamEmails/"

makeCountTable 
  = do spamWords <- getWordsFromDir spamDir
       hamWords  <- getWordsFromDir hamDir
       return $ updateWords updateHam  [0, 1] hamWords 
              $ updateWords updateSpam [1, 0] spamWords BST.empty
  where
  	updateSpam [s, h] = [s+1, h]
  	updateHam  [s, h] = [s, h+1]

updateWords f d ls m = foldr (\k m -> BST.insertWith g k d m) m ls 
  where
    g _ = f 

getWordsFromDir dir 
  = do files <- map (dir ++) <$> filter validFile <$> getDirectoryContents dir
       cts   <- mapM readFile files
       return $ concatMap processEmail cts 
  where
  	validFile x = x /= "." && x /= ".."

\end{code}
module Main where

import Types
import MailProcessor (processEmail)

import Control.Applicative

main = do 
  vc     <- makeVocubulary <$> readFile "vocab.txt"
  spam1  <- readFile "spamSample1.txt"
  spam2  <- readFile "spamSample2.txt"
  email1 <- readFile "emailSample1.txt"
  email2 <- readFile "emailSample2.txt"
  return $ show (show vc ++ "\n\n" ++ show (processEmail vc test1)) 




emailFeatures voc 

makeVocubulary :: String -> Vocabulary 
makeVocubulary = vfromList . go . words
  where
--   	go :: [String] -> [(String, Int)]
    go [] = []
    go (n:w:xs) = (w, read n :: Int):go xs 


test1 = " > Anyone knows how much it costs to host a web portal ? > Well, it depends on how many visitors youre expecting. This can be anywhere from less than 10bucks a month to a couple of $ 100. You should checkout http://www.rackspace.com/ or perhaps Amazon EC2 if youre running something big..To unsubscribe yourself from this mailing list, send an email to:groupname-unsubscribe@egroups.com"

module Spam.MailProcessor (processEmail) where

import Data.Char
import Data.Maybe
import Data.List (isPrefixOf)

import NLP.Stemmer

processEmail :: String -> [String]
processEmail = mapMaybe processWord . words . deleteChars 
  where
    deleteChars = filter (not . (`elem` punc))
    punc = ['!', '?', '.', ',']


processWord :: String -> Maybe String 
processWord = fmap (stem English) . stripHTML . normalize . map toLower


stripHTML :: String -> Maybe String
stripHTML x | isNonWord x = Nothing 
            | otherwise   = Just x

isNonWord x = isHTML x || x == ">"

normalize = normalizeURL . normalizeEmail . normalizeNumer . normalizeDollar



normalizeIf p f x = if p x then f x else x  

normalizeURL    = normalizeIf isURL    $ const "httpaddr"
normalizeEmail  = normalizeIf isEmail  $ const "email"
normalizeDollar = normalizeIf isDollar $ const "dollar"
normalizeNumer  = normalizeIf isNum    toNum

isHTML x = head x == '<' && last x == '>'

isURL x  = isPrefixOf "http://" x || isPrefixOf "https://" x

isDollar = ("$" ==)

isNum    = any isDigit

isEmail  = any ('@' ==)

toNum    = go False
  where 
    go wasDigit []     | wasDigit  = "number"
  	                   | otherwise = []
    go wasDigit (x:xs) | isDigit x = go True xs 
  	                   | wasDigit  = "number" ++ go False xs 
  	                   | otherwise = x:go False xs  
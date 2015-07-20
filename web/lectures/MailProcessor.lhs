\begin{code}
module MailProcessor (processEmail) where

import Data.Char
import Data.Maybe
import Data.List (isPrefixOf)

import NLP.Stemmer
\end{code}



`processEmail` is the mail function that processed the email. 
It goes from the full content of the emai (typed `String`) 
to the list of the _normalized_ words of the input email.
It does three steps

- `deleteChars :: String -> String` deletes the characters that are punctuation,
   specifically the ones defined in the list `punc` (fill in the defition of the function)

- `words :: String -> [String]` is a function imported from `Data.Char` that splits a String into its words

- `process :: [String] -> [String]` processed each word accodring to `processWord` (fill in the definition of the function)

\begin{code}
processEmail :: String -> [String]
processEmail = process . words . deleteChars 
  where
  	deleteChars :: String -> String
  	deleteChars = undefined 
  	punc        = ['!', '?', '.', ',']

  	process :: [String] -> [String]
  	process = undefined 
\end{code}


`processWord` is the function that actually processes each word. 
It proceeds in four steps:
- converts the word to lower case letters (fill in `toLowerWord`)
- normalizes the word
- strips out the HTML code, and
- stems the word according to the NLP algorithm. 

Stemming just keeps the roots of the words, i.e. it will make the following 
transformations that are crucial for email crassification

```
am, are, is ->  be 
car, cars, car's, cars' ->  car
```
\begin{code}
processWord :: String -> Maybe String 
processWord = fmap (stem English) . stripHTML . normalize . toLowerWord
\end{code}


\begin{code}
toLowerWord :: String -> String
toLowerWord = undefined 
\end{code}


Normalization of a word normalizes URLS, emails, dollars, and numbers:

\begin{code}
normalize :: String -> String
normalize = normalizeURL . normalizeEmail . normalizeNumber . normalizeDollar
\end{code}

Next, you should fill in the definitions for the normalization functions.

Function `normalizeDollar` replaces the character '$' with the word "dollar":

< normalizeDollar "$"   = "dollar"
< normalizeDollar "foo" = "foo"

\begin{code}
normalizeDollar :: String -> String 
normalizeDollar  = undefined
\end{code}

Function `normalizeURL` replaces URLS with the word "httpaddr":

< normalizeURL "http://google.com"  = "httpaddr"
< normalizeURL "https://google.com" = "httpaddr"
< normalizeURL "foo"                = "foo"

\begin{code}
normalizeURL :: String -> String 
normalizeURL  = undefined
\end{code}


Function `normalizeEmail` replaces email addresses with the word "email":

< normalizeEmail "nvazou@cs.ucsd.edu"   = "email"
< normalizeEmail "foo"                  = "foo"

\begin{code}
normalizeEmail :: String -> String 
normalizeEmail  = undefined
\end{code}


Finally, function `normalizeNumber` replaces numbers with the word number:

< normalizeNumber "42"  = "number"
< normalizeNumber "42$" = "number$"

\begin{code}
normalizeNumber :: String -> String 
normalizeNumber  = undefined
\end{code}

- Stripping out HTML code

\begin{code}
stripHTML :: String -> Maybe String
stripHTML x | isNonWord x = Nothing 
            | otherwise   = Just x

isNonWord x = isHTML x || x == ">"
isHTML x    = head x == '<' && last x == '>'
\end{code}
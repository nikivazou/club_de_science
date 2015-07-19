module Types where

import Data.Map as M

type Vocabulary = M.Map String Int

emptyVocabulary = M.empty
vfromList :: [(String, Int)] -> Vocabulary
vfromList       = M.fromList 
strToInt :: String -> Vocabulary -> Maybe Int 
strToInt = M.lookup  
import Control.Applicative

import GHC.Real

-- import Data.Probability
-- import Control.Monad.Distribution

-- import Control.Monad.MonoidValue

-- import Spam.SpamClassifier 
import Spam.PPrint 
import Spam.MailProcessor 
import Spam.SpamClassifier

import System.Directory (getDirectoryContents)

import qualified Data.BST as BST

import Data.List (foldl')

main :: IO ()
main = clasifyEmail mailPath

-- boolDist :: Prob -> BDDist Bool
boolDist p =
    weighted [(True, p'), (False, 1-p')]
  where p' = fromProb p



entryFor :: Enum a => a -> [b] -> b
entryFor x ys = ys !! fromEnum x


mailPath = "mail.txt"

clasifyEmail :: String -> IO ()
clasifyEmail path = do 
  emailwords     <- processEmail <$> readFile path
  wordCountTable <- makeCountTable 
  let res = bayes (hasWords wordCountTable emailwords initmsgType)
  print $ pprint res 


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


-- | initCounts the initial spam and ham email count 
initCounts :: [Ratio Integer]
initCounts = [102, 57]


initmsgType :: Dist d => d MsgType
initmsgType =
  weighted (zipWith (,) [Spam,Ham] initCounts)




-- uniform = weighted . map (\x -> (x, 1))


-- | hasWord _ "free" prior
-- | (Note prior has info on how ofter you get spam)
-- | P(isSpam | w = "free") = P(w = "free" | isSpam) * P(isSpam) / normalization
-- |

hasWord wc word prior = do
  msgType <- prior
  -- | returns a list of distributions for the word
  -- | here only (spam | ham) so just two distributions
  wordPresent <- wordPresentIn wc msgType word
  -- | for every element I am normalizing with respect to 
  -- | this element being true
  guard wordPresent
  return msgType


wordPresentIn wc msgType word =
    boolDist (prob (n/total))
  where wordCounts = findWordCounts wc word
        n     = entryFor msgType wordCounts
        total = entryFor msgType initCounts


findWordCounts wordCountTable word =
  BST.findWithDefault [1,1] word wordCountTable


hasWords _ []     prior = prior
hasWords wc (w:ws) prior = do
  hasWord wc  w (hasWords wc ws prior)





---- Probs

newtype Prob = P Float
  deriving (Eq, Ord, Num)


fromProb (P n) = n
prob n = P n 

instance Show Prob where
  show (P p) = show intPart ++ "." ++ show fracPart ++ "%"
    where digits = round (1000 * p)
          intPart = digits `div` 10
          fracPart = digits `mod` 10

data Perhaps a = Perhaps a Prob
  deriving (Show)


neverHappens (Perhaps _ 0) = True
neverHappens _             = False

never = Perhaps undefined 0

instance Functor Perhaps where
  fmap f (Perhaps x p) = Perhaps (f x) p

instance Monad Perhaps where
  return x = Perhaps x 1
  ph >>= f  | neverHappens ph  = never
            | otherwise        = Perhaps x (p1 * p2)
    where (Perhaps (Perhaps x p1) p2) = fmap f ph


newtype PerhapsT m a = PerhapsT { runPerhapsT :: m (Perhaps a) }
type Dist = PerhapsT ([])

weighted :: [(a, Float)] -> Dist a
weighted [] =
  error "Empty probability distributuion"
weighted xws = PerhapsT (map weight xws)
  where weight (x,w) = Perhaps x (P (w / sum))
        sum = foldl' (\w1 (_,w2) -> w1+w2) 0 xws




type FDist' = MaybeT FDist
guard :: Bool -> FDist' ()
guard = MaybeT . return . toMaybe
  where toMaybe True  = Just ()
        toMaybe False = Nothing
bayes :: FDist' a -> [Perhaps a]
bayes = exact . onlyJust . runMaybeT

module Spam.SpamClassifier  where

import Control.Monad.Distribution
import Control.Monad.MonoidValue
import Data.List
import Data.Maybe
import Data.Probability


-- Each message is spam (junk mail) or "ham" (good mail).
data MsgType = Spam | Ham
  deriving (Show, Eq, Enum, Bounded, Ord)


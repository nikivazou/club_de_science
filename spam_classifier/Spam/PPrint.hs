{-# LANGUAGE FlexibleInstances #-}

module Spam.PPrint (pprint) where

import Control.Applicative

import GHC.Real

import Data.Probability
import Control.Monad.Distribution

import Control.Monad.MonoidValue

import Spam.SpamClassifier 


printSpamResult spamp hamp
  | spamp >= 0.8 && hamp < 0.2 
  = "It is definitely a spam!"
  | hamp >= 0.8  && spamp < 0.2 
  = "It is definitely a ham!"
  | spamp >= 0.5 && hamp < 0.5 
  = "It may be a spam"
  | hamp >= 0.5  && spamp < 0.5
  = "It may be a ham"
  | otherwise 
  = "Could not classify your email"

class PPrint a where
	pprint :: a -> String

instance PPrint a => PPrint (Maybe a) where
	pprint Nothing = "Not enought info!"
	pprint (Just x) = pprint x

instance PPrint (MVT Prob [] MsgType) where
	pprint (MVT x) = pprint x

instance PPrint ([MV Prob MsgType]) where
	pprint ls = case (spamp, hamp) of 
                 ([sp], [hp]) -> printSpamResult sp hp
                 _            -> "Something went wrong!"
		where 
			spamp = [p | MV p Spam <- ls] 
			hamp  = [p | MV p Ham  <- ls] 


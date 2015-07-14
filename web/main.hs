{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow ((>>>))
import Control.Monad

import Hakyll

copyDir = do route   idRoute
             compile copyFileCompiler

main :: IO ()
main = hakyll $ do
    match "static/*"     copyDir 
    
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "templates/*" $ compile templateCompiler
    match "lectures/*"  $ myMakeHTML
    match "homeworks/*" $ myMakeHTML
    match "*.markdown"  $ myMakeHTML

tops = [ "index.markdown"
       , "lectures.markdown"
       , "links.markdown"
       , "assignments.markdown"]

myMakeHTML 
  = do route   $ setExtension "html"
       -- route   $ setExtension "lhs"
       compile $ pandocCompiler
                 >>= loadAndApplyTemplate "templates/default.html" defaultContext
                 >>= relativizeUrls

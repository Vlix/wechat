module TestParse where

import Control.Monad (zipWithM_)

import Text.XML.Light

import Web.WeChat.XMLParse
import Web.WeChat.Types

test :: IO ()
test = do
  x <- readFile "test.xml"
  let contents = onlyElems $ parseXML x
  zipWithM_ (\i elt -> putStr (show i ++ " ") >> print (parseInMessage' elt)) [1..] $ contents


-- main program for unit test

module Main where

--import Second
import Test.HUnit
import Universal

--main = runTestTT tests
main = do
  res <- getDesc "data/CS"
  print (show res)
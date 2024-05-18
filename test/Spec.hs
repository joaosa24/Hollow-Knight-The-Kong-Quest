module Main where

import Test.HUnit

import Tarefa1Spec
import Tarefa2Spec
import Test.HUnit

main :: IO ()
main = runTestTTAndExit $ test [testesTarefa1, testesTarefa2]
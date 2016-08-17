module Main where

import           Test.Tasty

import qualified Persistent as Persistent

tests = testGroup "tests" [ Persistent.tests
                          ]


main = defaultMain tests

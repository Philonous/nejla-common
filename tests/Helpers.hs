{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Helpers where

import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as Text
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

import           Language.Haskell.TH

import           NejlaCommon.Helpers

prop_showText_safeRead_inverts = \(item :: Integer) ->
  safeRead (Text.unpack $ showText item) == Just item


prop_downcase :: String -> Bool
prop_downcase = \str -> case (str, downcase str) of
  ([], []) -> True
  (s:ss, d:dd) -> and [ not $ generalCategory d == UppercaseLetter
                      , toLower s == d
                      , ss == dd
                      ]

prop_upcase :: String -> Bool
prop_upcase = \str -> case (str, upcase str) of
  ([], []) -> True
  (s:ss, d:dd) -> and [ not $ generalCategory d == LowercaseLetter
                      , toUpper s == d
                      , ss == dd
                      ]

-- Upper case letter
newtype UCLetter = UCLetter {unUCLetter :: Char}

instance Show UCLetter where
  show (UCLetter c) = show c

instance Arbitrary UCLetter where
  arbitrary = UCLetter <$> elements ['A'..'Z']
  shrink _ = []

prop_cctu :: [Char] -> [Char] -> UCLetter -> [Char] -> Bool
prop_cctu = \delim left' (UCLetter r) right' ->
              let left = toLower <$> left'
                  right = r : (toLower <$> right')
              in if left == "" || right == ""
                 then True
                 else cctu delim (left ++ right)
                      == (left ++ delim ++ downcase right)

prop_without_prefix :: String -> [Char] -> Bool
prop_without_prefix = \pre str -> withoutPrefix pre (pre ++ str) == str

tests :: TestTree
tests = $testGroupGenerator

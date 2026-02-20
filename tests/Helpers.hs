{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Helpers where

import Data.Char
import qualified Data.Text as Text
import NejlaCommon.Helpers
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

newtype Str = Str {str :: String} deriving (Show)

-- This needs to be expanded. Not quite clear what the right character class is.
-- Arbitrary Chars don't work (invalid unicode), the functions being tested also
-- exist in a speciifc context, e.g. usually for template haskell code working
-- on Haskell indentifiers. Greek letters *might* come up, emoji hopefully not.
-- English alphabet will have to do for now, it's at least some reassurance that
-- the code does something sensible
instance Arbitrary Str where
  arbitrary = Str <$> listOf1 (elements $ ['a' .. 'z'] ++ ['A' .. 'Z'])

-- Upper case letter
newtype UCLetter
  = UCLetter
  { unUCLetter :: Char
  }

instance Show UCLetter where
  show (UCLetter c) = show c

instance Arbitrary UCLetter where
  arbitrary = UCLetter <$> elements ['A' .. 'Z']

  shrink _ = []

spec :: Spec
spec = describe "helpers" $ do
  prop "showText and safeRead are inverse of each other" $ \(item :: Integer) ->
    safeRead (Text.unpack $ showText item) == Just item

  prop "upcase works as expected" $ \str ->
    case (str, downcase str) of
      ([], []) -> True
      (s : ss, d : dd) ->
        and
          [ generalCategory d /= UppercaseLetter,
            -- Lower-casing only makes sense on upper case letters
            not (isUpper s) || toLower s == d,
            ss == dd
          ]

  prop "upcase works as expected" $ \str ->
    case (str, upcase str) of
      ([], []) -> True
      (s : ss, d : dd) ->
        and [generalCategory d /= LowercaseLetter, toUpper s == d, ss == dd]

  prop "cctu works as expected" $
    \(Str delim)
     (Str left')
     (UCLetter r)
     (Str right') ->
        let left = toLower <$> left'
            right = r : (toLower <$> right')
         in or
              [ left == "",
                right == "",
                cctu delim (left ++ right) == (left ++ delim ++ downcase right)
              ]

  prop "withoutPrefix removes prefix" $ \pre str ->
    withoutPrefix pre (pre ++ str) == str

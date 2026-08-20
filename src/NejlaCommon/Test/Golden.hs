{-# LANGUAGE OverloadedStrings #-}

module NejlaCommon.Test.Golden where

import Conduit
import Control.Monad (guard, when)
import Control.Monad.Catch
import Data.Algorithm.Diff (PolyDiff (..), getGroupedDiff)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LText
import System.IO.Error (isDoesNotExistError)
import Test.HUnit (assertFailure)
import Test.Hspec (Expectation, expectationFailure)
import qualified Text.Pretty.Simple as Pretty

-- | Assert that @expected@ matches the golden file at @path@.
-- If the golden file doesn't exist yet, writes a @.candidate@ file so the
-- developer can review and bless it manually
goldenCheck ::
  -- | Path to "golden" file containing expected output
  FilePath ->
  -- | Text to check against "golden" file
  LText.Text ->
  Expectation
goldenCheck path expected = do
  -- Try reading file and handle "does not exist" rather than check-then-open
  result <- tryJust (guard . isDoesNotExistError) (BSL.readFile path)
  case result of
    Right bs -> do
      -- Decode file - assumes UTF-8
      actual <-
        either
          (assertFailure . ("UTF-8 decode error: " <>) . show)
          pure
          (LText.decodeUtf8' bs)
      -- Diff actual and expected text, produce pretty output on failure
      when (actual /= expected) $ do
        let diff =
              colorDiff
                (lines $ LText.unpack actual)
                (lines $ LText.unpack expected)
        expectationFailure $
          "Golden mismatch for " <> path <> ":\n" <> diff
    Left () -> do
      -- No .golden file yet, so write current output to .candidate
      -- Clobbers existing candidate, that's expected
      let candidate = path <> ".candidate"
      BSL.writeFile candidate (LText.encodeUtf8 expected)
      expectationFailure $
        "Golden file missing: "
          <> path
          <> "\nWrote candidate to "
          <> candidate
          <> "\nReview it, then: mv "
          <> candidate
          <> " "
          <> path

-- | Colorized unified-ish diff for terminal output.
colorDiff :: [String] -> [String] -> String
colorDiff old new =
  unlines $ concatMap renderGroup (getGroupedDiff old new)
  where
    renderGroup (First ls) = map (\l -> red $ "- " <> l) ls
    renderGroup (Second ls) = map (\l -> green $ "+ " <> l) ls
    renderGroup (Both ls _) = case ls of
      -- Show context lines around changes, collapse long unchanged runs
      _
        | length ls <= 6 -> map ("  " <>) ls
        | otherwise ->
            map ("  " <>) (take 3 ls)
              <> ["  " <> dim ("... (" <> show (length ls - 6) <> " unchanged lines)")]
              <> map ("  " <>) (drop (length ls - 3) ls)

    red s = "\ESC[31m" <> s <> "\ESC[0m"
    green s = "\ESC[32m" <> s <> "\ESC[0m"
    dim s = "\ESC[2m" <> s <> "\ESC[0m"

-- | Parse an input file and golden-test the result.
goldenParseSource ::
  (ConduitT () ByteString (ResourceT IO) () -> IO (Either String LText.Text)) ->
  FilePath ->
  Expectation
goldenParseSource parse path = do
  let input = sourceFile path
  -- Check if files parses, then compare to .golden
  parsed <- parse input >>= either assertFailure pure
  goldenCheck (path <> ".golden") parsed

-- | Parse an input file and gold-test the result, shows and pretty-prints result
goldenParseSourceShow ::
  (Show a) =>
  (ConduitT () ByteString (ResourceT IO) () -> IO (Either String a)) ->
  FilePath ->
  Expectation
goldenParseSourceShow parse =
  goldenParseSource
    ( fmap
        -- Add newline since pShowNoColor does not produce it
        -- And editors will commonly add it when saving
        -- This prevents some spurious test failures
        (fmap ((<> "\n") . Pretty.pShowNoColor))
        . parse
    )

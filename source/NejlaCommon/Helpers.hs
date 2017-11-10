{-# LANGUAGE OverloadedStrings #-}
module NejlaCommon.Helpers where

import           Control.Lens
import           Data.Aeson.TH
import           Data.Char
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import qualified Data.List as List
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import           Language.Haskell.TH

--------------------------------------------------------------------------------
-- General String/Text helpers -------------------------------------------------
--------------------------------------------------------------------------------

-- | Product Text output using show instance
showText :: Show a => a -> Text
showText = Text.pack . show

safeRead :: Read a => String -> Maybe a
safeRead str = case reads str of
     ((v,_):_) -> Just v
     _ -> Nothing

-- | Convert the first character in a String to lower case
downcase :: String -> String
downcase [] = []
downcase (x:xs) = toLower x : xs

-- | Convert the first character in a String to upper case
upcase :: String -> String
upcase [] = []
upcase (x:xs) = toUpper x : xs

-- | Convert CamelCase to under_score
cctu :: [Char] -- ^ Delimiter to use (e.g. "_")
     -> [Char] -- ^ Input String
     -> [Char]
cctu delim = go
  where
    go [] = []
    go [c] = [toLower c]
    -- Handle All-caps acronyms followed by capitalized word
    -- (e.g. EUBar => EU-Bar)
    go (c1 : c2 : cs@(c3:_))
      | isUpper c1 && isUpper c2 && isLower c3 =
          [toLower c1] ++ delim ++ [toLower c2] ++ go cs
    go (c1 : cs@(c2:_))
      | (not $ isUpper c1) && isUpper c2 = [c1] ++ delim ++ go cs
      | otherwise = [toLower c1] ++ go cs

-- | Remove a prefix from a String, throwing an error of the prefix is not found
withoutPrefix :: String -- ^ Prefix to remove
              -> String -- ^ Input string
              -> String
withoutPrefix pre' l = case List.stripPrefix pre' l of
    Nothing -> error $ pre' <> " is not a prefix of " <> l
    Just l' -> l'

--------------------------------------------------------------------------------
-- Aeson helpers ---------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Default options for creating JSON instances using Aeson.
aesonTHOptions :: [Char] -- ^ field prefix to strip
               -> Options
aesonTHOptions pre' = defaultOptions{ fieldLabelModifier = mkName'
                                    , constructorTagModifier = mkCName
                                    }
  where
    delim = "_"
    mkName' = cctu delim . withoutPrefix pre'
    mkCName = cctu delim . withoutPrefix (upcase pre')

--------------------------------------------------------------------------------
-- Lens helpers ----------------------------------------------------------------
--------------------------------------------------------------------------------


-- | Default replacements for camelCaseFields'
--
-- Contains:
--
-- * type => type'
-- * default => default'
defaultReplacements :: HashMap String String
defaultReplacements = HMap.fromList
  [ ("type", "type'")
  , ("default", "default'")
  ]


-- | Lens' camelCaseFields code doesn't check for illegal names like "type" or
-- "default". This function fixes them by looking them up in a map of
-- replacements
camelCaseFieldsReplacing :: HashMap String String -> LensRules
camelCaseFieldsReplacing replacements = camelCaseFields & lensField %~
                   (\lf -> \typeName fieldNames fieldName ->
                              substNames <$> lf typeName fieldNames fieldName)
  where
    substNames (TopName name) = TopName (fixName replacements name)
    substNames (MethodName className methodName) =
        MethodName className (fixName replacements methodName)

fixName :: HashMap String String -> Name -> Name
fixName replacements name =
    let nb = nameBase name
    in case HMap.lookup nb replacements of
        Nothing -> name
        Just replace -> mkName replace

camelCaseFields' :: LensRules
camelCaseFields' = camelCaseFieldsReplacing defaultReplacements

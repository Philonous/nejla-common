module NejlaCommon.Persistence.Util where

import           Data.Char
import           Data.List                  ( dropWhileEnd )

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax

sql :: QuasiQuoter
sql =
  QuasiQuoter
  { quoteExp  = \txt ->
      let ls   -- Remove empty lines at beginning and end
             = dropWhile (all isSpace) . dropWhileEnd (all isSpace) $ lines txt
          ind = minimum . map (length . takeWhile isSpace) $ ls
          unindented = unlines $ map (drop ind) ls
      in litE $ stringL unindented
  , quotePat  = \_ -> error "quotePat sql is undefined"
  , quoteType = \_ -> error "quoteType sql is undefined"
  , quoteDec  = \_ -> error "quoteDec sql is undefined"
  }

embedFileString :: FilePath -> Q Exp
embedFileString filename = do
  addDependentFile filename
  contents <- runIO $ readFile filename
  litE $ stringL contents

sqlFile :: FilePath -> Q Exp
sqlFile = embedFileString

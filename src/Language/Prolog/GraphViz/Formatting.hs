module Language.Prolog.GraphViz.Formatting
  ( GraphFormatting(..)
  , defaultFormatting
  , queryStyle, resolutionStyle
  , legacyStyle
  ) where

import Language.Prolog (Unifier, Goal, Term(Struct))
import Data.Char (isAlphaNum)
import Data.GraphViz.Attributes.HTML (TextItem (..))
import qualified Data.Text.Lazy
import Data.List (intercalate, intersperse)

data GraphFormatting = GraphFormatting
  { formatGoals :: [Goal] -> TextItem
  , formatUnifier :: Unifier -> [TextItem]
  , formatSolution :: Unifier -> [TextItem]
  }

legacyStyle :: GraphFormatting
legacyStyle = GraphFormatting
  { formatGoals = legacyGoals
  , formatUnifier = legacyUnifier
  , formatSolution = legacyUnifier
  }

defaultFormatting :: GraphFormatting
defaultFormatting = queryStyle

textItem :: String -> TextItem
textItem = Str . Data.Text.Lazy.pack

legacyGoals :: [Goal] -> TextItem
legacyGoals = textItem . intercalate "," . map show

legacyUnifier :: Unifier -> [TextItem]
legacyUnifier [] = [textItem " "]
legacyUnifier u  = intersperse (Newline []) [ textItem $ show v ++ " = " ++ show t | (v,t) <- u ]

queryStyle :: GraphFormatting
queryStyle = GraphFormatting
  { formatGoals = goalQuery
  , formatUnifier = slashUnifier
  , formatSolution = legacyUnifier
  }

resolutionStyle :: GraphFormatting
resolutionStyle = GraphFormatting
  { formatGoals = goalSet
  , formatUnifier = slashUnifier
  , formatSolution = legacyUnifier
  }

goalSet :: [Goal] -> TextItem
goalSet xs = textItem $
  "{"++ intercalate ", " (map format xs) ++"}"
  where
    format (Struct "false" _) = "false"
    -- contains operator
    format term@(Struct (c:_) _)
      | not (isAlphaNum c) = "¬(" ++ show term ++ ")"
    format term = "¬" ++ show term

goalQuery :: [Goal] -> TextItem
goalQuery xs = textItem $
  "?- "++ intercalate ", " (show <$> xs) ++ "."

slashUnifier :: Unifier -> [TextItem]
slashUnifier [] = [textItem " "]
slashUnifier u = intersperse (Newline []) [ textItem $ show v ++ "/" ++ show t | (v,t) <- u ]

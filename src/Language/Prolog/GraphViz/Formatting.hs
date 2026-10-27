module Language.Prolog.GraphViz.Formatting
  ( GraphFormatting(..)
  , defaultFormatting
  , queryStyle, resolutionStyle
  , legacyStyle
  ) where

import Language.Prolog (
  Unifier,
  Goal,
  Term(Struct),
  prettifyTerm,
  prettifyVariableName,
  )
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

unifier :: Char -> Unifier -> [TextItem]
unifier _ [] = [textItem " "]
unifier symbol u = intersperse
  (Newline [])
  [ textItem $ unwords
      [ prettifyVariableName v
      , [symbol]
      , prettifyTerm t
      ]
  | (v,t) <- u
  ]

legacyGoals :: [Goal] -> TextItem
legacyGoals = textItem . intercalate "," . map prettifyTerm

legacyUnifier :: Unifier -> [TextItem]
legacyUnifier = unifier '='

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
    format (Struct "false" _) = "true"
    -- contains operator
    format term@(Struct atom@(c:_) _)
      | not (isAlphaNum c) || atom == "is" = "¬(" ++ prettifyTerm term ++ ")"
    format term = "¬" ++ prettifyTerm term

goalQuery :: [Goal] -> TextItem
goalQuery xs = textItem $
  "?- "++ intercalate ", " (prettifyTerm <$> xs) ++ "."

slashUnifier :: Unifier -> [TextItem]
slashUnifier = unifier '/'

module Main where

import Language.Prolog (consult, parseQuery)
import ParseArgs (parseArgs)
import Language.Prolog.GraphViz (
  resolveTreeToFileWith,
  resolveFirstTreeToFileWith,
  defaultFormatting,
  resolutionStyle,
  )
import Text.Parsec (ParseError)
import Data.Functor (void)

main :: IO ()
main = do
   (queryString, files, output, setNotation, onlyFirst) <- parseArgs
   p <- concat <$> mapM ((abortOnError=<<) . consult) files
   q <- abortOnError $ parseQuery queryString
   let style = if setNotation then resolutionStyle else defaultFormatting
   void $
    if onlyFirst
      then resolveFirstTreeToFileWith style output p q
      else resolveTreeToFileWith style output p q

abortOnError :: Either ParseError b -> IO b
abortOnError = either (error . show) return

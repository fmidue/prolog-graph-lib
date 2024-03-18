module Main where

import Language.Prolog (consult, parseQuery)
import ParseArgs (parseArgs)
import Language.Prolog.GraphViz (resolveTreeToFile,resolveFirstTreeToFile)
import Text.Parsec (ParseError)
import Data.Functor (void)

main :: IO ()
main = do
   (queryString, files, output, onlyFirst) <- parseArgs
   p <- concat <$> mapM ((abortOnError=<<) . consult) files
   q <- abortOnError $ parseQuery queryString
   void $
    if onlyFirst
      then resolveFirstTreeToFile output p q
      else resolveTreeToFile output p q

abortOnError :: Either ParseError b -> IO b
abortOnError = either (error . show) return

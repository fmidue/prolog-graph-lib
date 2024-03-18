{-# LANGUAGE DeriveDataTypeable #-}
module ParseArgs (
   parseArgs
) where

import System.Console.CmdArgs
import System.Environment (getProgName)

data Options = Options
   { query :: String
   , file  :: [String]
   , output :: String
   , positional :: [String]
   , first_result :: Bool
   }
  deriving (Data, Typeable)

options = Options
  { query  = def &= typ "QUERY" &= help "Set Prolog query (If not set, first positional argument is used)"
  , file   = def &= typ "FILE"  &= help "Consult file before executing query"
  , output = "graph.png" &= typ "FILE"  &= help "Save generated image to file (default: 'graph.png')"
  , positional = def &= args &= typ "QUERY [FILE]..."
  , first_result = def &= help "Resolve only until the first success"
  }
  &= versionArg [ignore]
  &= helpArg [name "h"]

parseArgs = do
   opts <- getProgName >>= cmdArgs . ((options &=) . program)
   return $ case opts of
      Options q fs o []      b -> (q, fs,      o,b)
      Options _ fs o [q]     b -> (q, fs,      o,b)
      Options _ fs o (q:fs') b -> (q, fs++fs', o,b)

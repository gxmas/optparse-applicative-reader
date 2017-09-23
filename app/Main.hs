module Main where

import Control.Applicative (many)
import Control.Monad (forM_)
import Data.Maybe
import Data.Monoid ((<>))
import Options.Applicative
import Options.Applicative.Builder.Reader

data CmdLine = CmdLine { clOpts :: Maybe NamedValue
                       , clArgs :: [NamedValue]
                       } deriving Show

cmdline :: Parser CmdLine
cmdline = CmdLine
          <$> optional (option namedValue
                          (short 'p' <> long "param" <> metavar "<name=value>"))
          <*> many (argument namedValue
                      (metavar "<name=value>..."))

opts :: ParserInfo CmdLine
opts = info (cmdline <**> helper)
             (fullDesc <> progDesc "Demonstrate command line parsing for NamedValue")

printNamedValue :: NamedValue -> IO ()
printNamedValue nv = putStrLn $ "\tname: " ++ fst nv ++ "  value: " ++ snd nv

main :: IO ()
main = execParser opts >>= \cl -> do
  putStrLn "Option"
  maybe (putStrLn "\tNo option specified") printNamedValue $ clOpts cl
  putStrLn "Arguments"
  forM_ (clArgs cl) printNamedValue

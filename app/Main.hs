module Main
  ( main
  ) where

import Lib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [f] -> do
      contents <- readFile f
      res <- runEitherT $ startBf contents
      case res of
        Left err -> print err
        Right _ -> return ()
    _ -> print "Usage: ./brainfuhs <pathToProgramFile>"

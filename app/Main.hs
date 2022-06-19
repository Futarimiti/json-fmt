module Main where

import           Lib                (fmt)
import           System.Environment (getArgs)
import           System.IO          (stdin)

main :: IO ()
main = do args <- getArgs
          if null args
          then getContents >>= putStrLn . fmt
          else fstArg >>= putStrLn . fmt
            where fstArg = head <$> getArgs


module Main where

import           FmtConfig          (FmtConfig, parseConfig)
import           Lib                (fmt, fmt')
import           System.Directory   (doesFileExist)
import           System.Environment (getArgs, lookupEnv)
import           System.IO          (IOMode (ReadMode), hGetContents, hPutStrLn,
                                     openFile, stderr)
import Control.Monad (when)


main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs []           = getContents >>= fmtWithMaybeConfig False
parseArgs ["-v", filepath] = readFile filepath >>= fmtWithMaybeConfig True
parseArgs [filepath, "-v"] = readFile filepath >>= fmtWithMaybeConfig True
parseArgs (filepath:_) = readFile filepath >>= fmtWithMaybeConfig False

fmtWithMaybeConfig :: Bool -> String -> IO ()
fmtWithMaybeConfig verbose str = do maybeConf' <- maybeConf
                                    case maybeConf' of Left msg -> do when verbose $ hPutStrLn stderr msg
                                                                      putStrLn $ fmt str
                                                       Right conf -> putStrLn $ fmt' conf str

  {-
    get config from $JSONFMT_CONFIG
      if not exist, get from $XDG_CONFIG_HOME/json-fmt/config.json
        if $XDG_CONFIG_HOME not set or set but not a valid conf, use default while report error to stderr
      if exist but not a valid conf, use default while report error to stderr
    -}
maybeConf :: IO (Either String FmtConfig)
maybeConf = do maybePath <- lookupEnv envVarName
               case maybePath of Just envPath -> do configStr <- readFile envPath
                                                    case parseConfig configStr of Left msg -> return $ Left $ "In " ++ envVarName ++ ": " ++ msg
                                                                                  Right conf -> return $ Right conf
                                 Nothing -> do maybePath <- lookupEnv "XDG_CONFIG_HOME"
                                               case maybePath of Just xdgpath -> do let fullPath = xdgpath ++ defaultConfigPath
                                                                                    exist <- doesFileExist fullPath
                                                                                    if exist then do configStr <- readFile fullPath
                                                                                                     case parseConfig configStr of Left msg -> return $ Left $ "In $XDG_CONFIG_HOME" ++ defaultConfigPath ++ ": " ++ msg
                                                                                                                                   Right conf -> return $ Right conf
                                                                                             else return $ Left "No config file found, using default config"
                                                                 Nothing -> return $ Left "Both $JSONFMT_CONFIG and $XDG_CONFIG_HOME not set, using default config"

envVarName :: String
envVarName = "JSONFMT_CONFIG"

defaultConfigPath :: String
defaultConfigPath = "/json-fmt/config.json" -- in XDG_CONFIG_HOME


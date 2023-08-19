module Main (main)
where

import           Data.List          (intercalate, isPrefixOf)
import           FmtConfig          (FmtConfig, defaultConfig, parseConfig)
import           Lib                (fmtDefault, fmtWithConf)
import           Logged
import           Prelude            hiding (log)
import           System.Directory   (doesFileExist)
import           System.Environment (getArgs, lookupEnv)
import           System.FilePath    ((</>))
import           System.IO          (hPutStrLn, stderr, stdin)
import           Util

main :: IO ()
main = (getArgs >>= manageArgsLogged) >>= output . setOutputs

manageArgsLogged :: [String] -- args
                 -> IO (Logged (String, [Option]))
manageArgsLogged args = do Logged fmtLogs res <- doFmtLogged (maybeFP, opts)
                           return $ Logged (parseLogs ++ fmtLogs) (res, opts)
                             where Logged parseLogs (maybeFP, opts) = parsePathOptsLogged args

doFmtLogged :: (Maybe FilePath, [Option]) -> IO (Logged String)
doFmtLogged (maybeFP, opts) = input >>= fmt
  where input :: IO String
        input = getContentsFrom maybeFP

        getContentsFrom :: Maybe FilePath -> IO String
        getContentsFrom Nothing   = getContents
        getContentsFrom (Just fp) = readFile fp

        fmt :: String -> IO (Logged String)
        fmt jsonStr = do Logged confLogs conf <- ioConf
                         case fmtWithConf conf jsonStr of
                           Left msg -> return $ Logged (confLogs ++ ["Format error: " ++ msg]) jsonStr
                           Right fmtStr -> return $ Logged confLogs fmtStr

        -- try to read external configurations, or use default if unsuccessful
        ioConf :: IO (Logged FmtConfig)
        ioConf = do envVarRes <- lookupEnv envVarName
                    case envVarRes of
                      Just path -> do exists <- doesFileExist path
                                      if exists
                                      then do configFile <- readFile path
                                              let Logged parseConfLogs conf = parseConfig configFile
                                              return $ Logged (("Using config file pointed by $" ++ envVarName):parseConfLogs) conf
                                      else return $ Logged ["File pointed by $" ++ envVarName ++ " does not exist, using default config"] defaultConfig
                      Nothing -> do maybeXDG <- lookupEnv "XDG_CONFIG_HOME"
                                    case maybeXDG of
                                      Just xdg -> do let xdgConfPath = xdg </> defaultConfigPathWithinXDG
                                                     existsXDGConf <- doesFileExist xdgConfPath
                                                     if existsXDGConf
                                                       then do confStr <- readFile xdgConfPath
                                                               let Logged parseConfLogs conf = parseConfig confStr
                                                               return $ Logged (("Using config file " ++ ("$XDG_CONFIG_HOME" </> defaultConfigPathWithinXDG)) : parseConfLogs) conf
                                                       else return $ Logged ["Did not find any config files, using default config"] defaultConfig
                                      Nothing -> return $ Logged ["Found neither $" ++ envVarName ++ " nor $XDG_CONFIG_HOME, using default config"] defaultConfig

parsePathOptsLogged :: [String] -- args
                    -> Logged (Maybe FilePath, [Option]) -- filepath and opts
parsePathOptsLogged = foldr iter $ Logged [] (Nothing, [])
  where iter :: String -> Logged (Maybe FilePath, [Option]) -> Logged (Maybe FilePath, [Option])
        iter x acc@(Logged logs (Just fp, opts))
          | isLongOpt x = case readLongOpt x of Nothing -> log ("Unrecognised long option: " ++ x) acc
                                                Just opt -> (Just fp, opt:opts) <$ acc
          | isShortOpt x = case readShortOpt x of Nothing -> log ("Unrecognised short option: " ++ x) acc
                                                  Just opt -> (Just fp, opt:opts) <$ acc
          | otherwise = log ("Multiple file paths: " ++ fp ++ " and " ++ x) acc
        iter x acc@(Logged logs (Nothing, opts))
          | isLongOpt x = case readLongOpt x of Nothing -> log ("Unrecognised long option: " ++ x) acc
                                                Just opt -> (Nothing, opt:opts) <$ acc
          | isShortOpt x = case readShortOpt x of Nothing -> log ("Unrecognised short option: " ++ x) acc
                                                  Just opt -> (Nothing, opt:opts) <$ acc
          | otherwise = Logged logs (Just x, opts)

        isLongOpt str = "--" `isPrefixOf` str
        isShortOpt str = "-" `isPrefixOf` str && not ("--" `isPrefixOf` str)

        readLongOpt :: String -> Maybe Option
        readLongOpt "--verbose" = return Verbose
        readLongOpt _           = Nothing

        readShortOpt :: String -> Maybe Option
        readShortOpt "-v" = return Verbose
        readShortOpt _    = Nothing

  {-
    enum for all command line opts
    -}
data Option = Verbose
              deriving Eq

setOutputs :: Logged (String, [Option])
           -> (Maybe String, String) -- to stderr, to stdout
setOutputs (Logged [] (str, opts)) = (Nothing, str)
setOutputs (Logged logs (str, opts))
  | Verbose `elem` opts = (Just (intercalate "\n" logs), str)
  | otherwise = (Nothing, str)

output :: (Maybe String, String) -> IO ()
output (Nothing, out)  = putStrLn out
output (Just err, out) = hPutStrLn stderr err >> putStrLn out


envVarName :: String
envVarName = "JSONFMT_CONFIG"

defaultConfigPathWithinXDG :: String
defaultConfigPathWithinXDG = "json-fmt/config.json" -- in XDG_CONFIG_HOME


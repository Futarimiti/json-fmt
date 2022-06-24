module Main (main)
where

import           Data.List          (intercalate, isPrefixOf)
import           FmtConfig          (FmtConfig, defaultConfig, parseConfig)
import           Lib                (fmtDefault, fmtWithConf)
import           System.Directory   (doesFileExist)
import           System.Environment (getArgs, lookupEnv)
import           System.FilePath    ((</>))
import           System.IO          (IOMode (ReadMode), hGetContents, hPutStrLn,
                                     openFile, stderr, stdin)
import           Util

main :: IO ()
main = getArgs >>= manageArgs >>= setOutputs >>= output

type Log = String

manageArgs :: [String]
           -> IO (Maybe Log, String, [Option]) -- maybe (entire) log, (maybe) formatted result, opts
manageArgs args = do (fmtLogs, fmtRes) <- doFmt (maybeFP, opts)
                     return (intercalate "\n" <$> merge parseLogs fmtLogs, fmtRes, opts) -- "\n" will be automatically converted to \r\n on Windows
  where (parseLogs, (maybeFP, opts)) = parsePathOpts args

-- assuming args containing 0 or 1 arg of filepath and any number of valid opts
parsePathOpts :: [String] -> ([Log], (Maybe FilePath, [Option])) -- log (for e.g. unrecognised opts), filepath and opts
parsePathOpts = foldr iter ([], (Nothing, []))
  where
  iter :: String -> ([Log], (Maybe FilePath, [Option])) -> ([Log], (Maybe FilePath, [Option]))
  iter x (logs, (Just fp, opts))
    | isLongOpt x = case readLongOpt x of Nothing -> (("Unrecognised long option: " ++ x):logs, (Just fp, opts))
                                          Just opt -> (logs, (Just fp, opt:opts))
    | isShortOpt x = case readShortOpt x of Nothing -> (("Unrecognised short option: " ++ x):logs, (Just fp, opts))
                                            Just opt -> (logs, (Just fp, opt:opts))
    | otherwise = (("Multiple file paths: " ++ fp ++ " and " ++ x):logs, (Just fp, opts))
  iter x (logs, (Nothing, opts))
    | isLongOpt x = case readLongOpt x of Nothing -> (("Unrecognised long option: " ++ x):logs, (Nothing, opts))
                                          Just opt -> (logs, (Nothing, opt:opts))
    | isShortOpt x = case readShortOpt x of Nothing -> (("Unrecognised short option: " ++ x):logs, (Nothing, opts))
                                            Just opt -> (logs, (Nothing, opt:opts))
    | otherwise = (logs, (Just x, opts))

  isLongOpt str = "--" `isPrefixOf` str
  isShortOpt str = "-" `isPrefixOf` str && not ("--" `isPrefixOf` str)

readLongOpt :: String -> Maybe Option
readLongOpt "--verbose" = return Verbose
readLongOpt _           = Nothing

readShortOpt :: String -> Maybe Option
readShortOpt "-v" = return Verbose
readShortOpt _    = Nothing

-- read file or stdin, do fmt (optionally using opts), report errors
doFmt :: (Maybe FilePath, [Option]) -> IO ([Log], String)
doFmt (maybeFP, opts) = getContentsFrom maybeFP >>= maybeFmt
  where getContentsFrom :: Maybe FilePath -> IO String
        getContentsFrom Nothing   = getContents
        getContentsFrom (Just fp) = readFile fp

        maybeFmt :: String -> IO ([Log], String) -- logs and res json, fmt or not
        maybeFmt jsonStr = do (confLogs, conf) <- ioConf
                              case fmtWithConf conf jsonStr
                                of Left msg -> return (confLogs ++ ["Format error: " ++ msg], jsonStr)
                                   Right fmtStr -> return (confLogs, fmtStr)

        -- try to read external configurations, or use default if unsuccessful
        ioConf :: IO ([Log], FmtConfig)
        ioConf = do envVarRes <- lookupEnv envVarName
                    case envVarRes of
                      Just path -> do configFile <- readFile path
                                      case parseConfig configFile of
                                        Left msg -> return (["Found environment variable $" ++ envVarName ++ ", but got error while parsing config: " ++ msg, "Using default configurations"], defaultConfig)
                                        Right conf -> return (["Using configurations from environment variable " ++ envVarName], conf)
                      Nothing -> do maybeXDG <- lookupEnv "XDG_CONFIG_HOME"
                                    case maybeXDG of
                                      Just xdg -> do let xdgConfPath = xdg </> defaultConfigPathWithinXDG
                                                     existsXDGConf <- doesFileExist xdgConfPath
                                                     if existsXDGConf
                                                       then do confStr <- readFile xdgConfPath
                                                               case parseConfig confStr of
                                                                 Left msg -> return (["Found XDG configurations but got error while parsing config: " ++ msg, "Using default configurations"], defaultConfig)
                                                                 Right conf -> return (["Using XDG configurations"], conf)
                                                       else return (["Found neither environment variable $" ++ envVarName ++ " nor XDG configurations, using default configurations"], defaultConfig)
                                      Nothing -> return (["Found neither environment variable $" ++ envVarName ++ " nor $XDG_CONFIG_HOME, using default configurations"], defaultConfig)

  {-
    enum for all command line opts
    -}
data Option = Verbose
              deriving Eq

-- decides which go to stdout and which go to stderr
setOutputs :: (Maybe Log, String, [Option])
           -> IO (Maybe String, String) -- to stderr, to stdout
setOutputs (maybeLog, str, opts)
  | Verbose `elem` opts = return (maybeLog, str)
  | otherwise = return (Nothing, str)

-- do print
output :: (Maybe String, String) -> IO ()
output (Nothing, out) = putStrLn out
output (Just err, out) = do hPutStrLn stderr err
                            putStrLn out

envVarName :: String
envVarName = "JSONFMT_CONFIG"

defaultConfigPathWithinXDG :: String
defaultConfigPathWithinXDG = "json-fmt/config.json" -- in XDG_CONFIG_HOME


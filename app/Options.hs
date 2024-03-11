module Options (parseArgs, Action (..)) where

import           Action              (Action (Action), FormatAction (..))
import           Options.Applicative (Alternative ((<|>)), Parser, ParserInfo,
                                      execParser, fullDesc, header, help,
                                      helper, info, long, metavar, progDesc,
                                      short, strArgument, switch)

parseArgs :: IO Action
parseArgs = execParser parserInfo

parserInfo :: ParserInfo Action
parserInfo = info (helper <*> action)
  (fullDesc <> header "comma-leading JSON formatter" <> progDesc "format input JSON from stdin or a file")

action :: Parser Action
action = Action <$> verbose <*> format

format :: Parser FormatAction
format = file <|> stdin
  where
    file, stdin :: Parser FormatAction
    file = FormatFile <$> strArgument (metavar "FILE" <> help "format input file, output result to stdout")
    stdin = pure FormatStdin

verbose :: Parser Bool
verbose = switch (long "verbose" <> short 'v' <> help "Verbose output")

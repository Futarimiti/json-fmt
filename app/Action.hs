module Action where

data Action = Action
  { verbose :: Bool  -- otherwise only "important" logs are printed (errors, warnings, custom logs)
  , format  :: FormatAction
  } deriving (Show, Eq)

data FormatAction = FormatStdin | FormatFile FilePath
  deriving (Show, Eq)

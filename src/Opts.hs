module Opts where

import System.Console.GetOpt

data MemoMode = MemoRead | MemoWrite | MemoServe | MemoNotifOmg

data Options = Options
    { optHelp :: Bool
    , optMode :: Maybe MemoMode
    , optPullHosts :: [String]
    , optShowIds :: Bool
    }

defOpts :: Options
defOpts = Options
    { optHelp = False
    , optMode = Nothing
    , optPullHosts = []
    , optShowIds = False
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option "h" ["help"] (NoArg (\ o -> o {optHelp = True})) ""
    , Option "r" ["read"] (NoArg (\ o -> o {optMode = Just MemoRead}))
      "Read messages as they appear."
    , Option "s" ["serve"] (NoArg (\ o -> o {optMode = Just MemoServe}))
      "Connect writes to reads."
    , Option "w" ["write"] (NoArg (\ o -> o {optMode = Just MemoWrite}))
      "Accept new messages on stdin."
    , Option "N" ["notifieromg"]
      (NoArg (\ o -> o {optMode = Just MemoNotifOmg}))
      "Connect to custom terminal-based notifier."
    , Option "p" ["pull-from-host"] 
      (ReqArg (\ s o -> o {optPullHosts = s : optPullHosts o}) "HOST")
      "Host to also pull memos from. (multiple -p possible)"
    , Option "i" ["show-ids"] (NoArg (\ o -> o {optShowIds = True}))
      "Only useful for memoplex communication between servers."
    ]


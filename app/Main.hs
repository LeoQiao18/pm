module Main where

import           System.Console.GetOpt                    ( ArgDescr
                                                            ( NoArg
                                                            , ReqArg
                                                            )
                                                          , ArgOrder
                                                            ( RequireOrder
                                                            )
                                                          , OptDescr(..)
                                                          , getOpt
                                                          , usageInfo
                                                          )
import           System.Environment                       ( getArgs
                                                          , getProgName
                                                          )
import           System.Exit                              ( exitSuccess )
import           System.IO

import           Chess                                    ( Player(..) )
import           Minimax                                  ( Depth )
import qualified Seq.Move                      as S


data PMStrategy
  = MinimaxSeq
  | MinimaxPar
  deriving (Read, Show, Eq)

data Mode
  = Interactive
  | SingleMove
  deriving (Read, Show, Eq)

data Options = Options
  { optPMStrategy :: PMStrategy
  , optDepth      :: Depth
  , optMode       :: Mode
  }
  deriving (Show, Eq)

defaultOptions :: Options
defaultOptions =
  Options { optPMStrategy = MinimaxSeq, optDepth = 5, optMode = Interactive }

usage :: IO Options
usage = do
  prg <- getProgName
  let header = "Usage: " ++ prg ++ " [option]... [file]"
  hPutStrLn stderr (usageInfo header options)
  exitSuccess

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option "m"
           ["mode"]
           (ReqArg (\mode opt -> return opt { optMode = read mode }) "<mode>")
           "Mode to run the engine"
  , Option
    "d"
    ["depth"]
    (ReqArg (\depth opt -> return opt { optDepth = read depth }) "<depth>")
    "Depth of minimax search"
  , Option
    "s"
    ["strategy"]
    (ReqArg (\pmStrat opt -> return opt { optPMStrategy = read pmStrat })
            "<strategy>"
    )
    "Strategy for minimax"
  , Option "h" ["help"] (NoArg (const usage)) "Print help"
  ]

main :: IO ()
main = do
  args <- getArgs
  let (actions, filenames, errors) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return defaultOptions) actions
  mapM_ putStrLn filenames
  print opts
  S.printSeq

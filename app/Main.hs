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
import           System.IO                                ( hFlush
                                                          , hPutStrLn
                                                          , print
                                                          , readFile
                                                          , stderr
                                                          , stdout
                                                          )

import           Chess                                    ( Board(..)
                                                          , Game(..)
                                                          , Player(..)
                                                          , defaultBoard
                                                          , defaultGame
                                                          , parseBoard
                                                          , prettyBoard
                                                          )
import           Control.Monad                            ( unless )
import           Data.Char                                ( isSpace )
import           Data.List.Split                          ( splitOn )
import           Data.Monoid                              ( Alt(getAlt) )
import           Minimax.Common                           ( Depth )
import           Minimax.Move                             ( PMStrategy(..)
                                                          , bestMove
                                                          )
import           Rules                                    ( isGameOver )


data Mode
  = Interactive
  | Test
  deriving (Read, Show, Eq)

data Options = Options
  { optPMStrategy :: PMStrategy
  , optMode       :: Mode
  , optPlayer     :: Player
  , optBoardSrc   :: String
  }
  deriving (Show, Eq)

defaultOptions :: Options
defaultOptions = Options { optPMStrategy = MinimaxSeq 5
                         , optMode       = Interactive
                         , optPlayer     = Black
                         , optBoardSrc   = ""
                         }

usage :: IO Options
usage = do
  prg <- getProgName
  let header = "Usage: " ++ prg ++ " [option]... [player] [file]"
  hPutStrLn stderr (usageInfo header options)
  exitSuccess

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option "m"
           ["mode"]
           (ReqArg (\mode opt -> return opt { optMode = read mode }) "<mode>")
           "Mode to run the engine"
  , Option
    "s"
    ["strategy"]
    (ReqArg
      (\pmStrat opt ->
        let splitStrat = splitOn "," pmStrat
            pmStrat'   = case head splitStrat of
              "MinimaxSeq" -> MinimaxSeq (read $ splitStrat !! 1)
              "MinimaxPar" ->
                MinimaxPar (read $ splitStrat !! 1) (read $ splitStrat !! 2)
              _ -> error "Invalid PMStrategy"
        in  return opt { optPMStrategy = pmStrat' }
      )
      "<strategy>"
    )
    "Strategy for minimax"
  , Option
    "p"
    ["player"]
    (ReqArg (\player opt -> return opt { optPlayer = read player }) "<player>")
    "Player that the engine is playing as"
  , Option
    "b"
    ["boardSrc"]
    (ReqArg (\boardSrc opt -> return opt { optBoardSrc = boardSrc })
            "<boardSrc>"
    )
    "File path specifying custom initial board layout"
  , Option "h" ["help"] (NoArg (const usage)) "Print help"
  ]

main :: IO ()
main = do
  args <- getArgs
  let (actions, filenames, errors) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return defaultOptions) actions
  mapM_ putStrLn filenames
  print opts
  startGame opts
 where
  startGame opts@Options { optBoardSrc = src, optPlayer = player } = do
    g <- initGame src player
    loop 1 g opts
  initGame src player
    | null src || all isSpace src = return Game { gamePlayer = player
                                                , gameBoard  = defaultBoard
                                                }
    | otherwise = do
      contents <- readFile src
      return Game { gamePlayer = player, gameBoard = parseBoard contents }
  loop turn g opts = do
    putStrLn
      $  "> Turn "
      ++ show turn
      ++ ", "
      ++ show (gamePlayer g)
      ++ "'s move:"
    putStrLn $ prettyBoard $ gameBoard g
    putStrLn ""
    unless ((turn >= 3 && optMode opts == Test) || isGameOver g) $ do
      let g' = bestMove (optPMStrategy opts) g
      loop (turn + 1) g' opts

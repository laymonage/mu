module Main (main) where

import System.IO
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.List (intercalate)
import Text.Megaparsec
import Mu.Parser
import Mu.Evaluator
import Mu.Util

run :: Aliases -> T.Text -> IO Aliases
run as source =
  case runParser program "repl" source of
    Left e -> do
      putStrLn $ errorBundlePretty e
      return as
    Right exprs -> do
      let (res, as') = runState (sequence $ map evaluate exprs) as
      putStrLn $ intercalate " ; " $ map (T.unpack . prettyAST) res
      return as'


runSilent :: Aliases -> T.Text -> Aliases
runSilent as source =
  case runParser program "repl" source of
    Left _ -> as
    Right exprs -> do
      let (_, as') = runState (sequence $ map evaluate exprs) as
      as'


repl :: Aliases -> IO ()
repl as = do
  putStr "> "
  hFlush stdout
  input <- getLine
  as' <- run as $ T.pack input
  repl as'


initialInputs :: [String]
initialInputs =
  [ "S := \\w.\\y.\\x.y(w y x)"
  , "0 := \\s.\\z.z"
  , "1 := S 0"
  , "2 := S 1"
  , "3 := S 2"
  , "4 := S 3"
  , "5 := S 4"
  , "6 := S 5"
  , "7 := S 6"
  , "8 := S 7"
  , "9 := S 8" ]

initialAliases :: [T.Text]
initialAliases = map T.pack initialInputs

initialEnvironment :: Foldable t => t T.Text -> Aliases
initialEnvironment = foldl runSilent M.empty


main :: IO ()
main = repl $ initialEnvironment initialAliases

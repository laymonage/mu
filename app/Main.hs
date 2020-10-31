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


-- | Evaluates an input silently (without printing the final result).
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


-- | Predefined inputs to be evaluated in the environment.
initialInputs :: [String]
initialInputs =
  [ "S := \\w.\\y.\\x.y(w y x)"
  , "+ := S"
  , "* := \\x.\\y.\\z.x(y z)"
  , "0 := \\s.\\z.z"
  , "1 := S 0"
  , "2 := S 1"
  , "3 := S 2"
  , "4 := S 3"
  , "5 := S 4"
  , "6 := S 5"
  , "7 := S 6"
  , "8 := S 7"
  , "9 := S 8"
  , "T := \\x.\\y.x"
  , "F := \\x.\\y.y"
  , "True := T"
  , "False := F"
  , "and := \\p.\\q.p q F"
  , "or := \\p.\\q.p T q"
  , "not := \\p.p F T"
  , "Z := \\x.x F not F"
  ]

-- | Evaluates a list of Text in the environment.
initialEnvironment :: Foldable t => t T.Text -> Aliases
initialEnvironment = foldl runSilent M.empty

initialMessage :: String
initialMessage = "Please evaluate one lambda expression before using "
  ++ "this program (e.g. \\x.x).\n"
  ++ "This is necessary in order to flush the traces of Church's numerals "
  ++ "initialization."

main :: IO ()
main = do
  putStrLn(initialMessage)
  repl $ initialEnvironment $ map T.pack initialInputs

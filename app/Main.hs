module Main (main) where

import System.IO
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Text.Megaparsec
import Mu.Parser
import Mu.Evaluator
import Mu.Util

-- | Prints the decimal digit representation of a lambda expression
-- | (VERY NAIVELY).
printNumber :: T.Text -> IO ()
printNumber text = do
  let numPlus = T.count "(y " text  -- number produced from + operations
  let numMult = T.count "z " text  -- number produced from * operations
  if numMult >= 1
    then do
      putStrLn (show numMult)
    else if or [numPlus >= 1, text == "λs.λz.z"]  -- 0 does not have y
    then do
      putStrLn (show numPlus)
    else putStr ""


run :: Aliases -> T.Text -> IO Aliases
run as source =
  case runParser program "repl" source of
    Left e -> do
      putStrLn $ errorBundlePretty e
      return as
    Right exprs -> do
      let (res, as') = runState (sequence $ map evaluate exprs) as
      let text = T.intercalate " ; " (map prettyAST res)
      putStrLn $ T.unpack text
      printNumber text
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


-- | Force the initialization to be evaluated and the traces to be flushed.
replEntry :: Aliases -> IO ()
replEntry as = do
  _ <- run as $ T.pack "Initialization finished"
  repl as


-- | Predefined inputs to be evaluated in the environment.
initialInputs :: [String]
initialInputs =
  [ "S     := λw.λy.λx.y(w y x)"
  , "+     := S"
  , "*     := λx.λy.λz.x(y z)"
  , "0     := λs.λz.z"
  , "1     := S 0"
  , "2     := S 1"
  , "3     := S 2"
  , "4     := S 3"
  , "5     := S 4"
  , "6     := S 5"
  , "7     := S 6"
  , "8     := S 7"
  , "9     := S 8"
  , "T     := λx.λy.x"
  , "F     := λx.λy.y"
  , "True  := T"
  , "False := F"
  , "not   := λp.p F T"
  , "and   := λp.λq.p q F"
  , "or    := λp.λq.p T q"
  , "Z     := λx.x F not F"
  ]

-- | Evaluates a list of Text in the environment.
initialEnvironment :: Foldable t => t T.Text -> Aliases
initialEnvironment = foldl runSilent M.empty

initialMessage :: String
initialMessage = "Initializing predefined aliases..."

main :: IO ()
main = do
  putStrLn(initialMessage)
  replEntry $ initialEnvironment $ map T.pack initialInputs

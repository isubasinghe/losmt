module Main where

import CLI
import qualified Data.Text.IO as TIO
import Options.Applicative (execParser, fullDesc, header, helper, info, progDesc, (<**>))
import qualified SAT.Parser as P
import qualified SAT.AST as A 
import qualified SAT.DPLL as D
import Text.Megaparsec (errorBundlePretty, runParser)

main :: IO ()
main = runLoSMT =<< execParser opts
  where
    opts =
      info
        (cliOptions <**> helper)
        ( fullDesc
            <> progDesc "LoSMT is a simple SMT solver which also exposes a SAT solver"
            <> header "SAT \\/ SMT solver"
        )

runSolver :: A.Expr -> Bool
runSolver = D.satisfiable

runLoSMT :: CLIOptions -> IO ()
runLoSMT c = do
  let f = file c
  inp <- TIO.readFile f
  let parseTree = runParser P.parseExpr f inp
  case parseTree of
    Right ast -> do
      print ast
      print $ runSolver ast
    Left err -> putStrLn $ errorBundlePretty err

module Main where

import CLI
import Options.Applicative (execParser, fullDesc, header, helper, info, progDesc, (<**>))

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

runLoSMT :: CLIOptions -> IO ()
runLoSMT c = print c

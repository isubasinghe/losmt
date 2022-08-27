module CLI where

-- import Data.Semigroup ((<>))
import Options.Applicative

data CLIOptions = CLIOptions {file :: String, mode :: Mode }
  deriving (Show, Eq)

data SATSolver = CDCL | DPLL
  deriving (Show, Eq)

data SATOptions = SATOptions
  { solver :: SATSolver
  }
  deriving (Show, Eq)

satOptions :: Parser SATOptions
satOptions =
  SATOptions
    <$> flag
      CDCL
      DPLL
      ( long "dpll"
          <> short 'd'
          <> help "Use the DPLL solver"
      )

data SMTOptions = SMTOptions {}
  deriving (Show, Eq)

smtOptions :: Parser SMTOptions
smtOptions = pure SMTOptions {}

data Mode
  = SAT SATOptions
  | SMT SMTOptions
  deriving (Show, Eq)


cliOptions :: Parser CLIOptions
cliOptions =
  CLIOptions
    <$> argument str (metavar "FILE")
    <*> (flag' () (long "sat") *> (SAT <$> satOptions) <|> flag' (SMT (SMTOptions {})) (long "smt"))

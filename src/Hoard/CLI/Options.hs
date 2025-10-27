module Hoard.CLI.Options
    ( Options (..)
    , parseOptions
    , optsParser
    )
where

import Data.Text qualified as T
import Options.Applicative qualified as Opt

import Hoard.Types.Environment (Environment, parseEnvironment)


-- | Command line options
data Options = Options
    { environment :: Maybe Environment
    }
    deriving stock (Eq, Show)


-- | Parse command line options
parseOptions :: Opt.Parser Options
parseOptions =
    Options
        <$> Opt.optional
            ( Opt.option
                readEnvironment
                ( Opt.long "env"
                    <> Opt.metavar "ENV"
                    <> Opt.help "Environment to run in (dev, staging, prod, ci)"
                )
            )
  where
    readEnvironment = Opt.eitherReader $ \s ->
        case parseEnvironment (T.pack s) of
            Just env -> Right env
            Nothing -> Left $ "Invalid environment: " <> s <> ". Must be one of: dev, staging, prod, ci"


-- | Main options parser with program description
optsParser :: Opt.ParserInfo Options
optsParser =
    Opt.info
        (parseOptions Opt.<**> Opt.helper)
        ( Opt.fullDesc
            <> Opt.progDesc "Run the Hoard Cardano node"
            <> Opt.header "hoard - A Cardano hoarding node"
        )

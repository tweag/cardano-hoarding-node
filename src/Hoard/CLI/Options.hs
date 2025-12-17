module Hoard.CLI.Options
    ( Options (..)
    , parseOptions
    , optsParser
    )
where

import Options.Applicative qualified as Opt

import Hoard.Types.Deployment (Deployment, parseDeployment)


-- | Command line options
data Options = Options
    { deployment :: Maybe Deployment
    }
    deriving stock (Eq, Show)


-- | Parse command line options
parseOptions :: Opt.Parser Options
parseOptions =
    Options
        <$> Opt.optional
            ( Opt.option
                readDeployment
                ( Opt.long "env"
                    <> Opt.metavar "ENV"
                    <> Opt.help "Deployment environment to run in (dev, staging, prod, ci)"
                )
            )
  where
    readDeployment = Opt.eitherReader $ \s ->
        case parseDeployment (toText s) of
            Just env -> Right env
            Nothing -> Left $ "Invalid deployment: " <> s <> ". Must be one of: dev, staging, prod, ci"


-- | Main options parser with program description
optsParser :: Opt.ParserInfo Options
optsParser =
    Opt.info
        (parseOptions Opt.<**> Opt.helper)
        ( Opt.fullDesc
            <> Opt.progDesc "Run the Hoard Cardano node"
            <> Opt.header "hoard - A Cardano hoarding node"
        )

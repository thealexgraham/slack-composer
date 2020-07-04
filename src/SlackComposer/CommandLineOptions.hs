
module SlackComposer.CommandLineOptions where

import           System.Console.GetOpt
import           System.Environment (getProgName)
import           System.Exit
import           System.IO

data CommandLineOptions = CommandLineOptions {
         clPort        :: Maybe String
        ,clAPIKey      :: Maybe String
        ,clDatabaseURI :: Maybe String
    } deriving (Show)

commandLineOptions
    :: [OptDescr (CommandLineOptions -> IO CommandLineOptions)]
commandLineOptions = [
     Option "p" ["port"]
    (ReqArg
        (\arg opt -> return opt {clPort = Just arg})
        "port")
        "port to run server on"
    ,Option "k" ["api-key"]
    (ReqArg
        (\arg opt -> return opt {clAPIKey = Just arg})
        "api-key")
        "Amper API key to connect with"
    ,Option "d" ["database"]
    (ReqArg
        (\arg opt -> return opt {clAPIKey = Just arg})
        "database")
        "Optional URI of Postgres database to connect to"
    ,Option "h" ["help"]
    (NoArg
        (\_ -> do
            prg <- getProgName
            hPutStrLn stderr (usageInfo prg commandLineOptions)
            exitSuccess))
        "Show help."]

defaultCommandLineOptions :: CommandLineOptions
defaultCommandLineOptions = CommandLineOptions Nothing Nothing Nothing

parseCommandLineOptions
    :: [String] -> IO CommandLineOptions
parseCommandLineOptions args = do
    let (actions,_,_) = getOpt RequireOrder commandLineOptions args
    foldl (>>=) (return defaultCommandLineOptions) actions

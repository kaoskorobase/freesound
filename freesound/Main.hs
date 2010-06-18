{-# LANGUAGE TemplateHaskell #-}

-- | Access the freesound database from the command line.
--
-- /TODO/:
--   * flesh out error handling
--

import qualified Data.ByteString.Lazy as BS
import           Control.Category ((.))
import           Control.Monad.Trans (liftIO)
import           Sound.Freesound
import qualified Sound.Freesound.Properties as Props
import           Sound.Freesound.Query
import           Sound.Freesound.Util
import           System.IO (hGetLine, hGetEcho, hSetEcho, stdin)
import           Text.XML.Light as XML
import           System.Console.GetOpt

import           Data.Record.Label

import           Prelude hiding ((.))

import           System.Exit
import           System.IO
import           System.Environment (getArgs)

data Options = Options
  { _optHelp            :: Bool
  , _optUser            :: Maybe String
  , _optPassword        :: Maybe String
  , _optSearch          :: SearchOptions
  , _optSearchSimilar   :: SearchSimilarOptions
  , _optProperties      :: PropertiesOptions
  , _optDownload        :: DownloadOptions
  } deriving (Eq, Show)

data SearchOptions = SearchOptions
  {
  } deriving (Eq, Show)

data SearchSimilarOptions = SearchSimilarOptions
  { _optSimilarity :: Similarity
  } deriving (Eq, Show)

data PropertiesOptions = PropertiesOptions
  { _optXML :: Bool
  } deriving (Eq, Show)

data DownloadOptions = DownloadOptions
  { _optOutputFile          :: Maybe FilePath
  , _optAddExtension        :: Bool
  , _optUseOriginalFileName :: Bool
  } deriving (Eq, Show)

$(mkLabels [
    ''Options
  , ''SearchSimilarOptions
  , ''PropertiesOptions
  , ''DownloadOptions
  ])

type Action = Freesound ()
type Result = Either String Action

data Command = Command {
    cmdOptions :: [OptDescr (Options -> Options)],
    cmdHelp :: String,
    cmdAction :: Options -> [String] -> Result
}

defaultOptions :: Options
defaultOptions = Options
  { _optHelp = False
  , _optUser = Nothing
  , _optPassword = Nothing
  , _optSearch = SearchOptions
        { }
  , _optSearchSimilar = SearchSimilarOptions
        { _optSimilarity = Similar }
  , _optProperties = PropertiesOptions
        { _optXML = False }
  , _optDownload = DownloadOptions
        { _optOutputFile = Nothing
        , _optAddExtension = False
        , _optUseOriginalFileName = False }
  }

globalOptions :: [OptDescr (Options -> Options)]
globalOptions =
    [ Option ['h'] ["help"]
        (NoArg (set optHelp True))
        "Print this help message."
    , Option ['u'] ["user"]
        (ReqArg (set optUser . Just) "STRING")
        "Freesound user name."
    , Option ['p'] ["password"]
        (ReqArg (set optPassword . Just) "STRING")
        "Freesound password."
    ]

searchOptions :: [OptDescr (Options -> Options)]
searchOptions = [ ]

searchSimilarOptions :: [OptDescr (Options -> Options)]
searchSimilarOptions =
    [ Option [] ["dissimilar"]
        (NoArg (set (optSimilarity.optSearchSimilar) Dissimilar))
        "Dissimilar"
    ]

propertiesOptions :: [OptDescr (Options -> Options)]
propertiesOptions =
    [ Option [] ["xml"]
        (NoArg (set (optXML.optProperties) True))
        "List xml"
    ]

downloadOptions :: [OptDescr (Options -> Options)]
downloadOptions =
    [ Option ['o'] ["output-file"]
        (ReqArg (set (optOutputFile.optDownload) . Just) "PATH")
        "Output file name (overrides --use-original-filename)"
    , Option ['e'] ["add-extension"]
        (NoArg (set (optAddExtension.optDownload) True))
        "Add original extension to output file"
    , Option ['O'] ["use-original-filename"]
        (NoArg (set (optUseOriginalFileName.optDownload) True))
        "Use original file name for output"
    ]

-- Utilities

readSample :: [String] -> Either String Sample
readSample []    = Left "Missing sample ID"
readSample (x:_) = case readMaybe x of
                    Nothing -> Left "Invalid sample ID"
                    Just i  -> Right (Sample i)

printSamples :: [Sample] -> Action
printSamples = liftIO . mapM_ (print . sampleId)

-- Commands

do_search :: Options -> [String] -> Result
do_search options args = Right $ do
     samples <- search (stringQuery $ unwords args)
     printSamples samples

do_searchSimilar :: Options -> [String] -> Result
do_searchSimilar options args =
    case readSample args of
        Left e  -> Left e
        Right s -> Right $ searchSimilar (get (optSimilarity.optSearchSimilar) options) s >>= printSamples

do_properties :: Options -> [String] -> Result
do_properties options args =
    case readSample args of
        Left e  -> Left e
        Right s -> if get (optXML.optProperties) options
                    then Right $ propertiesXML s >>= liftIO . putStrLn . XML.ppElement
                    else Right $ properties    s >>= liftIO . print

do_download :: Options -> [String] -> Result
do_download options args =
    case readSample args of
        Left e  -> Left e
        Right s -> Right $ do
            path <- if get (optUseOriginalFileName.optDownload) options
                    then properties s >>= return . Just . Props.originalFileName
                    else case get (optOutputFile.optDownload) options of
                        Nothing -> return Nothing
                        Just p -> if get (optAddExtension.optDownload) options
                                  then properties s >>= return . Just . (\props -> p ++ "." ++ Props.extension props)
                                  else return (Just p)
            download s >>= maybe (liftIO . BS.putStrLn) (\p -> liftIO . BS.writeFile p) path

commands :: [(String, Command)]
commands = [
    ("search",     Command searchOptions        "freesound search [OPTION...] PREDICATES.." do_search),
    ("similar",    Command searchSimilarOptions "freesound similar [OPTION...] SAMPLE"      do_searchSimilar),
    ("properties", Command propertiesOptions    "freesound properties [OPTION...] SAMPLE"   do_properties),
    ("download",   Command downloadOptions      "freesound download [OPTION...] SAMPLE"     do_download)
    ]

printHelp :: ExitCode -> String -> [OptDescr (Options -> Options)] -> IO a
printHelp code header options = do
    hPutStr stderr (usageInfo header options)
    exitWith code

parseOptions :: String -> [OptDescr (Options -> Options)] -> [String] -> IO (Options, [String])
parseOptions header options argv = 
    case getOpt Permute options argv of
        (o, n, []) -> let o' = foldl (flip ($)) defaultOptions o in
                        if get optHelp o'
                            then printHelp ExitSuccess header options
                            else return (o', n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo header options))

globalHelp :: String
globalHelp = "Usage: freesound COMMAND [OPTION...] [ARG...]\
              \\n\n\
              \Try freesound COMMAND --help for individual command options.\
              \\n\n\
              \Commands:\n\n\
              \  search\n\
              \  similar\n\
              \  properties\n\
              \  downloads\n\n\
              \Global options:\n"

-- | Read username from stdin.
getUser :: IO String
getUser = putStrLn "User:" >> hGetLine stdin

-- | Read password from stdin.
getPassword :: IO String
getPassword = do
    putStrLn "Password:"
    echo <- hGetEcho stdin
    hSetEcho stdin False
    password <- hGetLine stdin
    hSetEcho stdin echo
    return password

-- | Read credentials that are still unknown from stdin.
getCredentials :: Maybe String -> Maybe String -> IO (String, String)
getCredentials user password = do
    user'     <- maybe getUser return user
    password' <- maybe getPassword return password
    return (user', password')

-- | Perform a command with the given user name and password.
doCommand :: String -> String -> Action -> IO ()
doCommand user password action = do
    res <- withFreesound user password action
    case res of
        Left e  -> putStrLn ("ERROR: " ++ errorString e)
        Right r -> return ()

main :: IO ()
main = do
    argv <- getArgs
    case argv of
        [] -> printHelp (ExitFailure 1) globalHelp globalOptions
        (cmdName:rest) ->
            case lookup cmdName commands of
                Nothing ->
                    printHelp (ExitFailure 1) globalHelp globalOptions
                Just cmd -> do
                    (options, args) <- parseOptions (cmdHelp cmd) (globalOptions ++ cmdOptions cmd) rest
                    case cmdAction cmd options args of
                        Left e  -> putStrLn ("ERROR: " ++ e)
                        Right a -> do
                            (user, password) <- getCredentials (get optUser options) (get optPassword options)
                            doCommand user password a

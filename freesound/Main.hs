{-# LANGUAGE TemplateHaskell #-}

-- | Access the freesound database from the command line.
--
-- /TODO/:
--   * flesh out error handling
--

import qualified Data.ByteString.Lazy as BS
import           Control.Monad.Trans (liftIO)
import           Sound.Freesound
import qualified Sound.Freesound.Properties as Props
import           Sound.Freesound.Query
import           Sound.Freesound.Util
import           System.IO (hGetLine, hGetEcho, hSetEcho, stdin)
import           Text.XML.Light as XML
import           System.Console.GetOpt

import           Data.Accessor.Template
import           Data.Accessor

import           System.Exit
import           System.IO
import           System.Environment (getArgs)

data Options = Options
  { optHelp_            :: Bool
  , optUser_            :: Maybe String
  , optPassword_        :: Maybe String
  , optSearch_          :: SearchOptions
  , optSearchSimilar_   :: SearchSimilarOptions
  , optProperties_      :: PropertiesOptions
  , optDownload_        :: DownloadOptions
  } deriving (Eq, Show)

data SearchOptions = SearchOptions
  {
  } deriving (Eq, Show)

data SearchSimilarOptions = SearchSimilarOptions
  { optSimilarity_ :: Similarity
  } deriving (Eq, Show)

data PropertiesOptions = PropertiesOptions
  { optXML_ :: Bool
  } deriving (Eq, Show)

data DownloadOptions = DownloadOptions
  { optOutputFile_          :: Maybe FilePath
  , optAddExtension_        :: Bool
  , optUseOriginalFileName_ :: Bool
  } deriving (Eq, Show)

$( deriveAccessors ''Options )
-- $( deriveAccessors ''SearchOptions )
$( deriveAccessors ''SearchSimilarOptions )
$( deriveAccessors ''PropertiesOptions )
$( deriveAccessors ''DownloadOptions )

type Action = Freesound ()
type Result = Either String Action

data Command = Command {
    cmdOptions :: [OptDescr (Options -> Options)],
    cmdHelp :: String,
    cmdAction :: Options -> [String] -> Result
}

defaultOptions :: Options
defaultOptions = Options
  { optHelp_     = False
  , optUser_     = Nothing
  , optPassword_ = Nothing
  , optSearch_ = SearchOptions { }
  , optSearchSimilar_ = SearchSimilarOptions {
    optSimilarity_   = Similar }
  , optProperties_ = PropertiesOptions {
    optXML_      = False }
  , optDownload_ = DownloadOptions
        { optOutputFile_ = Nothing
        , optAddExtension_ = False
        , optUseOriginalFileName_ = False }
  }

globalOptions :: [OptDescr (Options -> Options)]
globalOptions =
    [ Option ['h'] ["help"]
        (NoArg (optHelp ^= True))
        "Print this help message."
    , Option ['u'] ["user"]
        (ReqArg (\x -> optUser ^= Just x) "STRING")
        "Freesound user name."
    , Option ['p'] ["password"]
        (ReqArg (\x -> optPassword ^= Just x) "STRING")
        "Freesound password."
    ]

searchOptions :: [OptDescr (Options -> Options)]
searchOptions = [ ]

searchSimilarOptions :: [OptDescr (Options -> Options)]
searchSimilarOptions =
    [ Option [] ["dissimilar"]
        (NoArg (optSearchSimilar ^: optSimilarity ^= Dissimilar))
        "Dissimilar"
    ]

propertiesOptions :: [OptDescr (Options -> Options)]
propertiesOptions =
    [ Option [] ["xml"]
        (NoArg (optProperties ^: optXML ^= True))
        "List xml"
    ]

downloadOptions :: [OptDescr (Options -> Options)]
downloadOptions =
    [ Option ['o'] ["output-file"]
        (ReqArg (\x -> optDownload ^: optOutputFile ^= Just x) "PATH")
        "Output file name (overrides --use-original-filename)"
    , Option ['e'] ["add-extension"]
        (NoArg (optDownload ^: optAddExtension ^= True))
        "Add original extension to output file"
    , Option ['O'] ["use-original-filename"]
        (NoArg (optDownload ^: optUseOriginalFileName ^= True))
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
        Right s -> Right $ searchSimilar (options^.optSearchSimilar^.optSimilarity) s >>= printSamples

do_properties :: Options -> [String] -> Result
do_properties options args =
    case readSample args of
        Left e  -> Left e
        Right s -> if options^.optProperties^.optXML
                    then Right $ propertiesXML s >>= liftIO . putStrLn . XML.ppElement
                    else Right $ properties    s >>= liftIO . print

do_download :: Options -> [String] -> Result
do_download options args =
    case readSample args of
        Left e  -> Left e
        Right s -> Right $ do
            path <- if options^.optDownload^.optUseOriginalFileName
                    then properties s >>= return . Just . Props.originalFileName
                    else case options^.optDownload^.optOutputFile of
                        Nothing -> return Nothing
                        Just p -> if options^.optDownload^.optAddExtension
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
                        if o'^.optHelp
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
                            (user, password) <- getCredentials (options^.optUser) (options^.optPassword)
                            doCommand user password a

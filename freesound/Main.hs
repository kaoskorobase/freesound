import Control.Monad.Trans  (liftIO)
import Sound.Freesound
import Sound.Freesound.Query
import System.IO            (hGetLine, hGetEcho, hSetEcho, stdin)
import Text.XML.Light       as XML

readCredentials :: IO (String, String)
readCredentials = do
    putStrLn "User:"
    user <- hGetLine stdin
    putStrLn "Password:"
    echo <- hGetEcho stdin
    hSetEcho stdin False
    password <- hGetLine stdin
    hSetEcho stdin echo
    return (user, password)

readQuery :: IO Query
readQuery = do
    putStrLn "Query:"
    stringQuery `fmap` getLine

main :: IO ()
main = do
    (user, password) <- readCredentials
    query <- readQuery
    res   <- withFreesound user password $ do
        samples <- search query
        liftIO $ print samples
        propertiesXML (head samples)
        -- download handle (head samples)
    case res of
        Left e  -> putStrLn (errorString e)
        Right r -> putStrLn (XML.ppElement r)
        -- Right r -> putStrLn (show r)

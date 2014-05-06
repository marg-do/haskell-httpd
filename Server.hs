import System.IO
import Data.List
import Data.List.Split
import Network
import Control.Exception
import Control.Concurrent

type Log = String
type Request = String
type RequestLine = String

type Env = (String, String)

--------------------------------------------------
-- Configuration
portNumber :: PortNumber
portNumber = 8080

rootDir :: FilePath
rootDir = "/home/marg_do/work/haskell/Server/www"

-- Logger
logStartServer :: PortNumber -> Log
logStartServer port = "server has started, listening on: " ++ show port

-- Request Parser(Old)
parseToPath :: RequestLine -> String
parseToPath reql = splitOn " " reql !! 1

reqFilePath :: RequestLine -> FilePath
reqFilePath reql = rootDir ++ parseToPath reql

isRequestGet :: RequestLine -> Bool
isRequestGet reql = if method == "GET" then True
                    else False
  where method = splitOn " " reql !! 0

-- Request Parser
reqToReql :: Request -> [RequestLine]
reqToReql = lines

reqlToEnv :: RequestLine -> Env
reqlToEnv reql | any (== ':') reql = (splited_cln !! 0, splited_cln !! 1)
               | otherwise = (splited_space !! 0, splited_space !! 1)
  where splited_cln = splitOn ": " reql
        splited_space = splitOn " " reql

parseRequest :: Request -> [Env]        
parseRequest req = map reqlToEnv $ reqToReql req

--------------------------------------------------

onError :: String -> SomeException ->  IO String
onError mes e = do
  putStrLn mes
  return mes

hPutStaticFile :: Handle -> FilePath -> IO ()
hPutStaticFile clientHandle filepath = do
  contents <- catch (readFile filepath) (onError ("404 Error" ++ (show filepath)))
  hPutStrLn clientHandle contents

putLogLn :: Log -> IO ()
putLogLn = putStrLn

main :: IO ()
main = do
  withSocketsDo $ do
    serverSocket <- listenOn (PortNumber portNumber)
    putLogLn $ logStartServer portNumber
    acceptLoop serverSocket `finally` sClose serverSocket

acceptLoop socket = do
  (clientHandle, clientHost, clientPort) <- accept socket
  forkOS $ echoLoop clientHandle
  acceptLoop socket

echoLoop handle = do
  sequence_ (repeat (do {
                        request <- hGetLine handle;
                        if isRequestGet request then hPutStaticFile handle (reqFilePath $ (lines request !! 0)) else nilIO;
                            hFlush handle
                        }))
    `catch` (\(SomeException e) -> return ())
    `finally` hClose handle

nilIO :: IO ()
nilIO = do
  return ()

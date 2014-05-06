import System.IO
import System.IO.Error
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

-- Development
header :: String
header = "Content-type: text/html\n\n"

-- General
assoc :: Eq a => a -> [(a, b)] -> Maybe (a, b)
assoc itm [] = Nothing
assoc itm lst = if (fst tpl) == itm then Just tpl
                else assoc itm $ tail lst
  where tpl = head lst

fromJust :: Maybe a -> a
fromJust (Just x) = x

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

main :: IO ()
main = do
  withSocketsDo $ do
    serverSocket <- listenOn (PortNumber portNumber)
    putLogLn $ logStartServer portNumber
    acceptLoop serverSocket `finally` sClose serverSocket

putLogLn :: Log -> IO ()
putLogLn = putStrLn

acceptLoop socket = do
  (clientHandle, clientHost, clientPort) <- accept socket
  forkOS $ httpHandler clientHandle
  acceptLoop socket

hGetRequest handle acc = do
  l <- hGetLine handle
  if l == "\n" then return acc
    else hGetRequest handle $ acc ++ l
  `catch` (\e -> if isEOFError e
                   then return acc
                   else ioError e)

httpHandler handle = do
  sequence_ $ repeat $ do {
    request <- hGetLine handle;
--    request <- hGetRequest handle "";
    putLogLn "Access Accepted";
    hPutStrLn handle header;
    hPutStrLn handle request;
    putLogLn (if request == "\n" then "CR" else request);
    hFlush handle;
    putLogLn "Access End";
    hClose handle
                          }
    `catch` (\(SomeException e) -> return ())
    `finally` hClose handle

-- Old
echoLoop handle = do
  sequence_ (repeat (do {
                        request <- hGetLine handle;
                        if isRequestGet request then hPutStaticFile handle (reqFilePath $ (lines request !! 0)) else nilIO;
                            hFlush handle
                        }))
    `catch` (\(SomeException e) -> return ())
    `finally` hClose handle
    
onError :: String -> SomeException ->  IO String
onError mes e = do
  putStrLn mes
  return mes

hPutStaticFile :: Handle -> FilePath -> IO ()
hPutStaticFile clientHandle filepath = do
  contents <- catch (readFile filepath) (onError ("404 Error" ++ (show filepath)))
  hPutStrLn clientHandle contents


nilIO :: IO ()
nilIO = do
  return ()

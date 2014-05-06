import System.IO
import Data.List.Split
import Network
import Control.Exception
import Control.Concurrent

type Log = String
type RequestLine = String

portNumber :: PortNumber
portNumber = 8080

rootDir :: FilePath
rootDir = "/home/marg_do/work/haskell/Server/www"

logStartServer :: PortNumber -> Log
logStartServer port = "server has started, listening on: " ++ show port

parseToPath :: RequestLine -> String
parseToPath reql = splitOn " " reql !! 1

reqFilePath :: RequestLine -> FilePath
reqFilePath reql = rootDir ++ parseToPath reql




hPutStaticFile :: Handle -> FilePath -> IO ()
hPutStaticFile clientHandle filepath = do
  handle <- openFile filepath ReadMode
  contents <- hGetContents handle
  hPutStrLn clientHandle contents
  hClose handle

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
                        hPutStaticFile handle (reqFilePath $ (lines request !! 0));
                            hFlush handle
                        }))
    `catch` (\(SomeException e) -> return ())
    `finally` hClose handle

module Libaddutil.STMServer (startServer, broadcast, Client)
where 

import Prelude hiding (catch)
import Network (listenOn, accept, sClose, Socket, withSocketsDo, PortID(..)) 
import System.IO 
import Control.Exception (finally, catch)
import Control.Concurrent 
import Control.Concurrent.STM 
import Control.Monad.State.Lazy

import Libaddutil.ListUtils

-- TODO: find a way to change these from IO monad to m
type ClientCallback = (Handle -> IO ())
type DataCallback = (Handle -> String -> IO ())

startServer :: Int -> ClientCallback -> DataCallback -> IO (TVar [Client])
startServer port a b = withSocketsDo $ do 
         servSock <- listenOn $ PortNumber (fromIntegral port)
         putStrLn $ "listening on: " ++ show port
         cl <- atomically $ newTVar []
         forkIO ((start servSock a b cl) `finally` sClose servSock)
         putStrLn "Server is running"
         return cl

start :: Socket -> ClientCallback -> DataCallback -> TVar [Client] -> IO ()
start servSock a b c = do
  acceptChan <- atomically newTChan
  forkIO $ acceptLoop servSock acceptChan
  mainLoop servSock acceptChan a b c
  putStrLn "Killed server thread"

type Client = (TChan String, Handle) 

acceptLoop :: Socket -> TChan Client -> IO () 
acceptLoop servSock chan = do 
  putStrLn "Accepting connections"
  (cHandle, _, _) <- accept servSock
  cChan <- atomically newTChan
  putStrLn "New connection accepted"
  forkIO $ clientLoop cHandle cChan    -- discard thread ID
  atomically $ writeTChan chan (cChan, cHandle)
  acceptLoop servSock chan 

clientLoop :: Handle -> TChan String -> IO () 
clientLoop handle chan = 
    listenLoop (hGetLine handle) chan 
                   `catch` (\e -> putStrLn ("Error during listen: " ++ show e)) 
                   `finally` hClose handle 

listenLoop :: IO a -> TChan a -> IO () 
listenLoop act chan = 
    sequence_ (repeat (act >>= atomically . writeTChan chan)) 

mainLoop :: Socket -> TChan Client -> ClientCallback -> DataCallback -> TVar [Client] -> IO ()
mainLoop servSock acceptChan newClient newData ctvar = do 
  clientlist <- atomically $ readTVar ctvar
  r <- atomically $ (Left `fmap` readTChan acceptChan) 
       `orElse` 
       (Right `fmap` tselect clientlist) 
  case r of 
    Left (ch,h) -> do 
           putStrLn "New client"
           (newClient h) `catch` (\e -> putStrLn ("Error while initializing connection: " ++ show e) >> mainLoop servSock acceptChan newClient newData ctvar)
           atomically $ writeTVar ctvar ((ch,h):clientlist)
           mainLoop servSock acceptChan newClient newData ctvar
    Right (line,h) -> do 
--           putStrLn $ "Data: " ++ line 
           newData h line
           mainLoop servSock acceptChan newClient newData ctvar

broadcast :: TVar [Client]          -- ^ client pool
          -> String                 -- ^ line to send to the clients
          -> IO [Handle]            -- ^ list of lost client handles
broadcast cvar line = do
    clientlist <- atomically $ readTVar cvar
    clients' <- mapM (broadcast' line) clientlist
--    let dropped = length $ filter null clients'
--    when (dropped > 0) $
--        putStrLn ("Clients lost: " ++ show dropped)
    atomically $ writeTVar cvar (catLefts clients')
    return (catRights clients')

broadcast' :: String -> Client -> IO (Either Client Handle)
broadcast' l (ch,h) = do
    (hPutStrLn h l >> return (Left (ch,h))) `catch` (\e -> putStrLn ("Lost a client: " ++ show e) >> hClose h >> return (Right h))

tselect :: [(TChan a, t)] -> STM (a, t) 
tselect = foldl orElse retry 
          . map (\(ch, ty) -> (flip (,) ty) `fmap` readTChan ch) 

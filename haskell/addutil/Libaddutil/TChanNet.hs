module Libaddutil.STMNet (startServer)
where 

import Prelude hiding (catch)
import Network (listenOn, accept, sClose, Socket, withSocketsDo, PortID(..)) 
import System.IO 
import System.Environment (getArgs)
import Control.Exception (finally, catch)
import Control.Concurrent 
import Control.Concurrent.STM 
import Control.Monad (forM, filterM, liftM, when) 
import Libaddutil.Net

type ConnectionCallback = (Net ())
type DataCallback       = (String -> Net ())
type IntervalCallback   = (String)

startServer :: Int -> Int -> IntervalCallback -> ConnectionCallback -> DataCallback -> IO ()
startServer p i a b c = withSocketsDo $ do 
         [portStr] <- getArgs 
         let port = fromIntegral (p)
         servSock <- listenOn $ PortNumber port 
         putStrLn $ "Listening on: " ++ show port 
         start servSock a b c `finally` sClose servSock

start :: Network.Socket -> ConnectionCallback -> DataCallback -> IO ()
start servSock a b = do 
  acceptChan <- atomically newTChan 
  forkIO $ acceptLoop servSock acceptChan 
  serverLoop servSock acceptChan [] a b

type Client = (TChan String, Handle) 

acceptLoop :: Socket -> TChan Client -> IO () 
acceptLoop servSock chan = do 
  (cHandle, host, port) <- accept servSock 
  cChan <- atomically newTChan 
  cTID <- forkIO $ clientLoop cHandle cChan 
  atomically $ writeTChan chan (cChan, cHandle) 
  acceptLoop servSock chan 

clientLoop :: Handle -> TChan String -> IO () 
clientLoop handle chan = 
    listenLoop (hGetLine handle) chan 
                   `catch` (const $ return ()) 
                   `finally` hClose handle 

listenLoop :: IO a -> TChan a -> IO () 
listenLoop act chan = 
    sequence_ (repeat (act >>= atomically . writeTChan chan)) 

serverLoop :: Socket -> TChan Client -> [Client] -> ConnectionCallback -> DataCallback -> IO () 
serverLoop servSock acceptChan clients newConnection newData = do 
  r <- atomically $ (Left `fmap` readTChan acceptChan) 
       `orElse` 
       (Right `fmap` tselect clients) 
  case r of 
    Left (ch,h) -> do 
           newConnection h
           putStrLn "new client" 
           serverLoop servSock acceptChan ((ch,h):clients) newConnection newData
    Right (line,_) -> do 
           putStrLn $ "data: " ++ line 
           let broadcast = newData line
           clients' <- forM clients $ 
                      \(ch,h) -> do 
                        when (length broadcast > 0) $ do
                             hPutStrLn h broadcast
                             hFlush h 
                        return [(ch,h)] 
                        `catch` const (hClose h >> return []) 
           let dropped = length $ filter null clients' 
           when (dropped > 0) $ 
                putStrLn ("clients lost: " ++ show dropped) 
           serverLoop servSock acceptChan (concat clients') newConnection newData

tselect :: [(TChan a, t)] -> STM (a, t) 
tselect = foldl orElse retry 
          . map (\(ch, ty) -> (flip (,) ty) `fmap` readTChan ch) 

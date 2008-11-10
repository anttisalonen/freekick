module Libaddutil.Net (connect, listen, send, 
                       disconnect, serve, 
                       receive)
where

import Text.Printf
import System.IO
import Network
import Prelude hiding (catch)
import Control.Exception
import Control.Concurrent

connect :: String -> Int -> (Handle -> IO ()) -> IO ()
connect s p f = bracket (connect' s p) (disconnect "Disconnected.") (\h -> catch (f h) connectionError)

serve :: (Integral a) => a -> (Handle -> IO ()) -> IO c
serve p f = bracket (listenOn (PortNumber (fromIntegral p)))
                    (sClose)
                    (loop f)
    where loop g st          = accept st >>= handle' g >> loop g st
          handle' g (h, _, _) = forkIO $ catch (g h) connectionError

connectionError :: Exception -> IO ()
connectionError e = hPutStrLn stderr ("Error during connection: " ++ show e)

disconnect :: String -> Handle -> IO ()
disconnect s h = do
    hClose h
    putStrLn s

connect' :: String -> Int -> IO Handle
connect' s p = notify ("Connecting to " ++ s ++ "...\n") $ do
            h <- connectTo s (PortNumber (fromIntegral p))
            putStrLn "Connected!"
            hSetBuffering h LineBuffering
            return h

notify :: String -> IO c -> IO c
notify s a = bracket_
             (printf s >> hFlush stdout)
             (putStrLn "Finished.")
             a

{-
getClient :: (Integral a) => a -> IO Connection
getClient p = notify ("Listening to " ++ (show p) ++ "...\n") $ do
                s <- listenOn (PortNumber (fromIntegral p))
                (h, n, pp) <- accept s
                putStrLn "Connection accepted!"
                return (Connection h)
-}

listen :: Handle -> (String -> Handle -> IO ()) -> IO ()
listen h f = forever' $ do
               s <- hGetLine h
--               putStrLn s
               f s h
    where forever' a = a >> forever' a

send :: String -> Handle -> IO ()
send s h = do
    hPutStrLn h s

receive :: Handle -> IO String
receive h = do
    s <- hGetLine h
    return s

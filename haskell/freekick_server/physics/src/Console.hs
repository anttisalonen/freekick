module Console
where

import Control.Monad.State
import Control.Concurrent.STM
import Char
import Control.Exception
import Prelude hiding (catch)

import Libaddutil.STMServer

import Freekick.Libmatch.MatchStatus

data ConsoleStatus = ConsoleStatus { status :: Bool }

startConsoleStatus :: ConsoleStatus
startConsoleStatus = ConsoleStatus False

doInterpreter :: TVar [Client] -> TVar MatchStatus -> ConsoleStatus -> IO ()
doInterpreter c mb s = do
    n <- getLine
    cl <- atomically $ readTVar c
    m <- atomically $ readTVar mb
    when (length n > 0) $ do
        case (map Char.toLower (head (words n))) of
           "clients" -> putStrLn ("Number of clients connected: " ++ show (length cl))
           "help"    -> putStrLn consoleHelp
           "status"  -> when (length (tail (words n)) > 0) $ do
                            case (map Char.toLower (head (tail (words n)))) of
                              "on"  -> doInterpreter c mb s{status = True}
                              _     -> doInterpreter c mb s{status = False}
           "ball"    -> if (length (tail (words n)) > 0) then do
                            nbp <- evaluate (read (unwords (tail (words n)))) `catch` \e -> putStrLn ("Invalid parameters: " ++ (show (e :: IOException))) >> return (ballplay m)
                            let newm = m{ballplay=nbp}
                            atomically $ writeTVar mb newm
                        else do
                            putStrLn (show (ballplay m))
           _         -> putStrLn consoleHelp
    doInterpreter c mb s

consoleHelp :: String
consoleHelp = "List of commands:\nclients\nhelp\nstatus on|off"


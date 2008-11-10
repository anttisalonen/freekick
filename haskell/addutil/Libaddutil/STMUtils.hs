module Libaddutil.STMUtils
where

import Control.Concurrent.STM

getAllFromTChan :: TChan a -> IO [a]
getAllFromTChan c = do
    x <- atomically $ isEmptyTChan c
    if x
        then return $ []
        else do
            this <- atomically $ readTChan c
            rest <- getAllFromTChan c
            return $ (this : rest)


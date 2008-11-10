module Main
where

import System.IO
import Prelude hiding (catch)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Map
import Data.Maybe
import System.Exit
import Data.Word

import Graphics.UI.SDL as SDL

import Libaddutil.Net
import Libaddutil.Entity
import Libaddutil.Vector
import Libaddutil.Misc

import Freekick.Libsoccer.MatchInfo
import Freekick.Libsoccer.Club
import Freekick.Libsoccer.Pitch
--import Freekick.Libsoccer.Player

import Freekick.Libmatch.MatchStatus
import Freekick.Libmatch.PlayerInfo
import Freekick.Libmatch.Control
--import Freekick.Libmatch.Rules

server :: String
server = "127.0.0.1"

port :: Int
port   = 32105

pixtometer :: Float 
pixtometer = 8

heightmeter :: Float
heightmeter = 0.4

extrapixel :: Float
extrapixel = 20

kickCoefficient :: Float
kickCoefficient = 3.0

main :: IO ()
main = connect server port start

type Colors = Data.Map.Map String Pixel

data ControlState = ControlState { kickHeight :: Float
                                 , kickMode   :: ControlMode
                                 , lockpos    :: (Word16, Word16)
                                 }
                    deriving (Eq, Show, Read)

data ControlMode = Running
                 | Kicking
                 | Kicked
                   deriving (Eq, Show, Read)

start :: Handle -> IO ()
start h = do
    t <- hGetLine h
    let c :: MatchInfo
        c = read t
    let m = createMatchStatusFromMatchInfo c
    let (pwidth, _, pheight) = (area (pitch m))
    let height = pwidth * pixtometer + (extrapixel * 2)
    let width = pheight * pixtometer + (extrapixel * 2)
    matchBox <- atomically $ newTVar m
    putStrLn $ Freekick.Libsoccer.Club.name $ homecl c
    putStrLn $ Freekick.Libsoccer.Club.name $ awaycl c
    let pid = fst $ Data.Map.findMin (homeplayers m)
    let str = "\nOK [" ++ (show pid) ++ "]"
    putStrLn str
    send str h
    forkIO $ startSDL h pid matchBox (fromEnum width) (fromEnum height)
    listen h (eventHandler matchBox)

getPalette :: Surface -> IO Colors
getPalette screen = do
    let names = ["red", "green", "blue", "yellow", "white", "black"]
    let format = surfaceGetPixelFormat screen
    red    <- mapRGB format 0xFF 0    0
    green  <- mapRGB format 0    0x80 0
    blue   <- mapRGB format 0    0    0xFF
    yellow <- mapRGB format 0xFF 0xFF 0         
    white  <- mapRGB format 0xFF 0xFF 0xFF
    black  <- mapRGB format 0    0    0
    let pixels = [red, green, blue, yellow, white, black]
    return $ Data.Map.fromList (zip names pixels)

startSDL :: Handle -> Integer -> TVar MatchStatus -> Int -> Int -> IO ()
startSDL h pid matchBox width height = withInit [InitVideo, InitEventthread] $
    do setVideoMode width height 16 [SWSurface]
       setCaption "Freekick SDL client" ""
       enableUnicode True
       screen <- getVideoSurface
       cols <- getPalette screen
       kh <- atomically $ newTVar (ControlState 0.5 Running (0, 0))
       sdltid <- myThreadId
       disptid <- forkIO $ display h cols pid matchBox kh
       loop [sdltid,disptid] kh

col :: Colors -> String -> Pixel
col c n = Data.Map.findWithDefault (Pixel 0x00) n c

loop :: [ThreadId] -> TVar ControlState -> IO ()
loop ts nb = do
    event <- waitEvent
    ocs <- atomically $ readTVar nb
    let ncs = case event of
--                Quit                  -> exitWith ExitSuccess
--                MouseMotion a b c d   -> putStrLn ((show a) ++ " " ++ (show b) ++ (show c) ++ (show d))
                MouseButtonDown _ _ ButtonWheelUp   -> ocs{kickHeight=clamp 0 1 ((kickHeight ocs)+ 0.1  )}
                MouseButtonDown _ _ ButtonWheelDown -> ocs{kickHeight=clamp 0 1 ((kickHeight ocs)+(-0.1))}
                MouseButtonDown x y ButtonLeft      -> ocs{kickMode=Kicking,lockpos=(x,y)}
                MouseButtonUp   _ _ ButtonLeft      -> ocs{kickMode=Kicked}
                _                     -> ocs
    when (event == Quit) $ cleanExit ts
    atomically $ writeTVar nb ncs
--    putStrLn $ show ncs
    loop ts nb

cleanExit :: [ThreadId] -> IO a
cleanExit ts = mapM_ killThread ts >> exitWith ExitSuccess

ptToPi :: Integral a => a -> Float
ptToPi n = ((fromIntegral n) - extrapixel) / pixtometer

display :: Handle -> Colors -> Integer -> TVar MatchStatus -> TVar ControlState -> IO ()
display h c pid matchBox kickBox = do
         screen <- getVideoSurface

         m <- atomically $ readTVar matchBox
         cs <- atomically $ readTVar kickBox
         let kh = kickHeight cs
         let km = kickMode cs
         (mxposs, myposs, _) <- getMouseState 
         let (tx,ty) = (ptToPi mxposs, ptToPi myposs)
         let (lx,ly) = if km /= Running then (ptToPi (fst (lockpos cs)), ptToPi (snd (lockpos cs))) else (tx,ty)
         let pl = getPlayerInfoWithID m pid
         when (not (isNothing pl)) $ do
             let ploc = (location . plentity) (fromJust pl)
             let (xdifr,_,zdifr) = (ly,0,lx) `diffVector3` ploc
             let str = runToEvent (xdifr,0,zdifr) pid
             hPutStrLn h str
             when (km == Kicked && fst (lockpos cs) > 0 && snd (lockpos cs) > 0) $ do
                 let (xdifk,_,zdifk) = (ty,0,tx) `diffVector3` ploc
                 let khp = kh * (vectorLength (xdifk,0,zdifk))
                 let kvec = (xdifk, khp, zdifk) `scaleVector3` kickCoefficient
                 hPutStrLn h (kickToEvent kvec pid)
                 putStrLn ("Kicked to " ++ (show xdifk) ++ " " ++ (show khp) ++ " " ++ (show zdifk))
                 atomically $ writeTVar kickBox (cs{kickMode=Running})

         drawPitch screen (pitch m) (col c "green") (col c "white")
         let hples = map plentity (Data.Map.elems (homeplayers m))
         let aples = map plentity (Data.Map.elems (awayplayers m))
         let ble   = ballentity (ball m)
         drawEntity screen (col c "white") (col c "black") ble
         mapM_ (drawEntity screen (col c "red")  (col c "black")) hples
         mapM_ (drawEntity screen (col c "blue") (col c "black")) aples
         drawKickBar screen (col c "black") kh

         SDL.flip screen
         threadDelay 20000
         display h c pid matchBox kickBox

drawKickBar :: Surface -> Pixel -> Float -> IO Bool
drawKickBar screen c v = do
    let y = 400
    let h = fromEnum $ v * 300
    fillRect screen (Just (Rect 2 (y - h) 5 h)) c

drawPitch :: Surface -> Pitch -> Pixel -> Pixel -> IO Bool
drawPitch screen p g w = do
    let top    = fromEnum extrapixel
    let left   = fromEnum extrapixel
    let (pwid, _, phei) = area p
    let width  = fromEnum (pwid * pixtometer)  -- short side
    let height = fromEnum (phei * pixtometer)  -- long side
    let bottom = top  + width
    let right  = left + height
    let midheight = left + (height `div` 2)
    let n = 2
    fillRect screen Nothing g
    fillRect screen (Just (Rect  left      top    height n    )) w
    fillRect screen (Just (Rect  left      top    n      width)) w
    fillRect screen (Just (Rect  left      bottom height n    )) w
    fillRect screen (Just (Rect  right     top    n      width)) w
    fillRect screen (Just (Rect  midheight top    n      width)) w

drawEntity :: Surface -> Pixel -> Pixel -> Entity -> IO Bool
drawEntity screen c s e = do
    fillRect screen (Just (Rect x  z  (fromEnum pixtometer) (fromEnum pixtometer))) s
    fillRect screen (Just (Rect dx dz (fromEnum pixtometer) (fromEnum pixtometer))) c
    where x          = (fromEnum extrapixel) + fromEnum (ex * pixtometer - (pixtometer / 2))
          y          =                         fromEnum (ey * pixtometer * heightmeter + 2)
          z          = (fromEnum extrapixel) + fromEnum (ez * pixtometer - (pixtometer / 2))
          (ez,ey,ex) = location e
          dx         = x - y
          dz         = z - y

module Freekick.Libsoccer.DatabaseHelper
where

import Libaddutil.XMLParse
import Data.Maybe
import Libaddutil.Primitives
import Libaddutil.Colors

parseArea :: Node -> Area
parseArea n = ((minx, miny, minz), (maxx, maxy, maxz))
    where minx = if isNothing mnx then 0.0 else read (fromJust mnx)
          miny = if isNothing mny then 0.0 else read (fromJust mny)
          minz = if isNothing mnz then 0.0 else read (fromJust mnz)
          maxx = if isNothing mnx then 0.0 else read (fromJust mxx)
          maxy = if isNothing mny then 0.0 else read (fromJust mxy)
          maxz = if isNothing mnz then 0.0 else read (fromJust mxz)
          mnx  = lookup "minx" (eAttrs n)
          mny  = lookup "miny" (eAttrs n)
          mnz  = lookup "minz" (eAttrs n)
          mxx  = lookup "maxx" (eAttrs n)
          mxy  = lookup "maxy" (eAttrs n)
          mxz  = lookup "maxz" (eAttrs n)

parsePointLightsXML :: [Node] -> [PointLight]
parsePointLightsXML []     = []
parsePointLightsXML (n:ns) = if eName n /= "light" then parsePointLightsXML ns else (parsePointLight n : parsePointLightsXML ns)

parsePointLight :: Node -> PointLight
parsePointLight n = (parsePoint pn, parseColor cn)
    where pn = fromJust $ findNodeByName (eChildren n) "loc"
          cn = fromJust $ findNodeByName (eChildren n) "color"

parsePoint :: Node -> Point
parsePoint n = (x, y, z)
        where 
          x  = if isNothing mx then 0.0 else read (fromJust mx)
          y  = if isNothing my then 0.0 else read (fromJust my)
          z  = if isNothing mz then 0.0 else read (fromJust mz)
          mx = lookup "x" (eAttrs n)
          my = lookup "y" (eAttrs n)
          mz = lookup "z" (eAttrs n)

parseColor :: Node -> Color
parseColor n = Color r g b
        where
          r  = if isNothing mr then 0 else read (fromJust mr)
          g  = if isNothing mg then 0 else read (fromJust mg)
          b  = if isNothing mb then 0 else read (fromJust mb)
          mr = lookup "r" (eAttrs n)
          mg = lookup "g" (eAttrs n)
          mb = lookup "b" (eAttrs n)          

findNodeByName :: [Node] -> String -> Maybe Node
findNodeByName []     _    = Nothing
findNodeByName (n:ns) name = if eName n == name then Just n else findNodeByName ns name

findNodesByName :: [Node] -> String -> [Node]
findNodesByName [] _        = []
findNodesByName (n:ns) name = if eName n == name then n : (findNodesByName ns name) else findNodesByName ns name

getIntegerAttr :: String -> Node -> Int
getIntegerAttr s n = read $ fromMaybe "0" $ lookup s $ eAttrs n

getMaybeIntegerAttr :: String -> Node -> Maybe Int
getMaybeIntegerAttr s n = if isNothing val then Nothing else Just $ read $ fromJust val
    where val = lookup s $ eAttrs n

getMaybeStringAttr :: String -> Node -> Maybe String
getMaybeStringAttr s n = lookup s $ eAttrs n

getBigIntegerAttr :: String -> Node -> Integer
getBigIntegerAttr s n = read $ fromMaybe "0" $ lookup s $ eAttrs n

getStringAttr :: String -> Node -> String
getStringAttr s n = fromMaybe "" $ lookup s $ eAttrs n

getBoolAttr :: String -> Node -> Bool
getBoolAttr s n = if fromMaybe "0" (lookup s (eAttrs n)) == "0" then False else True

getChildOf :: Node -> String -> Node
getChildOf p n = fromMaybe nullNode $ findNodeByName (eChildren p) n

module Libaddutil.Colors
where

data Color
    = Color Int Int Int
    deriving (Show, Eq, Read)

isValidColor :: Color -> Bool
isValidColor (Color r g b) = (r >= 0   && g >= 0   && b >= 0 &&
                              r <= 255 && g <= 255 && b <= 255)

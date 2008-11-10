module Libaddutil.XMLParse (parseXml, Node (..), nullNode)
where

import Text.XML.Expat.IO as EIO
import Data.Tree
import Data.ByteString.Lazy (readFile, ByteString)
import Data.Maybe
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

data Node = Element { eName :: String, eAttrs :: [(String,String)],
                      eChildren :: [Node] }
          | Text String
          deriving Show

nullNode :: Node
nullNode = Element "" [] []

modifyChildren :: ([Node] -> [Node]) -> Node -> Node
modifyChildren f node = node { eChildren = f (eChildren node) }

parseXml :: FilePath -> IO (Maybe Node)
parseXml file = do
  xml <- Data.ByteString.Lazy.readFile file
  return (parseXmlByteString xml)

parseXmlByteString :: Data.ByteString.Lazy.ByteString -> Maybe Node
parseXmlByteString doc = unsafePerformIO $ runParse where
  runParse = do
    parser <- EIO.newParser Nothing
    -- We maintain the invariant that the stack always has one element,
    -- whose only child at the end of parsing is the root of the actual tree.
    stack <- newIORef [Element "" [] []]
    EIO.setStartElementHandler  parser (\n a -> modifyIORef stack (start n a))
    EIO.setEndElementHandler    parser (\n -> modifyIORef stack (end n))
    EIO.setCharacterDataHandler parser (\s -> modifyIORef stack (text s))
    ok <- EIO.parse parser doc
    if ok
      then do
        [Element _ _ [root]] <- readIORef stack
        return $ Just $ modifyChildren reverse root
      else return Nothing
  start name attrs stack = Element name attrs [] : stack
  text str (cur:rest) = if '\t' `elem` str || '\n' `elem` str || length (filter (/= ' ') str) == 0
                          then (cur:rest)
                          else modifyChildren (Text str:) cur : rest
  end name (cur:parent:rest) =
    if eName cur /= name then error "name mismatch" else
    let node = modifyChildren reverse cur in
    modifyChildren (node:) parent : rest

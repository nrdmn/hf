module Main where

import qualified Network.Simple.TCP as TCP
import Data.ByteString.Char8 (pack)
import Data.Word (Word8)
import Text.Printf (printf)
import System.Environment

data Pixel = Pixel { position :: (Int, Int), color :: (Word8, Word8, Word8) }
instance Show Pixel where
    show p = printf "PX %d %d %02x%02x%02x" x y r g b
        where (x, y)    = position p
              (r, g, b) = color p

screen = pack $ unlines $ [show $ Pixel (a, b) (toEnum a, toEnum b, 0) | a <- [0..255], b <- [0..255]]

main = do
    args <- getArgs
    TCP.connect (args!!0) (args!!1) sendscreen
        where sendscreen (socket, addr) = do
                TCP.send socket screen
                sendscreen (socket, addr)

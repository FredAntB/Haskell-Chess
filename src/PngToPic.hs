module PngToPic
    (pngToPic)
    where

import Data.ByteString as B
import Data.Word

import Codec.Picture.Png
import Codec.Picture.Repa
import qualified Data.ByteString.Internal as BI
import Data.Array.Repa ((:.)(..), Z, Z(..), extent, DIM3, Array)
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.ForeignPtr as F
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap


pngToPic :: ByteString -> Picture
pngToPic png
    = let
        Right img -- unsafe
            = decodePng png
        repa
            = imgData (convertImage img :: Img RGBA)
    in repaToPicture True (R.computeS repa)

repaToPicture :: Bool -> Array F.F DIM3 Word8 -> Picture
repaToPicture b arr
    = bitmapOfByteString row col (BitmapFormat BottomToTop PxABGR) bs b
    where
        bs
            = BI.fromForeignPtr fptr 0 (R.size sh)
        fptr
            = F.toForeignPtr arr'
        sh@(Z :. col :. row :. depth)
            = extent arr'
        arr'
            = flipVert arr

flipVert :: Array F.F DIM3 Word8 -> Array F.F DIM3 Word8
flipVert g
    = R.computeS $ R.backpermute e flop g
    where
        e@(Z :. x :. y :. _)
            = extent g
        flop (Z :. i         :. j         :. k)
            = Z :. x - i - 1 :. j :. k
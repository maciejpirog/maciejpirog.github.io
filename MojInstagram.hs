module MojInstagram where

import Codec.Picture
import Codec.Picture.Types
import qualified Codec.Picture.Metadata as MD
import Data.Maybe

type Width = Int
type Height = Int
type X = Int
type Y = Int

transformBMP :: FilePath -> (Width -> Height -> X -> Y -> (Int, Int, Int) -> (Int, Int, Int)) -> FilePath -> IO ()
transformBMP input trans output = do
  Right (ImageRGB8 img, meta) <- readImageWithMetadata input
  let w = fromIntegral (fromJust (MD.lookup MD.Width meta))
  let h = fromIntegral (fromJust (MD.lookup MD.Height meta))
  let img' = pixelMapXY (\x y (PixelRGB8 r g b) ->
                          case trans w h x y (fromIntegral r, fromIntegral g, fromIntegral b) of
                            (r', g', b') -> PixelRGB8 (fromIntegral (inB r')) (fromIntegral (inB g')) (fromIntegral (inB b'))) img
  writeBitmap output img'

inB n = if n < 0 then 0 else if n > 255 then 255 else n

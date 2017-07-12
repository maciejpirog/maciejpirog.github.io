module MojInstagram where

import Codec.Picture
import Codec.Picture.Types
import qualified Codec.Picture.Metadata as MD
import Data.Maybe

type Width = Int
type Height = Int
type X = Int
type Y = Int
type Filter = Width -> Height -> X -> Y -> (Int, Int, Int) -> (Int, Int, Int)

transformBMP :: FilePath -> Filter -> FilePath -> IO ()
transformBMP input trans output = do
  Right (ImageRGB8 img, meta) <- readImageWithMetadata input
  let w = fromIntegral (fromJust (MD.lookup MD.Width meta))
  let h = fromIntegral (fromJust (MD.lookup MD.Height meta))
  let img' = pixelMapXY (\x y (PixelRGB8 r g b) ->
                          case trans w h x y (fromIntegral r, fromIntegral g, fromIntegral b) of
                            (r', g', b') -> PixelRGB8 (fromIntegral (inB r')) (fromIntegral (inB g')) (fromIntegral (inB b'))) img
  writeBitmap output img'

inB n = if n < 0 then 0 else if n > 255 then 255 else n

--- NAKLADANIE FILTROW

(+++) :: Filter -> Filter -> Filter
(f +++ g) w h x y c = g w h x y (f w h x y c)

-- PRZYKLADOWE FILTRY

invert _ _ _ _ (r,g,b) = (255-r,255-g,255-b)

toGS _ _ _ _ (r,g,b) = (c,c,c)
 where
  c = (299*r + 587*g + 114*b) `div` 1000

onlyRed _ _ _ _ (r,g,b) = (r,0,0)

-- wywolanie: transformBMP "cat.bmp" (invert +++ onlyRed) "cat-filtered.bmp"

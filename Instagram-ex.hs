import Codec.Picture
import Codec.Picture.Types
import qualified Codec.Picture.Metadata as MD
import Data.Maybe
import Data.Char
import Data.ByteString.Lazy (ByteString, unpack)
import System.IO.Unsafe

foo = writeBitmap "out.bmp" (generateImage aux 255 255)
 where
  aux x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 100

goo = do
  Right (ImageRGB8 img, meta) <- readImageWithMetadata "cat.bmp"
  let h = fromIntegral (fromJust (MD.lookup MD.Height meta))
  let w = fromIntegral (fromJust (MD.lookup MD.Width meta))
  let img' = pixelMapXY (aux h w) img
  writeBitmap "out.bmp" img'

aux :: Int -> Int -> Int -> Int -> PixelRGB8 -> PixelRGB8
aux h w x y (PixelRGB8 r g b) = PixelRGB8 (fromIntegral ((fromIntegral r + (y`mod`255)))) g (fromIntegral ((fromIntegral b + (y`mod`255))))

-----------------------

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

readPixels :: FilePath -> [(Int, Int, Int)]
readPixels input = unsafePerformIO $ do
  Right (dimg, meta) <- readImageWithMetadata input
  let Right vs = encodeDynamicBitmap dimg
  let bs = unpack vs
  return (toTriples bs)

toTriples [] = []
toTriples (r:g:b:xs) = (fromIntegral r, fromIntegral g, fromIntegral b) : toTriples xs

inB n = if n < 0 then 0 else if n > 255 then 255 else n

invert _ _ _ _ (r,g,b) = (255-r,255-g,255-b)

toGS _ _ _ _ (r,g,b) = (c,c,c)
 where
  c = (299*r + 587*g + 114*b) `div` 1000

onlyRed _ _ _ _ (r,g,b) = (r,0,0)

fadeRed _ _ _ _ (r,g,b) = (fade r,g, b)
 where
  fade x = 127 + (x `div` 2)

redFrame w h x y (r,g,b) = if (x<30) || (x>w-30) || (y<30) || (y>h-30)
  then (255,0,0)
  else (r,g,b)

fadeFrame w h x y (r,g,b) = (inB (r - fromEdge), inB (g - fromEdge), inB (b - fromEdge))
 where
  fromEdge = inB (150 - minimum [x, y, w-x, h-y])

lightFrame = invert +++ fadeFrame +++ invert

lighter w h x y (r,g,b) = (r+120,g+80,b+90)

sepia = toGS +++ fadeRed +++ fadeFrame

checker w h x y _ = ((((x) `div` 100) `mod` 2) * 255,(((y) `div` 100) `mod` 2) * 255, (((x) `div` 100)+1 `mod` 2) * 255)
 where
  c = (((x*y) `div` 100) `mod` 2) * 255

shape w h x y _ = ((x*y`div`40)`mod`256, x, y)

(+++) :: (Width -> Height -> X -> Y -> (Int, Int, Int) -> (Int, Int, Int)) -> (Width -> Height -> X -> Y -> (Int, Int, Int) -> (Int, Int, Int)) -> (Width -> Height -> X -> Y -> (Int, Int, Int) -> (Int, Int, Int))
(f +++ g) w h x y c = g w h x y (f w h x y c)

toBinary :: Int -> [Int]
toBinary 0 = []
toBinary n = toBinary (n `div` 2) ++ [n `mod` 2]

toBinary8 :: Int -> [Int]
toBinary8 n = reverse (take 8 (reverse (toBinary n) ++ [0,0,0,0,0,0,0,0,0,0,0]))

fromBinary :: [Int] -> Int
fromBinary l = aux (reverse l)
 where
  aux []     = 0
  aux (b:bs) = b + 2 * aux bs

charToBin :: Char -> [Int]
charToBin c = toBinary8 (ord c)

binToChar :: [Int] -> Char
binToChar xs = chr (fromBinary xs)

encode :: String -> [Int]
encode [] = []
encode (x:xs) = charToBin x ++ encode xs

decode :: [Int] -> String
decode xs = filter isPrint (decode' xs)

decode' :: [Int] -> String
decode' xs =
  if length xs < 8
    then ""
    else binToChar (take 8 xs) : decode (drop 8 xs)

swapLastBit :: Int -> [Int] -> [Int]
swapLastBit b xs = reverse (b : tail (reverse xs))

hideMessage m w h x y (r, g, b) =
  if x + y*w >= length m
    then (r,g,b)
    else (r,g, fromBinary (swapLastBit (m !! (x + y*w)) (toBinary8 b)))

showMessage input n = take n (decode [ b `mod` 2 | (r,g,b) <- readPixels input])

showPixels input n = take n [ b`mod`2 | (r,g,b) <- readPixels input]

pixt _ _ x _ _ = (0,0,x`mod`2) 

{-
convertRGB8 :: DynamicImage -> Image PixelRGB8
convertRGB8 dynImage = case dynImage of
  ImageY8     img -> promoteImage img
  ImageY16    img -> promoteImage (decimateBitDepth img :: Image Pixel8)
  ImageYF     img -> promoteImage (decimateBitDepth img :: Image Pixel8)
  ImageYA8    img -> promoteImage img
  ImageYA16   img -> promoteImage (decimateBitDepth img :: Image PixelYA8)
  ImageRGB8   img -> img
  ImageRGB16  img -> error "wrongversion of JP"
  ImageRGBF   img -> error "wrongversion of JP"
  ImageRGBA8  img -> dropAlphaLayer img
  ImageRGBA16 img -> error "wrongversion of JP"
  ImageYCbCr8 img -> convertImage img
  ImageCMYK8  img -> convertImage img
  ImageCMYK16 img -> error "wrongversion of JP"
-}

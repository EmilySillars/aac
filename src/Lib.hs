module Lib
  ( convert,
  )
where

import Codec.Picture as J
import Codec.Picture.Repa as R
--import Data.Array.Repa.Index
--import Control.Monad
import Data.Array.Repa hiding ((++))
import qualified Data.ByteString.Char8 as B
import Data.Text (pack)
import Data.Text.Encoding as TSE
import Data.List (intercalate)
import Data.List.Split
--import Data.Typeable (typeOf)
--import Data.Word
import Data.Word (Word8)

--import Codec.Picture.Types (ColorSpaceConvertible(convertImage))
-- someFunc :: IO ()
-- someFunc = putStrLn "someFunc"
-- readPng :: FilePath -> IO (Either String DynamicImage)
{-Installing library in
C:\sr\snapshots\d574fa6f\lib\x86_64-windows-ghc-8.10.7\JuicyPixels-3.3.5-1kvarO6VLos4MgV6H2geEO
-}
{-
All images are held in a three dimensional repa array.
If the image format is only two dimensional (ex: R, G, or B)
then the shape is Z :. y :. x :. 1.

Constructors:
Img
imgData :: Array D DIM3 Word8

//a phantom type
Collapsable RGBA (Word8, Word8, Word8, Word8)

Goals:
- get out a single pixel's rgba values
- find out how many pixels (dimensions of repa array)
- split into tiles and return smaller repa array?

-}
convert :: FilePath -> IO String
convert png =
  do
    img <- J.readImage png
    case img of
      (Right v) ->
        return $ dim ++ lamp ++ bi
        where
          w =  imageWidth imgRGB
          dim = "(" ++ show (imageWidth imgRGB) ++ " x " ++ show (imageHeight imgRGB) ++ ")"
          imgRGB = convertRGB8 v
          imgRepa = R.convertImage imgRGB :: Img RGB
          -- elt = imgData imgRepa ! (Z :. 1 :. 3 :. 1)
          r = imgData imgRepa ! (Z :. 0 :. 240 :. 0)
          g = imgData imgRepa ! (Z :. 0 :. 240 :. 1)
          b = imgData imgRepa ! (Z :. 0 :. 240 :. 2)
          --   test = show $ reverse $ listOfShape $ extent $ imgData imgRepa
          vals = show (r, g, b) -- show $ typeOf elt --show elt
          bi = "noodle \n" ++ babaa 
          babaa = intercalate "\n" $ chunksOf w $ toList $ bijection $ imgData imgRepa
          lamp = " size " ++ (show $ size $ extent $ imgData imgRepa) ++ " elt " ++ vals ++ "\n"
      -- paulBourkeRamp = show $ typeOf $ TSE.encodeUtf8 $ pack "$@B%8&WM#*oahkbdpqwmZO0QLCJUYXzcvunxrjft/\\|()1{}[]?-_+~<>i!lI;:,\"^`\'. "

      -- lamp = "size "++ show (size <$> imgRepa) ++"\n"
      (Left err) ->
        return $ "Read Error: " ++ err

-- convert png = do
--     Right (ImageRGB8 v) <- readPng png
--     "(" ++ show (imageWidth v) ++ " x " ++ show (imageHeight v) ++ ")"

--convert f = "I'm supposed to convert " ++ f ++ " to ASCII.\nhoodle noodle ho."
{-
 extra-deps:
- JuicyPixels-repa-0.7.1.0
- git: https://github.com/TomMD/JuicyPixels-repa
  commit: 2a69a8853190b85086869e85bd2cb26c315cf9b5
#@sha256:ffbebd96efbf6af83eb14ee9c89259acf5a9a7d504644f2a35577bd66406ae69,898

- PLImmutable (PLIHackage (PackageIdentifier {pkgName = PackageName "JuicyPixels-repa",
pkgVersion = mkVersion [0,7,1,0]}) ffbebd96efbf6af83eb14ee9c89259acf5a9a7d504644f2a35577bd66406ae69,898
(TreeKey 951d640678453f8e41fa6aac78eed1fea260d71152abfcd1b7e8c2fe936d2581,222))

-}

--surjection TODO
--a ! (Z:.5)
-- fromFunction :: sh -> (sh -> a) -> Array D sh a
-- let a = fromFunction (Z:.10) (\(Z:.i) -> i * 10 :: Int)
-- computeS a :: Array U DIM1 Int
-- let mymap f a = fromFunction (extent a) (\i -> f (a ! i))
--round $ 0.2989 * i r + 0.5870 * i g + 0.1140 * i b
--bijection
-- TS.pack   :: String -> TS.Text
-- TSE.encodeUtf8 :: TS.Text -> BS.ByteString

bijection :: Array D DIM3 Word8 -> Array D DIM2 Char
bijection pixels = a --computeS a ::Array U DIM2 Word8
  where
    (height : width : _) = reverse $ listOfShape $ extent pixels
    a = fromFunction (Z :. height :. width) toGray
    toGray = \(Z :. i :. j) ->
      let r = fromIntegral $ pixels ! (Z :. i :. j :. 0)
       in let g = fromIntegral $ pixels ! (Z :. i :. j :. 1)
           in let b = fromIntegral $ pixels ! (Z :. i :. j :. 2)
               in ramp `B.index` (gray (r, g, b) `mod` 70)

gray :: (Double, Double, Double) -> Int
gray (r, g, b) = round $ 0.2989 * r + 0.5870 * g + 0.1140 * b

ramp :: B.ByteString
ramp = TSE.encodeUtf8 $ pack "$@B%8&WM#*oahkbdpqwmZO0QLCJUYXzcvunxrjft/\\|()1{}[]?-_+~<>i!lI;:,\"^`\'. "

-- brush = fromListUnboxed (Z:.70) paulBourkeRamp::[char]

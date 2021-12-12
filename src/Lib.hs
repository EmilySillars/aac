module Lib
  ( convert,
  )
where

import Codec.Picture as J
import Codec.Picture.Repa as R
import Data.Array.Repa hiding ((++))
import qualified Data.ByteString.Char8 as B
import Data.List (intercalate)
import Data.List.Split
import Data.Text (pack)
import Data.Text.Encoding as TSE
import Data.Word (Word8)
import Data.Typeable (typeOf)
import GHC.ExecutionStack (Location(functionName))
import Text.Printf (IsChar(toChar))
--import Data.Word
--import Data.Array.Repa.Index
--import Control.Monad

ramp :: B.ByteString
ramp =
  TSE.encodeUtf8 $
    pack
      "$@B%8&WM#*oahkbdpqwmZO0QLCJUYXzcvunxrjft/\\|()1{}[]?-_+~<>i!lI;:,\"^`\'. "

gray :: (Double, Double, Double) -> Int
gray (r, g, b) = round $ 0.2989 * r + 0.5870 * g + 0.1140 * b

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
        return $ dim ++ bi
        where
          imgRGB = convertRGB8 v
          w = imageWidth imgRGB
          h = imageHeight imgRGB
          dim = "(" ++ show w ++ " x " ++ show h ++ ")\n"
          imgRepa = R.convertImage imgRGB :: Img RGB
          bi = babaa ++ "\n again but smaller: \n" ++ q
          -- ++ "\n" ++ test ++ "\n" ++ test2 ++ "\n" ++ test3 
                     -- ++ "\n" ++ m ++ n ++ "\nmatrixTest: \n" 
                     -- ++ q ++ "\n matrixTest2:\n" ++ p
        --  test3 = show $ toList t1 == toList t2
          babaa = intercalate "\n" $ chunksOf w $ toList $ bijection $ imgData imgRepa
          moo = intercalate "\n" $ chunksOf w $ toList $ bijection $ imgData imgRepa
        --   test = show (typeOf $ t1) ++ ": " ++ show (listOfShape $ extent t1)
        --    --  Array D ((:.) ((:.) ((:.) Z Int) Int) Int) Word8
        --   test2 = show (typeOf $ t2) ++ ": " ++ show (listOfShape $ extent t2)
        --   t1 = imgData imgRepa
        --   t2 = reshape (Z :. (32::Int) :. (32::Int) :. (4::Int) :. (3::Int)) $ imgData imgRepa
        --   --foldS :: (Shape sh, Source r a, Unbox a) => 
        --   -- (a -> a -> a) -> a -> Array r (sh :. Int) a -> Array U sh a
        --   matrix = fromListUnboxed (Z :. (2::Int) :. (2::Int)) [1::Int,2,3,4] 
        --   foldedMatrix = foldS (+) 0 matrix
        --   m = show matrix ++ "\nfolded: " ++ show foldedMatrix
        --   dumb =concat [[0::Int,0,0], [1,1,1], [2,2,2], [3,3,3],
        --           [0,0,0], [1,1,1], [2,2,2], [3,3,3],
        --           [0,0,0], [1,1,1], [2,2,2], [3,3,3],
        --           [0,0,0], [1,1,1], [2,2,2], [3,3,3]]
        --   dummyMatrix = 
        --       fromListUnboxed (Z :. (2::Int) :. (2::Int) :. (4::Int) :. (3::Int)) dumb
        --   foldedDummy = foldS (+) 0 dummyMatrix
        --   n = "\ndummy " ++ show dummyMatrix ++ "\nfolded dummy: " ++ show foldedDummy
          q = intercalate "\n" $ chunksOf 32 $ toList $ matrixTest $ imgData imgRepa
         -- p = intercalate "\n" $ chunksOf 8 $ toList $ matrixTest2 $ imgData imgRepa
      (Left err) ->
        return $ "Read Error: " ++ err


matrixTest :: Array D DIM3 Word8 -> Array D DIM2 Char
matrixTest img = step4
  where results = "\n" ++ show (typeOf start) ++ "\n" ++ show (typeOf step1) ++ 
                  "\n" ++ show (typeOf step2) ++ " " ++ show (listOfShape $ extent step2) ++
                  "\n" ++ show (typeOf step3)
        -- break into tiles
        start = reshape (Z :. (32::Int) :. (32::Int) :. (4::Int) :. (3::Int)) img
        -- sum up pixels in each tile
        step1 = toTriple start
        toTriple :: Array D DIM4 Word8 -> Array D DIM3 (Double,Double,Double)
        toTriple st = fromFunction (Z :. (32::Int) :. (32::Int) :. (4::Int)) triple
          where triple = \(Z :. i :. j :. k) -> 
                        let a = fromIntegral $ st ! (Z :. i :. j :. k :. 0)  in
                        let b = fromIntegral $ st ! (Z :. i :. j :. k :. 1) in
                        let c = fromIntegral $ st ! (Z :. i :. j :. k :. 2) in
                        (a,b,c)
        step2 = foldS (\(a,b,c) (v1,v2,v3) -> (a+v1, b+v2, c+v3)) (0,0,0) step1
        -- divide each sum by number of pixels/tile
        step3 = fromFunction (Z :. (32::Int) :. (32::Int)) func
          where func = \(Z :. i :. j) -> 
                        case step2 ! (Z :. i :. j) of
                       (a,b,c) -> (a/4, b/4, c/4)
        step4 = fromFunction (Z :. (32::Int) :. (32::Int)) toChar
          where toChar = \(Z :. i :. j) -> 
                         let g = gray $ step3 ! (Z :. i :. j) in
                         ramp `B.index` (g `mod` 70)


matrixTest2 :: Array D DIM3 Word8 -> Array D DIM2 Char
matrixTest2 img = step4
  where results = "\n" ++ show (typeOf start) ++ "\n" ++ show (typeOf step1) ++ 
                  "\n" ++ show (typeOf step2) ++ " " ++ show (listOfShape $ extent step2) ++
                  "\n" ++ show (typeOf step3)
        -- break into tiles
        start = reshape (Z :. (16::Int) :. (32::Int) :. (8::Int) :. (3::Int)) img
        -- sum up pixels in each tile
        step1 = toTriple start
        toTriple :: Array D DIM4 Word8 -> Array D DIM3 (Double,Double,Double)
        toTriple st = fromFunction (Z :. (16::Int) :. (32::Int) :. (8::Int)) triple
          where triple = \(Z :. i :. j :. k) -> 
                        let a = fromIntegral $ st ! (Z :. i :. j :. k :. 0)  in
                        let b = fromIntegral $ st ! (Z :. i :. j :. k :. 1) in
                        let c = fromIntegral $ st ! (Z :. i :. j :. k :. 2) in
                        (a,b,c)
        step2 = foldS (\(a,b,c) (v1,v2,v3) -> (a+v1, b+v2, c+v3)) (0,0,0) step1
        -- divide each sum by number of pixels/tile
        step3 = fromFunction (Z :. (16::Int) :. (32::Int)) func
          where func = \(Z :. i :. j) -> 
                        case step2 ! (Z :. i :. j) of
                       (a,b,c) -> (a/8, b/8, c/8)
        step4 = fromFunction (Z :. (16::Int) :. (32::Int)) toChar
          where toChar = \(Z :. i :. j) -> 
                         let g = gray $ step3 ! (Z :. i :. j) in
                         ramp `B.index` (g `mod` 70)



        -- now just need to take average, turn to gray, and make a character!!!
             
        -- toTriple :: (Int,Int,Int) -> Int -> (Int,Int,Int)
        -- toTriple triple v = case triple of
        --     (-1, -1, -1) -> (v, -1, -1)
        --     (a, -1, -1) -> (a ,v, -1)
        --     (a, b, -1) -> (a, b, -1)
        --     (_,_,_) -> (-1,-1,-1)
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

          w = imageWidth imgRGB
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

surjection :: Array D DIM3 Word8 -> Array D DIM2 Char
surjection pixels = b
  where
       b = fromFunction (Z:.10:.1) (\(Z:.i:._) -> head $ show (i * 10))
       a = pixels
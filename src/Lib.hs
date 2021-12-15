module Lib
  ( convert
  ) where
import           Codec.Picture                 as J
import           Codec.Picture.Repa            as R
import           Control.DeepSeq                ( NFData )
import           Control.Monad                  ( join )
import           Data.Array.Repa       as A  hiding ( (++) )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import qualified Data.ByteString.Lazy          as BL
import           Data.Functor.Identity as F 
import qualified Data.List                     as L
import Data.Vector.Storable as V (toList) 

import           Data.List.Split                ( chunksOf )
import           Data.Text                      ( pack )
import           Data.Text.Encoding            as TSE
import           Data.Text.Internal.Fusion.Size ( charSize )
import           Data.Typeable                  ( typeOf )
import           Data.Word                      ( Word8 )
import           GHC.ExecutionStack             ( Location(functionName) )
import           GHC.RTS.Flags                  ( TickyFlags(tickyFile) )
import           Text.Printf                    ( IsChar(toChar) )
--import Data.Word
--import Data.Array.Repa.Index
--import Control.Monad

convert :: Bool -> BL.ByteString -> String
convert par png =
  let img = myReadPng png
  in
    case img of
      (Right v) -> imgAsText
       where
        imgRGB    = convertRGB8 v
        -- w         = imageWidth imgRGB
        -- h         = imageHeight imgRGB
        -- dim       = "(" ++ show w ++ " x " ++ show h ++ ")\n"
       -- imgRepa   = R.convertImage imgRGB :: Img RGB
       -- bi        = L.intercalate "\n" $ chunksOf w $ A.toList imgAsText
        imgAsText = if par
          then bijectionPar imgRGB
          else bijectionPar imgRGB
      (Left err) -> "Read Error: " ++ err

bijection :: Array D DIM3 Word8 -> Array D DIM2 Char
bijection pixels = a -- :: Array D DIM2 Char
 where
  (height : width : _) = reverse $ listOfShape $ extent pixels
  a                    = fromFunction (Z :. height :. width) toGray
  toGray               = \(Z :. i :. j) ->
    let r = fromIntegral $ pixels ! (Z :. i :. j :. 0)
    in  let g = fromIntegral $ pixels ! (Z :. i :. j :. 1)
        in  let b = fromIntegral $ pixels ! (Z :. i :. j :. 2)
            in  ramp `BC.index` (gray (r, g, b) `mod` 70)

ramp :: B.ByteString
ramp = TSE.encodeUtf8 $ pack $ reverse
  "$@B%8&WM#*oahkbdpqwmZO0QLCJUYXzcvunxrjft/\\|()1{}[]?-_+~<>i!lI;:,\"^`\'. "

gray :: (Double, Double, Double) -> Int
gray (r, g, b) = round $ 0.2989 * r + 0.5870 * g + 0.1140 * b

bijectionP :: Array D DIM3 Word8 -> Array U DIM2 Char
bijectionP pixels = runIdentity $ computeP a
 where
  (height : width : _) = reverse $ listOfShape $ extent pixels
  a                    = fromFunction (Z :. height :. width) toGray
  toGray               = \(Z :. i :. j) ->
    let r = fromIntegral $ pixels ! (Z :. i :. j :. 0)
    in  let g = fromIntegral $ pixels ! (Z :. i :. j :. 1)
        in  let b = fromIntegral $ pixels ! (Z :. i :. j :. 2)
            in  ramp `BC.index` (gray (r, g, b) `mod` 70)

myReadPng :: BL.ByteString -> Either String DynamicImage
myReadPng = myWithImageDecoder J.decodeImage

myWithImageDecoder
  :: (NFData a)
  => (B.ByteString -> Either String a)
  -> BL.ByteString
  -> Either String a
myWithImageDecoder decoder path = decoder $ BL.toStrict path

-- convertFrame :: BL.ByteString -> BC.ByteString 
-- convertFrame frame = 
--   case myReadPng frame of
--     (Right v) -> toSingleString $ bijection $ imgData imgRepa
--      where
--       imgRepa   = R.convertImage $ convertRGB8 v :: Img RGB
--     (Left err) -> BC.pack $ "Read Error: " ++ err
--convertAnimation frames = fromFunction (extent frames) (\i -> convertFrame (frames ! i) )   
-- convertAnimation
--   :: Array D DIM1 BL.ByteString -> (Int, Int) -> Array D DIM3 Char
-- convertAnimation frames (w, h) = fromFunction
--   (Z :. s :. h :. w)
--   (\(Z :. i :. j :. k) ->
--     let frame = convertFrame $ frames ! (Z :. (i :: Int))
--     in  frame ! (Z :. (j :: Int) :. (k :: Int))
--   )
--   where [s] = listOfShape $ extent frames


toSingleString :: Array D DIM2 Char -> BC.ByteString
toSingleString chars = BC.pack $ L.intercalate "\n" $ chunksOf w $ A.toList chars
  where [_, w] = reverse $ listOfShape $ extent chars

  -- (Z:.(i::Int):.(j::Int):.(k::Int) 
  --          = frame ! (Z:.(j::Int):.(k::Int)) 
  --         where frame = convertFrame (frames ! (Z:.(i::Int))) in

-- helper :: Array D Dim2 Char -> Array D DIM3 Char
-- helper frame = fromFunction sh (sh -> a)
--bijection :: Array D DIM3 Word8 -> Array U DIM2 Char

-- convertFrame :: BL.ByteString -> Array D DIM2 Char
-- convertFrame frame = case myReadPng frame of
--   (Right v) -> bijection $ imgData imgRepa
--     where imgRepa = R.convertImage $ convertRGB8 v -- :: ImageRGB8
--   (Left err) -> fromFunction (Z :. 1 :. length msg)
--                              (\(z :. 0 :. j) -> msg !! j)
--     where msg = "Read Error: " ++ err


bijectionPar :: Image PixelRGB8 -> String
bijectionPar img = L.intercalate "\n" $ chunksOf (imageWidth img) chars
 where
    chars = (\px->  ramp `BC.index` (fromIntegral px `mod` 70)) <$>  V.toList pixels
    pixels = imageData grayImg
    grayImg = pixelMap toGray img
    toGray :: PixelRGB8 -> Pixel8 
    toGray (PixelRGB8 r g b) = round $ 0.2989 * fromIntegral r + 0.5870 * fromIntegral g + 0.1140 * fromIntegral b
    --pixelMap :: forall a b. (Pixel a, Pixel b) => (a -> b) -> Image a -> Image b 

  -- (height : width : _) = reverse $ listOfShape $ extent pixels
  -- a                    = fromFunction (Z :. height :. width) toGray
  -- toGray               = \(Z :. i :. j) ->
  --   let r = fromIntegral $ pixels ! (Z :. i :. j :. 0)
  --   in  let g = fromIntegral $ pixels ! (Z :. i :. j :. 1)
  --       in  let b = fromIntegral $ pixels ! (Z :. i :. j :. 2)
          --  in  ramp `BC.index` (gray (r, g, b) `mod` 70)

{-
brightnessRGB8 :: Int -> Image PixelRGB8 -> Image PixelRGB8
brightnessRGB8 add = pixelMap brightFunction
     where up v = fromIntegral (fromIntegral v + add)
           brightFunction (PixelRGB8 r g b) =
                   PixelRGB8 (up r) (up g) (up b)



-}




-- matrixTest :: Array D DIM3 Word8 -> Array D DIM2 Char
-- matrixTest img = step4
--   where results = "\n" ++ show (typeOf start) ++ "\n" ++ show (typeOf step1) ++
--                   "\n" ++ show (typeOf step2) ++ " " ++ show (listOfShape $ extent step2) ++
--                   "\n" ++ show (typeOf step3)
--         -- break into tiles
--         start = reshape (Z :. (32::Int) :. (32::Int) :. (3::Int) :. (4::Int)) img
--         -- sum up pixels in each tile
--         step1 = toTriple start
--         toTriple :: Array D DIM4 Word8 -> Array D DIM3 (Double,Double,Double)
--         toTriple st = fromFunction (Z :. (32::Int) :. (32::Int) :. (4::Int)) triple
--           where triple = \(Z :. i :. j :. k) ->
--                         let a = fromIntegral $ st ! (Z :. i :. j :. k :. 0)  in
--                         let b = fromIntegral $ st ! (Z :. i :. j :. k :. 1) in
--                         let c = fromIntegral $ st ! (Z :. i :. j :. k :. 2) in
--                         (a,b,c)
--         step2 = foldS (\(a,b,c) (v1,v2,v3) -> (a+v1, b+v2, c+v3)) (0,0,0) step1
--         -- divide each sum by number of pixels/tile
--         step3 = fromFunction (Z :. (32::Int) :. (32::Int)) func
--           where func = \(Z :. i :. j) ->
--                         case step2 ! (Z :. i :. j) of
--                        (a,b,c) -> (a/4, b/4, c/4)
--         step4 = fromFunction (Z :. (32::Int) :. (32::Int)) toChar
--           where toChar = \(Z :. i :. j) ->
--                          let g = gray $ step3 ! (Z :. i :. j) in
--                          ramp `B.index` (g `mod` 70)


-- matrixTest2 :: Array D DIM3 Word8 -> Array D DIM2 Char
-- matrixTest2 img = step4
--   where results = "\n" ++ show (typeOf start) ++ "\n" ++ show (typeOf step1) ++
--                   "\n" ++ show (typeOf step2) ++ " " ++ show (listOfShape $ extent step2) ++
--                   "\n" ++ show (typeOf step3)
--         -- break into tiles
--         start = reshape (Z :. (16::Int) :. (32::Int) :. (8::Int) :. (3::Int)) img
--         -- sum up pixels in each tile
--         step1 = toTriple start
--         toTriple :: Array D DIM4 Word8 -> Array D DIM3 (Double,Double,Double)
--         toTriple st = fromFunction (Z :. (16::Int) :. (32::Int) :. (8::Int)) triple
--           where triple = \(Z :. i :. j :. k) ->
--                         let a = fromIntegral $ st ! (Z :. i :. j :. k :. 0)  in
--                         let b = fromIntegral $ st ! (Z :. i :. j :. k :. 1) in
--                         let c = fromIntegral $ st ! (Z :. i :. j :. k :. 2) in
--                         (a,b,c)
--         step2 = foldS (\(a,b,c) (v1,v2,v3) -> (a+v1, b+v2, c+v3)) (0,0,0) step1
--         -- divide each sum by number of pixels/tile
--         step3 = fromFunction (Z :. (16::Int) :. (32::Int)) func
--           where func = \(Z :. i :. j) ->
--                         case step2 ! (Z :. i :. j) of
--                        (a,b,c) -> (a/8, b/8, c/8)
--         step4 = fromFunction (Z :. (16::Int) :. (32::Int)) toChar
--           where toChar = \(Z :. i :. j) ->
--                          let g = gray $ step3 ! (Z :. i :. j) in
--                          ramp `B.index` (g `mod` 70)



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

-- printyPrinty :: Array D DIM2 Char -> String
-- printyPrinty img = intercalate "\n" $ chunksOf width $ toList img
--     where 
--     [_, width] = reverse $ listOfShape $ extent img

-- surjection :: Array D DIM3 Word8 -> Int -> Array D DIM2 Char
-- surjection pixels rows = a--fromFunction (Z:.(2::Int):.(2::Int)) (\(Z:.i:.j) -> 'a') --intercalate "\n" res 
--   where
--        res = [wh,rqrs,rat,pwh,printwh,someElts,allTiles]
--        rqrs = "requested cols: " ++ show rows
--        (height : width : _) = reverse $ listOfShape $ extent pixels
--        wh = "(" ++ show width ++ " x " ++ show height ++ ")"
--     --    reduceBy = fromIntegral rows / fromIntegral width
--     --    cols = round $ reduceBy * width
--        rat = "pixel to char ratio: " ++ show ratio
--        ratio = round $ (fromIntegral width) / (fromIntegral rows) -- pixel to char ratio
--        paddedWidth = case r of
--            0 -> width
--            _ -> width + r
--        paddedHeight = case r of
--             0 -> height
--             _-> height + r
--        (q,r) = width `quotRem` ratio
--        pwh = "padded: (" ++ show paddedWidth ++ " x " ++ show paddedHeight ++ ")"
--        -- b = fromFunction (Z:.10:.1) (\(Z:.i:._) -> head $ show (i * 10)) pixels    
--        paddedImage = toIndexedTriple $ fromFunction (Z:.paddedHeight:.paddedWidth:.3) expand

--        expand :: DIM3 -> Word8
--        expand (Z:.i:.j:.c)
--         | (i >= height) && (j >= width) =pixels ! (Z:.(height-1):.(width -1):.c)
--         | i >= height = pixels ! (Z:.(height - 1):.j:.c)
--         | j >= width = pixels ! (Z:.i:.(width - 1):.c)
--         | otherwise = pixels ! (Z:.i:.j:.c)

--        toIndexedTriple :: Array D DIM3 Word8 -> Array D DIM2 (Int,Int,(Double,Double,Double))
--        toIndexedTriple st = fromFunction (Z:.paddedHeight:.paddedWidth) triple
--          where triple = \(Z :. i :. j) ->
--                        let a = fromIntegral $ st ! (Z :. i :. j :. 0) in
--                        let b = fromIntegral $ st ! (Z :. i :. j :. 1) in
--                        let c = fromIntegral $ st ! (Z :. i :. j :. 2) in
--                        (i,j,(a,b,c))
--        someElts = intercalate " " $ show <$> (take 50 $ toList $ paddedImage)
--        printHeight = round $ (fromIntegral paddedHeight) / (fromIntegral ratio)
--        printWidth = round $ (fromIntegral paddedWidth) / (fromIntegral ratio)
--        printwh = "printing: (" ++ show printWidth ++ " x " ++ show printHeight ++ ")"
--        tiled = fromFunction (Z:.printHeight:.printWidth) tile
--        allTiles ="\nTILES\n" ++ (intercalate " " $ show <$> (toList $ tiled))
--        tile :: DIM2 -> (Double,Double,Double)
--        tile (Z:.i:.j) = if len == 0 then (-99,-99,-99) else (a/len,b/len,c/len) --(a/len,b/len,c/len)
--          where
--              (a,b,c) = foldl (\(a,b,c) (_,_,(v1,v2,v3)) -> (a+v1, b+v2, c+v3)) (0,0,0) members
--              members = filter inTile (toList paddedImage)
--              len = fromIntegral $ length members
--              inTile:: (Int,Int,(Double,Double,Double)) -> Bool
--            --  inTile (x,y,_) = x `mod` 2 == 0
--              inTile (y,x,_) = (x <= right) && (y >= top) && (y <= bottom) && (x >= left)
--                where
--                 top = (ratio-1)*i
--                 bottom = i + (ratio-1) + ((ratio-1)*i)
--                 left = (ratio-1)*j
--                 right = j + (ratio-1) + ((ratio-1)*j)
--        a = fromFunction (Z:.printHeight:.printWidth) toChar
--           where toChar = \(Z :. i :. j) ->
--                          let g = gray $ tiled ! (Z :. i :. j) in
--                          ramp `B.index` (g `mod` 70)




          -- computed = computeS imgAsText :: Array D DIM2 Char
          --moo = surjection (imgData imgRepa) 40
          --moo = printyPrinty $ surjection (imgData imgRepa) 120
        --   test = show (typeOf $ t1) ++ ": " ++ show (listOfShape $ extent t1)
        --    --  Array D ((:.) ((:.) ((:.) Z Int) Int) Int) Word8
        --   test2 = show (typeOf $ t2) ++ ": " ++ show (listOfShape $ extent t2)
        --   t1 = imgData imgRepa
        --   t2 = reshape (Z :. (32::Int) :. (32::Int) :. (4::Int) :. (3::Int)) $ imgData imgRepa
        --   --foldS :: (Shape sh, Source r a, Unbox a) => 
        --   -- (a -> a -> a) -> a -> Array r (sh :. Int) a -> Array U sh a



          -- matrix = fromListUnboxed (Z :. (2::Int) :. (2::Int)) [1::Int,2,3,4]
          -- --foldedMatrix = foldS (+) 0 matrix
          -- aRow = slice matrix (Any :. (0::Int) :. All)
          -- zeroth = "\n0th row: " ++ show (toList aRow)
          -- oneth = "\n1st row: " ++ show (toList $ slice matrix (Any :. (1::Int) :. All))
          -- top = "\nLeft col: " ++ show (toList $ slice matrix (Any :. (0::Int)))
          -- bot = "\nRight col: " ++ show (toList $ slice matrix (Any :. (1::Int)))
          -- extended = extend (Any :. (2::Int) :. All) matrix --copy a row
          -- --filtered = select (isOdd) 
          -- m = show matrix ++ zeroth ++ oneth ++ top ++ bot ++ "\n"
          --                 ++ show (toList extended)
        --   dumb =concat [[0::Int,0,0], [1,1,1], [2,2,2], [3,3,3],
        --           [0,0,0], [1,1,1], [2,2,2], [3,3,3],
        --           [0,0,0], [1,1,1], [2,2,2], [3,3,3],
        --           [0,0,0], [1,1,1], [2,2,2], [3,3,3]]
        --   dummyMatrix = 
        --       fromListUnboxed (Z :. (2::Int) :. (2::Int) :. (4::Int) :. (3::Int)) dumb
        --   foldedDummy = foldS (+) 0 dummyMatrix
        --   n = "\ndummy " ++ show dummyMatrix ++ "\nfolded dummy: " ++ show foldedDummy
        --  q = intercalate "\n" $ chunksOf 32 $ toList $ matrixTest $ imgData imgRepa
         -- p = intercalate "\n" $ chunksOf 8 $ toList $ matrixTest2 $ imgData imgRepa

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

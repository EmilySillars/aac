module Lib
  ( convertS
  , convertIVar
  , convertRepa
  , readLazyImg
  ) where
import           Codec.Picture                 as J
import           Codec.Picture.Repa            as R
                                                ( Img(imgData)
                                                , RGB
                                                , convertImage
                                                )
import           Control.DeepSeq                ( NFData )
import           Control.Monad                  ( join )
import           Control.Monad.Par              ( parMap
                                                , runPar
                                                )
import           Control.Parallel.Strategies    ( parList
                                                , rdeepseq
                                                , runEval
                                                , using
                                                )
import           Data.Array.Repa               as A
                                         hiding ( (++) )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import qualified Data.ByteString.Lazy          as BL
import           Data.Functor.Identity         as F
import qualified Data.List                     as L
import           Data.List.Split                ( chunksOf )
import           Data.Text                      ( pack )
import           Data.Text.Encoding            as TSE
import           Data.Text.Internal.Fusion.Size ( charSize )
import           Data.Typeable                  ( typeOf )
import           Data.Vector.Storable          as V
                                                ( toList )
import           Data.Word                      ( Word8 )
import           GHC.ExecutionStack             ( Location(functionName) )
import           GHC.RTS.Flags                  ( TickyFlags(tickyFile) )
import           Text.Printf                    ( IsChar(toChar) )


-- Paul Bourke's 70 levels of gray character ramp
ramp :: B.ByteString
ramp = TSE.encodeUtf8 $ pack $ reverse
  "$@B%8&WM#*oahkbdpqwmZO0QLCJUYXzcvunxrjft/\\|()1{}[]?-_+~<>i!lI;:,\"^`\'. "

-- gray = f(R,G,B) = 0.2989 * R + 0.5870 * G + 0.1140 * B
gray :: (Double, Double, Double) -> Int
gray (r, g, b) = round $ 0.2989 * r + 0.5870 * g + 0.1140 * b

-- | Sequential Version

-- convert an image to ascii and return as string
-- helper for sequential implementation
convertS :: FilePath -> IO String
convertS png = do
  img <- J.readImage png
  case img of
    (Right v) -> return bi
     where
      imgRGB    = convertRGB8 v
      w         = imageWidth imgRGB
      imgRepa   = R.convertImage imgRGB :: Img RGB
      bi        = L.intercalate "\n" $ chunksOf w $ A.toList imgAsText
      imgAsText = surjectionS $ imgData imgRepa
    (Left err) -> return $ "Read Error: " ++ err

-- sequential image to ascii conversion using REPA
surjectionS :: Array D DIM3 Word8 -> Array U DIM2 Char
surjectionS pixels = computeS a -- :: Array D DIM2 Char -- computeS a 
 where
  (height : width : _) = reverse $ listOfShape $ extent pixels
  a                    = fromFunction (Z :. height :. width) toGray
  toGray               = \(Z :. i :. j) ->
    let r = fromIntegral $ pixels ! (Z :. i :. j :. 0)
    in  let g = fromIntegral $ pixels ! (Z :. i :. j :. 1)
        in  let b = fromIntegral $ pixels ! (Z :. i :. j :. 2)
            in  ramp `BC.index` (gray (r, g, b) `mod` 70)

-- | parMap + REPA version 

-- convert image to ascii array, then convert to string
-- top level wrapper for surjectionP
convertRepa :: Either String (Image PixelRGB8) -> String
convertRepa (Left  err) = err
convertRepa (Right img) = L.intercalate "\n" $ chunksOf w $ A.toList imgAsText
 where
  repaImg     = R.convertImage img :: Img RGB
  imgAsText   = surjectionP $ imgData repaImg
  (_ : w : _) = reverse $ listOfShape $ extent $ imgData repaImg

-- parallel image to ascii conversion using REPA
surjectionP :: Array D DIM3 Word8 -> Array U DIM2 Char
surjectionP pixels = runIdentity $ computeP a
 where
  (height : width : _) = reverse $ listOfShape $ extent pixels
  a                    = fromFunction (Z :. height :. width) toGray
  toGray               = \(Z :. i :. j) ->
    let r = fromIntegral $ pixels ! (Z :. i :. j :. 0)
    in  let g = fromIntegral $ pixels ! (Z :. i :. j :. 1)
        in  let b = fromIntegral $ pixels ! (Z :. i :. j :. 2)
            in  ramp `BC.index` (gray (r, g, b) `mod` 70)

-- | parMap + parMap version
-- convert image to ascii array, then convert to string
-- top level wrapper for surjectionIVar
convertIVar :: BL.ByteString -> String
convertIVar png =
  -- finish reading in file (convert from lazy to strict bytestring)
  let img = myReadPng png
  in  case img of
        (Right v) -> imgAsText
         where
          -- convert image to text
          imgAsText = surjectionIVar imgRGB
          -- represent each pixel with three bytes, one for R, G, and B
          imgRGB    = convertRGB8 v
        (Left err) -> "Read Error: " ++ err

-- parallel image to ascii conversion using parmap on gray pixels
surjectionIVar :: Image PixelRGB8 -> String
surjectionIVar img =
 -- separate each row of characters with a newline
                     L.intercalate "\n" $ chunksOf (imageWidth img) chars
 where
   -- convert pixels to ascii characters in parallel
  chars   = runPar $ toChar `parMap` V.toList pixels
  toChar  = \px -> ramp `BC.index` (fromIntegral px `mod` 70)
  pixels  = imageData grayImg
  -- convert color pixels to gray pixels sequentially
  grayImg = pixelMap toGray img
  toGray :: PixelRGB8 -> Pixel8
  toGray (PixelRGB8 r g b) =
    round
      $ 0.2989
      * fromIntegral r
      + 0.5870
      * fromIntegral g
      + 0.1140
      * fromIntegral b

-- | Parallel Reading Helper Functions 

-- Convert Lazy ByteString to Image
readLazyImg :: BL.ByteString -> Either String (Image PixelRGB8)
readLazyImg png =
    -- finish reading in file (convert from lazy to strict bytestring)
  let img = myReadPng png
  in  case img of
          -- convert file to Juicy Pixel image
        (Right v  ) -> Right (convertRGB8 v)
        (Left  err) -> Left ("Read Error: " ++ err)

myReadPng :: BL.ByteString -> Either String DynamicImage
myReadPng = myWithImageDecoder J.decodeImage

myWithImageDecoder
  :: (NFData a)
  => (B.ByteString -> Either String a)
  -> BL.ByteString
  -> Either String a
myWithImageDecoder decoder path = decoder $ BL.toStrict path


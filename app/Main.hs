module Main where
import           Data.List                      ( sort
                                                , intercalate )
import           Lib                            ( convert)
import           System.Directory               ( listDirectory )
import           System.Environment             ( getArgs
                                                , getProgName
                                                )
import           System.Exit                    ( die ) --someFunc
import qualified Control.Exception as Exc ( catch, IOException )
import qualified Data.ByteString.Lazy as BL
--import System.Console.ANSI
import Control.Monad.Par(parMap, runPar)
--import Control.Parallel.Strategies hiding (parMap)
--import Control.Monad (join)
--import Data.Bits (Bits(xor))
import  Control.Parallel.Strategies(using,  parList,  rdeepseq)
import           Data.Array.Repa         hiding ( (++) )
import           Data.Functor.Identity
import           Data.List.Split (chunksOf)
--listDirectory :: FilePath -> IO [FilePath]
--https://hackage.haskell.org/package/JuicyPixels-repa-0.7.1.0/docs/Codec-Picture-Repa.html


-- import Graphics.Text.TrueType( loadFontFile )
-- import Codec.Picture( PixelRGBA8( .. ), writePng )
-- import Graphics.Rasterific
-- import Graphics.Rasterific.Texture

-- main :: IO ()
-- main = do
--   fontErr <- loadFontFile "fonts/RobotoMono-Regular.ttf"
--   case fontErr of
--     Left err -> putStrLn err
--     Right font ->
--       writePng "text_example.png" .
--           renderDrawing 300 140 (PixelRGBA8 255 255 255 255)
--               . withTexture (uniformTexture $ PixelRGBA8 0 0 0 255) $
--               do
--                       printTextAt font (PointSize 12) (V2 20 40)
--                            "A simple text test!\n hoo hoo"
--                       printTextAt font (PointSize 12) (V2 20 80)
--                            "A simple text test!\n hoo hoo"









main :: IO ()
main = do
    args <- getArgs
    case args of
        [dir, "p"] -> do -- parallel
            files <- listDirectory dir
            --mapM_ putStrLn (sort files)
            if null $ sort files
                then
                    die
                    $  "Error: "
                    ++ dir
                    ++ " must contain at least one png file"
                else do
                   -- converted <- ((convert True. (\x -> concat [dir, "/", x]))) `parMap` files
                      --let lazyFiles = (\x -> L.readFile $ concat [dir, "/", x]) <$> files
                      lazyFrames <- rewrap $ (\x -> BL.readFile $ concat [dir, "/", x]) <$> files
                     -- let test = fromFunction (Z :. length lazyFrames) (fromListLazy lazyFrames)
                      -- let test = fromListUnboxed (Z :. (length prepFiles)) prepFiles -- no dice here
                      --let converted = runPar $ (convert True) `parMap` lazyFrames   
                    --  let converted = runIdentity $ computeP (convertAnimation test (480,343)) :: Array U DIM3 Char 
                    --  let asText = toSingleString `parMap` (toList converted)        
                      -- mapM_  putStrLn converted
                      --let test = convert True <$> lazyFrames
                      let converted = runPar $ (convert True) `parMap` lazyFrames
                      mapM_ putStrLn $ converted
                    -- a <- head converted
                    -- b <- converted !!
                    --  mapM_ putStrLn (toStrings (length lazyFrames,480) converted)
        [dir] -> do -- sequential
            files <- listDirectory dir
            if null $ sort files
                then
                    die
                    $  "Error: "
                    ++ dir
                    ++ " must contain at least one png file"
                else do
                      let converted = (\x-> do 
                                            y <- BL.readFile $ concat [dir, "/", x]
                                            return $ convert True y) <$> files                      
                      mapM_ (\x -> (do
                                    str <- x
                                    putStrLn  str)) converted
        _ -> do
            pn <- getProgName
            die $ "Usage: " ++ pn ++ " <directory path> <optional p flag>"
            
rewrap :: [IO BL.ByteString] -> IO [BL.ByteString]
rewrap fls = foldr combine (return []::(IO [BL.ByteString])) fls
  where
combine :: IO BL.ByteString -> IO [BL.ByteString] -> IO [BL.ByteString]
combine x acc = do
                       thing <- x
                       list <- acc
                       return (thing:list)

--(Z :. i :. j)
--func :: [L.ByteString] -> Array D DIM1 L.ByteString 
fromListLazy :: [BL.ByteString] -> DIM1 ->  BL.ByteString 
fromListLazy files (Z :. i) = files !! i

toStrings :: (Int,Int) -> Array U DIM3 Char -> [String]
toStrings (sz,w) chars = 
   intercalate ["\n"] $ chunksOf w <$> chunksOf sz $ toList chars 
   --  intercalate "\n" $ chunksOf w 



-- toStrings :: (Int,Int,Int) -> Array D DIM3 Char -> String
-- toStrings sz chars = toList $ fromFunction (Z:.sz) (\(Z:.i) -> toSingleString (chars ! i))
--  where toSingleString :: Array D DIM2 Char -> String
--        toSingleString chars = intercalate "\n" $ chunksOf w $ toList chars
--          where [_,w] = reverse $ listOfShape $ extent chars
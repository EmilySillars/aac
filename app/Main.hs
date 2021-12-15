module Main where
import           Data.List                      ( sort )
import           Lib                            ( convert )
import           System.Directory               ( listDirectory )
import           System.Environment             ( getArgs
                                                , getProgName
                                                )
import           System.Exit                    ( die ) --someFunc
import qualified Control.Exception as Exc ( catch, IOException )
import qualified Data.ByteString.Lazy as L
--import System.Console.ANSI
import Control.Monad.Par(parMap, runPar)
--import Control.Parallel.Strategies hiding (parMap)
--import Control.Monad (join)
--import Data.Bits (Bits(xor))
import  Control.Parallel.Strategies(using,  parList,  rdeepseq)
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
            mapM_ putStrLn (sort files)
            if null $ sort files
                then
                    die
                    $  "Error: "
                    ++ dir
                    ++ " must contain at least one png file"
                else do
                   -- converted <- ((convert True. (\x -> concat [dir, "/", x]))) `parMap` files
                      let lazyFiles = (\x -> L.readFile $ concat [dir, "/", x]) <$> files
                      prepFiles <- prep lazyFiles
                      let converted = runPar $ (convert True) `parMap` prepFiles                   
                      -- mapM_  putStrLn converted
                    -- a <- head converted
                    -- b <- converted !!
                      mapM_ (putStrLn) converted
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
                                            y <- L.readFile $ concat [dir, "/", x]
                                            return $ convert True y) <$> files                      
                      mapM_ (\x -> (do
                                    str <- x
                                    putStrLn  str)) converted
        _ -> do
            pn <- getProgName
            die $ "Usage: " ++ pn ++ " <directory path> <optional p flag>"
            
prep :: [IO L.ByteString] -> IO [L.ByteString]
prep fls = foldr combine (return []::(IO [L.ByteString])) fls
  where
combine :: IO L.ByteString -> IO [L.ByteString] -> IO [L.ByteString]
combine x acc = do
                       thing <- x
                       list <- acc
                       return (thing:list)
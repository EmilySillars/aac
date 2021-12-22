module Main where
import           Data.List                      ( sort )
import           Lib                            (  convertS
                                                 , convertIVar
                                                 , convertRepa
                                                 , readLazyImg)
import           System.Directory               ( listDirectory )
import           System.Environment             ( getArgs
                                                , getProgName
                                                )
import           System.Exit                    ( die ) 
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Par(parMap, runPar)

main :: IO ()
main = do
    args <- getArgs
    case args of 
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
                                            convertS $ concat [dir, "/", x]
                                            ) <$> files                      
                      mapM_ (\x -> (do
                                    str <- x
                                    putStrLn  str)) converted
        [dir, "-repa-par-read"] 
         -> do -- parallel read
            files <- listDirectory dir
            if null $ sort files
                then
                    die
                    $  "Error: "
                    ++ dir
                    ++ " must contain at least one png file"
                else do
                      -- read in part of each file sequentially
                      lazyFrames <- rewrap $ (\x -> BL.readFile $ concat [dir, "/", x]) <$> files
                      let frames = runPar $ readLazyImg `parMap` lazyFrames
                      -- convert files sequentially
                      -- with each image's pixel conversions in parallel  
                      let converted = convertRepa <$> frames 
                       -- print result sequentially
                      mapM_ putStrLn converted
        [dir, "-ivars"]
         -> do
            files <- listDirectory dir
            if null $ sort files
                then
                    die
                    $  "Error: "
                    ++ dir
                    ++ " must contain at least one png file"
                else do
                      -- read in part of each file sequentially
                      lazyFrames <- rewrap $ (\x -> BL.readFile $ concat [dir, "/", x]) <$> files
                      -- convert each file in parallel
                      let converted = runPar $ convertIVar `parMap` lazyFrames
                      -- print result sequentially
                      mapM_ putStrLn converted
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
 
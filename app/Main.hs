module Main where
import Data.List(sort)
import System.Exit(die)
import System.Environment(getArgs, getProgName)
import System.Directory(listDirectory)
import Lib(convert) --someFunc

--listDirectory :: FilePath -> IO [FilePath]
--https://hackage.haskell.org/package/JuicyPixels-repa-0.7.1.0/docs/Codec-Picture-Repa.html

main :: IO ()
main = do args <- getArgs
          case args of 
           [dir] -> do
              files <- listDirectory dir
              -- mapM_ putStrLn $ sort files
              if null $sort files 
              then die $ "Error: " ++ dir ++ " must contain at least one png file"
              else do
               let fpath = concat [dir,"/",minimum files]
               theFile <- convert $ fpath -- one file for now
               putStrLn theFile
           _ -> do pn <- getProgName
                   die $ "Usage: " ++ pn ++ " <directory path>"

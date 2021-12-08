module Main where
import Data.List(sort)
import System.Exit(die)
import System.Environment(getArgs, getProgName)
import System.Directory(listDirectory)
import Lib(convert) --someFunc

--listDirectory :: FilePath -> IO [FilePath]

main :: IO ()
main = do args <- getArgs
          case args of 
           [dir] -> do
              files <- listDirectory dir
              -- mapM_ putStrLn $ sort files
              if null $sort files 
              then die $ "Error: " ++ dir ++ " must contain at least one png file"
              else putStrLn <$> convert $ minimum files -- one file for now
           _ -> do pn <- getProgName
                   die $ "Usage: " ++ pn ++ " <directory path>"

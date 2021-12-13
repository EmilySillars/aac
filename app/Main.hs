module Main where
import           Data.List                      ( sort )
import           Lib                            ( convert )
import           System.Directory               ( listDirectory )
import           System.Environment             ( getArgs
                                                , getProgName
                                                )
import           System.Exit                    ( die ) --someFunc

--listDirectory :: FilePath -> IO [FilePath]
--https://hackage.haskell.org/package/JuicyPixels-repa-0.7.1.0/docs/Codec-Picture-Repa.html

main :: IO ()
main = do
    args <- getArgs
    case args of
        [dir, "p"] -> do -- parallel
            files <- listDirectory dir
            if null $ sort files
                then
                    die
                    $  "Error: "
                    ++ dir
                    ++ " must contain at least one png file"
                else do
                    let fpath = concat [dir, "/", minimum files]
                    theFile <- convert True fpath -- one file for now
                    putStrLn theFile
        [dir] -> do -- sequential
            files <- listDirectory dir
            if null $ sort files
                then
                    die
                    $  "Error: "
                    ++ dir
                    ++ " must contain at least one png file"
                else do
                    let fpath = concat [dir, "/", minimum files]
                    theFile <- convert False fpath -- one file for now
                    putStrLn theFile
        _ -> do
            pn <- getProgName
            die $ "Usage: " ++ pn ++ " <directory path> <optional p flag>"

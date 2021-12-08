module Lib
    ( convert
    ) where
import Codec.Picture
-- someFunc :: IO ()
-- someFunc = putStrLn "someFunc"
-- readPng :: FilePath -> IO (Either String DynamicImage)
{-Installing library in 
C:\sr\snapshots\d574fa6f\lib\x86_64-windows-ghc-8.10.7\JuicyPixels-3.3.5-1kvarO6VLos4MgV6H2geEO
-}
convert :: FilePath -> String
-- convert png = do
--     Right (ImageRGB8 v) <- readPng png
--     "(" ++ show (imageWidth v) ++ " x " ++ show (imageHeight v) ++ ")"
    
convert f = "I'm supposed to convert " ++ f ++ " to ASCII.\nhoodle noodle ho."

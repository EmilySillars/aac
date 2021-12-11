module Lib
    ( convert
    ) where
import Codec.Picture as J
import Codec.Picture.Repa as R
import Control.Monad
-- someFunc :: IO ()
-- someFunc = putStrLn "someFunc"
-- readPng :: FilePath -> IO (Either String DynamicImage)
{-Installing library in 
C:\sr\snapshots\d574fa6f\lib\x86_64-windows-ghc-8.10.7\JuicyPixels-3.3.5-1kvarO6VLos4MgV6H2geEO
-}
convert :: FilePath -> IO String
convert png = 
    do img <- J.readImage png
       case img of
         (Right v) 
          -> return $ "(" ++ show (imageWidth imgRGB) ++ " x " ++ show (imageHeight imgRGB) ++ ")"
             where imgRGB = convertRGB16 v
         (Left err)
          -> return $ "Read Error: " ++ err
  
-- convert png = do
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







-}

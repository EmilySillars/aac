# AAC: ASCII Animation Converter

Given a directory name, this program will convert every png image in a folder to an ASCII graphics equivalent, and then output those ASCII equivalent images to the console as frames in an animation.

Sources:

- https://www.geeksforgeeks.org/converting-image-ascii-image-python/ 

- https://codereviewstackexchange.com/questions/263823/haskell-convert-an-image-to-ascii-art

- https://hackage.haskell.org/package/JuicyPixels-3.3.6/docs/Codec-Picture.html

- https://hackage.haskell.org/package/rainbow-0.4.0.0/docs/System-Console-Rainbow.html

- https://www.jonathan-petitcolas.com/2017/12/28/converting-image-to-ascii-art.html

- http://paulbourke.net/dataformats/asciiart/

- https://stackoverflow.com/questions/3232074/what-is-the-best-way-to-convert-string-to-bytestring

Example animation:

- https://media.giphy.com/media/u2Tll62t3t52PGCk0u/giphy.gif

  

Building:

    stack build
    
    stack exec aac-exe maya_rudolph_frames
    
    stack run
    
    stack install

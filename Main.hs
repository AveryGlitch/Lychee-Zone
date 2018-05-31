module Main where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
  want ["_build/index.html"]

  "_build/index.html" %> \out -> do
      let src = "site" </> dropDirectory1 out
      need [src]
      cmd_ "cp" [src] [out]

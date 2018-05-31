module Main where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

indexes :: [FilePath] -> [FilePath]
indexes = map (\file -> "_site" </> file </> "index.html")

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do

  want (indexes ["", "about", "languages/dapiica"])

  "_site/index.html" %> \out -> do
      let src = "src" </> dropDirectory1 out
      need [src]
      cmd_ "cp" [src] [out]

  "//*index.html" %> \out -> do
      let src = "src" </> getMiddle out <.> "md"
      need [src]
      cmd_ "mkdir -p" (takeDirectory out)
      cmd_ "pandoc --standalone -o" [out] [src]

  -- "_site/*/index.html" %> \out -> do
  --     let src = "src" </> getMiddle out <.> "md"
  --     need [src]
  --     cmd_ "mkdir -p" (takeDirectory out)
  --     cmd_ "pandoc --standalone -o" out [src]

  -- "_site/languages/dapiica/index.html" %> \out -> do
  --     cmd_ "mkdir -p" (takeDirectory out)
  --     cmd_ "echo fake" (">" ++ out)



getMiddle string = dropDirectory1 $ takeDirectory string

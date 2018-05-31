module Main where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

indexes :: [FilePath] -> [FilePath]
indexes = map (\file -> "_site" </> file </> "index.html")

headerGeneric :: FilePath
headerGeneric = "src/header.html"



main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do

  want (indexes ["", "about", "languages/dapiica", "doctor-who"])
  want ["_site/style.css"]
  want ["_site/doctor-who/style.css"]

  phony "clean" $ do
      putNormal "Cleaning files from _site and _build"
      removeFilesAfter "_site" ["//*"]
      removeFilesAfter "_build" ["//*"]

  "//*index.html" %> \out -> do
      case out of
        "_site/doctor-who/index.html"
          -> do let db = "src/dw/DrWhoDB"
                need [db]
                cmd_ "mkdir -p" (takeDirectory out)
                command_ [Cwd "src/dw"] "runhaskell" ["DrWho.hs", "output", "../../" ++ out]
        _
          -> do let src = case getMiddle out of
                            "" -> "src/index.md"
                            name -> "src" </> name <.> "md"
                let navbar = "_build" </> getMiddle out </> "header.html"
                let css = generatePrefix (takeDirectory out) ++ "style.css"
                need [src, navbar]
                cmd_ "mkdir -p" (takeDirectory out)
                cmd_ "pandoc --standalone -B" [navbar] "--css" [css] "-o" [out] [src]

  "//*header.html" %> \out -> do
      let prefix = generatePrefix (takeDirectory out)
      cmd_ "mkdir -p" (takeDirectory out)
      writeFile' out (header prefix)

  "_site/style.css" %> \out -> do
      let css = "src/style.css"
      copyFile' css out

  "_site/doctor-who/style.css" %> \out -> do
      let css = "src/dw/style.css"
      copyFile' css out



getMiddle :: FilePath -> FilePath
getMiddle string = dropDirectory1 $ takeDirectory string

countLayers s = length (filter (=='/') s)

generatePrefix s = generatePrefix' (countLayers s)
  where
    generatePrefix' 0 = "./"
    generatePrefix' n = concat $ replicate n "../"

header prefix = "<div id=\"header\">\n"
                ++ "<div id=navigation>\n"
                ++ "<a href=\"" ++ prefix ++ "\">Home</a>\n"
                ++ "<a href=\"" ++ prefix ++ "languages/dapiica\">Dapiica</a>\n"
                ++ "<a href=\"" ++ prefix ++ "about\">About</a>\n"
                ++ "</div></div>\n"

module Main where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

indexes :: [FilePath] -> [FilePath]
indexes = map (\file -> "_site" </> file </> "index.html")

csses :: [FilePath] -> [FilePath]
csses = map (\file -> "_site" </> file <.> "css")


main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do

  want (indexes ["", "about", "articles", "languages/dapiica", "languages/tava", "doctor-who", "log"])
  want (csses ["style", "dapiica", "doctor-who/style"])
  want ["_site/images/marker.txt"]

  phony "push" $ do
      cmd_ "neocities push _site"

  phony "clean" $ do
      putNormal "Cleaning files from _site and _build"
      removeFilesAfter "_site" ["//*"]
      removeFilesAfter "_build" ["//*"]

  "//*index.html" %> \out -> do
      case out of
        "_site/doctor-who/index.html"
          -> do let db   = "src/dw/DrWhoDB"
                let code = "src/dw/DrWho.hs"
                need [db, code]
                mkdir (takeDirectory out)
                command_ [Cwd "src/dw"] "runhaskell" ["DrWho.hs", "output", "../../" ++ out]
        _
          -> do let src = case getMiddle out of
                            "" -> "src/index.md"
                            "log" -> "_build/logfile.html"
                            name -> "src" </> name <.> "md"
                let navbar = "_build" </> getMiddle out </> "header.html"
                let css = generatePrefix (takeDirectory out)
                          ++ if contains "languages/dapiica" (getMiddle out)
                             then "dapiica.css" else "style.css"
                dirExists <- doesDirectoryExist ("src" </> getMiddle out)
                (if dirExists
                  then do files <- getDirectoryFiles ("src" </> getMiddle out) ["//*md"]
                          let buildnames = map (\file -> "_site" </> getMiddle out </> dropExtension file </> "index.html") files
                          need buildnames
                  else pure ())
                need [src, navbar]
                mkdir (takeDirectory out)
                cmd_ "pandoc --standalone -B" [navbar] "--css" [css] "-o" [out] [src]

  "//*header.html" %> \out -> do
      let prefix = generatePrefix (takeDirectory out)
      mkdir (takeDirectory out)
      writeFile' out (header prefix)

  "_site/style.css" %> \out -> do
      let css = "src/style.css"
      copyFile' css out

  "_site/dapiica.css" %> \out -> do
      let css = "src/dapiica.css"
      copyFile' css out

  "_site/doctor-who/style.css" %> \out -> do
      let css = "src/dw/style.css"
      copyFile' css out

  "_site/archive-2017.zip" %> \out -> do
      let zip = "src/archive-2017.zip"
      copyFile' zip out

  "_build/logfile.html" %> \out -> do
      logs <- getDirectoryFiles "src/log" ["//*.md"]
      let builtLogs = map (\log -> "_build/log/" ++ (log -<.> "html")) logs
      need builtLogs
      cmd_ "pandoc --standalone --metadata=title:Log -o" out (reverse builtLogs)
      -- cmd_ "pandoc --file-scope --template" [template] "-o" [out] logs

  "_site/images/marker.txt" %> \out -> do
      mkdir (takeDirectory out)
      images <- getDirectoryFiles "src/images" ["//*"]
      mapM_ (\img -> copyFile' ("src/images" </> img) (takeDirectory out </> img)) images
      writeFile' out markerText

  (\filepath -> (contains "_build/log" filepath) && (not $ contains "header" filepath)) ?> \out -> do
      let src = "src" </> dropDirectory1 out -<.> "md"
      let template = "src/log.template"
      let date = dropExtension $ takeBaseName out
      need [src, template]
      cmd_ "pandoc --template" [template] "-o" [out] ("--metadata=date:" ++ date) src



getMiddle :: FilePath -> FilePath
getMiddle string = dropDirectory1 $ takeDirectory string

countLayers s = length (filter (=='/') s)

generatePrefix s = generatePrefix' (countLayers s)
  where
    generatePrefix' 0 = "./"
    generatePrefix' n = concat $ replicate n "../"

header prefix = "<div id=\"header\">\n"
                ++ "<div id=navigation>\n"
                ++ link headerStyle (prefix) "Home"
                ++ "\n"
                ++ link headerStyle (prefix ++ "doctor-who") "Doctor&nbsp;Who&nbsp;Guide"
                ++ "\n"
                ++ link headerStyle (prefix ++ "languages/dapiica") "Dapiica"
                ++ "\n"
                ++ link headerStyle (prefix ++ "about") "About"
                ++ "\n"
                ++ link headerStyle (prefix ++ "log") "Log"
                ++ "\n"
                ++ "</div></div>\n"
  where headerStyle = "headerlink"


link style url name = "<a class=\"" ++ style ++ "\" href=\"" ++ url ++ "\">" ++ name ++ "</a>"

contains (x:xs) [] = False
contains xs ys
  | prefix xs ys = True
  | contains xs (tail ys) = True
  | otherwise = False
  where
    prefix []     ys     = True
    prefix (x:xs) []     = False
    prefix (x:xs) (y:ys) = (x == y) && prefix xs ys


mkdir = cmd_ "mkdir -p"

markerText = "This file only exists to satisfy the build system"

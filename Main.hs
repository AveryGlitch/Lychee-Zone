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

  want (indexes ["", "about", "languages/dapiica", "doctor-who", "log"])
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
                            "log" -> "_build/logfile.html"
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

  "_build/logfile.html" %> \out -> do
      logs <- getDirectoryFiles "src/log" ["//*.md"]
      let builtLogs = map (\log -> "_build/log/" ++ (log -<.> "html")) logs
      need builtLogs
      cmd_ "pandoc --standalone --metadata=title:Log -o" out (reverse builtLogs)
      -- cmd_ "pandoc --file-scope --template" [template] "-o" [out] logs

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
                ++ link headerStyle (prefix ++ "languages/dapiica") "Dapiica"
                ++ link headerStyle (prefix ++ "doctor-who") "Doctor Who Guide"
                ++ link headerStyle (prefix ++ "about") "About"
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

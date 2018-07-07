{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
module DrWho where

import System.Environment (getArgs)
import System.Directory (renameFile)
import Prelude hiding (div)
import qualified Data.Yaml as Yaml
import qualified Data.ByteString as ByteString
import GHC.Generics
import System.IO (hSetBuffering, stdin, stdout, BufferMode (..))


-- First some types to make signatures more readable
type Name = String
type Note = String
type Synopsis = String
type Review = String
type NumEpisodes = Int
type Number = Int
type DoctorNum = Int
type SeasonNum = String


-- Our Table is just a list of Doctors...
type Table  = [Doctor]
-- A Doctor is just a number and a list of seasons...
data Doctor = Doctor
  {
    doctorNum :: DoctorNum
  , seasons   :: [Season]
  } deriving (Generic, Show, Read)
-- A Season is just a season number* and a list of stories
data Season = Season
  {
    seasonNum :: SeasonNum
  , stories   :: [Story]
  } deriving (Generic, Show, Read)
-- A story contains a bunch of info!
data Story  = Story
  {
    name           :: Name
  , number         :: Number
  , numEpisodes    :: NumEpisodes
  , missing        :: Missing
  , reccomendation :: Recommendation
  , note           :: (Maybe Note)
  , synopsis       :: Synopsis
  , review         :: Review
} deriving (Generic, Show, Read)
-- A story is either missing no episodes, all episodes, or some specific episodes
data Missing = None
             | All
             | Some [Int] deriving (Generic, Show, Read, Eq)
-- Our recommendations
data Recommendation = Highly | Yes | Maybe | Partial | No deriving (Generic, Show, Read)

instance Yaml.ToJSON Doctor
instance Yaml.ToJSON Season
instance Yaml.ToJSON Story
instance Yaml.ToJSON Missing
instance Yaml.ToJSON Recommendation

instance Yaml.FromJSON Doctor
instance Yaml.FromJSON Season
instance Yaml.FromJSON Story
instance Yaml.FromJSON Missing
instance Yaml.FromJSON Recommendation


-- an empty table.
emptyTable :: Table
emptyTable = []


-- | Checks if a certain doctor is in the table
hasDoctor :: DoctorNum -> Table -> Bool
hasDoctor _ [] = False
hasDoctor n (Doctor n' _ : rest) = n == n' || hasDoctor n rest


-- | Checks if a certain season is in the table
hasSeason :: SeasonNum -> Table -> Bool
hasSeason _ [] = False
hasSeason n (Doctor _ seasons : rest) = any (\(Season n' _) -> n == n') seasons || hasSeason n rest


-- | Tells you what the last doctor is (by number)
getLastDoctor :: Table -> DoctorNum
getLastDoctor [] = 0
getLastDoctor table = case last table of
                        Doctor n _ -> n


-- | Adds another Doctor to the table
addDoctor :: Table -> Table
addDoctor table = let nextDoctor = getLastDoctor table + 1
                  in table ++ [Doctor nextDoctor []]


-- | Adds a new season to the table. Requires both a doctor number and a season number
addSeason :: DoctorNum -> SeasonNum -> Table -> Maybe Table
addSeason doctorNum seasonNum table = if hasDoctor doctorNum table then Just (addSeason' doctorNum seasonNum table)
                                                                   else Nothing
  where
    addSeason' :: DoctorNum -> SeasonNum -> Table -> Table
    addSeason' _ _ [] = error "impossible"
    addSeason' doctorNum seasonNum (Doctor n seasons : rest)
      = if n == doctorNum
        then Doctor n (seasons ++ [Season seasonNum []]) : rest
        else Doctor n seasons : addSeason' doctorNum seasonNum rest


-- | Adds a story to a specific season in the table
addStory :: Story -> SeasonNum -> Table -> Maybe Table
addStory story season table = if hasSeason season table then Just (addStory' story season table)
                                                        else Nothing
  where
    addStory' story season [] = error "impossible"
    addStory' story season (Doctor n seasons : rest)
      = if any (\s -> case s of Season n' _ -> season == n') seasons
        then (Doctor n (addToSeason story season seasons)) : rest
        else Doctor n seasons : addStory' story season rest
    addToSeason story season [] = error "impossible"
    addToSeason story season (Season sn stories : rest)
      | sn == season = Season sn (stories ++ [story]) : rest
      | otherwise    = Season sn stories : (addToSeason story season rest)



--------------------------------------------------------------------------------
-- Output functions
--------------------------------------------------------------------------------
preamble, postamble, tableHeading :: String
preamble = "<html lang=\"en\"><head><title>Avery's Doctor Who Guide</title><meta charset=\"utf-8\" /><link rel=\"stylesheet\" href=\"style.css\"></head><body>"
postamble = "</body></html>"
tableHeading = "<table class=\"maintable\"><tr><th>Story</th><th>Watch?</th><th>Details</th></tr>\n"

output :: Table -> String
output table = preamble +. introduction +. output' table +. outro +. postamble
  where
    output' [] = ""
    output' (Doctor n seasons : rest)
      = "<a name=\"doctor" ++ show n ++ "\"></a>"
      +. h1 (ordinal n ++ " Doctor") ++ img ("../images/doctor-who/doctor" ++ show n ++ ".png") ("The " ++ ordinal n ++ " Doctor")
      +. if length rest > 0 then "<span style=\"font-size: small; text-align: right\">" ++ a ("#doctor" ++ show (n + 1)) " ↩ next doctor" ++ "</span>" else ""
      +. tableHeading +. concatMap outputSeason seasons +. "</table>"
      +. output' rest

outputSeason :: Season -> String
outputSeason (Season num stories)
  = tr' "season" ("<td colspan=3>Season " ++ num ++ "</td>")
    +. concatMap outputStory stories

outputStory :: Story -> String
outputStory (Story name number numEps missing recc note synopsis review)
  = tr' "name"
    (td' (if missing == None then "name" else "name-missing")
        ("<p class="
         ++ (if missing == None then "name" else "name-missing")
         ++ ">" ++ name ++ "</p>"
         ++ (if missing /= None then div "reconstruction" "Reconstruction" else "")
         +. "<table>"
         +. tr' "info" (td "Story Number" ++ td (show number))
         +. tr' "info" (td "Number of Episodes" ++ td (show numEps))
         +. case missing of
              None -> ""
              All  -> tr' "info" (td "Missing Episodes?" ++ td "Yes: all")
              Some eps -> tr' "info" (td "Missing Episodes?" ++ td ("Yes: " ++ showEps eps))
         +. "</table>\n")
    +. td (div (show recc)
            (
              (case recc of
                  Highly -> "✨ Highly Recommended ✨"
                  Yes    -> "Watch"
                  Maybe  -> "Maybe"
                  Partial-> "Partial watch"
                  No     -> "Don't watch"
              )
              ++ (case note of
                     Just text -> ", " ++ text
                     Nothing   -> ""
                 )
            )
          )
    +. td ("<table class=details>"
           +. tr' "details" (td' "details-tag" "Synopsis" ++ td' "details-text" synopsis)
           +. tr' "details" (td' "details-tag" "Review"   ++ td' "details-text" review)
           +. "</table>"))
    ++ "\n"

showEps :: [Int] -> String
showEps [] = "none"
showEps [e] = show e
showEps [e1, e2] = show e1 ++ " and " ++ show e2
showEps (e:rest) = show e ++ ", " ++ showEps rest

ordinal :: Int -> String
ordinal n = case n of
              1  -> "First"
              2  -> "Second"
              3  -> "Third"
              4  -> "Fourth"
              5  -> "Fifth"
              6  -> "Sixth"
              7  -> "Seventh"
              8  -> "Eighth"
              9  -> "Nineth"
              10 -> "Tenth"
              11 -> "Eleventh"
              12 -> "Twelfth"
              13 -> "Thirteenth"
              14 -> "Fourteenth"
              15 -> "Fifteenth"
              16 -> "Sixteenth"
              17 -> "Seventeenth"
              18 -> "Eighteenth"
              19 -> "Nineteenth"
              _  -> error "I haven't accounted for this many doctors"

toc :: Table -> String
toc [] = ""
toc table = h1 "Table of Contents"
            +. "<ol>" +. toc' table +. "</ol>"
            +. "<hr>"
  where
    toc' [] = ""
    toc' (Doctor n _ : rest) = li $ a ("#doctor" ++ show n) (ordinal n ++ " Doctor")

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------



-- Commands that can be run
data Command = Output | AddDoctor | AddSeason | AddStory | Usage

-- | Run a command on a table
run :: Command -> Maybe FilePath -> Table -> IO ()
run Usage _ _       = do args <- getArgs
                         putStrLn $ "did not recognise args: " ++ concat args
                         putStrLn "dw Output     : outputs the table as html"
                         putStrLn "dw add doctor : adds a new doctor to the table"
                         putStrLn "dw add season : adds a new season to the table (interactive)"
                         putStrLn "dw add story  : adds a new story to the table (interactive)"
                         putStrLn "dw edit story : edit a particular story"
                         putStrLn ""
                         putStrLn $ "The table is stored in " ++ file
run Output Nothing table           = putStrLn $ output table
run Output (Just outputFile) table = writeFile outputFile (output table)
run AddDoctor _ table = do writeOut $ addDoctor table
run AddSeason _ table = do doctor <- prompt "To doctor: "
                           season <- prompt "Season Number: "
                           let result = addSeason (read doctor) season table
                           case result of
                             Just newTable -> do writeOut newTable
                             Nothing -> print "couldn't add season!"
run AddStory _ table  = do season <- prompt "Season Number: "
                           name <- prompt "Name: "
                           number <- prompt "Number: "
                           numEpisodes <- prompt "Number of Episodes: "
                           missing <- prompt "Missing: "
                           recommendation <- prompt "Recommendation: "
                           note <- prompt "Note: "
                           synopsis <- prompt "Synopsis: "
                           review <- prompt "Review: "
                           let result = addStory (Story name (read number) (read numEpisodes) (read missing) (read recommendation) (readNote note) synopsis review) season table
                           case result of
                             Just newTable -> do writeOut newTable
                             Nothing       -> print "couldn't add story!"


realMain :: IO ()
realMain = do arg <- getArgs
              hSetBuffering stdin NoBuffering
              hSetBuffering stdout NoBuffering
              fileContent <- ByteString.readFile file
              let Just table = Yaml.decode fileContent
              case arg of
                ["output"]             -> run Output Nothing table
                ["output", outputFile] -> run Output (Just outputFile) table
                ["add", "doctor"]      -> run AddDoctor Nothing table
                ["add", "season"]      -> run AddSeason Nothing table
                ["add", "story"]       -> run AddStory Nothing table
                _                      -> run Usage Nothing table



-- | Prompt the user for a response
prompt :: String -> IO String
prompt text = putStr text >> getLine >>= return

file, tmpfile, backup :: FilePath
file = "DrWhoDB"
tmpfile = "DrWhoDB_tmp"
backup = "DrWhoDB.bak"


readNote :: String -> Maybe Note
readNote ""   = Nothing
readNote note = Just note


writeOut :: Table -> IO ()
writeOut table = do ByteString.writeFile tmpfile (Yaml.encode table)
                    renameFile file backup
                    renameFile tmpfile file

-- | Concats two strings, but puts a newline between them
(+.) :: String -> String -> String
x +. y = x ++ "\n" ++ y

h1, h2, h3, p, tr, td, li :: String -> String
h1  = simplehtml "h1"
h2  = simplehtml "h2"
h3  = simplehtml "h2"
p   = simplehtml "p"
tr  = simplehtml "tr"
td  = simplehtml "td"
li  = simplehtml "li"
tr', td', div :: String -> String -> String
tr' = styledhtml "tr"
td' = styledhtml "td"
div = styledhtml "div"

a :: String -> String -> String
a link name = "<a href=\"" ++ link ++ "\">" ++ name ++ "</a>"

img :: String -> String -> String
img url alt = "<img src=\"" ++ url ++ "\" alt=\"" ++ alt ++ "\">"


simplehtml :: String -> (String -> String)
simplehtml tag = \s -> "<" ++ tag ++ ">" ++ s ++ "</" ++ tag ++ ">"
styledhtml :: String -> String -> (String -> String)
styledhtml tag style = \s -> "<" ++ tag ++ " class=\"" ++ style ++ "\">"++ s ++ "</" ++ tag ++ ">"




introduction :: String
introduction
  = h1 "Avery's Doctor Who Guide"
    +. p "So, you want to watch Doctor Who, through the classic and modern era, but you're not so sure on how much to watch? You've come to the right place! This guide has several different tracks, depending on what you're interested in"
    +. "<table>"
    +. concatMap (tr' "intro")
    [
      (td' "Highly" "Fast Track"
           ++ td' "invisible" "The Highly recommended episodes. If you only want a small sampling of episodes, look here!")
    , (td' "Yes" "Recommended Track"
           +. td' "invisible" "For most people, you'll want to stick on the Recommended track - watch both the Fast track episodes and the recommended episodes (don't forget the partials, see the next section), and you'll get quite a lot of Doctor Who, without having to sit through the slower stuff.")
    , (td' "Maybe" "Maybe Track"
          +. td' "invisible" "If you're interested in a more thorough watch through, you can also watch the episodes on the maybe track. These aren't bad episodes by any right - they're just not neccessary to watch")
    , (td' "No" "Avoid"
          +. td' "invisible" "These episodes are only recommended if you're truly curious and dedicated.")
    ]
    +. "</table>"
    +. p "Additionally, some stories are marked as a <span class=Partial>partial watch</span> - this means you <strong>should</strong> watch it, but not all of it - just certain episodes."
    +. p "Many of the early episodes are missing. You will be able to tell which ones these are because the name of the story will be in italics, and it will be mentioned several times. These stories aren't unwatchable, surprisingly - reconstructions of the episodes have been made, and they are (relatively) watchable. If you don't want to watch the reconstuctions, though (and I don't blame you), they are easy to skip."
    +. p "\"Wait, but what if I want to watch <strong>everything</strong>?\" go ahead! There's nothing stopping you. But this guide is for people who want a more selective sampling of the series, or for those who will watch every episode, you can use this guide as a litmus test."
    +. p "This guide is currently a work in progress, and only goes as far as I've watched so far. I started watching through the episodes for this guide in early May 2018, and I'm still going strong."
    +. div "dimbox" (
      h3 "Important note"
      +. p "The early doctor who episodes are <i>excruciatingly</i> slow compared to what we see on modern TV, so I've judged them less harshly on pacing. <strong>I won't blame you for skipping the black and white episodes</strong>"
      +. p "I think my ratings for the black and white episodes are also the ones which have generated the most controversy - fans want me to rate more of them higher, average people think I should rate more of them lower."
      )
    +. "<hr>"

outro :: String
outro = "<hr>"
        +. div "dimbox" (
          h3 "Acknowledgements"
          +. p "Thanks to:"
          +. "<ul>"
          +. li (a "https://mastodon.social/@The_T" "@The_T@mastodon.social" ++ " for convincing me to upgrade the recommendations for The Aztecs, The Sensorites, and The Reign of Terror; as well as downgrading The Edge of Destruction")
          +. li (a "https://computerfairi.es/@nezumi" "@nezumi@computerfairi.es" ++ " for making the downgrade of The Edge of Destruction more solid, by pointing out how the plot contrivances make everyone act out of character")
          +. li (a "https://wandering.shop/@DialMForMara" "@DialMForMara@wandering.shop" ++ " for convincing me to review the reconstructions as well")
          +. li "And a bunch of others on the fediverse for helping me make the colourscheme in this document less garish."
          +. "</ul>"
          +. p ("This guide was not created manually, but was (somewhat) automated with a program I made one afternoon. You can find the source for it " ++  a "https://notabug.org/AveryLychee/Doctor-Who-Guide" "on NotABug")
        )
        +. div "return" (p $ a "../" "Return Home")



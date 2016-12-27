{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative ((<$>))
import           Control.Monad       (forM)
import           Control.Monad.State (StateT, evalStateT, state)
import           Control.Monad.Trans (lift)
import           Data.Char           (isSpace)
import           Data.List           (sort, sortBy)
import qualified Data.Map            as M
import           Data.Maybe          (mapMaybe)
import           Data.Ord            (comparing)
import           System.Directory    (doesDirectoryExist, getDirectoryContents,
                                      getHomeDirectory)
import           System.Environment  (getEnv)
import           System.Environment  (getArgs)
import           System.Exit         (ExitCode (..), exitFailure)
import           System.FilePath     (takeBaseName, (</>))
import           System.IO           (hPutStrLn, stderr)
import           System.Process      (rawSystem, readProcess)
import           Text.Printf         (printf)
import           Text.Read           (readMaybe)
import qualified Text.XML.Light      as Xml

--------------------------------------------------------------------------------
-- Openbox menu definition

type Menu = [MenuElement]

data MenuElement
    = Item      String String         -- ^ Label, command
    | Separator
    | Label     String
    | PipeMenu  String String String  -- ^ ID, label, command
    | SubMenu   String String Menu    -- ^ ID, label, submenu
    deriving (Show)

-- | Mostly used to get the width so we can align stuff.
menuElementLabel :: MenuElement -> String
menuElementLabel (Item     lbl _)   = lbl
menuElementLabel Separator          = ""
menuElementLabel (Label    lbl)     = lbl
menuElementLabel (PipeMenu _ lbl _) = lbl
menuElementLabel (SubMenu  _ lbl _) = lbl

--------------------------------------------------------------------------------
-- XML Rendering

menuToXml :: Menu -> Xml.Element
menuToXml = Xml.unode "openbox_pipe_menu" . map menuItemToXml
  where
    uattr k v = Xml.Attr (Xml.unqual k) v

    menuItemToXml (Label lbl) = Xml.unode "separator" [uattr "label" lbl]
    menuItemToXml Separator = Xml.unode "separator" ""
    menuItemToXml (Item lbl cmd) =
        Xml.unode "item" $
            ( [uattr "label" lbl]
            , Xml.unode "action"
                ( [uattr "name" "Execute"]
                , Xml.unode "command" cmd
                )
            )
    menuItemToXml (PipeMenu id' lbl cmd) = Xml.unode "menu"
        [uattr "id" id', uattr "label" lbl, uattr "execute" cmd]
    menuItemToXml (SubMenu id' lbl menu) = Xml.unode "menu"
        ( [uattr "id" id', uattr "label" lbl]
        , map menuItemToXml menu
        )

--------------------------------------------------------------------------------
-- Mini parser

type MiniParser a = StateT String Maybe a

runMiniParser :: MiniParser a -> String -> Maybe a
runMiniParser = evalStateT

parseRead :: Read a => MiniParser a
parseRead = do
    word <- parseWord
    lift $ readMaybe word

parseWord :: MiniParser String
parseWord = state $ \str ->
    let (w, remainder) = break isSpace (dropWhile isSpace str)
    in (w, dropWhile isSpace remainder)

parseRemainder :: MiniParser String
parseRemainder = state $ \str -> (str, "")

--------------------------------------------------------------------------------
-- General code for a gauge menu

makeGaugeMenu :: MenuElement -> Float -> (Int -> String) -> Menu
makeGaugeMenu title current makeCommand = (header ++) $ do
    level <- [0, 10 .. 100 :: Int]
    let indicator = if level == rounded then "o" else " "
    return $ Item (printf "%s%s%3d" indicator spaces level) (makeCommand level)
  where
    rounded = round (current / 10) * 10
    width   = length $ menuElementLabel title
    spaces  = replicate (max 1 $ width - 1 - 3) ' '

    header  = case title of
        Label _ -> [title]
        _       -> [title, Separator]

--------------------------------------------------------------------------------
-- Brightness menu

makeBrightnessMenu :: IO Menu
makeBrightnessMenu = do
    current <- read <$> readProcess "xbacklight" ["-get"] "" :: IO Float
    return $ makeGaugeMenu
        (Label "brightness") current (printf "xbacklight -set %d%%")

--------------------------------------------------------------------------------
-- Sound menu

makeVolumeMenu :: IO Menu
makeVolumeMenu = do
    current   <- read <$> readProcess "volume" ["get"] "" :: IO Float
    return $ makeGaugeMenu
        (Item "pavucontrol" "pavucontrol") current (printf "volume set %d")

--------------------------------------------------------------------------------
-- Rhythmbox menu

makeRhythmboxMenu :: IO Menu
makeRhythmboxMenu = do
    isRunningCode <- rawSystem "rhythmbox-client" ["--check-running"]
    case isRunningCode of
        ExitFailure _ -> return
            [ Label "not running"
            , Item "launch" "rhythmbox-client"
            ]
        ExitSuccess   -> do
            title  <- readPlaying "%tt"
            artist <- readPlaying "%ta"
            time   <- readPlaying "%te / %td"
            return $ case all isSpace title of
                True  ->
                    [ Label "not playing"
                    , Item "launch" "rhythmbox-client"
                    ]
                False ->
                    [ Label title
                    , Label artist
                    , Label time
                    , Item "launch" "rhythmbox-client"
                    , Item "next" "rhythmbox-client --next"
                    , Item "previous" "rhythmbox-client --previous"
                    ]
  where
    readPlaying code = readProcess "rhythmbox-client"
        ["--no-start", "--print-playing-format", code] ""

--------------------------------------------------------------------------------
-- Process menu

data Process = Process
    { processCommand :: String
    , processCpu     :: Float
    , processVsz     :: Int
    , processRss     :: Int
    , processTime    :: String
    } deriving (Show)

addProcess :: Process -> Process -> Process
addProcess p1 p2 = p1
    { processCpu  = processCpu p1  +     processCpu p2
    , processVsz  = processVsz p1  +     processVsz p2
    , processRss  = processRss p1  +     processRss p2
    , processTime = processTime p1 `max` processTime p2
    }

-- | This is particularly necessary for processes like chromium.
simplifyProcesses :: [Process] -> [Process]
simplifyProcesses table0 =
    M.elems $
    M.fromListWith addProcess $
    [ (processCommand p, p)
    | p <- table0
    ]

parseProcess :: String -> Maybe Process
parseProcess = evalStateT parse
  where
    parse = do
        processCommand <- parseWord
        processCpu     <- parseRead
        processVsz     <- parseRead
        processRss     <- parseRead
        processTime    <- parseWord
        return Process {..}

getProcesses :: IO [Process]
getProcesses = do
    user <- getEnv "USER"
    ps   <- readProcess "ps" ["-U", user, "-o", cols] ""
    return $ mapMaybe parseProcess (lines ps)
  where
    cols = "comm,pcpu,vsz,rss,time"

makeProcessesMenu :: IO Menu
makeProcessesMenu = do
    processes <- simplifyProcesses <$> getProcesses
    return
        [ SubMenu (processCommand p ++ "-process-menu") (processCommand p)
            [ Label $ printf "CPU: %.3f" (processCpu p)
            , Label $ printf "VSZ: %s"   (humanReadableKiB $ processVsz p)
            , Label $ printf "RSS: %s"   (humanReadableKiB $ processRss p)
            , Label $ printf "TIM: %s"   (processTime p)
            , Separator
            , Item "killall" ("killall " ++ processCommand p)
            ]
        | p <- sortBy (comparing processCommand) processes
        ]

humanReadableKiB :: Int -> String
humanReadableKiB kib
    | bytes > mega = show (bytes `div` mega) ++ "M"
    | bytes > kilo = show (bytes `div` kilo) ++ "K"
    | otherwise    = show bytes
  where
    bytes = kib * 1024
    kilo  = 1000
    mega  = kilo * 1000

--------------------------------------------------------------------------------
-- Directory menu

makeDirectoryMenu
    :: String -> IO Menu
makeDirectoryMenu dir = do
    contents <- sort . filter include <$> getDirectoryContents dir
    children <- forM contents $ \name -> do
        let path = dir </> name
        isDir <- doesDirectoryExist path
        if isDir
        then return $ PipeMenu path name ("obmenus dir " ++ quotePath path)
        else return $ Item name ("xdg-open " ++ quotePath path)

    return $
        [ Item (takeBaseName dir) ("xdg-open " ++ quotePath dir)
        , Separator
        ] ++
        if null children
        then [Label "empty"]
        else children
  where
    include ('.' : _) = False
    include _         = True

    quotePath path   = "\"" ++ concatMap escapePathChar path ++ "\""
    escapePathChar c
        | c `elem` "\"'$\\" = ['\\', c]
        | otherwise         = [c]

--------------------------------------------------------------------------------
-- Monitor menu

makeMonitorMenu :: IO Menu
makeMonitorMenu = do
    modes <- readProcess "monitor" [] ""
    return $
        [ Item "colorprof" "colorprof"
        , Separator
        ] ++
        [ Item mode ("monitor " ++ mode)
        | mode <- lines modes
        ]

--------------------------------------------------------------------------------
-- Main function

main :: IO ()
main = do
    args <- getArgs
    menu <- case args of
        ["brightness"] -> makeBrightnessMenu
        ["volume"]     -> makeVolumeMenu
        ["rhythmbox"]  -> makeRhythmboxMenu
        ["processes"]  -> makeProcessesMenu
        ["dir"]        -> getHomeDirectory >>= makeDirectoryMenu
        ["dir", dir]   -> makeDirectoryMenu dir
        ["monitor"]    -> makeMonitorMenu
        _              -> do
            hPutStrLn stderr $ "Error: need menu argument"
            exitFailure
    putStrLn $ Xml.showElement $ menuToXml menu

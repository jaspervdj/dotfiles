--------------------------------------------------------------------------------
-- | Utility that checks if, for all RAW files, a JPEG file exists.
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Data.Char            (toLower)
import qualified Data.Set             as S
import           Control.Monad.Trans  (liftIO)
import           Control.Monad.Writer (WriterT, runWriterT, tell)
import           Control.Monad        (forM_, unless)
import           Data.Function        (on)
import           Data.Maybe           (isJust)
import           Data.Monoid          (Monoid (..))
import           System.Environment   (getArgs)
import           Data.List            (find, isPrefixOf)
import           System.Directory     (getDirectoryContents, doesDirectoryExist)
import           System.FilePath      (takeExtension, (</>), dropExtension)


--------------------------------------------------------------------------------
main :: IO ()
main = do
    args <- getArgs
    let dir = case args of [] -> "."; (x : _) -> x
    ((), report) <- runWriterT $ checkDirectory dir
    printReport report


--------------------------------------------------------------------------------
type Check a = WriterT Report IO a


--------------------------------------------------------------------------------
data Report = Report
    { rNumRawFiles           :: !Int
    , rNumProcessedFiles     :: !Int
    , rMissingProcessedFiles :: [FilePath]
    , rUnknownExtensions     :: !(S.Set String)
    } deriving (Show)


--------------------------------------------------------------------------------
instance Monoid Report where
    mempty      = Report 0 0 [] S.empty
    mappend x y = Report
        { rNumRawFiles           = on (+)     rNumRawFiles           x y
        , rNumProcessedFiles     = on (+)     rNumProcessedFiles     x y
        , rMissingProcessedFiles = on (++)    rMissingProcessedFiles x y
        , rUnknownExtensions     = on S.union rUnknownExtensions     x y
        }


--------------------------------------------------------------------------------
data FileType
    = RawFile
    | ProcessedFile
    | UnknownFile String
    deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
getFileType :: FilePath -> FileType
getFileType path = case lower (takeExtension path) of
    ".nef"  -> RawFile
    ".raf"  -> RawFile
    ".jpg"  -> ProcessedFile
    ".jpeg" -> ProcessedFile
    ext     -> UnknownFile ext


--------------------------------------------------------------------------------
checkDirectory :: FilePath -> Check ()
checkDirectory dir = do
    contents <- liftIO $ getDirectoryContents' dir
    forM_ contents $ \name -> do
        let path = dir </> name
        isDir <- liftIO $ doesDirectoryExist path
        if isDir then checkDirectory path else checkFile contents name path
  where
    ignore = (`elem` [".", ".."])
    getDirectoryContents' =
        fmap (filter (not . ignore)) . getDirectoryContents


--------------------------------------------------------------------------------
checkFile :: [FilePath] -> FilePath -> FilePath -> Check ()
checkFile contents basename path = case fileType of
    RawFile         -> do
        unless (isJust $ findProcessedFile basename contents) $
            tell mempty {rMissingProcessedFiles = [path]}
        tell mempty {rNumRawFiles = 1}
    ProcessedFile   -> tell mempty {rNumProcessedFiles = 1}
    UnknownFile ext -> tell mempty {rUnknownExtensions = S.singleton ext}
  where
    fileType = getFileType basename


--------------------------------------------------------------------------------
findProcessedFile :: FilePath -> [FilePath] -> Maybe FilePath
findProcessedFile file = find
    (\p -> isPrefixOf prefix (lower p) && getFileType p == ProcessedFile)
  where
    prefix = lower (dropExtension file)


--------------------------------------------------------------------------------
lower :: String -> String
lower = map toLower


--------------------------------------------------------------------------------
printReport :: Report -> IO ()
printReport report = do
    putStr $ unlines $
        [ "# Info"
        , ""
        , "Num raw files: " ++ show (rNumRawFiles report)
        , "Num processed files: " ++ show (rNumProcessedFiles report)
        , "Num missing processed files: " ++
            show (length $ rMissingProcessedFiles report)
        , ""
        , "# Unknown extensions"
        , ""
        ] ++ list (map show . S.toList $ rUnknownExtensions report)

    putStr $ unlines $
        [ ""
        , "# Missing processed files"
        , ""
        ] ++ list (rMissingProcessedFiles report)
  where
    list = map ("- " ++)

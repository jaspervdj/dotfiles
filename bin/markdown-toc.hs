import           Control.Applicative (optional)
import           Data.Maybe          (fromMaybe)
import           Data.Monoid         ((<>))
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Data.Tree           (Forest, Tree (..))
import qualified Options.Applicative as OA
import           System.Environment  (getArgs, getProgName)
import           System.Exit         (exitFailure)
import qualified Text.Pandoc         as P

data Opts = Opts
    { oLevel   :: !Int
    , oOrdered :: !Bool
    , oFile    :: !(Maybe FilePath)
    } deriving (Show)

parseOpts :: OA.Parser Opts
parseOpts = Opts
    <$> OA.option OA.auto (
            OA.long "level" <>
            OA.short 'l' <>
            OA.value 3 <>
            OA.help "deepest header level")
    <*> OA.switch (
            OA.long "ol" <>
            OA.help "use an ordered list")
    <*> optional (OA.strArgument (
            OA.metavar "FILE" <>
            OA.help "markdown file to process"))

main :: IO ()
main = do
    opts <- OA.execParser parser
    markdownToc opts
  where
    parser = OA.info (OA.helper <*> parseOpts) $
        OA.fullDesc <>
        OA.progDesc "Build a table of contents for markdown files" <>
        OA.header "markdown-toc"

markdownToc :: Opts -> IO ()
markdownToc opts = do
    input <- case oFile opts of
        Just "-"  -> T.getContents
        Just path -> T.readFile path
        Nothing   -> T.getContents
    pandoc <- P.runIOorExplode $ P.readMarkdown readerSettings input
    let forest = makeTocForest (oLevel opts) pandoc
        toc    = renderTocForest (oOrdered opts) forest
    output <- P.runIOorExplode $ P.writeMarkdown P.def $ P.Pandoc mempty [toc]
    T.putStr output
  where
    readerSettings = P.def
        { P.readerExtensions = P.pandocExtensions
        }

makeTocForest :: Int -> P.Pandoc -> Forest [P.Inline]
makeTocForest maxLevel (P.Pandoc _ blocks0) =
    map (fmap $ fromMaybe [P.Str "Missing header"]) $
    go mempty blocks0
  where
    go forest [] = forest
    go forest (P.Header level (id_, _, _) title : blocks)
        | level <= maxLevel =
            let inline  = [P.Link ("", [], []) title ('#' : id_, "")]
                forest' = addTocNode level inline forest
            in go forest' blocks
        | otherwise = go forest blocks
    go forest (_ : blocks) = go forest blocks

addTocNode :: Int -> a -> Forest (Maybe a) -> Forest (Maybe a)
addTocNode 1 x forest = forest ++ [Node (Just x) []]
addTocNode n x []     = [Node Nothing $ addTocNode (n - 1) x []]
addTocNode n x forest =
    withLast (\(Node v cs) -> Node v (addTocNode (n - 1) x cs)) forest
  where
    withLast :: (a -> a) -> [a] -> [a]
    withLast f [x]      = [f x]
    withLast _ []       = []
    withLast f (x : xs) = x : withLast f xs

renderTocForest :: Bool -> Forest [P.Inline] -> P.Block
renderTocForest ordered = list . map renderTree
  where
    renderTree :: Tree [P.Inline] -> [P.Block]
    renderTree (Node title [])       = [P.Plain title]
    renderTree (Node title children) =
        [ P.Plain title
        , list $ map renderTree children
        ]

    list
        | ordered   = P.OrderedList (0, P.DefaultStyle, P.DefaultDelim)
        | otherwise = P.BulletList

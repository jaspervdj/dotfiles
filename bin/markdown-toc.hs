import           Control.Applicative (optional)
import           Data.Maybe          (fromMaybe)
import           Data.Monoid         ((<>))
import           Data.Tree           (Forest, Tree (..))
import qualified Options.Applicative as OA
import           System.Environment  (getArgs, getProgName)
import           System.Exit         (exitFailure)
import qualified Text.Pandoc         as P

data Opts = Opts
    { oLevel :: !Int
    , oFile  :: !(Maybe FilePath)
    } deriving (Show)

parseOpts :: OA.Parser Opts
parseOpts = Opts
    <$> OA.option OA.auto (
            OA.long "level" <>
            OA.short 'l' <>
            OA.value 3 <>
            OA.help "deepest header level")
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
        Just "-"  -> getContents
        Just path -> readFile path
        Nothing   -> getContents
    pandoc <- either (fail . show) return $ P.readMarkdown P.def input
    let forest = makeTocForest (oLevel opts) pandoc
        toc    = renderTocForest forest
    putStr $ P.writeMarkdown P.def $ P.Pandoc mempty [toc]

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

renderTocForest :: Forest [P.Inline] -> P.Block
renderTocForest = P.BulletList . map renderTree
  where
    renderTree :: Tree [P.Inline] -> [P.Block]
    renderTree (Node title [])       = [P.Plain title]
    renderTree (Node title children) =
        [ P.Plain title
        , P.BulletList $ map renderTree children
        ]

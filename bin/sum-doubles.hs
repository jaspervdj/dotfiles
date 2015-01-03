import           Data.List  (foldl')
import           Data.Maybe (mapMaybe)
import           Text.Read  (readMaybe)

main :: IO ()
main = do
    interact $ show . foldl' (+) (0 :: Double) . mapMaybe readMaybe . words
    putStrLn ""

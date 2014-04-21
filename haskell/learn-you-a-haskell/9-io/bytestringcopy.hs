import System.Environment
import qualified Data.ByteString.Lazy as B

main = do
  [first, second] <- getArgs
  copyFile' first second

copyFile' :: FilePath -> FilePath -> IO ()
copyFile' source destination = do
  contents <- B.readFile source
  B.writeFile destination contents

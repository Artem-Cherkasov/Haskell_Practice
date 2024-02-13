import Data.Char
import System.Environment
import System.IO

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

main :: IO ()
main = do

  [f1,f2] <- getArgs

  content <- readFile f1

  [replaceChar] <- prompt "Enter replacer character: "
  

  let replaced = map (\c -> if isPunctuation c then replaceChar else c) content

  writeFile f2 replaced
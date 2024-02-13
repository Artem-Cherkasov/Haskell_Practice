import System.Environment
import System.IO
import Data.Char
import Data.List
import System.Directory (removeFile, renameFile)

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

main = do

  args <- getArgs
  let sourceFile = args !! 0
  let destFile = args !! 1

  action <- prompt "Выберите действие: 1 - Просмотр; 2 - Добавление строки; 3 - Удаление строки; 4 - Копирование с фильтром: "

  case read action of
    1 -> do 
      content <- readFile sourceFile
      putStrLn content

    2 -> do
      newLine <- prompt "Введите строку для добавления: "
      appendFile sourceFile ("\n" ++ newLine ++ "\n")

    3 -> do 
      lineToRemove <- prompt "Введите номер строки для удаления: "
      content <- lines <$> readFile sourceFile
      (tempName, tempHandle) <- openTempFile "." "temp"
      let updated = delete (content !! (read lineToRemove - 1)) content
      hPutStr tempHandle $ unlines updated
      hClose tempHandle
      removeFile sourceFile
      renameFile tempName sourceFile  
      

    4 -> do 
  
      content <- readFile sourceFile

      filterType <- prompt "Введите тип фильтра: 1 - поиск по подстроке; 2 - поиск по префиксу: "

      case read filterType of 
        1 -> do
          searchString <- prompt "Введите строку для поиска: "  
          let filteredContent = filter (isInfixOf searchString) (lines content)
          writeFile destFile (unlines filteredContent)
        2 -> do
          searchString <- prompt "Введите префикс для поиска: "
          let filteredContent = filter (isPrefixOf searchString) (lines content)
          writeFile destFile (unlines filteredContent)
          
  putStrLn "Готово!"
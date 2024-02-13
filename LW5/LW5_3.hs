import System.Environment
import System.IO
import Data.Char
import Data.List

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

main = do
  -- получаем имена файлов из аргументов командной строки
  args <- getArgs
  let sourceFile = args !! 0
  let destFile = args !! 1

  -- вводим действие с клавиатуры
  action <- prompt "Выберите действие: 1 - Просмотр 2 - Добавление строки 3 - Удаление строки 4 - Копирование с фильтром"

  -- обрабатываем действие
  case read action of
    1 -> do 
      content <- readFile sourceFile
      putStrLn content

    2 -> do
      putStr "Введите строку для добавления: "
      newLine <- getLine
      content <- readFile sourceFile
      writeFile sourceFile (content ++ "\n" ++ newLine)

    3 -> do 
      putStr "Введите номер строки для удаления: "
      lineToRemove <- getLine
      content <- lines <$> readFile sourceFile
      let updated = delete (content !! (read lineToRemove - 1)) content
      writeFile sourceFile (unlines updated)

    4 -> do
      putStrLn "Выберите фильтр:\n1 - Только цифры\n2 - Только буквы"
      filterType <- getLine
      content <- readFile sourceFile
      let filteredContent = case read filterType of
                              1 -> filter Data.Char.isDigit content
                              2 -> filter Data.Char.isAlpha content
      writeFile destFile filteredContent

  putStrLn "Готово!"
import System.IO

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

main :: IO ()
main = do
  start <- prompt "Введите начальное значение: "
  count <- prompt "Введите количество значений: "
  step <- prompt "Введите кратность: "

  let result = take (read count) $ iterate (+ read step) (read start)

  print result
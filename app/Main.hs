module Main where
import System.Posix.Internals (puts)

type Item = String
type Items = [Item]

data Command
  = Quit
  | DisplayItems
  | AddItem String
  | Help


addItem :: Item -> Items -> Items
addItem item items = item : items

displayItems :: Items -> String
displayItems items =
  let
    displayItem index item = show index ++ " - " ++ item
    reversedList = reverse items
    displayedItemList = zipWith displayItem [1..] reversedList
  in
    unlines displayedItemList

removeItem :: Int -> Items -> Either String Items
removeItem reverseIndex allItems =
    impl (length allItems - reverseIndex) allItems
  where
    impl index items =
      case (index, items) of
        (0, _ : rest) ->
          Right rest
        (_, []) ->
          Left "Index out bound"
        (n, item : rest) ->
          case impl (n - 1) rest of
            Right newItems ->
              Right (item : newItems)
            Left errMsg ->
              Left errMsg

interactWithUser :: Items -> IO ()
interactWithUser items = do
  line <- getLine
  case parseCommand line of
    Right DisplayItems -> do
      putStrLn "items list:"
      putStrLn (displayItems items)
      interactWithUser items

    Right (AddItem item) -> do
      let newItems = addItem item items
      putStrLn "added...\n"
      interactWithUser newItems

    Right Quit -> do
      putStrLn "byebye"
      pure ()

    Right Help -> do
      putStrLn "Commands: quit, items, help, add - <item to add>"
      interactWithUser items

    Left errMsg -> do
      putStrLn ("error: " ++ errMsg)

parseCommand :: String -> Either String Command
parseCommand line = case words line of
  ["quit"] -> Right Quit
  ["items"] -> Right DisplayItems
  ["help"] -> Right Help
  "add" : "-" : item -> Right (AddItem (unwords item))
  _ -> Left "Unknown Command..."

main :: IO ()
main = do
  putStrLn "TODO App........"
  let initialList = []
  _ <- interactWithUser initialList
  putStrLn "yaayyy..... :D"

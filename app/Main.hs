module Main where

type Item = String
type Items = [Item]

data Command
  = Quit
  | DisplayItems
  | AddItem String
  | Help
  | Done Int


addItem :: Item -> Items -> Items
addItem item items = item : items

displayItems :: Items -> String
displayItems items =
  let
    displayItem :: Int -> String -> String
    displayItem index item = show index ++ " - " ++ item
    reversedList = reverse items
    displayedItemList = zipWith displayItem [1..] reversedList
    -- displayedItemList = zipWith displayItem ([1..] :: [Int]) reversedList
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

    Right (Done index) -> do
      let result = removeItem index items
      case result of
        Left errMsg -> do
          putStrLn ("Error: " ++ errMsg ++ "\n choose valid item")
          interactWithUser items
        Right newItems -> do
          putStrLn "item's done... /\\"
          interactWithUser newItems

    Right Help -> do
      putStrLn "Commands: help, quit, list, add - <item to add>, done <item index>"
      interactWithUser items

    Left errMsg -> do
      putStrLn ("error: " ++ errMsg)
      putStrLn "Choose Commands: help, quit, list, add - <item to add>, done <item index>"
      interactWithUser items

parseCommand :: String -> Either String Command
parseCommand line = case words line of
  ["quit"] -> Right Quit
  ["list"] -> Right DisplayItems
  ["help"] -> Right Help
  "add" : "-" : item -> Right (AddItem (unwords item))
  ["done", idxStr] ->
    if all (\c -> elem c "0123456789") idxStr
      then Right (Done (read idxStr))
      else Left "Invalid Index"
  _ -> Left "Unknown Command..."

main :: IO ()
main = do
  putStrLn "TODO App........"
  let initialList = []
  _ <- interactWithUser initialList
  putStrLn "yaayyy..... :D"

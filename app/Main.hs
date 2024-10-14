module Main where

type Item = String
type Items = [Item]

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

-- removeItem :: Int -> Items -> Either String Items

interactWithUser :: Items -> IO Items
interactWithUser items = do
  putStrLn "Insert TO-DO Item: "
  item <- getLine
  let newItems = addItem item items
  putStrLn "added...\n"
  putStrLn "new list...\n"
  putStrLn (displayItems newItems)
  pure newItems


main :: IO ()
main = do
  putStrLn "TODO App........"
  let initialList = []
  _ <- interactWithUser initialList
  putStrLn "yaayyy..... :D"

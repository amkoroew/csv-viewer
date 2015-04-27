import Data.Char (toLower)
import Data.List
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitSuccess), exitWith)

main :: IO ()
main = do
    [file, pageLength] <- getArgs
    csvContent <- fmap lines $ readFile file
    csvViewer ("No." : getColumns (head csvContent)) (zipWith (:) (fmap show [1 ..]) (fmap getColumns (tail csvContent))) 1 (read pageLength :: Int)

csvViewer :: [String] -> [[String]] -> Int -> Int -> IO ()
csvViewer header body currentPage pageLength = do
    let columns = take pageLength $ drop ((currentPage - 1) * pageLength) body
        columnWidths = fmap calculateColumnWidths (header : columns)
        maxColumnWidths = calculateMaxColumnWidths columnWidths
        totalPages = ceiling (fromIntegral (length body) / fromIntegral pageLength)
        page = createPage maxColumnWidths header columns currentPage totalPages
    putStr $ unlines page
    c <- getLine
    let input = parseInput (fmap toLower c) currentPage totalPages
    if input == -1
        then exitWith ExitSuccess
        else csvViewer header body input pageLength
        
parseInput :: String -> Int -> Int -> Int
parseInput input currentPage totalPages
  | input == "n" = min totalPages (currentPage + 1)
  | input == "p" = max 1 (currentPage - 1)
  | input == "f" = 1
  | input == "l" = totalPages
  | input == "x" = -1
  | input >= "1" = max 1 (min totalPages (read input :: Int))
  | otherwise    = currentPage

getContentOfPage :: [String] -> Int -> Int -> [String]
getContentOfPage lines page pageLength = take (pageLength) . drop ((page - 1) * pageLength) $ lines

getColumns :: String -> [String]
getColumns line = case dropWhile (== ';') line of
                                "" -> []
                                column -> col : getColumns column'
                                      where (col, column') =
                                             break (== ';') column

calculateColumnWidths :: [String] -> [Int]
calculateColumnWidths row = fmap length row

calculateMaxColumnWidths :: [[Int]] -> [Int]
calculateMaxColumnWidths columnWidthsOfRows = fmap maximum (transpose columnWidthsOfRows)

createPage :: [Int] -> [String] -> [[String]] -> Int -> Int -> [String]
createPage columnWidths header content currentPage totalPages = (createRow columnWidths header) : createSeparator columnWidths : fmap (createRow columnWidths) content ++ status : menu : []
                                           where
                                             status = "Page " ++ show currentPage ++ " of " ++ show totalPages
                                             menu   = "\n(N)ext (P)revious (F)irst (L)ast E(x)it"

createSeparator :: [Int] -> String
createSeparator columnWidths =  "|" ++ concat (head dashsList : ["+" ++ dashs | dashs <- tail dashsList]) ++ "|"
                                 where
                                   dashsList = [take n ['-', '-' ..] | n <- columnWidths]

createRow :: [Int] -> [String] -> String
createRow columnWidths content = concat ["|" ++ columnString | columnString <- columnStringList] ++ "|"
                                 where
                                   columnStringList = [take n (s ++ [' ', ' ' ..]) | (n, s) <- zip columnWidths content]

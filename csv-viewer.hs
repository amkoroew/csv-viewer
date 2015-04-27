import Data.List
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitSuccess), exitWith)

main :: IO ()
main = do
    [file, pageLength] <- getArgs
    csvContent <- fmap lines $ readFile file
    csvViewer (getColumns (head csvContent)) (fmap getColumns (tail csvContent)) 1 (read pageLength :: Int)

csvViewer :: [String] -> [[String]] -> Int -> Int -> IO ()
csvViewer header body currentPage pageLength = do
    let columns = take pageLength $ drop ((currentPage - 1) * pageLength) body
        columnWidths = fmap calculateColumnWidths (header : columns)
        maxColumnWidths = calculateMaxColumnWidths columnWidths
        page = createPage maxColumnWidths header columns
    putStr $ unlines page
    c <- getChar
    case c of
        'n' -> csvViewer header body (currentPage + 1) pageLength
        'p' -> csvViewer header body (currentPage - 1) pageLength
        'f' -> csvViewer header body 1 pageLength
        'l' -> csvViewer header body (ceiling (fromIntegral (length [1,2,3,4,5]) / 2)) pageLength
        'x' -> exitWith ExitSuccess
        _   -> csvViewer header body currentPage pageLength

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

createPage :: [Int] -> [String] -> [[String]] -> [String]
createPage columnWidths header content = (createRow columnWidths header) : createSeparator columnWidths : fmap (createRow columnWidths) content ++ menu : []
                                           where
                                             menu = "\n(N)ext (P)revious (F)irst (L)ast E(x)it"

createSeparator :: [Int] -> String
createSeparator columnWidths =  "|" ++ concat (head dashsList : ["+" ++ dashs | dashs <- tail dashsList]) ++ "|"
                                 where
                                   dashsList = [take n ['-', '-' ..] | n <- columnWidths]

createRow :: [Int] -> [String] -> String
createRow columnWidths content = concat ["|" ++ columnString | columnString <- columnStringList] ++ "|"
                                 where
                                   columnStringList = [take n (s ++ [' ', ' ' ..]) | (n, s) <- zip columnWidths content]

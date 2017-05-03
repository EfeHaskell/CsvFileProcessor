module Main where

import qualified Data.List as List
import qualified Data.ByteString as B
import Data.Char
import Text.Regex
import qualified Data.ByteString.Char8 as C
import System.IO (openFile, FilePath, Handle, hPutStr, hGetContents, hClose, IOMode(AppendMode,ReadMode))

parentDir = "/home/eariaroo/Documents/projects/haskell/daaremployeeuniuqueids/"
source1 = parentDir ++ "DetailedDAARSalariesandDeductions.csv"
source2 = parentDir ++ "EmployeeUniqueIds.csv"
output = parentDir ++ "output.csv"

myRegexSplit :: String -> String -> [String]
myRegexSplit regExp theString = filter (not . null) (splitRegex (mkRegex regExp) theString)

removeQuotes :: String -> String
removeQuotes = filter (not . (`elem` " \"\'"))

capitalise  =  map toUpper

replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

parseSource1 :: IO ()
parseSource1 = do
    csvLines <- myRegexSplit "\n" <$> (C.unpack <$> B.readFile source1)
    putStrLn $ "Number of lines: " ++ (show . length) csvLines

    outputFileHandle <- openFile output AppendMode
    mapM_ (processor outputFileHandle) csvLines
    hClose outputFileHandle

processor :: Handle -> String -> IO()
processor outputFileHandle aCsvLine = do
    let source1LineColumns =  splitRegex (mkRegex ",") aCsvLine

    uniqueEmployeeIdsCsvLines <- myRegexSplit "\n" <$> (C.unpack <$> B.readFile source2)
    looker outputFileHandle source1LineColumns uniqueEmployeeIdsCsvLines

looker :: Handle -> [String] -> [String] -> IO ()

looker outputFileHandle source1LineColumns [] = return ()
looker outputFileHandle source1LineColumns (first:rest) = do
    let firstName = (capitalise . removeQuotes) $ source1LineColumns !! 2
    let lastName = (capitalise . removeQuotes) $ source1LineColumns !! 3

    let fullName = firstName ++ lastName
    let fullNameReversed = lastName ++ firstName
    let empUniqueIdslineColumns =  splitRegex (mkRegex ",") first
    let empFullName = (capitalise . removeQuotes) $ empUniqueIdslineColumns !! 1

    if fullName == empFullName
        then do
            let empUniqueId = empUniqueIdslineColumns !! 0
            let newRow = replaceAtIndex 1 empUniqueId source1LineColumns
            hPutStr outputFileHandle $ (List.intercalate "," newRow) ++ "\n"
            looker outputFileHandle source1LineColumns rest
        else looker outputFileHandle source1LineColumns rest

main :: IO ()
main = parseSource1 >> putStrLn "All Done"

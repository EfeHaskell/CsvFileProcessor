module Main(main, myRegexSplit) where

import System.Directory (getCurrentDirectory)
import qualified Data.List as List
import qualified Data.ByteString as B
import Data.Char
import Text.Regex
import Control.Monad (liftM)
import qualified Data.ByteString.Char8 as C
import System.IO (openFile, FilePath, Handle, hPutStr, hGetContents, hClose, IOMode(AppendMode,ReadMode))


main :: IO ()
main = do 
    parentDir <- (flip (++) "/") <$> getCurrentDirectory
    putStrLn $ "Parent Directory: " ++ parentDir
    let source1 = parentDir ++ "DetailedDAARSalariesandDeductions.csv"

    csvLines <- myRegexSplit "\n" <$> (C.unpack <$> B.readFile source1)
    putStrLn $ "Number of lines: " ++ (show . length) csvLines

    let output = parentDir ++ "output2.csv"

    outputFileHandle <- openFile output AppendMode
    mapM_ (processor outputFileHandle parentDir) csvLines
    hClose outputFileHandle  
    putStrLn "All Done"

processor :: Handle -> String -> String -> IO ()
processor outputFileHandle parentDir aCsvLine = do
    let source1LineColumns =  splitRegex (mkRegex ",") aCsvLine
    let source2 = parentDir ++ "EmployeeUniqueIds.csv"

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

myRegexSplit :: String -> String -> [String]
myRegexSplit regExp theString = filter (not . null) (splitRegex (mkRegex regExp) theString)

removeQuotes :: String -> String
removeQuotes = filter (not . (`elem` " \"\'"))

capitalise = map toUpper

replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

pop [] = []
pop x = take ((length x) - 1) x 

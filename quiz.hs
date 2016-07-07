--see gdfAnalyzer.hs for the inspiration for many of these functions
import System.Environment
import Text.ParserCombinators.ReadP
import Data.Char
import System.Random

type Card = (String, String)

results :: String
results = "/Users/alexsmith/Programming/Flashcards/results.txt"

main :: IO ()
main = do
    args <- getArgs
    f <- readFile $ head args
    let cards = fileToCards f
    let num = read (args !! 1) :: Int
    study cards num


studyList :: [Card] -> IO ()
studyList [] = putStrLn "Done!"
studyList (a:as) = do
    printFront a
    _ <- getLine
    --can implement test of the back now
    --I'm not that interestd in this feature though
    --Just using this so that way I can press enter to see the english result
    printBack a
    --here put something that will compile the words gotten wrong
    --and put it in a file
    --keep track of stats for all runs of this file and for the last 100 runs of this file
    result <- getLine
    if result /= "" then 
        do
            appendFile results ((showCard a) ++ "\n")
            studyList as
        else
            do 
                studyList as

study :: [Card] -> Int -> IO ()
study cards num = do
    gen <- getStdGen
    let is =  randomList (1,length cards - 1) num 
    let indices = is gen
    let cardList = subset cards indices
    studyList cardList

subset :: [Card] -> [Int] -> [Card]
subset _ [] = []
subset cards (a:as) = cards !! a : subset cards as

--from http://stackoverflow.com/questions/9139649/how-to-generate-a-list-which-contains-a-given-number-of-random-numbers-within-a
randomList :: (Random a, RandomGen g) => (a, a) -> Int -> g -> [a]
randomList bnds n = take n . randomRs bnds



isEnglishWord :: Char -> Bool
isEnglishWord c = isAlphaNum c || c == ' ' || isPunctuation c

isChineseWord :: Char -> Bool
isChineseWord c = isAlphaNum c || isPunctuation c

parser :: ReadP Card
parser = do
    skipSpaces
    front <- munch isChineseWord
    skipSpaces
    back <- munch isEnglishWord
    return (front, back)

runParser :: ReadP a -> ReadS a
runParser = readP_to_S

parse :: String -> Card
parse str = fst . head $ runParser parser str

--have already read the file with readFile
fileToCards :: String -> [Card]
fileToCards file = fmap parse (lines file)

--file = "/Users/alexsmith/Desktop/Integrated Chinese 1 Vocab"

printFront :: Card -> IO ()
printFront (front, _) = putStrLn front

printBack :: Card -> IO ()
printBack (_, back) = putStrLn back

showCard :: Card -> String
showCard (a,b) = a ++ b

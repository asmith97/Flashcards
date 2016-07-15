--see gdfAnalyzer.hs for the inspiration for many of these functions
import System.Environment
import Text.ParserCombinators.ReadP
import Data.Char
import System.Random

type Card = (String, String)

results :: String
results = "/Users/alexsmith/Programming/Flashcards/results.txt"

--First arg = the file to read
--Second arg = if three args, then the file to store output in (if not specified, make there be a default)
--Second/Third arg = if 2 args, then it's the number to display in the quiz, if 3 args, then the third arg is the number to display in the quiz
--maybe have flags instead?
--other potential arguments: which side of the card to display
main :: IO ()
main = do
    args <- getArgs
    f <- readFile $ head args
    let cards = fileToCards f
    let num = read (args !! 1) :: Int
    study cards num

studyList :: [Card] -> Int -> Int -> IO ()
studyList [] right wrong = putStrLn $ "You got " ++ show right ++ "/" ++ show (right +wrong) ++ " correct."
studyList (a:as) right wrong = do
    printFront a
    _ <- getLine
    --can implement test of the back now
    --I'm not that interestd in this feature though
    --Just using this so that way I can press enter to see the english result
    printBack a
    result <- getLine
    if result /= "" then 
        do
            appendFile results ((showCard a) ++ "\n")
            studyList as right (wrong + 1)
        else
            studyList as (right + 1) wrong

study :: [Card] -> Int -> IO ()
study cards num = do
    gen <- getStdGen
    let is =  randomList (1,length cards - 1) num 
    let indices = is gen
    --should make sure that the input is within range of the length of the list
    let cardList = if num < length cards - 1 then subset cards indices else cards
    --get it to shuffle the card list even if it's using all of them
    studyList cardList 0 0

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

isPinYin :: Char -> Bool
isPinYin c = isEnglishWord c || c == '(' || c == ')'

isSep :: Char -> Bool
isSep c = c == '[' || c == ']' || c == '-'

parser :: ReadP Card
parser = do
    skipSpaces
    front <- munch isChineseWord
    skipSpaces
    back <- munch isEnglishWord
    return (front, back)


--might be able to just make this the parser
hsvFormatParser :: ReadP Card
hsvFormatParser = do
    skipSpaces
    front <- munch isChineseWord
    skipSpaces
    back <- munch isEnglishWord
    skipSpaces
    moreBack <- munch isEnglishWord
    return (front, back ++ " " ++ moreBack) 

--this does not work with the old format
newFormatParser :: ReadP Card
newFormatParser = do
    skipSpaces
    chinese <- munch isAlphaNum
    skipSpaces
    pinyin <- munch (\c -> isAlphaNum c || c == '(' || c == ')' || isSpace c)
    skipSpaces
    munch isSep
    skipSpaces
    english <- munch isEnglishWord
    return (chinese ++ " " ++ pinyin, english)

--seems like fst $ last (result of this parser) should work
--fix these
fParser :: ReadP [String]
fParser = sepBy sideParser sepParser

sideParser :: ReadP String
sideParser = do
    skipSpaces
    side <- munch (\c -> isAlphaNum c || c == '(' || c == ')' || isSpace c || c == ';' || c == '.') --maybe just make it isNot '[' or '=]'
    return side

sepParser :: ReadP String
sepParser = do
    skipSpaces
    satisfy (\c -> c == '[')
    satisfy (\c -> c == '-')
    satisfy (\c -> c == ']')
    return "sep"



runParser :: ReadP a -> ReadS a
runParser = readP_to_S

parse :: String -> Card
parse str = fst . head $ runParser hsvFormatParser str

parseHSV :: String -> Card
parseHSV str = fst . head $ runParser hsvFormatParser str

--have already read the file with readFile
fileToCards :: String -> [Card]
fileToCards file = fmap parse (lines file)

--file = "/Users/alexsmith/Desktop/Integrated Chinese 1 Vocab"

printFront :: Card -> IO ()
printFront (front, _) = putStrLn front

printBack :: Card -> IO ()
printBack (_, back) = putStrLn back

showCard :: Card -> String
showCard (a,b) = a ++ "\t" ++ b


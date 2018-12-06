import System.IO
import ParseLib.Abstract
import System.Environment
import Data.Char

import Prelude hiding ((<*), (<$), (*>))

-- Starting Framework

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc :: Bool }
    deriving (Eq, Ord)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord)

newtype Year  = Year { unYear :: Int }  deriving (Eq, Ord)
newtype Month = Month { unMonth :: Int } deriving (Eq, Ord)
newtype Day   = Day { unDay :: Int } deriving (Eq, Ord)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord)

newtype Hour   = Hour { unHour :: Int } deriving (Eq, Ord)
newtype Minute = Minute { unMinute :: Int } deriving (Eq, Ord)
newtype Second = Second { unSecond :: Int } deriving (Eq, Ord)


-- | The main interaction function. Used for IO, do not edit.
data Result = SyntaxError | Invalid DateTime | Valid DateTime deriving (Eq, Ord)

instance Show DateTime where
    show = printDateTime

instance Show Result where
    show SyntaxError = "date/time with wrong syntax"
    show (Invalid _) = "good syntax, but invalid date or time values"
    show (Valid x)   = "valid date: " ++ show x

main :: IO ()
main = mainDateTime

mainDateTime :: IO ()
mainDateTime = interact (printOutput . processCheck . processInput)
    where
        processInput = map (run parseDateTime) . lines
        processCheck = map (maybe SyntaxError (\x -> if checkDateTime x then Valid x else Invalid x))
        printOutput  = unlines . map show

mainCalendar :: IO ()
mainCalendar = do
    file:_ <- getArgs
    res <- readCalendar file
    putStrLn $ maybe "Calendar parsing error" (ppMonth (Year 2012) (Month 11)) res

-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <* symbol 'T' <*> parseTime <*> option isUTC False

--parseDate ::

--Parse Functions for the date
parseYear :: Parser Char Year
parseYear = (\a b c d -> Year $ read(a:b:c:[d])) <$> digit <*>  digit <*> digit <*> digit

parseMonth :: Parser Char Month
parseMonth = (\x y -> Month $ read (x:[y])) <$> digit <*> digit

parseDay :: Parser Char Day
parseDay = (\x y -> Day $ read (x:[y])) <$> digit <*> digit

parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay


--Parse funtions for the time

parseHour :: Parser Char Hour
parseHour = (\x y -> Hour $ read (x:[y])) <$> digit <*> digit

parseMinute :: Parser Char Minute
parseMinute = (\x y -> Minute $ read (x:[y])) <$> digit <*> digit

parseSecond :: Parser Char Second
parseSecond = (\x y -> Second $ read (x:[y])) <$> digit <*> digit

parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

isUTC :: Parser Char Bool
isUTC = const True <$> symbol 'Z'

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run p xs = if (null f)
                then Nothing
                else Just $ fst (head f)
            where f = filter(\(_,b) -> null b) (parse p xs)

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime (DateTime x y True) = show x ++ "T" ++ show y ++ "Z"
printDateTime (DateTime x y _) = show x ++ "T" ++ show y  

instance Show Date where
    show (Date x y z) = addZero 4 (show(unYear x)) ++ addZero 2 (show(unMonth y)) ++ addZero 2  (show(unDay z))

instance Show Time where
    show (Time x y z) = addZero 2 (show(unHour x)) ++ addZero 2 (show(unMinute y)) ++ addZero 2 (show(unSecond z))

addZero :: Int -> String -> String
addZero x y | length (y) < x = addZero x ("0" ++ y)
            | otherwise = y

-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime (DateTime a b _) = checkDate a && checkTime b

checkDate :: Date -> Bool
checkDate (Date y m d) = y < (Year 10000) && y > (Year 1000) && m < (Month 13) && validDay y m d

monthDayList = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

validDay :: Year -> Month -> Day -> Bool
validDay (Year y) (Month m) (Day d) = if (isLeap y && m == 2)
                                            then y < 10000 && y >= 1000 && m <= 12 && m > 0 && d <= 29 && d > 0
                                            else y < 10000 && y >= 1000 && m <= 12 && m > 0 && d <= monthDayList !! (m -1) && d > 0

isLeap :: Int -> Bool
isLeap y = (y `mod` 4 == 0 && y `mod` 100 /= 0 ) || (y `mod` 400 == 0)

checkTime :: Time -> Bool
checkTime (Time (Hour h) (Minute m) (Second s)) = h < 24 && h >= 0 && m < 60 && m >= 0 && s < 60 && s >= 0

-- Exercise 6

-- newtype Year  = Year { unYear :: Int }  deriving (Eq, Ord)
data Calendar = Calendar {calprop :: [CalProp], event :: [Event]}
    deriving (Eq, Ord, Show)

data CalProp = ProdId String | Version
    deriving (Eq, Ord, Show)
    

data Event = Event {eventProp :: [EventProperty]}
    deriving (Eq, Ord, Show)

data EventProperty = DTStamp DateTime | UID String | DTStart DateTime | DTEnd DateTime | Description String | Summary String | Location String
    deriving (Eq, Ord, Show)


-- Exercise 7
data Token = TBegin String
           | TEnd String
           | TProdId String
           | TVersion String
           | TDtStamp DateTime
           | TUID String
           | TDtStart DateTime
           | TDtEnd  DateTime
           | TDescription String
           | TSummary String
           | TLocation String
    deriving (Eq, Ord, Show)

scanCalendar :: Parser Char [Token]
scanCalendar = greedy scanToken <* eof

scanToken :: Parser Char Token
scanToken = tWithText TBegin "BEGIN:" <|>
            tWithText TEnd "END:" <|>
            tWithText TProdId "PRODID:" <|>
            tWithText TVersion "VERSION:" <|>
            tWithDateTime TDtStamp "DTSTAMP:" <|>
            tWithText TUID "UID:" <|>
            tWithDateTime TDtStart "DTSTART:" <|>
            tWithDateTime TDtEnd "DTEND:" <|>
            tWithText TDescription "DESCRIPTION:" <|>
            tWithText TSummary "SUMMARY:" <|>
            tWithText TLocation "LOCATION:"

tWithText :: (String -> Token) -> String -> Parser Char Token
tWithText f s = f <$ token s <*> many (notSymbol '\r') <* symbol '\r' <* symbol '\n'

tWithDateTime :: (DateTime -> Token) -> String -> Parser Char Token
tWithDateTime f s = f <$ token s <*> parseDateTime <* symbol '\r' <* symbol '\n'

notSymbol :: Eq s  => s -> Parser s s
notSymbol x = satisfy (/=x)

testDateTime :: String
testDateTime = "20111012T083945"

testCalendar :: String
testCalendar = "BEGIN:VCALENDAR\r\n\
\VERSION:2.0\r\n\
\PRODID:www.testMeiCalendar.net\r\n\
\BEGIN:VEVENT\r\n\
\DTSTART:20101231T230000\r\n\
\DTEND:20110101T010000\r\n\
\SUMMARY:New Years Eve Reminder\n\
\asdf\r\n\
\LOCATION:Downtown\r\n\
\DESCRIPTION:Let's get together for New Years Eve\r\n\
\UID:ABCD1234\r\n\
\DTSTAMP:20101125T112600\r\n\
\END:VEVENT\r\n\
\END:VCALENDAR\r\n\
\"


parseCalendar :: Parser Token Calendar
parseCalendar = Calendar <$ symbol (TBegin "VCALENDAR") <*> many parseCalProp <*> many parseEvent <* symbol (TEnd "VCALENDAR")

parseCalProp :: Parser Token CalProp
parseCalProp = parseProdId <|> parseVersion

parseProdId :: Parser Token CalProp
parseProdId = prodId <$> satisfy isProdId

isProdId :: Token -> Bool
isProdId (TProdId _) = True
isProdId _           = False

prodId :: Token -> CalProp
prodId (TProdId t) = ProdId t

parseVersion :: Parser Token CalProp
parseVersion = Version <$ token ([TVersion "2.0"])

parseEvent :: Parser Token Event
parseEvent = Event <$ symbol ( TBegin "VEVENT") <*> many parseEventProp <* symbol (TEnd "VEVENT")

parseEventProp :: Parser Token EventProperty
parseEventProp = tokenToEventProp <$> satisfy isValidEventPropToken

tokenToEventProp :: Token -> EventProperty
tokenToEventProp (TDtStamp dt) = DTStamp dt
tokenToEventProp (TUID s) = UID s
tokenToEventProp (TDtStart dt) = DTStart dt
tokenToEventProp (TDtEnd dt) = DTEnd dt
tokenToEventProp (TDescription s) = Description s
tokenToEventProp (TSummary s) = Summary s
tokenToEventProp (TLocation s) = Location s
tokenToEventProp _  = undefined 

isValidEventPropToken :: Token -> Bool
isValidEventPropToken (TDtStamp _)     = True
isValidEventPropToken (TUID _)         = True
isValidEventPropToken (TDtStart _)     = True
isValidEventPropToken (TDtEnd _)       = True
isValidEventPropToken (TDescription _) = True
isValidEventPropToken (TSummary _)     = True
isValidEventPropToken (TLocation _)    = True
isValidEventPropToken _                = False

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar z = do
        x <- openFile z ReadMode
        y <- hGetContents x
        return (recognizeCalendar y)
             

-- Exercise 9
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar (Calendar x y) = "BEGIN:VCALENDAR\r\n" ++ foldr (++) "" (map printProp x) ++ foldr (++) "" (map printEvent y) ++ "END:VCALENDAR\r\n"

printProp :: CalProp -> String
printProp (ProdId s) = "PRODID:" ++ s ++ "\r\n"
printProp (Version) = "VERSION:2.0" ++ "\r\n"

printEvent :: Event -> String
printEvent (Event x) = "BEGIN:VEVENT\r\n" ++ foldr (++) "" (map printEventProp x) ++ "END:VEVENT\r\n"

printEventProp :: EventProperty -> String
printEventProp (DTStamp dt) = "DTSTAMP:" ++ printDateTime dt ++ "\r\n"
printEventProp (UID s) = "UID:" ++ s ++ "\r\n"
printEventProp (DTStart dt) = "DTSTART:" ++ printDateTime dt ++ "\r\n"
printEventProp (DTEnd dt) =  "DTEND:" ++ printDateTime dt ++ "\r\n"
printEventProp (Description s) = "DESCRIPTION:" ++ s ++ "\r\n"
printEventProp (Summary s) = "SUMMARY:" ++ s ++ "\r\n"
printEventProp (Location s) = "LOCATION:" ++ s ++ "\r\n"

-- Exercise 10
countEvents :: Calendar -> Int
countEvents = undefined

findEvents :: DateTime -> Calendar -> [Event]
findEvents = undefined

checkOverlapping :: Calendar -> Bool
checkOverlapping = undefined

timeSpent :: String -> Calendar -> Int
timeSpent = undefined

-- Exercise 11
ppMonth :: Year -> Month -> Calendar -> String
ppMonth = undefined


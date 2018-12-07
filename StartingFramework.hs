import System.IO
import ParseLib.Abstract
import System.Environment
import Data.Char
import Data.Traversable
import Data.Maybe
import Control.Monad

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

--mainCalendar :: IO ()
--mainCalendar = do
 --   file:_ <- getArgs
 --   res <- readCalendar file
--    putStrLn $ maybe "Calendar parsing error" (ppMonth (Year 2012) (Month 11)) res

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

multiLineTextConstructor :: (String -> Token) -> [String] -> String -> Token
multiLineTextConstructor f s1 s2 = f (foldr (++) s2 s1)

tWithText :: (String -> Token) -> String -> Parser Char Token
tWithText f s = (multiLineTextConstructor f) <$ token s <*> many ( many (notSymbol '\r') <* symbol '\r' <* symbol '\n' <* symbol ' ') <*> ( some (notSymbol '\r') <* symbol '\r' <* symbol '\n')

tWithDateTime :: (DateTime -> Token) -> String -> Parser Char Token
tWithDateTime f s = f <$ token s <*> parseDateTime <* symbol '\r' <* symbol '\n'

notSymbol :: Eq s  => s -> Parser s s
notSymbol x = satisfy (/=x)

parseCalendar :: Parser Token Calendar
parseCalendar = validCalendar <$ symbol (TBegin "VCALENDAR") <*> many parseCalProp <*> many parseEvent <* symbol (TEnd "VCALENDAR")

validCalendar :: [CalProp] -> [Event] -> Calendar
validCalendar p e = if (validProps p && and (map validEventProps e))
                        then Calendar p e
                        else error "wrong calendar format"

validProps :: [CalProp] -> Bool
validProps p = length p == 2 && not(isProp (p !! 0) (p !! 1))

validEventProps :: Event -> Bool
validEventProps (Event e) = (and (map (\x -> (numTimesFound x e) == 1) [DTStamp dateTimeNull, UID "", DTStart dateTimeNull, DTEnd dateTimeNull])) && (and (map (\x -> (numTimesFound x e) <= 1) [Description "", Summary "", Location ""]))

isProp :: CalProp -> CalProp -> Bool
isProp (Version) (Version) = True
isProp (ProdId _) (ProdId _) = True
isProp _ _ = False

isEventProp :: EventProperty -> EventProperty -> Bool
isEventProp (DTStamp _) (DTStamp _) = True
isEventProp (UID _) (UID _) = True
isEventProp (DTStart _) (DTStart _) = True
isEventProp (DTEnd _) (DTEnd _) = True
isEventProp (Description _) (Description _) = True
isEventProp (Summary _) (Summary _) = True
isEventProp (Location _) (Location _) = True
isEventProp _ _ = False

-- elem counter from https://codereview.stackexchange.com/questions/139587/count-occurrences-of-an-element-in-a-list
numTimesFound :: EventProperty -> [EventProperty] -> Int
numTimesFound _ [] = 0
numTimesFound x xs = (length . filter (isEventProp x)) xs

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
        x <- openFile z ReadWriteMode
        hSetNewlineMode x noNewlineTranslation
        y <- hGetContents x
        return (recognizeCalendar y)

printReadFile :: FilePath -> IO ()
printReadFile z = do
                x <- openFile z ReadWriteMode
                hSetNewlineMode x noNewlineTranslation
                y <- hGetContents x
                print y
        
             

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
countEvents (Calendar p e) = length e

findEvents :: DateTime -> Calendar -> [Event]
findEvents dt (Calendar _ e) = filter (\x -> startTimeEvent x <= dt && endTimeEvent x > dt ) e

startTimeEvent :: Event -> DateTime
startTimeEvent (Event p) = timeOfProperty $ head $ filter (\x -> isEventProp (DTStart dateTimeNull) x) p

endTimeEvent :: Event -> DateTime
endTimeEvent (Event p) = timeOfProperty $ head $ filter (\x -> isEventProp (DTEnd dateTimeNull) x) p

timeOfProperty :: EventProperty -> DateTime
timeOfProperty (DTStart t) = t
timeOfProperty (DTEnd t) = t
timeOfProperty (DTStamp t) = t
timeOfProperty _ = dateTimeNull

checkOverlapping :: Calendar -> Bool
checkOverlapping (Calendar _ e) = or $ map (\x -> or $ map (isOverlapping x) dtpairs) dtpairs
                            where dtpairs = map (\x -> (startTimeEvent x, endTimeEvent x, getUID x)) e

getUID :: Event -> EventProperty
getUID (Event p) = head $ filter (\x -> isEventProp (UID "") x) p

getSummary :: Event -> EventProperty
getSummary (Event p) = head $ filter (\x -> isEventProp (Summary "") x) p

isOverlapping :: (DateTime, DateTime, EventProperty) -> (DateTime, DateTime, EventProperty) -> Bool
isOverlapping (dts1, dte1, id1) (dts2, dte2, id2) = ((dts2 < dte1 && dts2 >= dts1) || (dts1 < dte2 && dts1 >= dts2)) && id1 /= id2

timeSpent :: String -> Calendar -> Int
timeSpent s (Calendar _ e) = calculateTime $ filter (\x -> containsProperty x (Summary s)) e

calculateTime :: [Event] -> Int
calculateTime e = 0 --length $ mergeOverlapping e

containsProperty :: Event -> EventProperty -> Bool
containsProperty (Event e) p = elem p e

-- Exercise 11
ppMonth :: Year -> Month -> Calendar -> String
ppMonth = undefined




-- testing constants
testDateTime :: String
testDateTime = "20111012T083945"

dateTimeNull :: DateTime
dateTimeNull = DateTime (Date (Year 2000) (Month 01) (Day 01)) (Time (Hour 00) (Minute 00) (Second 00)) True

dateTimeNull2 :: DateTime
dateTimeNull2 = DateTime (Date (Year 2000) (Month 01) (Day 01)) (Time (Hour 10) (Minute 00) (Second 00)) True

dateTimeNull3 :: DateTime
dateTimeNull3 = DateTime (Date (Year 2000) (Month 01) (Day 01)) (Time (Hour 13) (Minute 00) (Second 00)) True

testCalendar :: String
testCalendar = "BEGIN:VCALENDAR\r\n\
\VERSION:2.0\r\n\
\PRODID:www.testMeiCalendar.net\r\n\
\BEGIN:VEVENT\r\n\
\DTSTART:20101231T230000\r\n\
\DTEND:20110101T010000\r\n\
\SUMMARY:New Years Eve Reminder\r\n\
\LOCATION:Downtown\r\n\
\DESCRIPTION:Let's get together for New Years Eve\r\n\
\UID:ABCD1234\r\n\
\DTSTAMP:20101125T112600\r\n\
\END:VEVENT\r\n\
\END:VCALENDAR\r\n\
\"

testCalendar2 :: String
testCalendar2 = "BEGIN:VCALENDAR\r\n\
\VERSION:2.0\r\n\
\PRODID:-//hacksw/handcal//NONSGML v1.0//EN\r\n\
\BEGIN:VEVENT\r\n\
\UID:12345@example.com\r\n\
\DTSTAMP:20111205T170000Z\r\n\
\DTSTART:20111205T170000Z\r\n\
\DTEND:20111205T210000Z\r\n\
\DTEND:20111205T210000Z\r\n\
\SUMMARY:This is a very long description th\r\n\
\ at runs over multiple lines.\r\n\
\  A third one even.\r\n\
\END:VEVENT\r\n\
\END:VCALENDAR\r\n"

testCalendar3 :: Calendar
testCalendar3 = (Calendar {calprop = [Version,ProdId "www.testMeiCalendar.net"], event = [Event {eventProp = [DTStart dateTimeNull,DTEnd dateTimeNull2,Summary "New Years Eve Reminder", Location "Downtown",Description "Let's get together for New Years Eve",UID "ABCD1234",DTStamp dateTimeNull]}
                                                                                         ,Event {eventProp = [DTStart dateTimeNull,DTEnd dateTimeNull3,Summary "New Years Eve Reminder", Location "Downtown",Description "Let's get together for New Years Eve",UID "ABCD12345",DTStamp dateTimeNull]}]})
import System.IO
import ParseLib.Abstract
import System.Environment
import Data.Char
import Data.List
import Data.Traversable
import Data.Maybe
import Control.Monad
import Text.PrettyPrint
import Data.Time.Calendar hiding (Day)
import Data.Time.Calendar.WeekDate

import Prelude hiding ((<*), (<$), (*>))
import Data.Time.Calendar hiding (Day)

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

-- prints date (makes sure month etc under 10 get a 0 in front)
instance Show Date where
    show (Date x y z) = addZero 4 (show(unYear x)) ++ addZero 2 (show(unMonth y)) ++ addZero 2  (show(unDay z))

    -- prints time (makes sure hour etc under 10 get a 0 in front)
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

-- Only accepts years between 1000-9999, because only 4-digit years are allowed
checkDate :: Date -> Bool
checkDate (Date y m d) = y < (Year 10000) && y > (Year 1000) && m < (Month 13) && validDay y m d

monthDayList :: [Int]
monthDayList = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

-- is this day allowed for given month + year
validDay :: Year -> Month -> Day -> Bool
validDay (Year y) (Month m) (Day d) = if (isLeap y && m == 2)
                                            then y < 10000 && y >= 1000 && m <= 12 && m > 0 && d <= 29 && d > 0
                                            else y < 10000 && y >= 1000 && m <= 12 && m > 0 && d <= monthDayList !! (m -1) && d > 0

-- leap years every 4, skipping every 400th
isLeap :: Int -> Bool
isLeap y = (y `mod` 4 == 0 && y `mod` 100 /= 0 ) || (y `mod` 400 == 0)

checkTime :: Time -> Bool
checkTime (Time (Hour h) (Minute m) (Second s)) = h < 24 && h >= 0 && m < 60 && m >= 0 && s < 60 && s >= 0

-- Exercise 6

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

-- scan for tokens until end of file/string
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

-- parses multiple strings to a single one, to a token
multiLineTextConstructor :: (String -> Token) -> [String] -> String -> Token
multiLineTextConstructor f s1 s2 = f (foldr (++) s2 s1)

-- look for many lines ending with "\r\n " (we assumed for multiline a space at the start was needed that gets eaten) (allows for multiline text) and then one line ending with just "\r\n"
tWithText :: (String -> Token) -> String -> Parser Char Token
tWithText f s = (multiLineTextConstructor f) <$ token s <*> many ( many (notSymbol '\r') <* symbol '\r' <* symbol '\n' <* symbol ' ') <*> some (notSymbol '\r') <* symbol '\r' <* symbol '\n'

-- parse a token with dateTime
tWithDateTime :: (DateTime -> Token) -> String -> Parser Char Token
tWithDateTime f s = f <$ token s <*> parseDateTime <* symbol '\r' <* symbol '\n'

-- any symbol, except s
notSymbol :: Eq s  => s -> Parser s s
notSymbol x = satisfy (/=x)

-- parses a calendar, errors when the calendar is invalid
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

dateTimeNull :: DateTime
dateTimeNull = (DateTime (Date (Year 1) (Month 1) (Day 1)) (Time (Hour 0) (Minute 0) (Second 0)) True)

isProp :: CalProp -> CalProp -> Bool
isProp (Version) (Version) = True
isProp (ProdId _) (ProdId _) = True
isProp _ _ = False

-- check if an event prop is form a certain type
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

-- parses any calendar property
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

-- parses any event
parseEvent :: Parser Token Event
parseEvent = Event <$ symbol ( TBegin "VEVENT") <*> many parseEventProp <* symbol (TEnd "VEVENT")

-- parses any event property
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
        -- sets the correct newline mode
        hSetNewlineMode x noNewlineTranslation
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
timeOfProperty _ = error "wrong event property"

checkOverlapping :: Calendar -> Bool
checkOverlapping (Calendar _ e) = or $ map (\x -> or $ map (isOverlapping x) dtpairs) dtpairs
                            -- makes the list of events a tuple of (starttime, endtime, uid)
                            where dtpairs = map (\x -> (startTimeEvent x, endTimeEvent x, getUID x)) e

getUID :: Event -> EventProperty
getUID (Event p) = head $ filter (\x -> isEventProp (UID "") x) p

getSummary :: Event -> EventProperty
getSummary (Event p) = head $ filter (\x -> isEventProp (Summary "") x) p

-- checks if two time windows overlap, but if the uids are identical, it means the window is from the same event, so it shouldnt overlap
isOverlapping :: (DateTime, DateTime, EventProperty) -> (DateTime, DateTime, EventProperty) -> Bool
isOverlapping (dts1, dte1, id1) (dts2, dte2, id2) = ((dts2 < dte1 && dts2 >= dts1) || (dts1 < dte2 && dts1 >= dts2)) && id1 /= id2

timeSpent :: String -> Calendar -> Int
timeSpent s (Calendar _ e) = calculateTime $ filter (\x -> containsProperty x (Summary s)) e

containsProperty :: Event -> EventProperty -> Bool
containsProperty (Event e) p = elem p e

-- calculates the total time, merging the overlapping windows
calculateTime :: [Event] -> Int
calculateTime e = totalTime $ mergeOverlapping $ map (\x -> (startTimeEvent x, endTimeEvent x, getUID x)) e

totalTime :: [(DateTime, DateTime, EventProperty)] -> Int
totalTime [] = 0
totalTime (x:xs)= minutesIn x + totalTime xs 

minutesIn :: (DateTime, DateTime, EventProperty) -> Int
minutesIn (x1, x2, _) = (secondsFromZero x2 - secondsFromZero x1) `div` 60

-- used to check the difference between two date times in seconds
secondsFromZero :: DateTime -> Int
secondsFromZero (DateTime (Date (Year x1) (Month x2) (Day x3)) (Time (Hour x4) (Minute x5) (Second x6)) _) = x6 + x5 * 60 + x4 * 3600 + x3 * 86400 + (daysInMonths x2) * 2592000 + x1 * 31536000 + (amountOfLeapYear x1) * 86400

amountOfLeapYear :: Int -> Int
amountOfLeapYear x = x `div` 4 - x `div` 400

daysInMonths :: Int -> Int
daysInMonths 1 = 31
daysInMonths 2 = 28 + daysInMonths 1
daysInMonths 3 = 31 + daysInMonths 2
daysInMonths 4 = 30 + daysInMonths 3
daysInMonths 5 = 31 + daysInMonths 4
daysInMonths 6 = 30 + daysInMonths 5
daysInMonths 7 = 31 + daysInMonths 6
daysInMonths 8 = 31 + daysInMonths 7
daysInMonths 9 = 30 + daysInMonths 8
daysInMonths 10 = 31 + daysInMonths 9
daysInMonths 11 = 30 + daysInMonths 10
daysInMonths 12 = 31 + daysInMonths 11

-- checks if a list of windows contains an overlapping time, and merges the windows if they do
mergeOverlapping :: [(DateTime, DateTime, EventProperty)] -> [(DateTime,DateTime, EventProperty)]
mergeOverlapping [] = []
mergeOverlapping ((x1,x2,x3):xs) = if (or $ map (isOverlapping (x1, x2, x3)) xs)
                                    then mergeTime (x1,x2,x3) ( head ( filter (isOverlapping (x1,x2,x3)) xs ) ) : mergeOverlapping (delete (head ( filter (isOverlapping (x1,x2,x3)) xs )) xs)
                                    else (x1,x2,x3) : mergeOverlapping xs

-- merge two windows to a single one
mergeTime :: (DateTime, DateTime, EventProperty) -> (DateTime, DateTime, EventProperty) -> (DateTime, DateTime, EventProperty)
mergeTime (x1,x2,x3) (y1,y2,y3) = (smallestTime x1 y1, biggestTime x2 y2, UID "0")

smallestTime :: DateTime -> DateTime -> DateTime
smallestTime x y = if (x<y)
                    then x
                    else y

biggestTime :: DateTime -> DateTime -> DateTime
biggestTime x y = if (x>y)
                    then x
                    else y

-- Exercise 11
ppMonth :: Year -> Month -> Calendar -> String
ppMonth (Year y) (Month m) (Calendar _ e) = show $ filter (\x -> (unMonth $ month $date (startTimeEvent x)) == m ) e
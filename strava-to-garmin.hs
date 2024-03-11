{-# LANGUAGE QuasiQuotes #-}
import Prelude
import System.Environment
import System.Exit
import Data.Either
import Data.List
import Data.Time
import Data.Time.Format.ISO8601
import Text.XML.Light

-- Parse relevant data from a .tcx file exported from Strava for Garmin import.
-- Assumes the .tcx is in Technogym's format, 1 lap only.

main = getArgs >>= parseArgs >>= readFile >>= (report . parseFile)

parseArgs [f] = pure f
parseArgs []  = putStrLn "Enter the filename of the input."                         >> exitFailure
parseArgs _   = putStrLn "Invalid arguments, input only the filename of the input." >> exitFailure

data Activity = Activity
  { startTime :: UTCTime
  , totalTimeSeconds :: Int
  , totalDistanceMeters :: Int
  , calories :: Int
  , trackpoints :: [TrackPoint]
  } deriving Show

data TrackPoint = TrackPoint
  { time :: UTCTime
  , distanceMeters :: Float
  , cadence :: Int
  , watts :: Int
  , heartRateBpm :: Int
  } deriving Show

parseFile :: String -> Maybe Activity
parseFile source = do
  let
    garmin_qURI = "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2"
    activity_qURI = "http://www.garmin.com/xmlschemas/ActivityExtension/v2"

    intContent = read . strContent

    fnd n = findElement (QName n (Just garmin_qURI) Nothing)
    fndActivity n = findElement (QName n (Just activity_qURI) Nothing)
    fndInt n l = intContent <$> fnd n l

    parseISO = formatParseM (iso8601Format @UTCTime)

    contents = parseXML source
    activity = head $ (findElements (QName "Activity" (Just garmin_qURI) Nothing)) `concatMap` onlyElems contents

  lap <- fnd "Lap" activity

  startTime <- findAttr (QName "StartTime" Nothing Nothing) lap >>= parseISO

  totalTimeSeconds <- fndInt "TotalTimeSeconds" lap
  totalDistanceMeters <- fndInt "DistanceMeters" lap
  calories <- fndInt "Calories" lap
  track <- elChildren <$> fnd "Track" lap

  let
    parseTrackPoint t = do
      time <- parseISO =<< strContent <$> fnd "Time" t
      distanceMeters <- read . strContent <$> fnd "DistanceMeters" t
      cadence <- fndInt "Cadence" t
      watts <- intContent <$> (fndActivity "TPX" t >>= fndActivity "Watts")
      heartRateBpm <- intContent <$> (fnd "HeartRateBpm" t >>= fnd "Value")
      pure TrackPoint { time, distanceMeters, cadence, watts, heartRateBpm }

  trackpoints <- traverse parseTrackPoint track

  pure Activity { startTime, totalTimeSeconds, totalDistanceMeters, calories, trackpoints }

report Nothing = print "Failed to parse."
report (Just Activity { trackpoints }) = putStrLn $ intercalate "\n" $ map (\t -> show (time t) ++ " " ++ show (watts t)) trackpoints

-- report (Left err) = putStrLn ("Error: Failed to parse file, ") >> exitFailure
-- report (Right res) = putStrLn res >> exitSuccess


{-# LANGUAGE QuasiQuotes #-}
import Prelude
import System.Environment
import System.Exit
import Data.Either
import Text.XML.Light

-- Parse relevant data from a .tcx file exported from Strava for Garmin import.
-- Assumes the .tcx is in Technogym's format, 1 lap only.

main = getArgs >>= parseArgs >>= readFile >>= (print . parseFile)

parseArgs [f] = pure f
parseArgs []  = putStrLn "Enter the filename of the input."                         >> exitFailure
parseArgs _   = putStrLn "Invalid arguments, input only the filename of the input." >> exitFailure

data Activity = Activity
  { startTime :: String
  , totalTimeSeconds :: Int
  , totalDistanceMeters :: Int
  , calories :: Int
  , trackpoints :: [TrackPoint]
  } deriving Show

data TrackPoint = TrackPoint
  { time :: String
  , distanceMeters :: Float
  , cadence :: Int
  , watts :: Int
  , heartRateBpm :: Int
  } deriving Show

parseFile source = do
  let
    garmin_qURI = "http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2"
    activity_qURI = "http://www.garmin.com/xmlschemas/ActivityExtension/v2"

    intContent = read . strContent

    fnd n = findElement (QName n (Just garmin_qURI) Nothing)
    fndActivity n = findElement (QName n (Just activity_qURI) Nothing)
    fndInt n l = do
      el <- fnd n l
      pure $ intContent el

    contents = parseXML source
    activity = head $ (findElements (QName "Activity" (Just garmin_qURI) Nothing)) `concatMap` onlyElems contents

  lap <- fnd "Lap" activity

  startTime <- findAttr (QName "StartTime" Nothing Nothing) lap

  totalTimeSeconds <- fndInt "TotalTimeSeconds" lap
  totalDistanceMeters <- fndInt "DistanceMeters" lap
  calories <- fndInt "Calories" lap
  track <- elChildren <$> fnd "Track" lap

  let
    parseTrackPoint t = do
      time <- strContent <$> fnd "Time" t
      distanceMeters <- read . strContent <$> fnd "DistanceMeters" t
      cadence <- fndInt "Cadence" t
      watts <- intContent <$> (fndActivity "TPX" t >>= fndActivity "Watts")
      heartRateBpm <- intContent <$> (fnd "HeartRateBpm" t >>= fnd "Value")
      pure TrackPoint { time, distanceMeters, cadence, watts, heartRateBpm }

  trackpoints <- traverse parseTrackPoint track

  pure Activity { startTime, totalTimeSeconds, totalDistanceMeters, calories, trackpoints }

report (Left err) = putStrLn ("Error: Failed to parse file, ") >> exitFailure
report (Right res) = putStrLn res >> exitSuccess


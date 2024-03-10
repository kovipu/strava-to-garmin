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
    garminQName s = QName s (Just garmin_qURI) Nothing
    activityQName s = QName s (Just activity_qURI) Nothing
    simpleQName s = QName s Nothing Nothing

    intContent = read . strContent

    contents = parseXML source
    activity = head $ (findElements (garminQName "Activity")) `concatMap` onlyElems contents

  lap <- findElement (garminQName "Lap") activity

  startTime <- findAttr (simpleQName "StartTime") lap

  totalTimeSeconds <- intContent <$> findElement (garminQName "TotalTimeSeconds") lap
  totalDistanceMeters <- intContent <$> findElement (garminQName "DistanceMeters") lap
  calories <- intContent <$> findElement (garminQName "Calories") lap
  track <- elChildren <$> findElement (garminQName "Track") lap
  trackpoints <- traverse (\t -> do
      time <- strContent <$> findElement (garminQName "Time") t
      distanceMeters <- (read . strContent) <$> findElement (garminQName "DistanceMeters") t
      cadence <- intContent <$> findElement (garminQName "Cadence") t
      watts <- intContent <$> (findElement (activityQName "TPX") t >>= findElement (activityQName "Watts"))
      heartRateBpm <- intContent <$> (findElement (garminQName "HeartRateBpm") t >>= findElement (garminQName "Value"))
      pure TrackPoint { time, distanceMeters, cadence, watts, heartRateBpm }
    ) track

  pure Activity { startTime, totalTimeSeconds, totalDistanceMeters, calories, trackpoints }

report (Left err) = putStrLn ("Error: Failed to parse file, ") >> exitFailure
report (Right res) = putStrLn res >> exitSuccess


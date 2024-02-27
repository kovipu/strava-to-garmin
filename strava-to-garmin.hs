{-# LANGUAGE QuasiQuotes #-}
import Prelude
import System.Environment
import System.Exit
import Data.Array ((!))
import Data.Either
import Text.RawString.QQ
import Text.Parsec
import Text.Parsec.Text

main = getArgs >>= parseArgs >>= readFile >>= (report . parse fileParser "")

parseArgs [f] = pure f
parseArgs []  = putStrLn "Enter the filename of the input."                         >> exitFailure
parseArgs _   = putStrLn "Invalid arguments, input only the filename of the input." >> exitFailure

fileParser = do
  many space
  string "<?xml version='1.0' encoding='utf8'?>"
  manyTill anyChar $ try $ string "<Activities>"
  activities <- manyTill anyChar $ try $ string "</Activities>"
  pure $ [r|<?xml version="1.0" encoding="UTF-8" ?>
<TrainingCenterDatabase
  xsi:schemaLocation="http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2 http://www.garmin.com/xmlschemas/TrainingCenterDatabasev2.xsd"
  xmlns:ns5="http://www.garmin.com/xmlschemas/ActivityGoals/v1"
  xmlns:ns3="http://www.garmin.com/xmlschemas/ActivityExtension/v2"
  xmlns:ns2="http://www.garmin.com/xmlschemas/UserProfile/v2"
  xmlns="http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:ns4="http://www.garmin.com/xmlschemas/ProfileExtension/v1">
    <Activities>|]
    ++ activities
    ++ "</Activities></TrainingCenterDatabase>"

report (Left err) = putStrLn ("Error: Failed to parse file, ") >> exitFailure
report (Right res) = putStrLn res >> exitSuccess


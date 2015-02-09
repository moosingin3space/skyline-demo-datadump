{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, DeriveGeneric #-}

import Data.Aeson
import Control.Monad
import GHC.Generics
import ClassyPrelude hiding (mapM_)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import Data.Time
import System.Random
import System.IO

data Metric = Metric { name :: Text, timestamp :: UTCTime, value :: Int }
              deriving (Show, Generic)

instance ToJSON Metric
instance FromJSON Metric

-- Randomly generate a UTCTime
instance Random UTCTime where
    randomR (UTCTime (ModifiedJulianDay day) time, UTCTime (ModifiedJulianDay day') time') gen 
      = let (rday, nextGen) = randomR (day, day') gen  
            toInt = truncate . toRational
            (rdiff, nextGen') = case (rday == day, rday == day') of
                                   (True, False)  -> randomR (toInt time, 86400) nextGen
                                   (False, True)  -> randomR (0, toInt time') nextGen
                                   (_, _) -> randomR (toInt time, toInt time') nextGen
        in (UTCTime (ModifiedJulianDay rday) (secondsToDiffTime rdiff), nextGen')

-- 2015-01-01 - 2015-02-09
    random gen = let (rday, nextGen) = randomR (57023, 57062) gen
                     (rtime, nextGen') = randomR (0, 86400) nextGen
                 in (UTCTime (ModifiedJulianDay rday)  (secondsToDiffTime rtime), nextGen')

randomData name num generator = map mkMetric $ take num $ zip (randoms generator :: [UTCTime]) (randomRs (0, 1000) generator :: [Int])
        where mkMetric tup = Metric name (fst tup) (snd tup)

sendMetric handle metric = do 
                hPut handle message
                hFlush handle
        where message = encode metric ++ "\n"

sendMetrics handle metrics = mapM_ (sendMetric handle) metrics

openHorizonConn hostname port = do
                -- Look up address (allows you to pass DNS names)
                addrInfos <- getAddrInfo Nothing (Just hostname) (Just port)
                let serveraddr = unsafeHead addrInfos
                -- Open socket
                sock <- socket (addrFamily serveraddr) Stream defaultProtocol
                connect sock (addrAddress serveraddr)
                -- Convert to handle
                h <- socketToHandle sock WriteMode
                hSetBuffering h (BlockBuffering Nothing) -- Manually flush
                return h

main = do
    g <- getStdGen
    handle <- openHorizonConn "localhost" "2026"
    let d = randomData "fun" 10 g
    sendMetrics handle d
    sendMetrics stdout d
    hClose handle


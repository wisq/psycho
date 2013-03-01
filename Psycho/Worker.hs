{-# LANGUAGE OverloadedStrings #-}

module Psycho.Worker (
	Worker,

	hostName,
	processId,
	jobClass,

	allWorkers,
	myWorkers
) where

import qualified Database.Redis as Redis
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString, concat)
import qualified Data.ByteString.Char8 as BS
import Data.Either (Either)

import Network.BSD (getHostName)

import Text.JSON
import Data.Maybe (isJust)

import Data.List.Split (splitOn)

import System.Locale
import Data.Time
import Data.Time.Format

data Worker = Worker {
	hostName  :: String,
	processId :: Int,
	jobClass  :: String,
	startTime :: UTCTime
} deriving (Show)

allWorkers :: Redis.ConnectInfo -> IO [Worker]
allWorkers connectInfo = do
	conn <- Redis.connect connectInfo
	Redis.runRedis conn $ do
		redis_ids   <- Redis.smembers "resque:workers"
		let worker_ids = fromRedis redis_ids

		redis_jsons <- mapM getWorker worker_ids
		let workers = parseWorkers $ zip worker_ids (map fromRedis redis_jsons)

		return workers
 where
	getWorker id = Redis.get $ BS.concat ["resque:worker:", id]
	parseWorkers = map parse . filter (isJust . snd)
	parse (id, Just json) = parseWorker id json
	fromRedis (Right x) = x
	fromRedis (Left _)  = error "Redis command failed"

myWorkers :: Redis.ConnectInfo -> IO [Worker]
myWorkers connectInfo = do
	host    <- getHostName
	workers <- allWorkers connectInfo
	return $ filter ((==) host . hostName) workers

parseWorker :: ByteString -> ByteString -> Worker
parseWorker wid = fromJSON . makeWorker wid . fromJSON . decode . BS.unpack
 where
	fromJSON (Ok x) = x
	fromJSON (Error e) = error $ "JSON parsing failed: " ++ e

makeWorker :: ByteString -> JSObject JSValue -> Result Worker
makeWorker wid js = do
	payload   <- js ! "payload"
	run_at    <- js ! "run_at"
	job_class <- payload ! "class"
	return Worker {
		hostName  = host,
		processId = pid,
		jobClass  = job_class,
		startTime = readTime defaultTimeLocale "%Y/%m/%d %H:%M:%S %Z" run_at
	}
 where
	(!) :: JSON a => JSObject JSValue -> String -> Result a
	(!) = flip valFromObj
	host = head splitWid
	pid  = read $ splitWid !! 1
	splitWid = splitOn ":" $ BS.unpack wid

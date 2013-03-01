{-# LANGUAGE OverloadedStrings #-}

import qualified Database.Redis as Redis
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString, concat)
import qualified Data.ByteString.Char8 as BS
import Data.Either

import Text.JSON
import Data.Maybe (isJust)


data Worker = Worker {
	worker_id :: String,
	job_class :: String
} deriving (Show)

makeWorker :: ByteString -> JSObject JSValue -> Result Worker
makeWorker wid js = do
	payload <- js ! "payload"
	job_class <- payload ! "class"
	return Worker {worker_id = BS.unpack(wid), job_class = job_class}
 where
	(!) :: JSON a => JSObject JSValue -> String -> Result a
	(!) = flip valFromObj


main = do
	conn <- Redis.connect Redis.defaultConnectInfo
	Redis.runRedis conn $ do
		redis_ids   <- Redis.smembers "resque:workers"
		let worker_ids = fromRedis redis_ids

		redis_jsons <- mapM getWorker worker_ids
		let workers = parseWorkers $ zip worker_ids (map fromRedis redis_jsons)

		liftIO $ print workers
 where
	getWorker id = Redis.get $ BS.concat ["resque:worker:", id]
	parseWorkers = map parse . filter (isJust . snd)
	parse (id, Just json) = parseWorker id json
	fromRedis (Right x) = x
	fromRedis (Left _)  = error "Redis command failed"

parseWorker :: ByteString -> ByteString -> Worker
parseWorker wid = fromJSON . makeWorker wid . fromJSON . decode . BS.unpack
 where
	fromJSON (Ok x) = x
	fromJSON (Error e) = error ("JSON parsing failed: " ++ e)

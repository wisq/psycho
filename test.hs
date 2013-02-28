{-# LANGUAGE OverloadedStrings #-}

import qualified Database.Redis as Redis
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString, concat)
import qualified Data.ByteString as BS
import Data.Either

import Text.JSON

main = do
	conn <- Redis.connect Redis.defaultConnectInfo
	Redis.runRedis conn $ do
		worker_ids   <- Redis.smembers "resque:workers"
		worker_jsons <- sequence $ getWorkers worker_ids
		let workers = parseWorkers worker_jsons
		liftIO $ print $ head workers
 where
	getWorkers (Right ids)  = map getWorker ids
	getWorker id = Redis.get $ BS.concat ["resque:worker:", id]

parseWorkers = foldl (++) [] . map parse
 where
	parse (Right (Just json)) = [parseWorker(json)]
	parse (Right Nothing) = []

parseWorker json = json

data Worker = Worker {
	job_class :: String
} deriving (Show)

makeWorker :: JSObject JSValue -> Result Worker
makeWorker json = do
	payload <- json ! "payload"
	job_class <- payload ! "class"
	return Worker {job_class = job_class}
 where
	(!) :: JSON a => JSObject JSValue -> String -> Result a
	(!) = flip valFromObj

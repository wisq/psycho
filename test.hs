{-# LANGUAGE OverloadedStrings #-}

import qualified Database.Redis as Redis
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString, concat)
import qualified Data.ByteString.Char8 as BS
import Data.Either

import Text.JSON


data Worker = Worker {
	job_class :: String
} deriving (Show)

instance JSON Worker where
	readJSON json = do
		obj <- readJSON json
		payload <- obj ! "payload"
		job_class <- payload ! "class"
		return Worker {job_class = job_class}
	    where
		(!) :: JSON a => JSObject JSValue -> String -> Result a
		(!) = flip valFromObj

	showJSON = undefined



main = do
	conn <- Redis.connect Redis.defaultConnectInfo
	Redis.runRedis conn $ do
		worker_ids   <- Redis.smembers "resque:workers"
		worker_jsons <- sequence $ getWorkers worker_ids
		let workers = parseWorkers worker_jsons
		liftIO $ print workers
 where
	getWorkers (Right ids)  = map getWorker ids
	getWorker id = Redis.get $ BS.concat ["resque:worker:", id]

parseWorkers = foldl (++) [] . map parse
 where
	parse (Right (Just json)) = [parseWorker json]
	parse (Right Nothing) = []

parseWorker :: ByteString -> Worker
parseWorker = fromOk . decode . BS.unpack
 where
	fromOk (Ok x) = x
	fromOk (Error e) = error ("JSON parsing failed: " ++ e)

{-# LANGUAGE OverloadedStrings #-}

import Database.Redis
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString, concat)
import qualified Data.ByteString as BS
import Data.Either

main = do
	conn <- connect defaultConnectInfo
	runRedis conn $ do
		worker_ids   <- smembers "resque:workers"
		worker_jsons <- sequence $ getWorkers worker_ids
		let workers = parseWorkers worker_jsons
		liftIO $ print $ head workers
 where
	getWorkers (Right ids)  = map getWorker ids
	getWorker id = get $ BS.concat ["resque:worker:", id]

parseWorkers = foldl (++) [] . map parse
 where
	parse (Right (Just json)) = [parseWorker(json)]
	parse (Right Nothing) = []

parseWorker json = json

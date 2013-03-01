import Psycho.Worker
import Psycho.Process

import System.Console.GetOpt
import System.Environment
import System.IO
import System.Exit

import qualified Database.Redis as Redis

import Data.Maybe

data Flag = RedisHost String
	  | RedisPort Int

data Options = Options
  { optRedisInfo :: Redis.ConnectInfo,
    optAllHosts  :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  { optRedisInfo = Redis.defaultConnectInfo
  , optAllHosts  = False
  }

options :: [ OptDescr (Options -> IO Options) ]
options =
	[ Option "h" ["host"]
		(ReqArg (\arg opt -> return $ setRedisHost opt arg)
		"HOST")
		"Redis server host"

	, Option "p" ["port"]
		(ReqArg (\arg opt -> return $ setRedisPort opt arg)
		"PORT")
		"Redis server port"

	, Option "a" ["all"]
		(NoArg (\opt -> return opt { optAllHosts = True }))
		"Select workers on all hosts"

	, Option [] ["help"]
		(NoArg (\_ -> do
		prg <- getProgName
		hPutStrLn stderr (usageInfo prg options)
		exitSuccess))
		"Show help"
	]
 where
	setRedisHost opt arg = opt { optRedisInfo = (optRedisInfo opt) { Redis.connectHost = arg } }
	setRedisPort opt arg = opt { optRedisInfo = (optRedisInfo opt) { Redis.connectPort = redisPort arg } }
	redisPort = Redis.PortNumber . fromIntegral . read


main = do
	args <- getArgs
	let (actions, nonOptions, errors) = getOpt RequireOrder options args
	opts <- foldl (>>=) (return defaultOptions) actions

	workers <- (if optAllHosts opts then allWorkers else myWorkers) $ optRedisInfo opts
	subs <- mapM (getSubProcess . processId) workers
	print $ catMaybes subs

import Psycho.Worker

import System.Console.GetOpt
import System.Environment
import System.IO
import System.Exit

import qualified Database.Redis as Redis

data Flag = RedisHost String
	  | RedisPort Int

data Options = Options
  { optRedisInfo :: Redis.ConnectInfo
  }

defaultOptions :: Options
defaultOptions = Options
  { optRedisInfo = Redis.defaultConnectInfo
  }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "h" ["host"]
	(ReqArg
	    (\arg opt -> return opt { optRedisInfo = (optRedisInfo opt) { Redis.connectHost = arg } })
	    "HOST")
	"Redis server host"

    , Option "p" ["port"]
	(ReqArg
	    (\arg opt -> return opt { optRedisInfo = (optRedisInfo opt) { Redis.connectPort = redisPort arg } })
	    "PORT")
	"Redis server port"

    , Option [] ["help"]
	(NoArg
	    (\_ -> do
		prg <- getProgName
		hPutStrLn stderr (usageInfo prg options)
		exitWith ExitSuccess))
	"Show help"
    ]
 where
	redisPort = Redis.PortNumber . fromIntegral . read


main = do
	args <- getArgs
	let (actions, nonOptions, errors) = getOpt RequireOrder options args
	opts <- foldl (>>=) (return defaultOptions) actions

	workers <- myWorkers $ optRedisInfo opts
	print workers

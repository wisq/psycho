import Psycho.Worker

import System.Console.GetOpt
import System.Environment
import System.IO
import System.Exit

import qualified Database.Redis as Redis

data Flag = RedisHost String
	  | RedisPort Int

data Options = Options
  { optRedisHost :: String
  , optRedisPort :: Int
  } deriving Show

defaultOptions :: Options
defaultOptions = Options
  { optRedisHost = "localhost"
  , optRedisPort = 6379
  }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "h" ["host"]
	(ReqArg
	    (\arg opt -> return opt { optRedisHost = arg })
	    "HOST")
	"Redis server host"

    , Option "p" ["port"]
	(ReqArg
	    (\arg opt -> return opt { optRedisPort = (read arg) })
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


main = do
    args <- getArgs

    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt RequireOrder options args

    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return defaultOptions) actions

    let Options { optRedisHost = host
		, optRedisPort = port } = opts

    print opts

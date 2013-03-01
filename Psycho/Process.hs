module Psycho.Process (
	getSubProcess
) where

--import Prelude hiding (catch)
import System.Linux.ProcStat
import Text.Regex.PCRE
import Control.Exception

getSubProcess :: Pid -> IO (Maybe Pid)
getSubProcess pid = do
	cmd <- safeGetCommand pid
	return $ readSubprocess cmd
 where
	readSubprocess Nothing  = Nothing
	readSubprocess (Just s) = extract $ s =~ "^resque-[^:]+: Forked (\\d+) at \\d"
	extract :: [[String]] -> Maybe Pid
	extract [gs] = Just $ read $ gs !! 1
	extract _ = Nothing

getCommand :: Pid -> IO String
getCommand pid = readFile cmdLine
 where
	cmdLine = "/proc/" ++ show pid ++ "/cmdline"

safeGetCommand :: Pid -> IO (Maybe String)
safeGetCommand pid = handle errorHandler $ fmap Just $ getCommand pid
 where
	errorHandler :: SomeException -> IO (Maybe String)
	errorHandler _ = return Nothing

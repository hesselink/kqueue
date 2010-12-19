{-# LANGUAGE CPP #-}
-- This file shows how to monitor a single file (name) through
-- creation and deletion. The actual code calling the kqueue functions
-- is in System.KQueue.HighLevel.
import Control.Monad
import System.Directory
import System.Environment
import System.FilePath
import System.KQueue.HighLevel

-- Monitor the file command line argument or ./foo for changes until
-- ctrl-C is received. Prints the events as they are received. The
-- file can be created or deleted during a run, and this is also
-- reported.
main :: IO ()
main =
  do args <- getArgs
     let file = if null args then "foo" else head args
     (dir, filename) <- splitFileName `liftM` canonicalizePath file
     putStrLn $ "Watching file " ++ filename
     putStrLn $ "Watching in " ++ dir
     watcher <- watchFile file (\chg -> putStrLn ("File " ++ filename ++ " was " ++ show chg))
     waitForTermination
     stopWatching watcher

-- Copied from happstack.
waitForTermination :: IO ()
waitForTermination
    = do
#ifdef UNIX
         istty <- queryTerminal stdInput
         mv <- newEmptyMVar
         installHandler softwareTermination (CatchOnce (putMVar mv ())) Nothing
         case istty of
           True  -> do installHandler keyboardSignal (CatchOnce (putMVar mv ())) Nothing
                       return ()
           False -> return ()
         takeMVar mv
#else
         let loop 'e' = return () 
             loop _   = getChar >>= loop
         loop 'c'
#endif

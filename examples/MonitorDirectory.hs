{-# LANGUAGE CPP #-}
import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.List
import Foreign.Ptr
import System.Directory
import System.FilePath
import System.IO
import System.KQueue
import System.Posix.IO
import System.Posix.Types

-- Monitor ./foo for changes until ctrl-C is received. Prints the
-- events as they are received. The file can be created or deleted
-- during a run, and this is also reported.
main :: IO ()
main = watchFile "foo" (\chg -> putStrLn ("File foo was " ++ show chg))

data EventType = Changed | Created | Deleted deriving Show

watchFile :: FilePath -> (EventType -> IO ()) -> IO ()
watchFile file callback =
  do hSetBuffering stdout LineBuffering
     kq <- kqueue
     putStrLn $ "Watching file " ++ file
     dir <- takeDirectory `liftM` canonicalizePath file
     putStrLn $ "Watching in " ++ dir
     _ <- forkIO $ watchDirectoryForFile kq dir file callback
     -- Doesn't seem to work...
     waitForTermination

watchDirectoryForFile :: KQueue -> FilePath -> FilePath -> (EventType -> IO ()) -> IO ()
watchDirectoryForFile kq dir file callback =
  do -- Event structures for the directory and file to monitor
     dfd <- openFd dir ReadOnly Nothing defaultFileFlags
     let dirEvent = KEvent
           { ident = fromIntegral dfd
           , evfilter = EvfiltVnode
           , flags = [EvOneshot]
           , fflags = [NoteWrite]
           , data_ = 0
           , udata = nullPtr
           }
         fileEvent ffd = KEvent
           { ident = fromIntegral ffd
           , evfilter = EvfiltVnode
           , flags = [EvOneshot]
           , fflags = [NoteDelete, NoteWrite, NoteRename]
           , data_ = 0
           , udata = nullPtr
           }
         setFlag flag ev = ev { flags = flag : flags ev }
     -- Initialize IORef holding possible file descriptor if the file
     -- we're monitoring exists
     existsInitial <- doesFileExist file
     mInitialFd <-
       if existsInitial
       then Just `liftM` openFd file ReadOnly Nothing defaultFileFlags
       else return Nothing
     mFdRef <- newIORef mInitialFd
     -- Add the event(s) to the queue.
     let eventsToAdd = dirEvent : maybe [] (return . fileEvent) mInitialFd
     _ <- kevent kq (map (setFlag EvAdd) eventsToAdd) 0 Nothing
     -- Forever listen to the events, calling the relevant callbacks.
     forever $ do
       -- Figure out which events we're currently monitoring.
       mFd <- readIORef mFdRef
       let eventsToMonitor = dirEvent : maybe [] (return . fileEvent) mFd
       -- Block until we get at least one event.
       [firstChg] <- kevent kq eventsToMonitor 1 Nothing
       -- Collect all other events that occurred at the same time.
       otherChgs  <- getAllChanges kq (map (setFlag EvAdd) eventsToMonitor)
       let chgs = firstChg : otherChgs
       -- If the file was written to, we call the relevant callback.
       when (NoteWrite `elem` [ fflag | fileChg <- chgs, ident fileChg /= fromIntegral dfd, fflag <- fflags fileChg]) $
         callback Changed
       -- If the file was created, we just get a NoteWrite
       -- on the directory. So we compare the existence of the file
       -- before and after the directory event. Delete is done in the
       -- same way for simplicity. Modifications are sometimes more
       -- difficult, see third case.
       exists <- doesFileExist file
       case (exists, mFd) of
         (True,  Nothing) -> -- The file was created.
           do callback Created
              -- Start monitoring it.
              fd <- openFd file ReadOnly Nothing defaultFileFlags
              _ <- kevent kq [setFlag EvAdd (fileEvent fd)] 0 Nothing
              writeIORef mFdRef (Just fd)
         (False, Just fd) -> -- The file was deleted.
           do callback Deleted
              -- Stop monitoring it.
              _ <- kevent kq [setFlag EvDelete (fileEvent fd)] 0 Nothing
              writeIORef mFdRef Nothing
         -- Sometimes (for example, when vi writes a file) we get a
         -- NoteRename, a NoteDelete, and a NoteWrite on the directory
         -- (the file being recreated). We want to report this as a
         -- change. We collect all these in one batch, so we see a
         -- delete and/or rename on the file, but it exists both
         -- before and after the events.
         (True,  Just fd) | not . null . filter (isDeleteOrRename fd) $ chgs ->
           do callback Changed
              -- Remove the event on the old file.
              _ <- kevent kq [setFlag EvDelete (fileEvent fd)] 0 Nothing
              -- Add the event on the new file.
              newFd <- openFd file ReadOnly Nothing defaultFileFlags
              _ <- kevent kq [setFlag EvAdd (fileEvent newFd)] 0 Nothing
              writeIORef mFdRef (Just newFd)
         -- A directory event, but not on the file we're interested
         -- in.
         (_,     _      ) -> return ()

isDeleteOrRename :: Fd -> KEvent -> Bool
isDeleteOrRename fd evt = fromIntegral fd == ident evt
                       && (not . null . intersect [NoteDelete, NoteRename] . fflags) evt

getAllChanges :: KQueue -> [KEvent] -> IO [KEvent]
getAllChanges kq evts = go []
  where
    go collectedChgs =
      do chgs <- kevent kq evts 10 (Just 0.1)
         if null chgs
           then return collectedChgs
           else go (collectedChgs ++ chgs)

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

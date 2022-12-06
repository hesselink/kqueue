-- | This module contains higher-level abstraction for monitoring file
-- system changes, built on top of the bindings from "System.KQueue".
module System.KQueue.HighLevel
  ( watchFile
  , stopWatching
  , EventType (..)
  , Watcher
  ) where

import Control.Concurrent  (ThreadId, forkIO, killThread)
import Control.Monad.State (StateT, evalStateT, forever, get, liftIO, liftM, put, when)
import Data.List           (intersect)
import Foreign.Ptr         (nullPtr)
import System.Directory    (canonicalizePath, doesFileExist)
import System.FilePath     (takeDirectory)
import System.Posix.IO     (OpenMode (ReadOnly), defaultFileFlags, openFd)
import System.Posix.Types  (Fd)

import System.KQueue

-- | The type of file change that occurred.
data EventType = Changed | Created | Deleted deriving Show

-- | An identifier for the watcher of a file. Allows you to stop
-- watching it later.
newtype Watcher = Watcher ThreadId deriving Show

-- | Watch a file for changes. The file doesn't have to exist, but the
-- directory it is in, does. Returns immediately. You can stop
-- watching by passing the 'Watcher' to 'stopWatching'.
watchFile :: FilePath -> (EventType -> IO ()) -> IO Watcher
watchFile file callback =
  do kq <- kqueue
     dir <- takeDirectory `liftM` canonicalizePath file
     tid <- forkIO $ watchDirectoryForFile kq dir file callback
     return $ Watcher tid

-- | Stop a watcher from watching.
stopWatching :: Watcher -> IO ()
stopWatching (Watcher tid) = killThread tid

-- | Watch a directory and a file for changes to that file. This
-- function does some initialization, and then runs 'monitorChanges'
-- forever.
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
         mkFileEvent ffd = KEvent
           { ident = fromIntegral ffd
           , evfilter = EvfiltVnode
           , flags = [EvOneshot]
           , fflags = [NoteDelete, NoteWrite, NoteRename]
           , data_ = 0
           , udata = nullPtr
           }
     -- Initialize IORef holding possible file descriptor if the file
     -- we're monitoring exists
     exists <- doesFileExist file
     mFd <-
       if exists
       then Just `liftM` openFd file ReadOnly Nothing defaultFileFlags
       else return Nothing
     -- Add the event(s) to the queue.
     let eventsToAdd = dirEvent : maybe [] (return . mkFileEvent) mFd
     _ <- kevent kq (map (setFlag EvAdd) eventsToAdd) 0 Nothing
     -- Forever listen to the events, calling the relevant callbacks.
     flip evalStateT mFd . forever $ monitorChanges kq dirEvent mkFileEvent callback file

-- | Monitor changes on a file and directory. This is just a wrapper
-- for the state updates; the actual work is done in
-- 'monitorChangesIO'.
monitorChanges :: KQueue -> KEvent -> (Fd -> KEvent) -> (EventType -> IO ()) -> FilePath -> StateT (Maybe Fd) IO ()
monitorChanges kq dirEvent mkFileEvent callback file =
  do mFd <- get
     newMFd <- liftIO $ monitorChangesIO kq dirEvent mkFileEvent callback file mFd
     put newMFd

-- | Monitor changes on a file and directory. Calls the callback when
-- the file has changed, or has been created or removed. Returns the
-- file descriptor of the file, if it exists.
monitorChangesIO :: KQueue -> KEvent -> (Fd -> KEvent) -> (EventType -> IO ()) -> FilePath -> Maybe Fd -> IO (Maybe Fd)
monitorChangesIO kq dirEvent mkFileEvent callback file mFd =
  do  -- Figure out which events we're currently monitoring.
     let eventsToMonitor = dirEvent : maybe [] (return . mkFileEvent) mFd
     -- Block until we get at least one event.
     [firstChg] <- kevent kq eventsToMonitor 1 Nothing
     -- Collect all other events that occurred at the same time.
     otherChgs  <- getAllEvents kq (map (setFlag EvAdd) eventsToMonitor)
     let chgs = firstChg : otherChgs
     -- If the file was written to, we call the relevant callback.
     when (NoteWrite `elem` [ fflag | fileChg <- chgs, ident fileChg /= ident dirEvent, fflag <- fflags fileChg]) $
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
            _ <- kevent kq [setFlag EvAdd (mkFileEvent fd)] 0 Nothing
            return (Just fd)
       (False, Just fd) -> -- The file was deleted.
         do callback Deleted
            -- Stop monitoring it.
            _ <- kevent kq [setFlag EvDelete (mkFileEvent fd)] 0 Nothing
            return Nothing
       -- Sometimes (for example, when vi writes a file) we get a
       -- NoteRename, a NoteDelete, and a NoteWrite on the directory
       -- (the file being recreated). We want to report this as a
       -- change. We collect all these in one batch, so we see a
       -- delete and/or rename on the file, but it exists both
       -- before and after the events.
       (True,  Just fd) | not . null . filter (isDeleteOrRename fd) $ chgs ->
         do callback Changed
            -- Remove the event on the old file.
            _ <- kevent kq [setFlag EvDelete (mkFileEvent fd)] 0 Nothing
            -- Add the event on the new file.
            newFd <- openFd file ReadOnly Nothing defaultFileFlags
            _ <- kevent kq [setFlag EvAdd (mkFileEvent newFd)] 0 Nothing
            return (Just newFd)
       -- A directory event, but not on the file we're interested
       -- in.
       (_,     _      ) -> return mFd

-- | Is the KEvent a delete or rename on the file descriptor given?
isDeleteOrRename :: Fd -> KEvent -> Bool
isDeleteOrRename fd evt = fromIntegral fd == ident evt
                       && (not . null . intersect [NoteDelete, NoteRename] . fflags) evt

-- | Get all events that are currently in the queue for the given
-- changelist. Waits 0.1s for new events, to consolidate multiple
-- related events on a file save.
getAllEvents :: KQueue -> [KEvent] -> IO [KEvent]
getAllEvents kq evts = go []
  where
    go collectedChgs =
      do chgs <- kevent kq evts 10 (Just 0.1)
         if null chgs
           then return collectedChgs
           else go (collectedChgs ++ chgs)

-- | Set (add) a flag on a 'KEvent'. Normally I would use fclabels
-- here, but since this is only one function, I'll define it here, so
-- I don't have to depend on the package.
setFlag :: (Flag -> KEvent -> KEvent)
setFlag flag ev = ev { flags = flag : flags ev }

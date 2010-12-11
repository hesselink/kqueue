import Control.Monad
import Foreign.Ptr
import System.KQueue
import System.Posix.IO

-- Monitor /tmp/foo for changes until it is deleted. Prints the events
-- as they are received. The file has to exist when this program is
-- started.
main = do
  -- Initialize the queue.
  kq <- kqueue
  -- Open file descriptor.
  fd <- openFd "/tmp/foo" ReadOnly Nothing defaultFileFlags
  -- Construct the event to monitor.
  let event = KEvent
        { ident = fromIntegral fd
        , evfilter = EvfiltVnode
        , flags = [EvAdd, EvOneshot]
        , fflags = [NoteDelete, NoteExtend, NoteWrite, NoteAttrib, NoteRename]
        , data_ = 0
        , udata = nullPtr
        }
  let poll = do
        -- Add or poll the event.
        chgs <- kevent kq [event] 1 Nothing
        -- If something happened, print it.
        when (not . null $ chgs) (print chgs)
        -- If the file was deleted, stop. Otherwise, continue polling.
        when (not (NoteDelete `elem` fflags (head chgs))) poll
  poll

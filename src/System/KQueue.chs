{-# LANGUAGE DeriveDataTypeable
           , EmptyDataDecls
           , ForeignFunctionInterface
           #-}
module System.KQueue
  ( KQueue
  , kqueue
  , KEvent (..)
  , Filter (..)
  , Flag (..)
  , FFlag (..)
  , kevent
  , KQueueException
  ) where

#include <sys/time.h>
#include <sys/event.h>

import Control.Applicative ( (<$>), (<*>) )
import Control.Exception   ( Exception, throwIO )
import Data.List           ( foldl' )
import Data.Maybe          ( mapMaybe )
import Data.Time.Clock     ( NominalDiffTime )
import Data.Typeable       ( Typeable )
import Foreign             ( (.|.)
                           , Ptr
                           , Storable (..)
                           , allocaArray
                           , bit
                           , bitSize
                           , maybeWith
                           , testBit
                           , peekArray
                           , with
                           , withArray
                           )
import Foreign.C           ( CInt
                           , CLong
                           , CShort
                           , CTime
                           , CUInt
                           , CULong
                           , CUShort
                           )

-- | A kernel event queue.
newtype KQueue = KQueue CInt -- The descriptor

-- | Create a new KQueue.
kqueue :: IO KQueue
kqueue = KQueue <$> {#call kqueue as kqueue_ #}

-- | A kernel event.
data KEvent = KEvent
  { ident    :: CULong  -- ^ The identifier for the event, often a file descriptor.
  , evfilter :: Filter  -- ^ The kernel filter (type of event).
  , flags    :: [Flag]  -- ^ Actions to perform on the event.
  , fflags   :: [FFlag] -- ^ Filter-specific flags.
  , data_    :: CLong   -- ^ Filter-specific data value.
  , udata    :: Ptr ()  -- ^ User-defined data, passed through unchanged.
  } deriving (Show, Eq)

-- TODO: nicer types for ident, data_ and udata.

#c
enum Filter
  { EvfiltRead = EVFILT_READ
  , EvfiltWrite = EVFILT_WRITE
  , EvfiltAio = EVFILT_AIO
  , EvfiltVnode = EVFILT_VNODE
  , EvfiltProc = EVFILT_PROC
  , EvfiltSignal = EVFILT_SIGNAL
  , EvfiltTimer = EVFILT_TIMER
// Not on Mac OS X
// , EvfiltUser = EVFILT_USER
  };
#endc

-- | The types of kernel events.
{#enum Filter {} deriving (Show, Eq) #}

#c
enum Flag
  { EvAdd      = EV_ADD
  , EvEnable   = EV_ENABLE
  , EvDisable  = EV_DISABLE
// Not on Mac OS X
//  , EvDispatch = EV_DISPATCH
  , EvDelete   = EV_DELETE
  , EvReceipt  = EV_RECEIPT
  , EvOneshot  = EV_ONESHOT
  , EvClear    = EV_CLEAR
  , EvEof      = EV_EOF
  , EvError    = EV_ERROR
  };
#endc

-- | The actions to perform on the event.
{#enum Flag {} deriving (Show, Eq) #}

#c
enum FFlag
  { NoteDelete = NOTE_DELETE
  , NoteWrite  = NOTE_WRITE
  , NoteExtend = NOTE_EXTEND
  , NoteAttrib = NOTE_ATTRIB
  , NoteLink   = NOTE_LINK
  , NoteRename = NOTE_RENAME
  , NoteRevoke = NOTE_REVOKE
// Seems to have the same value as NoteDelete
//  , NoteLowat  = NOTE_LOWAT
  , NoteExit   = NOTE_EXIT
  , NoteFork   = NOTE_FORK
  , NoteExec   = NOTE_EXEC
  , NoteSignal = NOTE_SIGNAL
  , NoteReap   = NOTE_REAP
  };
#endc

-- | The filter specific flags.
{#enum FFlag {} deriving (Show, Eq) #}

-- | Convert a list of enumeration values to an integer by combining
-- them with bitwise 'or'.
enumToBitmask :: Enum a => [a] -> Int
enumToBitmask = foldl' (.|.) 0 . map fromEnum

-- | Convert an integer to a list of enumeration values by testing
-- each bit, and if set, convert it to an enumeration member.
bitmaskToEnum :: Enum a => Int -> [a]
bitmaskToEnum bm = mapMaybe maybeBit [0 .. bitSize bm - 1]
  where
    maybeBit b | testBit bm b = Just . toEnum . bit $ b
               | otherwise    = Nothing

#c
typedef struct kevent kevent_t;
#endc

instance Storable KEvent where
  sizeOf _ = {#sizeof kevent_t #}
  alignment _ = 24
  peek e = KEvent <$>                                     ({#get kevent_t->ident  #} e)
                  <*> fmap (toEnum . fromIntegral)        ({#get kevent_t->filter #} e)
                  <*> fmap (bitmaskToEnum . fromIntegral) ({#get kevent_t->flags  #} e)
                  <*> fmap (bitmaskToEnum . fromIntegral) ({#get kevent_t->fflags #} e)
                  <*>                                     ({#get kevent_t->data   #} e)
                  <*>                                     ({#get kevent_t->udata  #} e)
  poke e ev =
    do {#set kevent_t->ident  #} e (ident                                   ev)
       {#set kevent_t->filter #} e (fromIntegral . fromEnum . evfilter    $ ev)
       {#set kevent_t->flags  #} e (fromIntegral . enumToBitmask . flags  $ ev)
       {#set kevent_t->fflags #} e (fromIntegral . enumToBitmask . fflags $ ev)
       {#set kevent_t->data   #} e (data_                                   ev)
       {#set kevent_t->udata  #} e (udata                                   ev)

data TimeSpec = TimeSpec
  { seconds     :: CTime
  , nanoseconds :: CLong
  } deriving (Show, Eq)

#c
typedef struct timespec timespec_t;
#endc

-- TODO: waarom krijg ik geen CTime maar een CLong als seconds bij gebruik van #get/#set?
instance Storable TimeSpec where
  sizeOf _ = {#sizeof timespec_t #}
  alignment _ = 8
  peek t = TimeSpec <$> (\ptr -> peekByteOff ptr 0 :: IO CTime)  t
                    <*> {#get timespec_t->tv_nsec #} t
  poke t ts =
    do (\ptr val -> pokeByteOff ptr 0 (val :: CTime)) t (seconds $ ts)
       {#set timespec_t->tv_nsec #} t (nanoseconds            ts)

nominalDiffTimeToTimeSpec :: NominalDiffTime -> TimeSpec
nominalDiffTimeToTimeSpec dt = TimeSpec
  { seconds     = fromInteger s
  , nanoseconds = floor . (* 1000000000) $ ns
  }
  where
    (s, ns) = properFraction dt

foreign import ccall "kevent" kevent_ :: CInt -> Ptr KEvent -> CInt -> Ptr KEvent -> CInt -> Ptr TimeSpec -> IO CInt

data KQueueException = KQueueException
  deriving (Show, Typeable)

instance Exception KQueueException

-- | Add events to monitor, or retrieve events from the kqueue. If an
-- error occurs, will throw a 'KQueueException' if there is no room in
-- the returned event list. Otherwise, will set 'EvError' on the event
-- and add it to the returned event list.
kevent ::  KQueue               -- ^ The kernel queue to operate on.
       -> [KEvent]              -- ^ The list of events to start monitoring, or changes to retrieve.
       -> Int                   -- ^ The maximum number of events to retrieve.
       -> Maybe NominalDiffTime -- ^ Timeout. When nothing, blocks until an event has occurred.
       -> IO [KEvent]           -- ^ A list of events that have occurred.
kevent (KQueue kq) changelist nevents mtimeout =
  withArray changelist $ \chArray ->
  allocaArray nevents  $ \evArray ->
  maybeWith with (nominalDiffTimeToTimeSpec <$> mtimeout) $ \timeout ->
    do ret <- kevent_ kq chArray (fromIntegral . length $ changelist) evArray (fromIntegral nevents) timeout
       case ret of
         -- Error while processing changelist, and no room in return array.
         -1 -> throwIO KQueueException
         -- Timeout.
         0  -> return []
         -- Returned n events. Can contain errors. The change that
         -- failed will be in the event list. EV_ERROR will be set on the
         -- event.
         n  -> peekArray (fromIntegral n) evArray

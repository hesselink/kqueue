{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module System.KQueue where

#include <sys/event.h>

import Control.Applicative
import Foreign
import Foreign.C

-- | A kernel event queue.
newtype KQueue = KQueue CInt -- The descriptor

foreign import ccall "kqueue" kqueue_ :: IO CInt

kqueue :: IO KQueue
kqueue = KQueue <$> kqueue_

data KEvent = KEvent
  { ident  :: CULong
  , filter :: CShort
  , flags  :: CUShort
  , fflags :: CUInt
  , data_  :: CLong
  , udata  :: Ptr ()
  }

#c
typedef struct kevent kevent_t;
#endc

instance Storable KEvent where
  sizeOf _ = {#sizeof kevent_t #}
  alignment _ = 24
  peek e = KEvent <$> ({#get kevent_t->ident  #} e)
                  <*> ({#get kevent_t->filter #} e)
                  <*> ({#get kevent_t->flags  #} e)
                  <*> ({#get kevent_t->fflags #} e)
                  <*> ({#get kevent_t->data  #} e)
                  <*> ({#get kevent_t->udata  #} e)

data TimeSpec

foreign import ccall "kevent" kevent_ :: CInt -> Ptr KEvent -> CInt -> Ptr KEvent -> CInt -> Ptr TimeSpec -> IO CInt

kevent :: KQueue -> [KEvent] -> [KEvent] -> TimeSpec -> IO ()
kevent (KQueue kq) changelist eventlist timespec = do
  kevent_ kq undefined undefined undefined undefined undefined
  return ()

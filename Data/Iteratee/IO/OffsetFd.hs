{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -Wall #-}

module Data.Iteratee.IO.OffsetFd (
      enumFdRandomOBS
    , enumFileRandomOBS
    , fileDriverRandomFdOBS
    , fileDriverRandomOBS
) where

import Control.Arrow (second)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Iteratee.Iteratee
import Data.Iteratee.Base.ReadableChunk (ReadableChunk (..))
import Data.Iteratee.Binary()
import Data.Offset (Offset(..))

import Control.Concurrent (yield)
import Control.Exception as Ex
import Control.Monad
import Control.Monad.CatchIO as CIO
import Control.Monad.IO.Class

import Foreign.Ptr

import System.IO (SeekMode(..))

import System.Posix hiding (FileOffset)

----------------------------------------------------------------------
-- Copied from Data.Iteratee.IO.Posix

import Foreign.C

-- |The following fseek procedure may throw an exception :(
myfdSeek:: Fd -> SeekMode -> FileOffset -> IO FileOffset
myfdSeek (Fd fd) mode off =
   throwErrnoIfMinus1 "zoom-cache:myfdseek" $ cLSeek fd off (mode2Int mode)
 where mode2Int :: SeekMode -> CInt     -- From GHC source
       mode2Int AbsoluteSeek = 0
       mode2Int RelativeSeek = 1
       mode2Int SeekFromEnd  = 2

foreign import ccall unsafe "unistd.h lseek" cLSeek
  :: CInt -> FileOffset -> CInt -> IO FileOffset

----------------------------------------------------------------------
-- Copied from Data.Iteratee.IO

-- | The default buffer size.
defaultBufSize :: Int
defaultBufSize = 1024

----------------------------------------------------------------------

makefdCallback ::
  (MonadIO m)
  => ByteCount
  -> Fd
  -> Callback st m B.ByteString
makefdCallback bufsize fd st = do
  let fillFn p = fmap fromIntegral . fdReadBuf fd (castPtr p) . fromIntegral
  (s,numCopied) <- liftIO $ fillFromCallback (fromIntegral bufsize) fillFn
  case numCopied of
    0   -> liftIO yield >> return ((Finished, st), s)
    _n  -> return ((HasMore, st), s)
{-# INLINABLE makefdCallback #-}


makefdCallbackOBS ::
  (MonadIO m) =>
  ByteCount
  -> Fd
  -> Callback st m (Offset B.ByteString)
makefdCallbackOBS bufsize fd st = do
  o <- liftIO $ myfdSeek fd RelativeSeek 0
  liftM (second (Offset o)) (makefdCallback bufsize fd st)

-- |A variant of enumFd that catches exceptions raised by the @Iteratee@.
enumFdCatchOBS
 :: forall e m a.(IException e, MonadCatchIO m)
    => Int
    -> Fd
    -> (e -> m (Maybe EnumException))
    -> Enumerator (Offset ByteString) m a
enumFdCatchOBS bs fd handler iter =
  let bufsize = bs
  in enumFromCallbackCatch (makefdCallbackOBS (fromIntegral bufsize) fd) handler () iter

-- |The enumerator of a POSIX File Descriptor: a variation of @enumFd@ that
-- supports RandomIO (seek requests).
enumFdRandomOBS
 :: forall m a.(MonadCatchIO m) =>
    Int
    -> Fd
    -> Enumerator (Offset ByteString) m a
enumFdRandomOBS bs fd iter = enumFdCatchOBS bs fd handler iter
  where
    handler (SeekException off) =
      liftM (either
             (\e -> let _ = e :: SomeException in Just $ enStrExc $ "Error seeking within file descriptor: " ++ show e)
             (const Nothing))
            . liftIO . Ex.try $ myfdSeek fd AbsoluteSeek $ fromIntegral off

fileDriverOBS
  :: (MonadCatchIO m) =>
     (Int -> Fd -> Enumerator (Offset ByteString) m a)
     -> Int
     -> Iteratee (Offset ByteString) m a
     -> FilePath
     -> m a
fileDriverOBS enumf bufsize iter filepath = CIO.bracket
  (liftIO $ openFd filepath ReadOnly Nothing defaultFileFlags)
  (liftIO . closeFd)
  (run <=< flip (enumf bufsize) iter)

-- |A version of fileDriverFd that supports seeking.
fileDriverRandomFdOBS
  :: (MonadCatchIO m) =>
     Int
     -> Iteratee (Offset ByteString) m a
     -> FilePath
     -> m a
fileDriverRandomFdOBS = fileDriverOBS enumFdRandomOBS

enumFile'OBS :: (MonadCatchIO m) =>
  (Int -> Fd -> Enumerator (Offset ByteString) m a)
  -> Int -- ^Buffer size
  -> FilePath
  -> Enumerator (Offset ByteString) m a
enumFile'OBS enumf bufsize filepath iter = CIO.bracket
  (liftIO $ openFd filepath ReadOnly Nothing defaultFileFlags)
  (liftIO . closeFd)
  (flip (enumf bufsize) iter)

enumFileRandomOBS ::
  (MonadCatchIO m)
  => Int                 -- ^Buffer size
  -> FilePath
  -> Enumerator (Offset ByteString) m a
enumFileRandomOBS = enumFile'OBS enumFdRandomOBS

-- |Process a file using the given Iteratee.  This function wraps
-- enumFdRandom as a convenience.
fileDriverRandomOBS
  :: (MonadCatchIO m) =>
     Iteratee (Offset ByteString) m a
     -> FilePath
     -> m a
fileDriverRandomOBS = fileDriverRandomFdOBS defaultBufSize

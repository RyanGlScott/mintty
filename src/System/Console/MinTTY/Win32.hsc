{-
This is a (mostly) direct copy of System.Win32.MinTTY from the Win32 library. We need
this for backwards compatibility with older versions of Win32 which do not ship
with this module.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.MinTTY
-- Copyright   :  (c) University of Glasgow 2006
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- A function to check if the current terminal uses MinTTY.
-- Much of this code was originally authored by Phil Ruffwind and the
-- git-for-windows project.
--
-----------------------------------------------------------------------------

module System.Console.MinTTY.Win32 (isMinTTY, isMinTTYHandle) where

import Graphics.Win32.Misc
import System.Win32.DLL
import System.Win32.File
import System.Win32.Types

#if MIN_VERSION_base(4,6,0)
import Control.Exception (catch)
#endif
import Control.Monad (void)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Foreign hiding (void)
import Foreign.C.Types
import System.FilePath (takeFileName)

#if __GLASGOW_HASKELL__ < 711
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

-- The headers that are shipped with GHC's copy of MinGW-w64 assume Windows XP.
-- Since we need some structs that are only available with Vista or later,
-- we must manually set WINVER/_WIN32_WINNT accordingly.
#undef WINVER
#define WINVER 0x0600
#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0600
##include "windows_cconv.h"
#include <windows.h>
#include "winternl_compat.h"

-- | Returns 'True' if the current process's standard error is attached to a
-- MinTTY console (e.g., Cygwin or MSYS). Returns 'False' otherwise.
isMinTTY :: IO Bool
isMinTTY = do
    h <- getStdHandle sTD_ERROR_HANDLE
           `catch` \(_ :: IOError) ->
             return nullHANDLE
    if h == nullHANDLE
       then return False
       else isMinTTYHandle h

-- | Returns 'True' is the given handle is attached to a MinTTY console
-- (e.g., Cygwin or MSYS). Returns 'False' otherwise.
isMinTTYHandle :: HANDLE -> IO Bool
isMinTTYHandle h = do
    fileType <- getFileType h
    if fileType /= fILE_TYPE_PIPE
      then return False
      else isMinTTYVista h `catch` \(_ :: IOError) -> isMinTTYCompat h
      -- GetFileNameByHandleEx is only available on Vista and later (hence
      -- the name isMinTTYVista). If we're on an older version of Windows,
      -- getProcAddress will throw an IOException when it fails to find
      -- GetFileNameByHandleEx, and thus we will default to using
      -- NtQueryObject (isMinTTYCompat).

isMinTTYVista :: HANDLE -> IO Bool
isMinTTYVista h = do
    fn <- getFileNameByHandle h
    return $ cygwinMSYSCheck fn
  `catch` \(_ :: IOError) ->
    return False

isMinTTYCompat :: HANDLE -> IO Bool
isMinTTYCompat h = do
    fn <- ntQueryObjectNameInformation h
    return $ cygwinMSYSCheck fn
  `catch` \(_ :: IOError) ->
    return False

cygwinMSYSCheck :: String -> Bool
cygwinMSYSCheck fn = ("cygwin-" `isPrefixOf` fn' || "msys-" `isPrefixOf` fn') &&
            "-pty" `isInfixOf` fn' &&
            "-master" `isSuffixOf` fn'
  where
    fn' = takeFileName fn
-- Note that GetFileInformationByHandleEx might return a filepath like:
--
--    \msys-dd50a72ab4668b33-pty1-to-master
--
-- But NtQueryObject might return something like:
--
--    \Device\NamedPipe\msys-dd50a72ab4668b33-pty1-to-master
--
-- This means we can't rely on "\cygwin-" or "\msys-" being at the very start
-- of the filepath. Therefore, we must take care to first call takeFileName
-- before checking for "cygwin" or "msys" at the start using `isPrefixOf`.

getFileNameByHandle :: HANDLE -> IO String
getFileNameByHandle h = do
  let sizeOfDWORD = sizeOf (undefined :: DWORD)
      -- note: implicitly assuming that DWORD has stronger alignment than wchar_t
      bufSize     = sizeOfDWORD + mAX_PATH * sizeOfTCHAR
  allocaBytes bufSize $ \buf -> do
    getFileInformationByHandleEx h fileNameInfo buf (fromIntegral bufSize)
    fni <- peek buf
    return $ fniFileName fni

getFileInformationByHandleEx
  :: HANDLE -> CInt -> Ptr FILE_NAME_INFO -> DWORD -> IO ()
getFileInformationByHandleEx h cls buf bufSize = do
  lib <- getModuleHandle (Just "kernel32.dll")
  ptr <- getProcAddress lib "GetFileInformationByHandleEx"
  let c_GetFileInformationByHandleEx =
        mk_GetFileInformationByHandleEx (castPtrToFunPtr ptr)
  failIfFalse_ "getFileInformationByHandleEx"
    (c_GetFileInformationByHandleEx h cls buf bufSize)

ntQueryObjectNameInformation :: HANDLE -> IO String
ntQueryObjectNameInformation h = do
  let sizeOfONI = sizeOf (undefined :: OBJECT_NAME_INFORMATION)
      bufSize   = sizeOfONI + mAX_PATH * sizeOfTCHAR
  allocaBytes bufSize $ \buf ->
    alloca $ \p_len -> do
      {-
      See Note [Don't link against ntdll]
      _ <- failIfNeg "NtQueryObject" $ c_NtQueryObject
             h objectNameInformation buf (fromIntegral bufSize) p_len
      -}
      ntQueryObject h objectNameInformation buf (fromIntegral bufSize) p_len
      oni <- peek buf
      return $ usBuffer $ oniName oni

-- See Note [Don't link against ntdll]
ntQueryObject :: HANDLE -> CInt -> Ptr OBJECT_NAME_INFORMATION
              -> ULONG -> Ptr ULONG -> IO ()
ntQueryObject h cls buf bufSize p_len = do
  lib <- getModuleHandle (Just "ntdll.dll")
  ptr <- getProcAddress lib "NtQueryObject"
  let c_NtQueryObject = mk_NtQueryObject (castPtrToFunPtr ptr)
  void $ failIfNeg "NtQueryObject" $ c_NtQueryObject h cls buf bufSize p_len

fileNameInfo :: CInt
fileNameInfo = #const FileNameInfo

mAX_PATH :: Num a => a
mAX_PATH = #const MAX_PATH

objectNameInformation :: CInt
objectNameInformation = #const ObjectNameInformation

type F_GetFileInformationByHandleEx =
  HANDLE -> CInt -> Ptr FILE_NAME_INFO -> DWORD -> IO BOOL

foreign import WINDOWS_CCONV "dynamic"
  mk_GetFileInformationByHandleEx
    :: FunPtr F_GetFileInformationByHandleEx -> F_GetFileInformationByHandleEx

data FILE_NAME_INFO = FILE_NAME_INFO
  { fniFileNameLength :: DWORD
  , fniFileName       :: String
  } deriving Show

instance Storable FILE_NAME_INFO where
    sizeOf    _ = #size      FILE_NAME_INFO
    alignment _ = #alignment FILE_NAME_INFO
    poke buf fni = withTStringLen (fniFileName fni) $ \(str, len) -> do
        let len'  = (min mAX_PATH len) * sizeOfTCHAR
            start = advancePtr (castPtr buf) (#offset FILE_NAME_INFO, FileName)
            end   = advancePtr start len'
        (#poke FILE_NAME_INFO, FileNameLength) buf len'
        copyArray start (castPtr str :: Ptr Word8) len'
        poke (castPtr end) (0 :: TCHAR)
    peek buf = do
        vfniFileNameLength <- (#peek FILE_NAME_INFO, FileNameLength) buf
        let len = fromIntegral vfniFileNameLength `div` sizeOfTCHAR
        vfniFileName <- peekTStringLen (plusPtr buf (#offset FILE_NAME_INFO, FileName), len)
        return $ FILE_NAME_INFO
          { fniFileNameLength = vfniFileNameLength
          , fniFileName       = vfniFileName
          }

{-
In an ideal world, we'd use this instead of the hack below.
See Note [Don't link against ntdll]

foreign import WINDOWS_CCONV "winternl.h NtQueryObject"
  c_NtQueryObject :: HANDLE -> CInt -> Ptr OBJECT_NAME_INFORMATION
                  -> ULONG -> Ptr ULONG -> IO NTSTATUS
-}

type F_NtQueryObject
  =  HANDLE -> CInt -> Ptr OBJECT_NAME_INFORMATION
  -> ULONG -> Ptr ULONG -> IO NTSTATUS

foreign import WINDOWS_CCONV "dynamic"
  mk_NtQueryObject :: FunPtr F_NtQueryObject -> F_NtQueryObject

type NTSTATUS = #type NTSTATUS
type ULONG    = #type ULONG

failIfNeg :: (Num a, Ord a) => String -> IO a -> IO a
failIfNeg = failIf (< 0)

newtype OBJECT_NAME_INFORMATION = OBJECT_NAME_INFORMATION
  { oniName :: UNICODE_STRING
  } deriving Show

instance Storable OBJECT_NAME_INFORMATION where
    sizeOf    _ = #size      OBJECT_NAME_INFORMATION
    alignment _ = #alignment OBJECT_NAME_INFORMATION
    poke buf oni = (#poke OBJECT_NAME_INFORMATION, Name) buf (oniName oni)
    peek buf = fmap OBJECT_NAME_INFORMATION $ (#peek OBJECT_NAME_INFORMATION, Name) buf

data UNICODE_STRING = UNICODE_STRING
  { usLength        :: USHORT
  , usMaximumLength :: USHORT
  , usBuffer        :: String
  } deriving Show

instance Storable UNICODE_STRING where
    sizeOf    _ = #size      UNICODE_STRING
    alignment _ = #alignment UNICODE_STRING
    poke buf us = withTStringLen (usBuffer us) $ \(str, len) -> do
        let len'  = (min mAX_PATH len) * sizeOfTCHAR
            start = advancePtr (castPtr buf) (#size UNICODE_STRING)
            end   = advancePtr start len'
        (#poke UNICODE_STRING, Length)        buf len'
        (#poke UNICODE_STRING, MaximumLength) buf (len' + sizeOfTCHAR)
        (#poke UNICODE_STRING, Buffer)        buf start
        copyArray start (castPtr str :: Ptr Word8) len'
        poke (castPtr end) (0 :: TCHAR)
    peek buf = do
        vusLength        <- (#peek UNICODE_STRING, Length)        buf
        vusMaximumLength <- (#peek UNICODE_STRING, MaximumLength) buf
        vusBufferPtr     <- (#peek UNICODE_STRING, Buffer)        buf
        let len          =  fromIntegral vusLength `div` sizeOfTCHAR
        vusBuffer        <- peekTStringLen (vusBufferPtr, len)
        return $ UNICODE_STRING
          { usLength        = vusLength
          , usMaximumLength = vusMaximumLength
          , usBuffer        = vusBuffer
          }

sizeOfTCHAR :: Int
sizeOfTCHAR = sizeOf (undefined :: TCHAR)

{-
Note [Don't link against ntdll]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We deliberately avoid using any direct foreign imports from ntdll, and instead
dynamically load any functions we need from ntdll by hand. Why? As it turns
out, if you're using some versions of the 32-bit mingw-w64-crt library (which
is shipped with GHC on Windows), statically linking against both ntdll and
msvcrt can lead to nasty linker redefinition errors. See GHC Trac #13431.
(Curiously, this bug is only present on 32-bit Windows, which is why it went
unnoticed for a while.)
-}

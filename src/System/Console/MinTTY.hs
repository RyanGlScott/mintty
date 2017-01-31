{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif

{-|
Module:      System.Console.Internal
Copyright:   (C) 2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: Portable

Exports functions that check if a process or handle is attached to a MinTTY
console on Windows, such as Cygwin or MSYS. On non-Windows operating systems,
the functions in this module will simply return 'False'.
-}
module System.Console.MinTTY (isMinTTY, isMinTTYHandle, HANDLE) where

#if defined(WINDOWS)
import System.Win32.Types (HANDLE)

# if MIN_VERSION_Win32(2,5,0)
import qualified System.Win32.MinTTY as Win32 (isMinTTY, isMinTTYHandle)
# else
-- NB: This is the backported definition local to this package, which we only
-- use if the bundled version of Win32 isn't recent enough.
import qualified System.Console.MinTTY.Win32 as Win32 (isMinTTY, isMinTTYHandle)
# endif
#else /* not Windows */
import Foreign.Ptr (Ptr)
#endif

-- | Returns 'True' if the current process's standard error is attached to a
-- MinTTY console (e.g., Cygwin or MSYS). Returns 'False' otherwise.
isMinTTY :: IO Bool
isMinTTY = dispatch Win32.isMinTTY

-- | Returns 'True' is the given handle is attached to a MinTTY console
-- (e.g., Cygwin or MSYS). Returns 'False' otherwise.
isMinTTYHandle :: HANDLE -> IO Bool
isMinTTYHandle = dispatch . Win32.isMinTTYHandle

#if !defined(WINDOWS)
type HANDLE = Ptr ()
#endif

-- Do the right thing on Windows. Otherwise, just return False.
dispatch :: IO Bool -> IO Bool
#if defined(WINDOWS)
dispatch x = x
#else
dispatch _ = return False
#endif

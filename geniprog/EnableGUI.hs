-- this is just to get the GUI running on my mac, no big deal
-- note: for Observe.lhs: -fglasgow-exts -cpp -package concurrent

module EnableGUI(enableGUI) where

import Data.Int
import Foreign
import qualified Main as Main2

{-
import Posix
import Concurrent
import Control.Exception
catchCtrlC = do
    main_thread <- myThreadId
    installHandler sigINT (Catch (hupHandler main_thread)) Nothing
    where
    hupHandler :: ThreadId -> IO ()
    hupHandler main_thread
      = throwTo main_thread  (ErrorCall "Control-C")
-}

main = do (enableGUI >> Main2.main)

type ProcessSerialNumber = Int64

foreign import ccall "GetCurrentProcess" getCurrentProcess :: Ptr ProcessSerialNumber -> IO Int16
foreign import ccall "_CGSDefaultConnection" cgsDefaultConnection :: IO ()
foreign import ccall "CPSEnableForegroundOperation" cpsEnableForegroundOperation :: Ptr ProcessSerialNumber -> IO ()
foreign import ccall "CPSSignalAppReady" cpsSignalAppReady :: Ptr ProcessSerialNumber -> IO ()
foreign import ccall "CPSSetFrontProcess" cpsSetFrontProcess :: Ptr ProcessSerialNumber -> IO ()

enableGUI = alloca $ \psn -> do
    getCurrentProcess psn
    cgsDefaultConnection
    cpsEnableForegroundOperation psn
    cpsSignalAppReady psn
    cpsSetFrontProcess psn

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

ref :: IORef [a]
ref = unsafePerformIO $ newIORef []

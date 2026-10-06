-- | Key-value database.
module KVDB
  ( KVDB,
    startKVDB,
    kvGet,
    kvPut,
  )
where

import Control.Monad (forM_)
import Data.List (partition)
import GenServer

data Msg k v
  = MsgPut k v
  | MsgGet k (ReplyChan v)

newtype KVDB k v = KVDB (Server (Msg k v))

data State k v = State
  { stateKVs :: [(k, v)],
    stateWaiters :: [(k, ReplyChan v)]
  }

kvdbEvent :: (Ord k) => Msg k v -> State k v -> IO (State k v)
kvdbEvent (MsgPut k v) state = do
  let (waiting, not_waiting) =
        partition ((== k) . fst) $ stateWaiters state
  forM_ waiting $ \(_, from) -> reply from v
  pure
    state
      { stateKVs = (k, v) : filter ((/= k) . fst) (stateKVs state),
        stateWaiters = not_waiting
      }
kvdbEvent (MsgGet k from) state =
  case lookup k $ stateKVs state of
    Nothing ->
      pure state {stateWaiters = (k, from) : stateWaiters state}
    Just v -> do
      reply from v
      pure state

startKVDB :: (Ord k) => IO (KVDB k v)
startKVDB =
  KVDB <$> spawn (loop (State [] []))
  where
    loop state c = do
      msg <- receive c
      state' <- kvdbEvent msg state
      loop state' c

kvGet :: KVDB k v -> k -> IO v
kvGet (KVDB s) k = requestReply s $ MsgGet k

kvPut :: KVDB k v -> k -> v -> IO ()
kvPut (KVDB s) k v = sendTo s $ MsgPut k v

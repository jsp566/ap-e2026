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

-- | A reference to a KVDB instance that stores keys of type 'k' and
-- corresponding values of type 'v'.
data KVDBMessage k v
 = MsgGet k (ReplyChan v)
 | MsgPut k v

data KVDB k v -- TODO
  = KVDB (Server (KVDBMessage k v))

data KVDBState k v = KVDBState 
  { kvpairs :: [(k,v)],
    waiters :: [(k, ReplyChan v)]
  }


-- | Start a new KVDB instance.
startKVDB :: (Ord k) => IO (KVDB k v)
startKVDB = do
  let initial_state =
        KVDBState
          { kvpairs = [],
            waiters = []
          }
  c <- spawn $ \c -> handleMsg initial_state c
  pure $ KVDB c

handleMsg :: (Ord k) => (KVDBState k v) -> (Chan (KVDBMessage k v)) -> IO ()
handleMsg state c = do
  msg <- receive c
  case msg of
    MsgPut key value -> do
      let (waiting_for_job, not_waiting_for_job) =
            partition ((== key) . fst) (waiters state)
      forM_ waiting_for_job $ \(_, rsvp) ->
        reply rsvp value
      handleMsg (state {
        kvpairs = (key,value) : filter ((/= key) . fst) (kvpairs state),
        waiters = not_waiting_for_job
        }) c
    MsgGet key rsvp -> 
      case (lookup key (kvpairs state)) of
        Just value -> do 
          reply rsvp value
          handleMsg state c
        Nothing -> do
          handleMsg (state {
        waiters = (key, rsvp) : waiters state
        }) c
        
-- | Retrieve the value corresponding to a given key. If that key does
-- not exist in the store, then this function blocks until another
-- thread writes the desired key with 'kvPut', after which this
-- function returns the now available value.
kvGet :: KVDB k v -> k -> IO v
kvGet (KVDB c) key = 
  requestReply c $ MsgGet key

-- | Write a key-value mapping to the database. Replaces any prior
-- mapping of the key.
kvPut :: KVDB k v -> k -> v -> IO ()
kvPut (KVDB c) key value = 
  sendTo c $ MsgPut key value

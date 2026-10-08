module SPC
  ( -- * SPC startup
    SPC,
    Job(..),
    jobAdd,
    JobDoneReason(..),
    JobStatus(..),
    jobStatus,
    jobCancel,
    jobWait,
    startSPC,
    pingSPC,
  )
where

import Control.Concurrent
  ( ThreadId,
    forkIO,
    killThread,
    threadDelay,
  )
import Control.Exception (SomeException, catch)
import Control.Monad (ap, forM_, forever, liftM, void)
import Data.List (partition, delete)
import GenServer
import System.Clock.Seconds (Clock (Monotonic), Seconds, getTime)

-- First some general utility functions.

-- | Retrieve Unix time using a monotonic clock. You cannot use this
-- to measure the actual world time, but you can use it to measure
-- elapsed time.
getSeconds :: IO Seconds
getSeconds = getTime Monotonic

-- | Remove mapping from association list.
removeAssoc :: (Eq k) => k -> [(k, v)] -> [(k, v)]
removeAssoc needle ((k, v) : kvs) =
  if k == needle
    then kvs
    else (k, v) : removeAssoc needle kvs
removeAssoc _ [] = []

-- Then the definition of the glorious SPC.

-- Messages sent to SPC.
data SPCMsg -- TODO: add messages.
  = MsgPing (ReplyChan Int)
  | MsgAddJob Job (ReplyChan JobId)
  | MsgGetJobStatus JobId (ReplyChan (Maybe JobStatus))
  | MsgCancelJob JobId
  | MsgAwaitJob JobId (ReplyChan (Maybe JobDoneReason))
  | MsgJobDone JobId
  | MsgJobCrashed JobId
  | Tick

-- | A Handle to the SPC instance.
data SPC = SPC (Server SPCMsg)

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { jobCounter :: JobId,
    pendingJobs :: [(JobId,Job)],
    doneJobs :: [(JobId,JobDoneReason)],
    waiters :: [(JobId, ReplyChan (Maybe JobDoneReason))],
    runningJob :: Maybe (JobId, ThreadId, Seconds),
    inc :: Chan SPCMsg

  }

-- | A job that is to be enqueued in the glorious SPC.
data Job = Job
  { -- | The IO action that comprises the actual action of the job.
    jobAction :: IO (),
    -- | The maximum allowed runtime of the job, counting from when
    -- the job begins executing (not when it is enqueued).
    jobMaxSeconds :: Int
  }

-- | A unique identifier of a job that has been enqueued.
newtype JobId = JobId Int
  deriving (Eq, Ord, Show)

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC s) job = 
  requestReply s $ MsgAddJob $ job


-- | How a job finished.
data JobDoneReason
  = -- | Normal termination.
    Done
  | -- | The job was killed because it ran for too long.
    DoneTimeout
  | -- | The job was explicitly cancelled.
    DoneCancelled
  | -- | The job crashed due to an exception.
    DoneCrashed
  deriving (Eq, Ord, Show)

-- | The status of a job.
data JobStatus
  = -- | The job is done and this is why.
    JobDone JobDoneReason
  | -- | The job is still running.
    JobRunning
  | -- | The job is enqueued, but is waiting for an idle worker.
    JobPending
  deriving (Eq, Ord, Show)


-- | Query the job status.
jobStatus :: SPC -> JobId -> IO (Maybe JobStatus)
jobStatus (SPC s) jobid = 
  requestReply s $ MsgGetJobStatus $ jobid


jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC s) jobid = 
  sendTo s $ MsgCancelJob jobid

-- | Synchronously block until job is done and return the reason.
-- Returns 'Nothing' if job is not known to this SPC instance.
jobWait :: SPC -> JobId -> IO (Maybe JobDoneReason)
jobWait (SPC c) jobid =
  requestReply c $ MsgAwaitJob jobid

checkTimeouts :: SPCM ()
checkTimeouts = do
  st <- get
  now <- io $ getSeconds
  case runningJob st of
    Just (jid, tid, deadline)
      | deadline < now -> do
            io $ killThread tid
            put $ st {runningJob = Nothing}
            jobDone jid DoneTimeout
    _ -> pure ()



newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))

instance Functor SPCM where
  fmap = liftM

instance Applicative SPCM where
  pure x = SPCM $ \st -> pure (x, st)
  (<*>) = ap

instance Monad SPCM where
  SPCM m >>= f = SPCM $ \st -> do
    (x, st') <- m st
    let SPCM f' = f x
    f' st'

get :: SPCM SPCState
get = SPCM $ \st -> pure (st, st)

put :: SPCState -> SPCM ()
put st = SPCM $ \_ -> pure ((), st)

io :: IO a -> SPCM a
io ioa = SPCM $ \st -> do
  x <- ioa
  pure (x, st)

runSPCM :: SPCState -> SPCM a -> IO a
runSPCM st (SPCM m) = fst <$> m st

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  checkTimeouts
  schedule
  msg <- io $ receive c
  case msg of
    MsgPing rc -> do
      st <- get
      let JobId i = jobCounter st
      io $ reply rc $ i
      put $ st {jobCounter = JobId $ succ $ i}
    MsgAddJob job rc -> do
      st <- get
      let JobId i = jobCounter st
      io $ reply rc $ JobId i
      put $ st {jobCounter = JobId $ succ $ i,
                pendingJobs = (JobId i, job) : pendingJobs st}
    MsgGetJobStatus jobid rc -> do
      st <- get
      io $ reply rc $ case (lookup jobid $ pendingJobs st, lookup jobid $ doneJobs st, runningJob st) of
        (Just _, _, _) -> Just JobPending
        (_, Just x, _) -> Just (JobDone x)
        (_, _, Just (runningjobid, _, _))
          | runningjobid == jobid -> Just JobRunning
        (Nothing, Nothing, _) -> Nothing
    MsgCancelJob canceljobid -> do
      st <- get
      case runningJob st of
        Just (jobid, tid, _)
          | jobid == canceljobid -> do
            io $ killThread tid
            put $ st {runningJob = Nothing}
            jobDone jobid DoneCancelled
        _ -> jobDone canceljobid DoneCancelled
    MsgAwaitJob jobid rc -> do
      st <- get
      case (lookup jobid $ pendingJobs st, lookup jobid $ doneJobs st, runningJob st) of
        (Just _, _, _) -> put $ st {waiters = (jobid, rc) : waiters st}
        (_, Just x, _) -> io $ reply rc $ Just x
        (_, _, Just (runjobid, _, _)) 
          | runjobid == jobid -> put $ st {waiters = (jobid, rc) : waiters st}
        (Nothing, Nothing, _) -> io $ reply rc $ Nothing
    MsgJobDone donejobid -> do
      st <- get
      case runningJob st of
        Just (jobid, _, _)
          | jobid == donejobid -> do
          put $ st {runningJob = Nothing}
          jobDone jobid Done
        _ -> pure ()
    MsgJobCrashed crashjobid -> do
      st <- get
      case runningJob st of
        Just (jobid, _, _)
          | jobid == crashjobid -> do
          put $ st {runningJob = Nothing}
          jobDone jobid DoneCrashed
        _ -> pure ()
    Tick -> pure()


jobDone :: JobId -> JobDoneReason -> SPCM ()
jobDone jobid reason = do
      st <- get
      case lookup jobid $ doneJobs st of 
        Just _ -> pure ()
        Nothing -> do
          let (waiting_for_jobid, rest) = partition ((== jobid) . fst) $ waiters st
          forM_ waiting_for_jobid $ \(_, wrc) -> io $ reply wrc (Just reason)
          put $ st {pendingJobs = removeAssoc jobid $ pendingJobs st,
                doneJobs = (jobid, reason) : doneJobs st,
                waiters = rest}

schedule :: SPCM ()
schedule = do 
  st <- get
  case (runningJob st, pendingJobs st) of
    (Just _, _) -> pure ()
    (Nothing, []) -> pure ()
    (Nothing, ((jid,job):xs)) -> do
      tid <- io $ forkIO $ do 
        let computation = do
              jobAction job
              send (inc st) (MsgJobDone jid)
            handler :: SomeException -> IO ()
            handler _ = send (inc st) (MsgJobCrashed jid)
        catch computation handler
      now <- io $ getSeconds
      let deadline = now + fromIntegral (jobMaxSeconds job)
      put $ st {pendingJobs = xs, runningJob = Just (jid, tid, deadline)}


startSPC :: IO SPC
startSPC = do
  let initial_st =
        SPCState
          { jobCounter = JobId 0,
            pendingJobs = [],
            doneJobs = [],
            waiters = [],
            runningJob = Nothing
          }
  server <- spawn $ \c -> runSPCM (initial_st {inc = c}) $ forever $ handleMsg c
  _ <- forkIO $ forever $ do 
    threadDelay 1000000
    sendTo server Tick 
  pure $ SPC server

pingSPC :: SPC -> IO Int
pingSPC (SPC s) =
  requestReply s $ MsgPing


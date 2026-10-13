module SPC
  ( -- * SPC startup
    SPC,
    startSPC,

    -- * Job functions
    Job (..),
    JobId,
    JobStatus (..),
    JobDoneReason (..),
    jobAdd,
    jobStatus,
    jobWait,
    jobCancel,

    -- * Worker functions
    WorkerName,
    workerAdd,
    workerStop,
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
import Data.List (partition)
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

-- | How a job finished.
data JobDoneReason
  = -- | Normal termination.
    Done
  | -- | The job was killed because it ran for too long.
    DoneTimeout
  | -- | The job was explicitly cancelled, or the worker
    -- it was running on was stopped.
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

-- | A worker decides its own human-readable name. This is useful for
-- debugging.
type WorkerName = String

-- | Messages sent to workers. These are sent both by SPC and by
-- processes spawned by the workes.
data WorkerMsg -- TODO: add messages.
  = -- Job to be done
    WMsgJobToBeDone (IO ())
  | -- Job is done
    WMsgJobDone
  | -- Job crashed
    WMsgJobCrashed
  | -- Cancel Job
    WMsgWorkerJobCancel
  | -- Shut down worker
    WMsgWorkerStop


-- Messages sent to SPC.
data SPCMsg
  = -- | Add the job, and reply with the job ID.
    MsgJobAdd Job (ReplyChan JobId)
  | -- | Cancel the given job.
    MsgJobCancel JobId
  | -- | Immediately reply the status of the job.
    MsgJobStatus JobId (ReplyChan (Maybe JobStatus))
  | -- | Reply when the job is done.
    MsgJobWait JobId (ReplyChan (Maybe JobDoneReason))
  | -- | Some time has passed.
    MsgTick
  | -- | Add worker
    MsgWorkerAdd WorkerName (ReplyChan (Either String Worker))
  | -- | Job is done
    MsgJobDone WorkerName
  | -- | Job crashed
    MsgJobCrashed WorkerName
  | -- | Worker stopper
    MsgWorkerStopped WorkerName

-- | A handle to the SPC instance.
data SPC = SPC (Server SPCMsg)

-- | A handle to a worker.
data Worker = Worker (Server WorkerMsg)

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { spcJobsPending :: [(JobId, Job)],
    spcJobsRunning :: [(JobId, (WorkerName, Worker, Seconds))],
    spcJobsDone :: [(JobId, JobDoneReason)],
    spcJobCounter :: JobId,
    -- TODO: you will need to add more fields.
    spcWaiting :: [(JobId, ReplyChan (Maybe JobDoneReason))],
    spcWorkersIdle :: [(WorkerName, Worker)],
    spcWorkersBusy :: [(WorkerName, (Worker, JobId))],
    spcWorkersStopped :: [WorkerName]

  }

-- | The monad in which the main SPC thread runs. This is a state
-- monad with support for IO.
newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))

instance Functor SPCM where
  fmap = liftM

instance Applicative SPCM where
  pure x = SPCM $ \state -> pure (x, state)
  (<*>) = ap

instance Monad SPCM where
  SPCM m >>= f = SPCM $ \state -> do
    (x, state') <- m state
    let SPCM f' = f x
    f' state'

-- | Retrieve the state.
get :: SPCM SPCState
get = SPCM $ \state -> pure (state, state)

-- | Overwrite the state.
put :: SPCState -> SPCM ()
put state = SPCM $ \_ -> pure ((), state)

-- | Modify the state.
modify :: (SPCState -> SPCState) -> SPCM ()
modify f = do
  state <- get
  put $ f state

-- | Lift an 'IO' action into 'SPCM'.
io :: IO a -> SPCM a
io m = SPCM $ \state -> do
  x <- m
  pure (x, state)

-- | Run the SPCM monad.
runSPCM :: SPCState -> SPCM a -> IO a
runSPCM state (SPCM f) = fst <$> f state

schedule :: SPCM ()
schedule = do
  state <- get
  case (spcJobsPending state, spcWorkersIdle state) of
    ((jobid, job) : jobs, (workername, (Worker c)) : workers) -> do
      now <- io $ getSeconds
      let deadline = now + fromIntegral (jobMaxSeconds job)
      io $ sendTo c $ WMsgJobToBeDone $ (jobAction job)
      put $ 
        state 
          { spcJobsPending = jobs,
            spcJobsRunning = (jobid, (workername, (Worker c), deadline)) : spcJobsRunning state,
            spcWorkersIdle = workers,
            spcWorkersBusy = (workername, ((Worker c), jobid)) : spcWorkersBusy state
          }
      schedule
    _ -> pure ()


jobDone :: JobId -> JobDoneReason -> SPCM ()
jobDone jobid reason = do
  state <- get
  case lookup jobid $ spcJobsDone state of
    Just _ -> pure () 
    Nothing -> do
      let (waiting_for_job, not_waiting_for_job) =
                  partition ((== jobid) . fst) (spcWaiting state)
      forM_ waiting_for_job $ \(_, rsvp) ->
        io $ reply rsvp $ Just reason
      put $ state
        { spcWaiting = not_waiting_for_job,
          spcJobsPending = removeAssoc jobid $ spcJobsPending state,
          spcJobsRunning = removeAssoc jobid $ spcJobsRunning state,
          spcJobsDone = (jobid, reason) : spcJobsDone state
          }
                 

workerIsIdle :: WorkerName -> Worker -> SPCM ()
workerIsIdle workername worker = do
  state <- get
  case (lookup workername $ spcWorkersIdle state) of
    Just _ -> pure ()
    Nothing ->
      put $ 
        state 
          { spcWorkersIdle = (workername, worker) : spcWorkersIdle state,
            spcWorkersBusy = removeAssoc workername $ spcWorkersBusy state
          }

workerIsGone :: WorkerName -> SPCM ()
workerIsGone workername = do
  state <- get
  if (elem workername $ spcWorkersStopped state)
    then pure ()
    else
      put $ 
        state 
          { spcWorkersIdle = removeAssoc workername $ spcWorkersIdle state,
            spcWorkersBusy = removeAssoc workername $ spcWorkersBusy state,
            spcWorkersStopped = workername : (spcWorkersStopped state)
          }

checkTimeouts :: SPCM ()
checkTimeouts = do
  state <- get
  now <- io getSeconds
  timeoutLoop now $ spcJobsRunning state

timeoutLoop :: Seconds -> [(JobId, (WorkerName, Worker, Seconds))] -> SPCM ()
timeoutLoop now jobs = do
    case jobs of
      [] -> pure ()
      (jobid, (workername, (Worker wc), deadline)) : rest -> do
        if now >= deadline
          then do
            io $ sendTo wc WMsgWorkerJobCancel
            workerIsIdle workername (Worker wc)
            jobDone jobid DoneTimeout
            timeoutLoop now rest
          else do timeoutLoop now rest



workerExists :: WorkerName -> SPCM Bool
workerExists workername = do
  state <- get
  case (lookup workername $ spcWorkersIdle state,
        lookup workername $ spcWorkersBusy state,
        elem workername $ spcWorkersStopped state
        ) of
    (Nothing, Nothing, False) -> pure False
    _ -> pure True


handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  checkTimeouts
  schedule
  msg <- io $ receive c
  case msg of
    MsgJobAdd job rsvp -> do
      state <- get
      let JobId jobid = spcJobCounter state
      put $
        state
          { spcJobsPending =
              (spcJobCounter state, job) : spcJobsPending state,
            spcJobCounter = JobId $ succ jobid
          }
      io $ reply rsvp $ JobId jobid
    MsgJobStatus jobid rsvp -> do
      state <- get
      io $ reply rsvp $ case ( lookup jobid $ spcJobsPending state,
                               lookup jobid $ spcJobsRunning state,
                               lookup jobid $ spcJobsDone state
                             ) of
        (Just _, _, _) -> Just JobPending
        (_, Just _, _) -> Just JobRunning
        (_, _, Just r) -> Just $ JobDone r
        _ -> Nothing

    MsgJobCancel cancel_jobid -> do
      state <- get 
      case lookup cancel_jobid $ spcJobsRunning state of
        Nothing -> jobDone cancel_jobid DoneCancelled
        Just (workername, (Worker wc), _) -> do
          io $ sendTo wc WMsgWorkerJobCancel
          workerIsIdle workername (Worker wc)
          jobDone cancel_jobid DoneCancelled

    MsgJobWait jobid rsvp -> do
      state <- get
      case lookup jobid $ spcJobsDone state of
        Just reason -> do
          io $ reply rsvp $ Just reason
        Nothing -> 
          if jobid >= spcJobCounter state
            then io $ reply rsvp Nothing
            else put $ state {spcWaiting = (jobid, rsvp) : spcWaiting state}

    MsgWorkerAdd workername rsvp -> do
      check <- workerExists workername
      if check 
        then io $ reply rsvp $ Left "Worker already exists"
        else do
          state <- get
          worker <- io $ spawn $ handleWorkerMsg workername c Nothing
          put $ state {spcWorkersIdle = (workername, (Worker worker)) : spcWorkersIdle state}
          io $ reply rsvp $ Right (Worker worker)

    MsgJobDone workername -> do
      state <- get
      case (lookup workername $ spcWorkersBusy state) of 
        Nothing -> pure ()
        Just (worker, jobid) -> do
          workerIsIdle workername worker
          jobDone jobid Done

    MsgJobCrashed workername -> do
      state <- get
      case (lookup workername $ spcWorkersBusy state) of 
        Nothing -> pure ()
        Just (worker, jobid) -> do
          workerIsIdle workername worker
          jobDone jobid DoneCrashed

    MsgWorkerStopped workername -> do
      state <- get
      case (lookup workername $ spcWorkersBusy state) of
        Nothing -> do
          workerIsGone workername
        Just (_, jobid) -> do
          jobDone jobid DoneCancelled
          workerIsGone workername

    MsgTick -> 
      pure ()

handleWorkerMsg :: WorkerName -> Chan SPCMsg -> Maybe ThreadId -> Chan WorkerMsg -> IO ()
handleWorkerMsg name rc tid c = do
  msg <- receive c
  case (msg, tid) of
    (WMsgJobToBeDone _, Just _) -> error (name ++ ": Already has job")
    (WMsgJobToBeDone action, Nothing) -> do
      newtid <- forkIO $ do
        let doJob = do
              action
              send c WMsgJobDone
            onException :: SomeException -> IO ()
            onException _ =
              send c WMsgJobCrashed
        doJob `catch` onException
      handleWorkerMsg name rc (Just newtid) c
      
    (WMsgJobDone, Just _) -> do
      send rc $ MsgJobDone name
      handleWorkerMsg name rc Nothing c
    (WMsgJobDone, Nothing) ->
      handleWorkerMsg name rc Nothing c

    (WMsgJobCrashed, Just _) -> do
      send rc $ MsgJobCrashed name
      handleWorkerMsg name rc Nothing c
    (WMsgJobCrashed, Nothing) -> 
      handleWorkerMsg name rc Nothing c

    (WMsgWorkerJobCancel, Just threadid) -> do
      killThread threadid
      handleWorkerMsg name rc Nothing c
    (WMsgWorkerJobCancel, Nothing) -> 
      handleWorkerMsg name rc Nothing c

    (WMsgWorkerStop, Just threadid) -> do
      killThread threadid
      send rc $ MsgWorkerStopped name
    (WMsgWorkerStop, Nothing) -> do
      send rc $ MsgWorkerStopped name

startSPC :: IO SPC
startSPC = do
  let initial_state =
        SPCState
          { spcJobCounter = JobId 0,
            spcJobsPending = [],
            spcJobsRunning = [],
            spcJobsDone = [],
            spcWaiting = [],
            spcWorkersIdle = [],
            spcWorkersBusy = [],
            spcWorkersStopped = []
          }
  c <- spawn $ \c -> runSPCM initial_state $ forever $ handleMsg c
  void $ spawn $ timer c
  pure $ SPC c
  where
    timer c _ = forever $ do
      threadDelay 1000000 -- 1 second
      sendTo c MsgTick

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job =
  requestReply c $ MsgJobAdd job

-- | Asynchronously query the job status.
jobStatus :: SPC -> JobId -> IO (Maybe JobStatus)
jobStatus (SPC c) jobid =
  requestReply c $ MsgJobStatus jobid

-- | Synchronously block until job is done and return the reason.
-- Returns 'Nothing' if job is not known to this SPC instance.
jobWait :: SPC -> JobId -> IO (Maybe JobDoneReason)
jobWait (SPC c) jobid =
  requestReply c $ MsgJobWait jobid

-- | Asynchronously cancel a job.
jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC c) jobid =
  sendTo c $ MsgJobCancel jobid

-- | Add a new worker with this name. Fails with 'Left' if a worker
-- with that name already exists.
workerAdd :: SPC -> WorkerName -> IO (Either String Worker)
workerAdd (SPC c) workername =
  requestReply c $ MsgWorkerAdd workername

-- | Shut down a running worker. No effect if the worker is already
-- terminated.
workerStop :: Worker -> IO ()
workerStop (Worker c) =
  sendTo c $ WMsgWorkerStop

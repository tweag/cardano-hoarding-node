module Hoard.TestHelpers
  ( withTestApp,
    withTestAppState,
    TestAppConfig (..),
    testConfig,
    listener,
    capture,
    EventCapture (..),
    waitForEvents,
    client,
    observer,
  )
where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM (TQueue, TVar, atomically, isEmptyTQueue, newTQueueIO, newTVarIO, orElse, readTVar, readTVarIO, registerDelay, retry, writeTQueue, writeTVar)
import Control.Exception (bracket)
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Data.Functor ((<&>))
import Data.Typeable (Typeable)
import Effectful (IOE, (:>))
import Eve qualified
import Hoard.API (API, Routes (..), server)
import Hoard.App (App, addListener, startEventSystem)
import Hoard.Effects (AppEff, Config (..), runEffectStack, runEffectStackReturningState)
import Hoard.Events (SomeEvent (..))
import Hoard.Types.HoardState (HoardState)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (testWithApplication)
import Servant (Proxy (..), hoistServer, serve)
import Servant.Client (BaseUrl (..), ClientError, ClientM, Scheme (..), mkClientEnv, runClientM)
import Servant.Client.Generic (AsClientT, genericClient)

-- | Internal signal to trigger event system shutdown
-- Used by queue monitor to exit event loop when no events remain
data ShutdownSignal = ShutdownSignal
  deriving (Typeable, Show)

-- | Wrapper for capturing events of any type in tests
data EventCapture where
  EventCapture :: (Typeable event) => TVar [event] -> EventCapture

-- | Existential wrapper for listeners of different event types
data SomeListener where
  MkListener ::
    (Typeable eventType) =>
    (forall es. (AppEff es, Typeable es) => eventType -> App es ()) ->
    SomeListener

-- | Configuration for test application
data TestAppConfig = TestAppConfig
  { captures :: [EventCapture],
    listeners :: [SomeListener]
  }

-- | Helper to create an event capture
capture :: (Typeable event) => TVar [event] -> EventCapture
capture = EventCapture

-- | Helper to wrap a listener for use in TestAppConfig
listener ::
  (Typeable eventType) =>
  (forall es. (AppEff es, Typeable es) => eventType -> App es ()) ->
  SomeListener
listener = MkListener

-- | Default test configuration with no captures or listeners
testConfig :: TestAppConfig
testConfig = TestAppConfig {captures = [], listeners = []}

-- | Register all listeners from config
registerListeners :: (AppEff es, Typeable es) => [SomeListener] -> App es ()
registerListeners listeners =
  forM_ listeners $ \(MkListener l) -> addListener l

-- | Create an observer TVar for capturing events in tests
-- Returns an empty TVar that will be populated by event capture listeners
--
-- Usage:
--   1. Create observer for specific event type (with type application):
--      @
--      rawEvents <- observer @RawGSIDataReceived
--      @
--
--   2. Create observer with inferred type:
--      @
--      parsedEvents <- observer  -- Type inferred from capture/waitForEvents usage
--      @
--
--   3. Use with capture and waitForEvents:
--      @
--      myEvents <- observer @MyEvent
--      withTestApp (testConfig {captures = [capture myEvents]}) $ \_ runClient -> do
--        -- Make requests that emit events
--        _ <- runClient (client.someEndpoint data)
--        -- Retrieve captured events
--        events <- waitForEvents myEvents 1 0.1
--      @
observer :: IO (TVar [event])
observer = newTVarIO []

-- | Run a test with the full application (server + event system)
-- Provides the port number and a runClient function to the test action
--
-- Use for: Event flow testing (HTTP endpoints, event emission/capture)
-- Event system runs in background and is forcibly killed when test completes
withTestApp ::
  -- | Test configuration
  TestAppConfig ->
  -- | Test action with port and runClient function
  (Int -> (forall a. ClientM a -> IO (Either ClientError a)) -> IO b) ->
  IO b
withTestApp config action = do
  eventQueue <- newTQueueIO

  -- Create the Wai application
  app <- makeApp eventQueue

  -- Start event system in background with test listener
  bracket
    (forkIO $ void $ runEventSystemWithListener eventQueue config.captures (registerListeners config.listeners))
    killThread
    $ \_ -> withTestServer app action

-- | Run a test with the full application and return final HoardState
-- Event system shuts down when ShutdownSignal is manually sent after action completes
--
-- Use for: State inspection testing (projections, state mutations)
-- Returns final HoardState after all events are processed for assertion
withTestAppState ::
  -- | Test configuration
  TestAppConfig ->
  -- | Action that makes HTTP requests to publish events
  (Int -> (forall a. ClientM a -> IO (Either ClientError a)) -> IO ()) ->
  IO HoardState
withTestAppState config action = do
  eventQueue <- newTQueueIO

  -- Create the Wai application
  app <- makeApp eventQueue

  -- Start event system with shutdown listener
  eventSystemAsync <- async $ runEventSystemWithListener eventQueue (captures config) $ do
    Eve.addListener_ shutdownListener
    registerListeners config.listeners

  -- Start server and run action
  withTestServer app $ \port runClient -> do
    action port runClient
    -- Wait for event queue to drain (or timeout after 500ms), then trigger shutdown
    waitForQueueDrain eventQueue 500000 $ do
      atomically $ writeTQueue eventQueue (SomeEvent ShutdownSignal)

  -- Wait for event system to complete and return final state
  snd <$> wait eventSystemAsync

-- | Create the Wai application
makeApp :: TQueue SomeEvent -> IO Application
makeApp eventQueue = do
  let servantApp = hoistServer (Proxy @API) (runEffectStack (Config eventQueue)) server
  pure $ serve (Proxy @API) servantApp

-- | Start test server and run action with client
-- Handles all the boilerplate of setting up Servant client environment
withTestServer ::
  Application ->
  (Int -> (forall a. ClientM a -> IO (Either ClientError a)) -> IO b) ->
  IO b
withTestServer app action =
  testWithApplication (pure app) $ \port -> do
    manager <- newManager defaultManagerSettings
    let baseUrl = BaseUrl Http "localhost" port ""
    let clientEnv = mkClientEnv manager baseUrl
    let runClient :: forall a. ClientM a -> IO (Either ClientError a)
        runClient = flip runClientM clientEnv
    action port runClient

-- | Wait for event queue to become stable (stay empty for a duration), then run callback
-- Uses STM with debouncing: waits for queue to be empty for stabilityMicros
-- before considering it drained. Has overall timeout to prevent infinite waiting.
--
-- Parameters:
--   - queue: The event queue to monitor
--   - stabilityMicros: How long queue must stay empty (e.g., 200000 = 200ms)
--   - callback: Action to run after queue drains (e.g., send shutdown signal)
waitForQueueDrain :: TQueue a -> Int -> IO () -> IO ()
waitForQueueDrain queue stabilityMicros callback = do
  let maxWaitMicros = stabilityMicros * 10 -- Overall timeout is 10x stability period
  overallTimeout <- registerDelay maxWaitMicros
  tryStabilize overallTimeout
  -- Run callback after queue is drained and stable
  callback
  where
    tryStabilize overallTimeout = do
      -- Wait for queue to be empty (or overall timeout)
      timedOut <- atomically $ do
        expired <- readTVar overallTimeout
        if expired
          then pure True
          else do
            empty <- isEmptyTQueue queue
            if empty then pure False else retry

      if timedOut
        then pure () -- Overall timeout reached
        else do
          -- Queue is empty, verify it stays empty for stability period
          stabilityTimer <- registerDelay stabilityMicros
          stable <-
            atomically $
              -- Success: stability timer expires while queue stays empty
              ( do
                  expired <- readTVar stabilityTimer
                  if expired then pure True else retry
              )
                `orElse`
                -- Failure: new event arrives before stability period ends
                ( do
                    empty <- isEmptyTQueue queue
                    if empty then retry else pure False
                )

          if stable
            then pure () -- Queue is stable!
            else tryStabilize overallTimeout -- New event arrived, try again

-- | Run the event system with custom initialization
-- Returns the final AppState and HoardState when the event system exits
runEventSystemWithListener ::
  TQueue SomeEvent ->
  [EventCapture] ->
  (forall es. (AppEff es, Typeable es) => App es ()) ->
  IO (Eve.AppState, HoardState)
runEventSystemWithListener eventQueue captures initBlock = do
  runEffectStackReturningState (Config eventQueue) $ do
    startEventSystem eventQueue $ do
      -- Run the provided initialization block
      initBlock
      -- Register test capture listeners for each event type
      forM_ captures $ \(EventCapture tvar) ->
        Eve.addListener_ (captureEventsListener tvar)

-- | Listener that captures events into a TVar
-- Runs in the App monad to work with Eve's listener system
captureEventsListener ::
  (IOE :> es) =>
  TVar [event] ->
  event ->
  App es ()
captureEventsListener eventsVar event = do
  lift . liftIO . atomically $ do
    events <- readTVar eventsVar
    writeTVar eventsVar (event : events)

-- | Wait for at least N events to be captured (with timeout)
-- Returns the captured events in reverse chronological order (newest first)
waitForEvents ::
  TVar [event] ->
  -- | Minimum number of events to wait for
  Int ->
  -- | Timeout in seconds
  Double ->
  IO [event]
waitForEvents eventsVar minCount timeoutSeconds = do
  let timeoutMicros = round (timeoutSeconds * 1_000_000) :: Int
  let maxAttempts = timeoutMicros `div` 10000 -- Check every 10ms
  go maxAttempts
  where
    go 0 = readTVarIO eventsVar <&> reverse -- Timeout - return what we have
    go n = do
      events <- readTVarIO eventsVar
      if length events >= minCount
        then pure $ reverse events
        else do
          threadDelay 10000 -- 10ms
          go (n - 1)

-- | Listener that triggers event system exit when ShutdownSignal is received
shutdownListener :: ShutdownSignal -> App es ()
shutdownListener ShutdownSignal = Eve.exit

-- | Generate servant client from API
client :: Routes (AsClientT ClientM)
client = genericClient

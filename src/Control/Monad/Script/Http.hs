{- |
Module      : Control.Monad.Script.Http
Description : A generic monad for expressing HTTP interactions.
Copyright   : 2018, Automattic, Inc.
License     : BSD3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

A basic type and monad for describing HTTP interactions.
-}

{-# LANGUAGE GADTs, Rank2Types, RecordWildCards #-}
module Control.Monad.Script.Http (
  -- * Http
    Http()
  , execHttpM

  -- * HttpT
  , HttpT()
  , execHttpTM
  , liftHttpT

  -- * Error
  , throwError
  , throwJsonError
  , throwHttpException
  , throwIOException
  , catchError
  , catchJsonError
  , catchHttpException
  , catchIOException
  , E()

  -- * Reader
  , ask
  , local
  , reader
  , R(..)
  , basicEnv
  , trivialEnv
  , LogOptions(..)
  , basicLogOptions
  , trivialLogOptions

  -- * Writer
  , logEntries
  , W()

  -- * State
  , gets
  , modify
  , S(..)
  , basicState

  -- * Prompt
  , prompt
  , P(..)
  , evalIO
  , evalMockIO

  -- * API
  , comment
  , wait
  , logEntry

  -- ** IO
  , Control.Monad.Script.Http.hPutStrLn
  , hPutStrLnBlocking

  -- ** HTTP calls
  , httpGet
  , httpSilentGet
  , httpPost
  , httpSilentPost
  , httpDelete
  , httpSilentDelete

  -- ** JSON
  , parseJson
  , lookupKeyJson
  , constructFromJson

  -- * Types
  , Url
  , JsonError(..)
  , HttpResponse(..)

  -- * Testing
  , checkHttpM
  , checkHttpTM
) where

import Prelude hiding (lookup)

import Control.Applicative
  ( Applicative(..), (<$>) )
import Control.Concurrent
  ( threadDelay )
import Control.Concurrent.MVar
  ( MVar, withMVar )
import Control.Exception
  ( IOException, Exception, try )
import Control.Monad
  ( Functor(..), Monad(..), ap )
import Control.Lens
  ( preview, (^.) )
import Data.Aeson
  ( Value(Object), Result(Success,Error), FromJSON, fromJSON, decode )
import Data.Aeson.Encode.Pretty
  ( encodePretty )
import Data.Aeson.Lens
  ( _Value )
import Data.ByteString.Lazy
  ( ByteString, fromStrict, readFile, writeFile )
import Data.ByteString.Lazy.Char8
  ( unpack, pack )
import Data.Functor.Identity
  ( Identity() )
import Data.HashMap.Strict
  ( lookup )
import Data.IORef
  ( IORef, newIORef, readIORef, writeIORef )
import Data.String
  ( fromString )
import Data.Text
  ( Text )
import Data.Time
  ( UTCTime )
import Data.Time.Clock.System
  ( getSystemTime, systemToUTCTime )
import Data.Typeable
  ( Typeable )
import Data.Monoid
  ( Monoid(..) )
import Network.HTTP.Client
  ( HttpException(..), CookieJar, HttpExceptionContent(StatusCodeException)
  , Response, responseCookieJar, responseBody
  , responseHeaders, responseVersion, responseStatus )
import Network.HTTP.Types
  ( HttpVersion, Status, ResponseHeaders )
import qualified Network.Wreq as Wreq
  ( Options, getWith, postWith, deleteWith, defaults, responseStatus, headers )
import qualified Network.Wreq.Session as S
  ( Session, newSession, getWith, postWith, deleteWith )
import System.IO
  ( Handle, hPutStrLn, hGetEcho, hSetEcho, hFlush
  , hFlush, hGetLine, hPutStr, hPutChar, stdout )
import System.IO.Error
  ( ioeGetFileName, ioeGetLocation, ioeGetErrorString )
import Test.QuickCheck
  ( Property, Arbitrary(..), Gen )

import qualified Control.Monad.Script as S
import Network.HTTP.Client.Extras
import Data.Aeson.Extras
import Data.MockIO
import Data.MockIO.FileSystem



-- | An HTTP session returning an @a@, writing to a log of type @W e w@, reading from an environment of type @R e w r@, with state of type @S s@, throwing errors of type @E e@, performing effectful computations described by @P p a@, and with inner monad @m@.
newtype HttpT e r w s p m a = HttpT
  { httpT :: S.ScriptT (E e) (R e w r) (W e w) (S s) (P p) m a
  } deriving Typeable

-- | An HTTP session returning an @a@, writing to a log of type @W e w@, reading from an environment of type @R e w r@, with state of type @S s@, throwing errors of type @E e@, performing effectful computations described by @P p a@. `HttpT` over `Identity`.
type Http e r w s p a = HttpT e r w s p Identity a

instance Functor (HttpT e r w s p m) where
  fmap f = HttpT . fmap f . httpT

instance Applicative (HttpT e r w s p m) where
  pure = return
  (<*>) = ap

instance Monad (HttpT e r w s p m) where
  return = HttpT . return
  (HttpT x) >>= f = HttpT (x >>= (httpT . f))



-- | Execute an `HttpT` session.
execHttpTM
  :: (Monad (m eff), Monad eff)
  => S s -- ^ Initial state
  -> R e w r -- ^ Environment
  -> (forall u. P p u -> eff u) -- ^ Effect evaluator
  -> (forall u. eff u -> m eff u) -- ^ Lift effects to the inner monad
  -> HttpT e r w s p (m eff) t
  -> m eff (Either (E e) t, S s, W e w)
execHttpTM s r p lift = S.execScriptTM s r p lift . httpT

-- | Turn an `HttpT` into a property; for testing with QuickCheck.
checkHttpTM
  :: (Monad (m eff), Monad eff)
  => S s -- ^ Initial state
  -> R e w r -- ^ Environment
  -> (forall u. P p u -> eff u) -- ^ Effect evaluator
  -> (forall u. eff u -> m eff u) -- ^ Lift effects to the inner monad
  -> (m eff (Either (E e) t, S s, W e w) -> IO q) -- ^ Condense to `IO`
  -> (q -> Bool) -- ^ Result check
  -> HttpT e r w s p (m eff) t
  -> Property
checkHttpTM s r eval lift cond check =
  S.checkScriptTM s r eval lift cond check . httpT

-- | Execute an `Http` session.
execHttpM
  :: (Monad eff)
  => S s -- ^ Initial state
  -> R e w r -- ^ Environment
  -> (forall u. P p u -> eff u) -- ^ Effect evaluator
  -> Http e r w s p t
  -> eff (Either (E e) t, S s, W e w)
execHttpM s r eval = S.execScriptM s r eval . httpT

-- | Turn an `Http` into a `Property`; for testing with QuickCheck.
checkHttpM
  :: (Monad eff)
  => S s -- ^ Initial state
  -> R e w r -- ^ Environment
  -> (forall u. P p u -> eff u) -- ^ Effect evaluator
  -> (eff (Either (E e) t, S s, W e w) -> IO q) -- ^ Condense to `IO`
  -> (q -> Bool) -- ^ Result check
  -> Http e r w s p t
  -> Property
checkHttpM s r eval cond check =
  S.checkScriptM s r eval cond check . httpT



-- | Retrieve the environment.
ask
  :: HttpT e r w s p m (R e w r)
ask = HttpT S.ask

-- | Run an action with a locally adjusted environment of the same type.
local
  :: (R e w r -> R e w r)
  -> HttpT e r w s p m a
  -> HttpT e r w s p m a
local f = HttpT . S.local f . httpT

-- | Run an action with a locally adjusted environment of a possibly different type.
transport
  :: (R e w r2 -> R e w r1)
  -> HttpT e r1 w s p m a
  -> HttpT e r2 w s p m a
transport f = HttpT . S.transport f . httpT

-- | Retrieve the image of the environment under a given function.
reader
  :: (R e w r -> a)
  -> HttpT e r w s p m a
reader f = HttpT (S.reader f)

-- | Retrieve the current state.
get
  :: HttpT e r w s p m (S s)
get = HttpT S.get

-- | Replace the state.
put
  :: S s
  -> HttpT e r w s p m ()
put s = HttpT (S.put s)

-- | Modify the current state strictly.
modify
  :: (S s -> S s)
  -> HttpT e r w s p m ()
modify f = HttpT (S.modify' f)

-- | Retrieve the image of the current state under a given function.
gets
  :: (S s -> a)
  -> HttpT e r w s p m a
gets f = HttpT (S.gets f)

-- | Do not export; we want to only allow writes to the log via functions that call @logNow@.
tell
  :: W e w
  -> HttpT e r w s p m ()
tell w = HttpT (S.tell w)

-- | Run an action that returns a value and a log-adjusting function, and apply the function to the local log.
pass
  :: HttpT e r w s p m (a, W e w -> W e w)
  -> HttpT e r w s p m a
pass = HttpT . S.pass . httpT

-- | Run an action, applying a function to the local log.
censor
  :: (W e w -> W e w)
  -> HttpT e r w s p m a
  -> HttpT e r w s p m a
censor f = HttpT . S.censor f . httpT

-- | Inject an 'Either' into a 'Script'.
except
  :: Either (E e) a
  -> HttpT e r w s p m a
except e = HttpT (S.except e)

-- | Raise an error
throw
  :: E e
  -> HttpT e r w s p m a
throw e = HttpT (S.throw e)

-- | Run an action, applying a handler in case of an error result.
catch
  :: HttpT e r w s p m a -- ^ Computation that may raise an error
  -> (E e -> HttpT e r w s p m a) -- ^ Handler
  -> HttpT e r w s p m a
catch x f = HttpT (S.catch (httpT x) (httpT . f))

-- | Inject an atomic effect.
prompt
  :: P p a
  -> HttpT e r w s p m a
prompt p = HttpT (S.prompt p)

-- | Lift a value from the inner monad
liftHttpT
  :: (Monad m)
  => m a
  -> HttpT e r w s p m a
liftHttpT = HttpT . S.lift



-- | Error type.
data E e
  = E_Http HttpException
  | E_IO IOException
  | E_Json JsonError
  | E e -- ^ Client-supplied error type.
  deriving Show

-- | Also logs the exception.
throwHttpException
  :: HttpException
  -> HttpT e r w s p m a
throwHttpException e = do
  logNow $ errorMessage $ E_Http e
  throw $ E_Http e

-- | Re-throws other error types.
catchHttpException
  :: HttpT e r w s p m a
  -> (HttpException -> HttpT e r w s p m a) -- ^ Handler
  -> HttpT e r w s p m a
catchHttpException x handler = catch x $ \err ->
  case err of
    E_Http e -> handler e
    _ -> throw err

-- | Also logs the exception.
throwIOException
  :: IOException
  -> HttpT e r w s p m a
throwIOException e = do
  logNow $ errorMessage $ E_IO e
  throw $ E_IO e

-- | Re-throws other error types.
catchIOException
  :: HttpT e r w s p m a
  -> (IOException -> HttpT e r w s p m a) -- ^ Handler
  -> HttpT e r w s p m a
catchIOException x handler = catch x $ \err ->
  case err of
    E_IO e -> handler e
    _ -> throw err

-- | Also logs the exception.
throwJsonError
  :: JsonError
  -> HttpT e r w s p m a
throwJsonError e = do
  logNow $ errorMessage $ E_Json e
  throw $ E_Json e

-- | Re-throws other error types.
catchJsonError
  :: HttpT e r w s p m a
  -> (JsonError -> HttpT e r w s p m a) -- ^ Handler
  -> HttpT e r w s p m a
catchJsonError x handler = catch x $ \err ->
  case err of
    E_Json e -> handler e
    _ -> throw err

-- | Also logs the exception.
throwError
  :: e
  -> HttpT e r w s p m a
throwError e = do
  logNow $ errorMessage $ E e
  throw $ E e

-- | Re-throws other error types.
catchError
  :: HttpT e r w s p m a
  -> (e -> HttpT e r w s p m a) -- ^ Handler
  -> HttpT e r w s p m a
catchError x handler = catch x $ \err ->
  case err of
    E e -> handler e
    _ -> throw err



-- | Generic session environment.
data R e w r = R
  { _logOptions :: LogOptions e w

  -- | Handle for printing logs
  , _logHandle :: Handle

  -- | Lock used to prevent race conditions when writing to the log.
  , _logLock :: Maybe (MVar ())

  -- | Identifier string for the session; used to help match log entries emitted by the same session.
  , _uid :: String

  -- | Function for elevating 'HttpException's to a client-supplied error type.
  , _httpErrorInject :: HttpException -> Maybe e 

  -- | Client-supplied environment type.
  , _env :: r
  }

-- | Environment constructor
basicEnv
  :: (Show e, Show w)
  => r -- ^ Client-supplied environment value.
  -> R e w r
basicEnv r = R
  { _httpErrorInject = const Nothing
  , _logOptions = basicLogOptions
  , _logHandle = stdout
  , _logLock = Nothing
  , _uid = ""
  , _env = r
  }

-- | Environment constructor
trivialEnv
  :: r -- ^ Client-supplied environment value.
  -> R e w r
trivialEnv r = R
  { _httpErrorInject = const Nothing
  , _logOptions = trivialLogOptions
  , _logHandle = stdout
  , _logLock = Nothing
  , _uid = ""
  , _env = r
  }

-- | Options for tweaking the logs.
data LogOptions e w = LogOptions
  { -- | Toggle color
    _logColor :: Bool

    -- | Toggle JSON pretty printing
  , _logJson :: Bool

    -- | Toggle to silence the logs
  , _logSilent :: Bool

    -- | Toggle for printing HTTP headers
  , _logHeaders :: Bool

    -- | Printer for log entries; first argument colorizes a string, and the tuple argument is (timestamp, uid, message).
  , _logEntryPrinter :: (String -> String) -> (String, String, String) -> String

    -- | Printer for client-supplied error type. The boolean toggles JSON pretty printing.
  , _printUserError :: Bool -> e -> String

    -- | Printer for client-supplied log type. the boolean toggles JSON pretty printing.
  , _printUserLog :: Bool -> w -> String
  }

-- | Noisy, in color, without parsing JSON responses, and using `Show` instances for user-supplied error and log types.
basicLogOptions :: (Show e, Show w) => LogOptions e w
basicLogOptions = LogOptions
  { _logColor = True
  , _logJson = False
  , _logSilent = False
  , _logHeaders = True
  , _logEntryPrinter = basicLogEntryPrinter
  , _printUserError = \_ e -> show e
  , _printUserLog = \_ w -> show w
  }

-- | Noisy, in color, without parsing JSON responses, and using trivial printers for user-supplied error and log types. For testing.
trivialLogOptions :: LogOptions e w
trivialLogOptions = LogOptions
  { _logColor = True
  , _logJson = False
  , _logSilent = False
  , _logHeaders = True
  , _logEntryPrinter = basicLogEntryPrinter
  , _printUserError = \_ _ -> "ERROR"
  , _printUserLog = \_ _ -> "LOG"
  }

basicLogEntryPrinter
  :: (String -> String)
  -> (String, String, String)
  -> String
basicLogEntryPrinter colorize (timestamp, uid, msg) =
  unwords $ filter (/= "")
    [ colorize timestamp, uid, msg ]



-- | Log type
data W e w = W [(UTCTime, Log e w)]

instance Monoid (W e w) where
  mempty = W []
  mappend (W a1) (W a2) = W (a1 ++ a2)

-- | Log entry type
data Log e w
  = L_Comment String
  | L_Request HttpVerb Url Wreq.Options (Maybe ByteString)
  | L_SilentRequest
  | L_Response HttpResponse
  | L_SilentResponse
  | L_Pause Int
  | L_HttpError HttpException
  | L_IOError IOException
  | L_JsonError JsonError

  -- | Client-supplied error type
  | L_Error e

  -- | Client-supplied log entry type
  | L_Log w
  deriving Show

-- | Used in the logs.
data HttpVerb
  = DELETE | GET | POST
  deriving (Eq, Show)



-- | Convert errors to log entries
errorMessage :: E e -> Log e w
errorMessage e = case e of
  E_Http err -> L_HttpError err
  E_IO err -> L_IOError err
  E_Json err -> L_JsonError err
  E e -> L_Error e

-- | Used to specify colors for user-supplied log entries.
data Color
  = Red | Blue | Green | Yellow | Magenta

inColor :: Color -> String -> String
inColor c msg = case c of
  Red -> "\x1b[1;31m" ++ msg ++ "\x1b[0;39;49m"
  Blue -> "\x1b[1;34m" ++ msg ++ "\x1b[0;39;49m"
  Green -> "\x1b[1;32m" ++ msg ++ "\x1b[0;39;49m"
  Yellow -> "\x1b[1;33m" ++ msg ++ "\x1b[0;39;49m"
  Magenta -> "\x1b[1;35m" ++ msg ++ "\x1b[0;39;49m"

printEntryWith
  :: Bool -- ^ Json
  -> Bool -- ^ Headers
  -> (Bool -> e -> String)
  -> (Bool -> w -> String)
  -> Log e w
  -> (Color, String)
printEntryWith asJson showHeaders printError printLog entry = case entry of
  L_Comment msg -> (Green, msg)

  L_Request verb url opt payload ->
    let
      head = case (asJson, showHeaders) of
        (True,  True)  -> unpack $ encodePretty $ jsonResponseHeaders $ opt ^. Wreq.headers
        (False, True)  -> show $ opt ^. Wreq.headers
        (_,     False) -> ""

      body = case (asJson, payload) of
        (True,  Just p)  -> case decode p of
          Nothing -> "JSON parse error:\n" ++ unpack p
          Just v -> unpack $ encodePretty (v :: Value)
        (False, Just p)  -> unpack p
        (_,     Nothing) -> ""

    in
      (Blue, unlines $ filter (/= "") [unwords ["Request", show verb, url], head, body])

  L_SilentRequest -> (Blue, "Silent Request")

  L_Response response ->
    let
      head = case (asJson, showHeaders) of
        (True,  True)  -> unpack $ encodePretty $ jsonResponseHeaders $ _responseHeaders response
        (False, True)  -> show $ _responseHeaders response
        (_,     False) -> ""

      body = case asJson of
        True  -> unpack $ encodePretty $ preview _Value $ _responseBody response
        False -> show response

    in
      (Blue, unlines $ filter (/= "") ["Response", head, body])

  L_SilentResponse -> (Blue, "Silent Response")

  L_Pause k -> (Magenta, "Wait for " ++ show k ++ "μs")

  L_HttpError e -> if asJson
    then
      let
        unpackHttpError :: HttpException -> Maybe (String, String)
        unpackHttpError err = case err of
          HttpExceptionRequest _ (StatusCodeException s r) -> do
            json <- decode $ fromStrict r
            let status = s ^. Wreq.responseStatus 
            return (show status, unpack $ encodePretty (json :: Value))
          _ -> Nothing
      in
        case unpackHttpError e of
          Nothing -> (Red, show e)
          Just (code, json) -> (Red, unlines [ unwords [ "HTTP Error Response", code], json ])

    else (Red, show e)

  L_IOError e -> (Red, unwords [ show $ ioeGetFileName e, ioeGetLocation e, ioeGetErrorString e ])

  L_JsonError e -> (Red, "JSON Error: " ++ show e)

  L_Error e -> (Red, unwords [ "ERROR", printError asJson e ])

  L_Log w -> (Yellow, unwords [ "INFO", printLog asJson w ])

-- | Render a log entry
printLogWith
  :: LogOptions e w
  -> ((String -> String) -> (String, String, String) -> String)
  -> (UTCTime, String, Log e w)
  -> Maybe String
printLogWith opt@LogOptions{..} printer (timestamp, uid, entry) =
  if _logSilent
    then Nothing
    else do
      let
        time :: String
        time = take 19 $ show timestamp

        color :: Color -> String -> String
        color c = if _logColor then inColor c else id

        (c,msg) = printEntryWith _logJson _logHeaders _printUserError _printUserLog entry

      Just $ printer (color c) (time, uid, msg)

-- | Extract the user-defined log entries.
logEntries :: W e w -> [w]
logEntries (W xs) = entries xs
  where
    entries [] = []
    entries ((_,w):ws) = case w of
      L_Log u -> u : entries ws
      _ -> entries ws



-- | State type
data S s = S
  { _httpOptions :: Wreq.Options
  , _httpSession :: Maybe S.Session
  , _userState :: s
  }

-- | State constructor
basicState :: s -> S s
basicState s = S
  { _httpOptions = Wreq.defaults
  , _httpSession = Nothing
  , _userState = s
  }



-- | Atomic effects
data P p a where
  HPutStrLn
    :: Handle -> String
    -> P p (Either IOException ())
  HPutStrLnBlocking
    :: MVar () -> Handle -> String
    -> P p (Either IOException ())

  GetSystemTime :: P p UTCTime
  ThreadDelay :: Int -> P p ()

  HttpGet
    :: Wreq.Options -> Maybe S.Session -> Url
    -> P p (Either HttpException HttpResponse)
  HttpPost
    :: Wreq.Options -> Maybe S.Session -> Url
    -> ByteString -> P p (Either HttpException HttpResponse)
  HttpDelete
    :: Wreq.Options -> Maybe S.Session -> Url
    -> P p (Either HttpException HttpResponse)

  P :: p a -> P p a

-- | Basic evaluator for interpreting atomic 'Http' effects in 'IO'.
evalIO
  :: (p a -> IO a) -- ^ Evaluator for user effects
  -> P p a
  -> IO a
evalIO eval x = case x of
  HPutStrLn handle string -> try $ do
    System.IO.hPutStrLn handle string
    hFlush handle

  HPutStrLnBlocking lock handle str -> try $ do
    withMVar lock (\() -> System.IO.hPutStrLn handle str)
    hFlush handle

  GetSystemTime -> fmap systemToUTCTime getSystemTime

  ThreadDelay k -> threadDelay k

  HttpGet opts s url -> case s of
    Nothing -> try $ readHttpResponse <$> Wreq.getWith opts url
    Just sn -> try $ readHttpResponse <$> S.getWith opts sn url

  HttpPost opts s url msg -> case s of
    Nothing -> try $ readHttpResponse <$> Wreq.postWith opts url msg
    Just sn -> try $ readHttpResponse <$> S.postWith opts sn url msg

  HttpDelete opts s url -> case s of
    Nothing -> try $ readHttpResponse <$> Wreq.deleteWith opts url
    Just sn -> try $ readHttpResponse <$> S.deleteWith opts sn url

  P act -> eval act

-- | Basic evaluator for interpreting atomic 'Http' effects in 'MockIO'.
evalMockIO
  :: (p a -> MockIO s a)
  -> P p a
  -> MockIO s a
evalMockIO eval x = case x of
  HPutStrLn handle str -> do
    incrementTimer 1
    fmap Right $ modifyMockWorld $ \w -> w
      { _files = appendLines handle (lines str) $ _files w }

  HPutStrLnBlocking _ handle str -> do
    incrementTimer 1
    fmap Right $ modifyMockWorld $ \w -> w
      { _files = appendLines handle (lines str) $ _files w }

  GetSystemTime -> do
    incrementTimer 1
    MockWorld{..} <- getMockWorld
    return _time

  ThreadDelay k -> incrementTimer k

  HttpGet _ _ url -> do
    incrementTimer 1
    MockWorld{..} <- getMockWorld
    let (r,t) = unMockNetwork (_httpGet url) _serverState
    modifyMockWorld $ \w -> w { _serverState = t }
    return r

  HttpPost _ _ url payload -> do
    incrementTimer 1
    MockWorld{..} <- getMockWorld
    let (r,t) = unMockNetwork (_httpPost url payload) _serverState
    modifyMockWorld $ \w -> w { _serverState = t }
    return r

  HttpDelete _ _ url -> do
    incrementTimer 1
    MockWorld{..} <- getMockWorld
    let (r,t) = unMockNetwork (_httpDelete url) _serverState
    modifyMockWorld $ \w -> w { _serverState = t }
    return r

  P p -> do
    incrementTimer 1
    eval p



-- | All log statements should go through @logNow@.
logNow
  :: Log e w
  -> HttpT e r w s p m ()
logNow msg = do
  time <- prompt GetSystemTime
  printer <- reader (_logEntryPrinter . _logOptions)
  R{..} <- ask
  case printLogWith _logOptions printer (time,_uid,msg) of
    Nothing -> return ()
    Just str -> case _logLock of
      Just lock -> hPutStrLnBlocking lock _logHandle str
      Nothing -> Control.Monad.Script.Http.hPutStrLn _logHandle str
  tell $ W [(time, msg)]

-- | Write a comment to the log
comment
  :: String
  -> HttpT e r w s p m ()
comment msg = logNow $ L_Comment msg

-- | Pause the thread
wait
  :: Int -- ^ milliseconds
  -> HttpT e r w s p m ()
wait k = do
  logNow $ L_Pause k
  prompt $ ThreadDelay k

-- | Write an entry to the log
logEntry
  :: w
  -> HttpT e r w s p m ()
logEntry = logNow . L_Log



-- | Write a line to a handle
hPutStrLn
  :: Handle
  -> String
  -> HttpT e r w s p m ()
hPutStrLn h str = do
  result <- prompt $ HPutStrLn h str
  case result of
    Right () -> return ()
    Left e -> throwIOException e

-- | Write a line to a handle, using the given `MVar` as a lock
hPutStrLnBlocking
  :: MVar ()
  -> Handle
  -> String
  -> HttpT e r w s p m ()
hPutStrLnBlocking lock h str = do
  result <- prompt $ HPutStrLnBlocking lock h str
  case result of
    Right () -> return ()
    Left e -> throwIOException e



-- | Run a @GET@ request
httpGet
  :: Url
  -> HttpT e r w s p m HttpResponse
httpGet url = do
  R{..} <- ask
  S{..} <- get
  logNow $ L_Request GET url _httpOptions Nothing
  result <- prompt $ HttpGet _httpOptions _httpSession url
  case result of
    Right response -> do
      logNow $ L_Response response
      return response
    Left err -> case _httpErrorInject err of
      Just z -> throwError z
      Nothing -> throwHttpException err

-- | Run a @GET@ request, but do not write the request or response to the logs.
httpSilentGet
  :: Url
  -> HttpT e r w s p m HttpResponse
httpSilentGet url = do
  R{..} <- ask
  S{..} <- get
  logNow L_SilentRequest
  result <- prompt $ HttpGet _httpOptions _httpSession url
  case result of
    Right response -> do
      logNow L_SilentResponse
      return response
    Left err -> case _httpErrorInject err of
      Just z -> throwError z
      Nothing -> throwHttpException err

-- | Run a @POST@ request
httpPost
  :: Url
  -> ByteString -- ^ Payload
  -> HttpT e r w s p m HttpResponse
httpPost url payload = do
  R{..} <- ask
  S{..} <- get
  logNow $ L_Request POST url _httpOptions (Just payload)
  result <- prompt $ HttpPost _httpOptions _httpSession url payload
  case result of
    Right response -> do
      logNow $ L_Response response
      return response
    Left err -> case _httpErrorInject err of
      Just z -> throwError z
      Nothing -> throwHttpException err

-- | Run a @POST@ request, but do not write the request or response to the logs.
httpSilentPost
  :: Url
  -> ByteString -- ^ Payload
  -> HttpT e r w s p m HttpResponse
httpSilentPost url payload = do
  R{..} <- ask
  S{..} <- get
  logNow L_SilentRequest
  result <- prompt $ HttpPost _httpOptions _httpSession url payload
  case result of
    Right response -> do
      logNow L_SilentResponse
      return response
    Left err -> case _httpErrorInject err of
      Just z -> throwError z
      Nothing -> throwHttpException err

-- | Run a @DELETE@ request
httpDelete
  :: Url
  -> HttpT e r w s p m HttpResponse
httpDelete url = do
  R{..} <- ask
  S{..} <- get
  logNow $ L_Request DELETE url _httpOptions Nothing
  result <- prompt $ HttpDelete _httpOptions _httpSession url
  case result of
    Right response -> do
      logNow $ L_Response response
      return response
    Left err -> case _httpErrorInject err of
      Just z -> throwError z
      Nothing -> throwHttpException err

-- | Run a @DELETE@ request, but do not write the request or response to the logs.
httpSilentDelete
  :: Url
  -> HttpT e r w s p m HttpResponse
httpSilentDelete url = do
  R{..} <- ask
  S{..} <- get
  logNow L_SilentRequest
  result <- prompt $ HttpDelete _httpOptions _httpSession url
  case result of
    Right response -> do
      logNow L_SilentResponse
      return response
    Left err -> case _httpErrorInject err of
      Just z -> throwError z
      Nothing -> throwHttpException err



-- | Parse a `ByteString` to a JSON `Value`.
parseJson
  :: ByteString
  -> HttpT e r w s p m Value
parseJson bytes = case preview _Value bytes of
  Just value -> return value
  Nothing -> throwJsonError $ JsonParseError bytes

-- | Object member lookup.
lookupKeyJson
  :: Text -- ^ Key name
  -> Value -- ^ JSON object
  -> HttpT e r w s p m Value
lookupKeyJson key v = case v of
  Object obj -> case lookup key obj of
    Nothing -> throwJsonError $ JsonKeyDoesNotExist key (Object obj)
    Just value -> return value
  _ -> throwJsonError $ JsonKeyLookupOffObject key v

-- | Decode a `A.Value` to some other type.
constructFromJson
  :: (FromJSON a)
  => Value
  -> HttpT e r w s p m a
constructFromJson value = case fromJSON value of
  Success x -> return x
  Error msg -> throwJsonError $ JsonConstructError msg

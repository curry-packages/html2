------------------------------------------------------------------------------
--- This module implements the management of sessions.
--- In particular, it defines a cookie that must be sent to the client
--- in order to enable the handling of sessions.
--- Based on sessions, this module also defines a session store
--- that can be used by various parts of the application in order
--- to hold some session-specific data.
---
--- @author Michael Hanus
--- @version May 2021
------------------------------------------------------------------------------

module HTML.Session
  ( sessionDataDir, inSessionDataDir
  , sessionCookie, doesSessionExist, withSessionCookie, withSessionCookieInfo
  , SessionData, emptySessionData, SessionStore, sessionStore
  , getSessionMaybeData, getSessionData
  , putSessionData, removeSessionData, modifySessionData
  ) where

import Control.Monad      ( unless )
import System.Directory   ( createDirectory, doesDirectoryExist )
import System.FilePath    ( (</>) )
import Data.List          ( findIndex, init, intercalate, replace, split )
import Data.Maybe         ( fromMaybe )
import System.Environment ( getEnv )
import Data.Time          (ClockTime, addMinutes, clockTimeToInt, getClockTime )

import Crypto.Hash        ( randomString )
import Data.Global
import HTML.Base

------------------------------------------------------------------------------
--- The name of the local directory where the session data,
--- e.g., cookie information, is stored.
--- For security reasons, the directory should be non-public readable.
sessionDataDir :: String
sessionDataDir = "sessiondata"

--- Prefix a file name with the directory where session data,
--- e.g., cookie information, is stored.
inSessionDataDir :: String -> String
inSessionDataDir filename = sessionDataDir </> filename

--- Ensures that the `sessionDataDir` directory exists.
--- If it does not exist, it will be created.
ensureSessionDataDir :: IO ()
ensureSessionDataDir = do
  exsdd <- doesDirectoryExist sessionDataDir
  unless exsdd $ createDirectory sessionDataDir

------------------------------------------------------------------------------
--- The life span in minutes to store data in sessions.
--- Thus, older data is deleted by a clean up that is initiated
--- whenever new data is stored in a session.
sessionLifespan :: Int
sessionLifespan = 60

--- The name of the persistent global where the last session id is stored.
sessionCookieName :: String
sessionCookieName = "currySessionId"

--- This global value contains a unique id used to create a fresh id
--- for each new session. Basically, it contains the clock time (represented
--- as an integer value) of the time where the last session was created.
--- Since the clock time might not be precise enough to distinguish
--- two new sessions, the second component is a counter incremented
--- whenever two sessions have the same clock time.
lastId :: GlobalP (Int, Int)
lastId = globalPersistent (inSessionDataDir sessionCookieName) (0, 0)


--- The abstract type to represent session identifiers.
data SessionId = SessionId String
 deriving (Eq, Read, Show)

getId :: SessionId -> String
getId (SessionId i) = i

--- Creates a new unused session id.
getUnusedId :: IO SessionId
getUnusedId = do
  ensureSessionDataDir
  (ltime,lsid) <- safeReadGlobalP lastId (0,0)
  clockTime <- getClockTime
  if clockTimeToInt clockTime /= ltime
    then writeGlobalP lastId (clockTimeToInt clockTime, 0)
    else writeGlobalP lastId (clockTimeToInt clockTime, lsid+1)
  rans <- randomString 30
  return (SessionId (show (clockTimeToInt clockTime) ++ show (lsid+1) ++ rans))

--- Checks whether the current user session is initialized,
--- i.e., whether a session cookie has been already set.
doesSessionExist :: IO Bool
doesSessionExist = do
    cookies <- getCookies
    return $ maybe False (const True) (lookup sessionCookieName cookies)

--- Gets the id of the current user session.
--- If this is a new session, a new id is created and returned.
getSessionId :: IO SessionId
getSessionId = do
    cookies <- getCookies
    case (lookup sessionCookieName cookies) of
      Just sessionCookieValue -> return (SessionId sessionCookieValue)
      Nothing                 -> getUnusedId

--- Creates a cookie to hold the current session id.
--- This cookie should be sent to the client together with every HTML page.
sessionCookie :: IO PageParam
sessionCookie = do
  sessionId <- getSessionId
  clockTime <- getClockTime
  dirpath   <- getScriptDirPath
  return $ PageCookie sessionCookieName (getId (sessionId))
                      [CookiePath (if null dirpath then "/" else dirpath),
                       CookieExpire (addMinutes sessionLifespan clockTime)]

--- Gets the directory path of the current CGI script via the
--- environment variable `SCRIPT_NAME`.
--- For instance, if the script is called with URL
--- `http://example.com/cgi/test/script.cgi?parameter`,
--- then `/cgi/test`  is returned.
--- If `SCRIPT_NAME` is not set, the returned string is empty.
getScriptDirPath :: IO String
getScriptDirPath = do
  scriptname <- getEnv "SCRIPT_NAME"
  let scriptpath = if null scriptname then []
                                      else split (=='/') (tail scriptname)
  if null scriptpath
    then return ""
    else return $ "/" ++ intercalate "/" (init scriptpath)

--- Decorates an HTML page with session cookie.
withSessionCookie :: HtmlPage -> IO HtmlPage
withSessionCookie p = do
  cookie <- sessionCookie
  return $ (p `addPageParam` cookie)

--- Decorates an HTML page with session cookie and shows an information
--- page when the session cookie is not set.
withSessionCookieInfo :: HtmlPage -> IO HtmlPage
withSessionCookieInfo p = do
  hassession <- doesSessionExist
  if hassession then withSessionCookie p
                else cookieInfoPage

-- Returns HTML page with information about the use of cookies.
cookieInfoPage :: IO HtmlPage
cookieInfoPage = do
  urlparam <- getUrlParameter
  withSessionCookie $ headerPage "Cookie Info"
    [ par [ htxt $ "This web site uses cookies for navigation and user " ++
                   "inputs and preferences. In order to proceed, "
          , bold [href ('?' : urlparam) [htxt "please click here."]]]]

------------------------------------------------------------------------------
-- Implementation of session data and stores.

--- The type of session data which represents the data used in a session.
--- The session data consists of a list of data items for each session in the
--- system together with the clock time of the last access.
--- The clock time is used to delete old data in the store.
data SessionData a = SessionData [(SessionId, Int, a)]
 deriving (Read,Show)

--- An initial value for the empty session data.
emptySessionData :: SessionData _
emptySessionData = SessionData []

--- The type of a session store is a persistent global entity containing
--- a session store with some data.
type SessionStore a = GlobalP (SessionData a)

--- A session store contains readable and showable data kept in
--- a store with a given name. The name is used as a file name
--- in the directory containing all session data.
sessionStore :: (Read a, Show a) => String -> SessionStore a
sessionStore name = globalPersistent (inSessionDataDir name) emptySessionData

--- Retrieves data for the current user session stored in a session store.
--- Returns `Nothing` if there is no data for the current session.
getSessionMaybeData :: (Read a, Show a) =>
                       SessionStore a -> FormReader (Maybe a)
getSessionMaybeData sessionData = toFormReader $ do
  ensureSessionDataDir
  sid <- getSessionId
  SessionData sdata <- safeReadGlobalP sessionData emptySessionData
  return (findInSession sid sdata)
 where
  findInSession si ((id, _, storedData):rest) =
    if getId id == getId si
      then Just storedData
      else findInSession si rest
  findInSession _ [] = Nothing

--- Retrieves data for the current user session stored in a session store
--- where the second argument is returned if there is no data
--- for the current session.
getSessionData :: (Read a, Show a) => SessionStore a -> a -> FormReader a
getSessionData sessiondata defaultdata =
  fmap (fromMaybe defaultdata) (getSessionMaybeData sessiondata)

--- Stores data related to the current user session in a session store.
putSessionData :: (Read a, Show a) => SessionStore a -> a -> IO ()
putSessionData sessionData newData = do
  ensureSessionDataDir
  sid <- getSessionId
  SessionData sdata <- safeReadGlobalP sessionData emptySessionData
  currentTime <- getClockTime
  case findIndex (\ (id, _, _) -> id == sid) sdata of
    Just i ->
      writeGlobalP sessionData
        (SessionData (replace (sid, clockTimeToInt currentTime, newData) i
                              (cleanup currentTime sdata)))
    Nothing ->
      writeGlobalP sessionData
                   (SessionData ((sid, clockTimeToInt currentTime, newData)
                                 : cleanup currentTime sdata))

--- Modifies the data of the current user session.
modifySessionData :: (Read a, Show a) =>
                     SessionStore a -> a -> (a -> a) -> IO ()
modifySessionData sessiondata defaultdata upd = do
  sd <- fromFormReader $ getSessionData sessiondata defaultdata
  putSessionData sessiondata (upd sd)

--- Removes data related to the current user session from a session store.
removeSessionData :: (Read a, Show a) => SessionStore a -> IO ()
removeSessionData sessionData = do
  ensureSessionDataDir
  sid <- getSessionId
  SessionData sdata <- safeReadGlobalP sessionData emptySessionData
  currentTime <- getClockTime
  writeGlobalP sessionData
               (SessionData (filter (\ (id, _, _) -> id /= sid)
                                    (cleanup currentTime sdata)))

-- expects that clockTimeToInt converts time into ascending integers!
-- we should write our own conversion-function
cleanup :: ClockTime -> [(SessionId, Int, a)] -> [(SessionId, Int, a)]
cleanup currentTime sessionData =
  filter (\ (_, time, _) ->
            time > clockTimeToInt (addMinutes (0-sessionLifespan) currentTime))
         sessionData

------------------------------------------------------------------------------

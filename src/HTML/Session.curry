--------------------------------------------------------------------------
--- This module implements the management of sessions.
--- In particular, it defines a cookie that must be sent to the client
--- in order to enable the handling of sessions.
--- Based on session, this module defines also a session store
--- that can be used by various parts of the application in order
--- to hold some session-specific data.
--------------------------------------------------------------------------

module HTML.Session (
  sessionCookie, withSessionCookie,
  SessionStore, emptySessionStore,
  getSessionMaybeData, getSessionData, putSessionData, removeSessionData,
  updateSessionData
  ) where

import FilePath       ( (</>) )
import Global
import List           ( findIndex, replace )
import Maybe          ( fromMaybe )
import Time           ( ClockTime, addMinutes, clockTimeToInt, getClockTime )

import HTML.Base
import System.Crypto  ( randomString )

--- The directory where information about cookies is stored.
storageDir :: String
storageDir = "."

--- The life span in minutes to store data in sessions.
--- Thus, older data is deleted by a clean up that is initiated
--- whenever new data is stored in a session.
sessionLifespan :: Int
sessionLifespan = 60

--- The name of the persistent global where the last session id is stored.
sessionCookieName :: String
sessionCookieName = "spiceySessionId"

--- This global value saves time and last session id.
lastId :: Global (Int, Int)
lastId = global (0, 0) (Persistent (storageDir </> sessionCookieName))


--- The abstract type to represent session identifiers.
data SessionId = SessionId String
 deriving Eq

getId :: SessionId -> String
getId (SessionId i) = i

--- Creates a new unused session id.
getUnusedId :: IO SessionId
getUnusedId = do
  (ltime,lsid) <- safeReadGlobal lastId (0,0)
  clockTime <- getClockTime
  if clockTimeToInt clockTime /= ltime
    then writeGlobal lastId (clockTimeToInt clockTime, 0)
    else writeGlobal lastId (clockTimeToInt clockTime, lsid+1)
  rans <- randomString 30
  return (SessionId (show (clockTimeToInt clockTime) ++ show (lsid+1) ++ rans))

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
  return (PageCookie sessionCookieName (getId (sessionId))
                     [CookiePath "/",
                      CookieExpire (addMinutes sessionLifespan clockTime)])

-- Decorates an HTML page with session cookie.
withSessionCookie :: HtmlPage -> IO HtmlPage
withSessionCookie p = do
  cookie <- sessionCookie
  return $ (p `addPageParam` cookie)

----------------------------------------------------------------------------
-- Implementation of session stores.

--- The type of a session store that holds particular data used in a session.
--- A session store consists of list of data items for each session in the
--- system together with the clock time of the last access.
--- The clock time is used to remove old data in the store.
data SessionStore a = SStore [(SessionId, Int, a)]

--- An initial value for the empty session store.
emptySessionStore :: SessionStore _
emptySessionStore = SStore []

--- Retrieves data for the current user session stored in a session store.
--- Returns `Nothing` if there is no data for the current session.
getSessionMaybeData :: Global (SessionStore a) -> IO (Maybe a)
getSessionMaybeData sessionData = do
    sid <- getSessionId
    SStore sdata <- safeReadGlobal sessionData emptySessionStore
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
getSessionData :: Global (SessionStore a) -> a -> IO a
getSessionData sessionData defaultdata =
  getSessionMaybeData sessionData >>= return . fromMaybe defaultdata

--- Stores data related to the current user session in a session store.
putSessionData :: Global (SessionStore a) -> a -> IO ()
putSessionData sessionData newData = do
  sid <- getSessionId
  SStore sdata <- safeReadGlobal sessionData emptySessionStore
  currentTime <- getClockTime
  case findIndex (\ (id, _, _) -> id == sid) sdata of
    Just i ->
      writeGlobal sessionData
                  (SStore (replace (sid, clockTimeToInt currentTime, newData) i
                                   (cleanup currentTime sdata)))
    Nothing ->
      writeGlobal sessionData
                  (SStore ((sid, clockTimeToInt currentTime, newData)
                           : cleanup currentTime sdata))

--- Updates the data of the current user session.
updateSessionData :: Global (SessionStore a) -> a -> (a -> a) -> IO ()
updateSessionData sessiondata defaultdata upd = do
  sd <- getSessionData sessiondata defaultdata
  putSessionData sessiondata (upd sd)

--- Removes data related to the current user session from a session store.
removeSessionData :: Global (SessionStore a) -> IO ()
removeSessionData sessionData = do
  sid <- getSessionId
  SStore sdata <- safeReadGlobal sessionData emptySessionStore
  currentTime <- getClockTime
  writeGlobal sessionData
              (SStore (filter (\ (id, _, _) -> id /= sid)
                              (cleanup currentTime sdata)))

-- expects that clockTimeToInt converts time into ascending integers!
-- we should write our own conversion-function
cleanup :: ClockTime -> [(SessionId, Int, a)] -> [(SessionId, Int, a)]
cleanup currentTime sessionData =
  filter (\ (_, time, _) ->
            time > clockTimeToInt (addMinutes (0-sessionLifespan) currentTime))
         sessionData

--------------------------------------------------------------------------

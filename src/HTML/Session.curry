------------------------------------------------------------------------------
-- | Author:  Michael Hanus
--   Version: October 2025
--
-- This module implements the management of sessions.
-- In particular, it defines a cookie that must be sent to the client
-- in order to enable the handling of sessions.
-- Based on sessions, this module also defines a session store
-- that can be used by various parts of the application in order
-- to hold some session-specific data.
------------------------------------------------------------------------------

module HTML.Session (
  -- * Cookies to support sessions
  sessionCookie, doesSessionExist, withSessionCookie, withSessionCookieInfo,
  -- * Session data handling
  SessionStore, sessionStore,
  getSessionMaybeData, getSessionData,
  putSessionData, removeSessionData, modifySessionData
  ) where

import Control.Monad      ( unless, when )
import Data.List          ( findIndex, init, intercalate, replace, split )
import Data.Maybe         ( fromMaybe )
import System.Environment ( getEnv )
import System.IO          ( IOMode(..), hGetContents, hPutStrLn, openFile
                          , stderr )

import System.Directory   ( createDirectory, doesDirectoryExist, doesFileExist
                          , getDirectoryContents, getModificationTime
                          , removeFile )
import System.FilePath    ( (</>) )
import System.IOExts      ( exclusiveIO )
import Data.Time          (ClockTime, addMinutes, clockTimeToInt, getClockTime )

import Crypto.Hash        ( randomString )
import Data.Global
import HTML.Base

------------------------------------------------------------------------------
-- | The life span in minutes to store data in sessions.
--   Thus, older data is deleted by a clean up that is initiated
--   whenever new data is stored in a session.
sessionLifespan :: Int
sessionLifespan = 60

-- | The name of the persistent global where the last session id is stored.
sessionCookieName :: String
sessionCookieName = "currySessionId"

-- | This global value contains a unique id used to create a fresh id
--   for each new session. Basically, it contains the clock time (represented
--   as an integer value) of the time where the last session was created.
--   Since the clock time might not be precise enough to distinguish
--   two new sessions, the second component is a counter incremented
--   whenever two sessions have the same clock time.
lastId :: GlobalP (Int, Int)
lastId = globalPersistent (inSessionDataDir "_CURRYSESSIONID_") (0, 0)


-- | The abstract type to represent session identifiers.
data SessionId = SessionId String
 deriving (Eq, Read, Show)

getId :: SessionId -> String
getId (SessionId i) = i

-- | Creates a new unused session identifier.
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

-- | Checks whether the current user session is initialized,
--   i.e., whether a session cookie has been already set.
doesSessionExist :: IO Bool
doesSessionExist = do
  cookies <- getCookies
  return $ maybe False (const True) (lookup sessionCookieName cookies)

-- | Gets the id of the current user session.
--   If this is a new session, a new id is created and returned.
getSessionId :: IO SessionId
getSessionId = do
  cookies <- getCookies
  case (lookup sessionCookieName cookies) of
    Just sessionCookieValue -> return (SessionId sessionCookieValue)
    Nothing                 -> getUnusedId

-- | Creates a cookie to hold the current session id.
--   This cookie should be sent to the client together with every HTML page.
sessionCookie :: IO PageParam
sessionCookie = do
  sessionId <- getSessionId
  clockTime <- getClockTime
  dirpath   <- getScriptDirPath
  return $ PageCookie sessionCookieName (getId sessionId)
                      [CookiePath (if null dirpath then "/" else dirpath),
                       CookieExpire (addMinutes sessionLifespan clockTime)]

-- | Gets the directory path of the current CGI script via the
--   environment variable `SCRIPT_NAME`.
--   For instance, if the script is called with URL
--   `http://example.com/cgi/test/script.cgi?parameter`,
--   then `/cgi/test`  is returned.
--   If `SCRIPT_NAME` is not set, the returned string is empty.
getScriptDirPath :: IO String
getScriptDirPath = do
  scriptname <- getEnv "SCRIPT_NAME"
  let scriptpath = if null scriptname then []
                                      else split (=='/') (tail scriptname)
  if null scriptpath
    then return ""
    else return $ "/" ++ intercalate "/" (init scriptpath)

-- | Decorates an HTML page with a session cookie.
withSessionCookie :: HtmlPage -> IO HtmlPage
withSessionCookie p = do
  cookie <- sessionCookie
  return $ (p `addPageParam` cookie)

-- | Decorates an HTML page with a session cookie and shows an information
--   page when the session cookie is not set.
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
-- Implementation of session stores.

-- | The type of a session store which contains data of a particular type.
--   The name of the session store is used to store the session data
--   persistently in files.
data SessionStore _ = SessionStore String

-- | The constructor for a session store containing readable and showable data
--   where a unique store name must be provided. The name is used as a
--   file name in the directory containing all session data.
sessionStore :: (Read a, Show a) => String -> SessionStore a
sessionStore name = SessionStore name

-- | The name of the local directory where the session data,
--   e.g., cookie information, is stored.
--   For security reasons, the directory should be non-public readable.
sessionDataDir :: String
sessionDataDir = "sessiondata"

-- | Prefix a file name with the directory where session data,
--   e.g., cookie information, is stored.
inSessionDataDir :: String -> String
inSessionDataDir filename = sessionDataDir </> filename

-- | Ensures that the `sessionDataDir` directory exists.
--   If it does not exist, it will be created.
ensureSessionDataDir :: IO ()
ensureSessionDataDir = do
  exsdd <- doesDirectoryExist sessionDataDir
  unless exsdd $ createDirectory sessionDataDir

-- | Ensures that the directory for the SessionStore exists.
--   If it does not exist, it will be created.
ensureSessionStoreDir :: SessionStore a -> IO ()
ensureSessionStoreDir (SessionStore sname) = do
  ensureSessionDataDir
  let storedir = inSessionDataDir sname
  fexists <- doesFileExist storedir
  -- remove possible file with same name (due to old session implementation)
  when fexists $ removeFile storedir
  exsdd <- doesDirectoryExist storedir
  unless exsdd $ createDirectory storedir

-- The file where the session data of a user session is stored.
sessionDataFile :: SessionStore a -> SessionId -> String
sessionDataFile (SessionStore sname) sid =
  inSessionDataDir sname </> getId sid

-- Reads the data possibly contained in a session data file.
readSessionDataFile :: Read a => SessionStore a -> SessionId -> IO (Maybe a)
readSessionDataFile sstore sid = exclusiveIO (f ++ ".LOCK") $ do
  fexists <- doesFileExist f
  if fexists
    then do
      cnt <- openFile f ReadMode >>= hGetContents
      case reads cnt of
        [(x,_)] -> return (Just x)
        _       -> do hPutStrLn stderr $
                        "Cannot read session data in file: " ++ f ++
                        "\nFile contents:\n" ++ cnt
                      return Nothing
    else return Nothing
 where
  f = sessionDataFile sstore sid

--- Writes data to a session data file.
writeSessionDataFile :: Show a => SessionStore a -> SessionId -> a -> IO ()
writeSessionDataFile sstore sid v = do
  let sdfile = sessionDataFile sstore sid
  exclusiveIO (sdfile ++ ".LOCK") $ writeFile sdfile (show v ++ "\n")


-- | Retrieves session data associates to the current user session.
--   Returns `Nothing` if there is no data for the current session.
getSessionMaybeData :: (Read a, Show a) =>
                       SessionStore a -> FormReader (Maybe a)
getSessionMaybeData sessionstore = toFormReader $ do
  ensureSessionStoreDir sessionstore
  sid <- getSessionId
  readSessionDataFile sessionstore sid

-- | Retrieves session data associated to the current user session
--   where the second argument is returned if there is no data currently
--   associated to the current session.
getSessionData :: (Read a, Show a) => SessionStore a -> a -> FormReader a
getSessionData sessiondata defaultdata =
  fmap (fromMaybe defaultdata) (getSessionMaybeData sessiondata)

-- | Stores session data associated to the current user session
--   in the persistent session store.
putSessionData :: (Read a, Show a) => SessionStore a -> a -> IO ()
putSessionData sessionstore newdata = do
  ensureSessionStoreDir sessionstore
  sid <- getSessionId
  writeSessionDataFile sessionstore sid newdata
  cleanUpSessionStore sessionstore

-- | Modifies the session data associated to the current user session
--   according to the update functions provided in the last argument.
modifySessionData :: (Read a, Show a) =>
                     SessionStore a -> a -> (a -> a) -> IO ()
modifySessionData sessiondata defaultdata upd = do
  sd <- fromFormReader $ getSessionData sessiondata defaultdata
  putSessionData sessiondata (upd sd)

-- | Removes data associated to the current user session from the
--   given session store.
removeSessionData :: (Read a, Show a) => SessionStore a -> IO ()
removeSessionData sstore = do
  sid <- getSessionId
  let sdfile = sessionDataFile sstore sid
  exfile <- doesFileExist sdfile
  when exfile $ removeFile sdfile

-- | Clean a session store by removing all data files which are older
--   than the 'sessionLifeSpan'.
cleanUpSessionStore :: SessionStore a -> IO ()
cleanUpSessionStore sstore@(SessionStore sname) = do
  ensureSessionStoreDir sstore
  let sdir = inSessionDataDir sname
  sdfiles <- fmap (filter (\fn -> head fn /= '.')) $ getDirectoryContents sdir
  curtime <- getClockTime
  mapM_ (deleteIfOld curtime) (map (sdir </>) sdfiles)
 where
  deleteIfOld curtime fn = do
    ftime <- getModificationTime fn
    when (addMinutes sessionLifespan ftime < curtime) $ removeFile fn

------------------------------------------------------------------------------

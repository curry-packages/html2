------------------------------------------------------------------------------
--- Compute infos about all `HtmlFormDef` operations occurring in a module.
---
--- @author Michael Hanus
--- @version March 2020
------------------------------------------------------------------------------

module ExtractForms ( extractFormsInProg, showQName )
 where

import Directory    ( doesFileExist, getModificationTime )
import FilePath     ( (</>), (<.>) )
import List         ( intercalate, partition )
import System       ( exitWith, getArgs, getPID, system )

import AbstractCurry.Files
import AbstractCurry.Select
import AbstractCurry.Types
import System.CurryPath  ( inCurrySubdir, lookupModuleSourceInLoadPath
                         , stripCurrySuffix )

-- The cache file for storing qualified form names of a module w.r.t.
-- a directory.
formCacheFile :: String -> String -> String
formCacheFile mdir mname = inCurrySubdir (mdir </> mname) <.> "htmlforms"

--- Extract and check all forms defined in a Curry module (third argument).
--- Returns the qualified names of the exported forms.
extractFormsInProg :: Int -> String -> String -> IO [QName]
extractFormsInProg verb curryroot mname =
  lookupModuleSourceInLoadPath mname >>=
  maybe (error $ "Module '" ++ mname ++ "' not found in load path!")
        extractWithFormCache
 where
  extractWithFormCache (mdir,mfile) = do
    let formfile = formCacheFile mdir mname
    ffexists <- doesFileExist formfile
    if not ffexists
      then readFormsInProg verb curryroot mname formfile
      else do
        ctime <- getModificationTime mfile
        ftime <- getModificationTime formfile
        if ctime>ftime
          then readFormsInProg verb curryroot mname formfile
          else do
            when (verb>1) $ putStrLn $ "Reading file '" ++ formfile ++ "'"
            readFile formfile >>= return . read

readFormsInProg :: Int -> String -> String -> String -> IO [QName]
readFormsInProg verb curryroot mname formfile = do
  unless (verb==0) $ putStrLn $
    "Extracting and checking forms contained in module '" ++ mname ++ "'..."
  when (verb>1) $ putStr $ "Reading module '" ++ mname ++ "'..."
  cprog <- readCurry mname
  when (verb>1) $ putStrLn "done!"
  let (formnames,privatenames) = extractFormOps cprog
  unless (null privatenames) $ putStrLn $
    "WARNING: Private form operations found (and not translated):\n" ++
    unwords (map snd privatenames)
  unless (verb==0 || null formnames) $ putStrLn $
    "Form operations found: " ++ unwords (map snd formnames)
  unless (null formnames) $ checkFormIDsInProg verb curryroot mname formnames
  when (verb>1) $ putStrLn $ "Writing form names to '" ++ formfile ++ "'"
  -- store form names in form cache file:
  catch (writeFile formfile (show formnames)) (const done)
  return formnames

--- Extract public and private form definitions from a program.
extractFormOps :: CurryProg -> ([QName], [QName])
extractFormOps prog =
  let (fds1,fds2) = partition (\fd -> funcVis fd == Public)
                              (filter hasFormDefType (functions prog))
  in (map funcName fds1, map funcName fds2)
 where
  hasFormDefType fdecl = case typeOfQualType (funcType fdecl) of
    CTApply (CTCons tc) _ -> tc == ("HTML.Base","HtmlFormDef")
    _                     -> False


-- Test whether all `HtmlFormDef` identifiers in a module are correct,
-- i.e., are identical to the string representation of their defining
-- operations.
checkFormIDsInProg :: Int -> String -> String -> [QName] -> IO ()
checkFormIDsInProg verb curryroot mname formnames = do
  pid <- getPID
  let testprogname = "TESTFORMPROG_" ++ show pid
  when (verb>1) $ putStrLn $
    "Generating check program '" ++ testprogname ++ "':"
  let testprog = unlines
        [ "import " ++ mname
        , "import HTML.Base"
        , "import System ( exitWith )"
        , ""
        , checkFormIDDefinition
        , ""
        , "main :: IO ()"
        , "main = sequence_ [" ++
                    intercalate "," (map genFormCall formnames) ++ "]"
        ]
  writeFile (testprogname ++ ".curry") testprog
  when (verb>2) $ putStrLn testprog
  when (verb>1) $ putStrLn $
    "Executing check program '" ++ testprogname ++ "'..."
  c <- system $ unwords
         [curryroot </> "bin" </> "curry",":set v0", ":load", testprogname,
          ":eval", "main", ":quit"]
  cleanProg testprogname
  unless (c==0) (exitWith c)
 where
  cleanProg modname = do
    system $ unwords [curryroot </> "bin" </> "cleancurry", modname]
    system $ "/bin/rm -f " ++ modname ++ ".curry"

  genFormCall qn =
    let s = showQName qn
    in "checkFormID (" ++ s ++ ",\"" ++ s ++ "\")"

showQName :: QName -> String
showQName (mn,fn) = mn ++ "." ++ fn

checkFormIDDefinition :: String
checkFormIDDefinition = unlines
 ["checkFormID :: (HtmlFormDef a, String) -> IO ()"
 ,"checkFormID (fd, s) = unless (formDefId fd == s)"
 ,"  (putStrLn (\"ERROR: form operation '\" ++ s ++ \"' has non-matching ID!\") >>"
 ,"   exitWith 1)"
 ]

{-
------------------------------------------------------------------------------
--- Auxiliary definitions used by the form checker.
------------------------------------------------------------------------------

import System ( exitWith )
import HTML.Base

checkFormID :: (HtmlFormDef a, String) -> IO ()
checkFormID (fd, s) =
  unless (formDefId fd == s) $ do
    putStrLn $ "ERROR: form operation '" ++ s ++ "' has non-matching ID!"
    exitWith 1

-}

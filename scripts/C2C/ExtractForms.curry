------------------------------------------------------------------------------
--- Compute infos about all `HtmlFormDef` operations occurring in a module
--- and transform FlatCurry programs by setting correct form IDs, if
--- necessary.
---
--- @author Michael Hanus
--- @version August 2020
------------------------------------------------------------------------------

module C2C.ExtractForms ( extractFormsInProg )
 where

import Directory    ( doesFileExist, getModificationTime )
import FilePath     ( (</>), (<.>) )
import List         ( intercalate, partition )
import System       ( exitWith, getArgs, getPID, system )

import AbstractCurry.Files
import AbstractCurry.Select
import AbstractCurry.Types
import System.CurryPath    ( inCurrySubdir, lookupModuleSourceInLoadPath )

import C2C.Options
import C2C.TransFlatCurryForms      ( setFormIDsInFlatCurry )
import C2C.TransTypedFlatCurryForms ( setFormIDsInTypedFlatCurry )

-- The cache file for storing qualified form names of a module w.r.t.
-- a directory.
formCacheFile :: String -> String -> String
formCacheFile mdir mname = inCurrySubdir (mdir </> mname) <.> "htmlforms"

--- Extract and check all forms defined in a Curry module (third argument).
--- Returns the qualified names of the exported forms.
extractFormsInProg :: Options -> String -> IO (Maybe String, [QName])
extractFormsInProg opts mname =
  lookupModuleSourceInLoadPath mname >>=
  maybe (error $ "Module '" ++ mname ++ "' not found in load path!")
        extractWithFormCache
 where
  extractWithFormCache (mdir,mfile) = do
    let formfile = formCacheFile mdir mname
    ffexists <- doesFileExist formfile
    if not ffexists
      then readFormsInProg opts mname formfile
      else do
        ctime <- getModificationTime mfile
        ftime <- getModificationTime formfile
        if ctime > ftime
          then readFormsInProg opts mname formfile
          else do
            putStrLnInter opts $ "Reading file '" ++ formfile ++ "'"
            readFile formfile >>= return . read

readFormsInProg :: Options -> String -> String -> IO (Maybe String, [QName])
readFormsInProg opts mname formfile = do
  putStrLnInfo opts $
    "Extracting and checking forms contained in module '" ++ mname ++ "'..."
  when (optVerb opts > 1) $ putStr $ "Reading module '" ++ mname ++ "'..."
  cprog <- readCurry mname
  putStrLnInter opts "done!"
  let (formnames,privatenames) = extractFormOps cprog
  unless (null privatenames) $ putStrLn $
    "WARNING: Private form operations found (and not translated):\n" ++
    unwords (map snd privatenames)
  unless (null formnames) $ putStrLnIfNQ opts $
    "Form operations found: " ++ unwords (map snd formnames)
  mbtrans <- if null formnames
               then return Nothing
               else checkFormIDsInProg opts mname formnames
  putStrLnInter opts $ "Writing form names to '" ++ formfile ++ "'"
  -- store form names in form cache file:
  catch (writeFile formfile (show (mbtrans, formnames))) (const done)
  return (mbtrans, formnames)

--- Extract public and private form definitions from a program.
extractFormOps :: CurryProg -> ([QName], [QName])
extractFormOps prog =
  let (fds1,fds2) = partition (\fd -> funcVis fd == Public)
                              (filter hasFormDefType (functions prog))
  in (map funcName fds1, map funcName fds2)
 where
  hasFormDefType fdecl = case typeOfQualType (funcType fdecl) of
    CTApply (CTCons tc) _ -> tc == formDefTypeName
    _                     -> False


-- Test whether all `HtmlFormDef` identifiers in a module are correct,
-- i.e., are identical to the string representation of their defining
-- operations. If there are some differences, transform the
-- (Typed) FlatCurry file (depending on the Curry system).
-- The result is `Nothing` when nothing is transformed, otherwise
-- it is `Just` the module name.
checkFormIDsInProg :: Options -> String -> [QName] -> IO (Maybe String)
checkFormIDsInProg opts mname formnames = do
  fidok <- testFormIDsInProg opts mname formnames
  if fidok
    then return Nothing
    else do
      putStrLnIfNQ opts $
        "Some forms have non-matching IDs: setting correct form IDs..."
      case optSysName opts of
        "pakcs" -> setFormIDsInFlatCurry opts mname
        "kics2" -> setFormIDsInTypedFlatCurry opts mname
        o       -> do putStrLn $ "Unknown Curry system '" ++ o ++ "'. " ++
                                 "Cannot set correct form IDs!"
                      exitWith 1
      return (Just mname)

-- Test whether all `HtmlFormDef` identifiers in a module are correct,
-- i.e., are identical to the string representation of their defining
-- operations.
testFormIDsInProg :: Options -> String -> [QName] -> IO Bool
testFormIDsInProg opts mname formnames = do
  pid <- getPID
  let testprogname = "TESTFORMPROG_" ++ show pid
  putStrLnInter opts $ "Generating check program '" ++ testprogname ++ "':"
  let testprog = unlines
        [ "import " ++ mname
        , "import HTML.Base"
        , "import System ( exitWith )"
        , ""
        , checkFormIDDefinition
        , ""
        , "main :: IO ()"
        , "main = do"
        , "  results <- sequence [" ++
                    intercalate "," (map genFormCall formnames) ++ "]"
        , "  unless (and results) (exitWith 1)"
        ]
  writeFile (testprogname ++ ".curry") testprog
  putStrLnDetail opts testprog
  putStrLnInter opts $ "Executing check program '" ++ testprogname ++ "'..."
  c <- system $ unwords
         [optSystem opts </> "bin" </> "curry",":set v0", ":load", testprogname,
          ":eval", "main", ":quit"]
  cleanProg testprogname
  return $ c == 0
 where
  cleanProg modname = do
    system $ unwords [optSystem opts </> "bin" </> "cleancurry", modname]
    system $ "/bin/rm -f " ++ modname ++ ".curry"

  genFormCall qn =
    let s = showQName qn
    in "checkFormID (" ++ s ++ ",\"" ++ s ++ "\")"

checkFormIDDefinition :: String
checkFormIDDefinition = unlines
 ["checkFormID :: (HtmlFormDef a, String) -> IO Bool"
 ,"checkFormID (fd, s) ="
 ,"  if (formDefId fd == s)"
 ,"    then return True"
 ,"    else do"
 ,"      putStrLn (\"Warning: Form definition '\" ++ s ++ \"' has non-matching ID\")"
 ,"      return False"
 ]

{-
------------------------------------------------------------------------------
--- Auxiliary definitions used by the form checker.
------------------------------------------------------------------------------

import System ( exitWith )
import HTML.Base

checkFormID :: (HtmlFormDef a, String) -> IO Bool
checkFormID (fd, s) =
  if (formDefId fd == s)
    then return True
    else do
      putStrLn $ "Warning: Form definition '" ++ s ++ "' has non-matching ID."
      return False

-}

------------------------------------------------------------------------------

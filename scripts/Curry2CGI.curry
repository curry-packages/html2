------------------------------------------------------------------------------
--- Script to compile a Curry program implementing a web script
--- using the package `html2` and the library `HTML.Base`
--- into a cgi script to be placed in a server-accessible directory
--- for executing cgi scripts.
---
--- @author Michael Hanus
--- @version August 2020
------------------------------------------------------------------------------

module Curry2CGI ( main )
 where

import Char         ( isSpace )
import Directory    ( createDirectoryIfMissing, doesFileExist )
import Distribution ( installDir )
import FileGoodies
import FilePath     ( (</>) )
import GetOpt
import IOExts       ( evalCmd )
import List         ( intercalate, isPrefixOf, nub )
import Maybe        ( catMaybes )
import ReadNumeric  ( readNat )
import System
import Time         ( calendarTimeToString, getLocalTime )

import AbstractCurry.Types       ( QName )
import FlatCurry.Files           ( readFlatCurry )
import FlatCurry.Annotated.Files ( readTypedFlatCurry )
import System.CurryPath          ( stripCurrySuffix )

import C2C.Options
import C2C.ExtractForms             ( extractFormsInProg )
import C2C.TransFlatCurryForms      ( copyTransFlatCurry )
import C2C.TransTypedFlatCurryForms ( copyTransTypedFlatCurry )

------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  (opts0,prog) <- processOptions args
  opts <- checkCurrySystem opts0
  modformops <- mapM (extractFormsInProg opts) (optFormMods opts)
  let (mbmods,formops) = unzip modformops
      transmods        = catMaybes mbmods
  compileCGI (opts { optForms = nub (concat formops) }) transmods prog

checkCurrySystem :: Options -> IO Options
checkCurrySystem opts = do
  let currybin = optSystem opts </> "bin" </> "curry"
  isexec <- doesFileExist currybin
  unless isexec $
    error $ "Curry system executable '" ++ currybin ++ "' does not exist!"
  (rc,out,_) <- evalCmd currybin ["--compiler-name"] ""
  unless (rc == 0) $
    error "Cannot determine kind of Curry system (pakcs,kics2)!"
  let sysname = filter (not . isSpace) out
  if sysname `elem` ["pakcs","kics2"]
    then return opts { optSysName = sysname }
    else do putStrLn $ "Unknown Curry system '" ++ sysname ++ "'."
            exitWith 1

-- Generate the main program containing the wrapper for all forms
-- and compile it into a CGI binary.
compileCGI :: Options -> [String] -> String -> IO ()
compileCGI opts transmods mname = do
  putStrLnIfNQ opts $ "Wrapping '" ++ mname ++ "' to generate CGI binary..."
  pid <- getPID
  let mainmod  = mname ++ "_CGIMAIN_" ++ show pid
      maincall = "main_cgi_9999_" ++ show pid
      cgifile  = if null (optOutput opts) then mname ++ ".cgi"
                                          else optOutput opts
      cgidir   = dirName cgifile
  createDirectoryIfMissing True cgidir
  let mainprog = genMainProg opts mname mainmod maincall
  when (optVerb opts > 1) $ putStr $ unlines
    [line, "GENERATED MAIN PROGRAM:", mainprog, line]
  writeFile (mainmod ++ ".curry") mainprog
  unless (null transmods) $ precompile mainmod
  -- compile main module:
  cf <- system $ unwords $
    [ optCPM opts, optSystem opts </> "bin" </> "curry" , "--nocypm" ] ++
    map (\rcopts -> "-D" ++ rcopts) (optCurryRC opts) ++
    [ ":set", 'v' : show (optVerb opts) ] ++
    optCurryOpts opts ++
    [ ":load", mainmod, ":save", maincall, ":quit" ]
  when (cf > 0) $ do
    putStrLn "Error occurred, generation aborted."
    cleanMain mainmod
    exitWith 1
  -- move compiled executable to final position and generate small shell
  -- script to call the executable with ulimit and correct path:
  system $ unwords ["mv", mainmod, cgifile ++ ".bin"]
  system $ unwords ["chmod", "755", cgifile ++ ".bin"]
  genShellScript opts cgifile
  cleanMain mainmod
  cdate <- getLocalTime >>= return . calendarTimeToString
  writeFile (cgifile ++ ".log") (cdate ++ ": cgi script compiled\n")
  putStrLnIfNQ opts $
    "New files \"" ++ cgifile ++ "*\" with compiled cgi script generated."
 where
  precompile mainmod = do
    putStrLnInter opts $ "Modules transformed by setting form IDs:\n" ++
                         unwords transmods
    putStrLnIfNQ opts $ "Pre-compiling " ++ mainmod ++ "..."
    case optSysName opts of
      "pakcs" -> do readFlatCurry mainmod
                    mapM_ (copyTransFlatCurry opts) transmods
      "kics2" -> do readTypedFlatCurry mainmod
                    mapM_ (copyTransTypedFlatCurry opts) transmods

  cleanMain mainmod = do
    system $ unwords [optSystem opts </> "bin" </> "cleancurry", mainmod]
    system $ "/bin/rm -f " ++ mainmod ++ ".curry"

-- Generates the small cgi shell script that actually calls the executable.
genShellScript :: Options -> String -> IO ()
genShellScript opts cgifile = do
  system $ "/bin/rm -f " ++ cgifile
  langenv <- getEnviron "LANG"
  let limit = optLimit opts
  let script = unlines $
                 ["#!/bin/sh"] ++
                 (if null langenv then []
                                  else ["LANG=" ++ langenv, "export LANG"]) ++
                 (if null limit then [] else ["ulimit " ++ limit]) ++
                 ["exec " ++ cgifile ++ ".bin 2>> " ++ cgifile ++ ".log"]
  writeFile cgifile script
  system $ unwords ["chmod", "755", cgifile]
  done

--- Generates the main program which is compiled as the CGI executable.
--- The program defines a main operation of the following form:
---
---     main :: IO ()
---     main = HTML.CGI.Exec.printMainPage
---              [ (<formid1>, HTML.CGI.Exec.execFormDef <formdef1>)
---              , ...
---              , (<formidk>, HTML.CGI.Exec.execFormDef <formdefk>)
---              ]
---              <mainpage>
---
--- where `<formid1>,...<formidk>` are the identifiers of all form definitions
--- to be compiled.
genMainProg :: Options -> String -> String -> String -> String
genMainProg opts mname mainmod maincall = unlines $
  [ "module " ++ mainmod ++ "(" ++ maincall ++ ") where"
  , "import HTML.Base"
  , "import HTML.CGI.Exec" ] ++
  (map ("import " ++) (nub (mname : optFormMods opts))) ++
  [ maincall ++ " :: IO ()"
  , maincall ++ " = HTML.CGI.Exec.printMainPage\n  [" ++
                      intercalate "\n  ," formCalls ++ "]\n" ++
                      "  (" ++ optMain opts ++ ")"
  ]
 where
  formCalls = map (\f -> "(\"" ++ f ++ "\", HTML.CGI.Exec.execFormDef " ++
                         f ++ ")")
                  (map showQName (optForms opts))

------------------------------------------------------------------------------

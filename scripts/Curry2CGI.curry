------------------------------------------------------------------------------
--- Script to compile a Curry program implementing a web script
--- using the package `html2` and the library `HTML.Base`
--- into a cgi script to be placed in a server-accessible directory
--- for executing cgi scripts.
---
--- @author Michael Hanus
--- @version January 2024
------------------------------------------------------------------------------

module Curry2CGI ( main )
 where

import Control.Monad      ( when, unless )
import Data.Char          ( isSpace )
import Data.List          ( intercalate, isPrefixOf, nub )
import Data.Maybe         ( catMaybes )
import Data.Time          ( calendarTimeToString, getLocalTime )
import System.Directory   ( createDirectoryIfMissing, doesFileExist
                          , getAbsolutePath )
import System.Environment ( getArgs, getEnv, setEnv )
import System.FilePath    ( (</>), isRelative, takeDirectory, takeFileName )
import System.IOExts      ( evalCmd )
import System.Process     ( getPID, exitWith, system )

import FlatCurry.Files               ( readFlatCurry )
import FlatCurry.TypeAnnotated.Files ( readTypeAnnotatedFlatCurry )

import C2C.Options
import C2C.ExtractForms              ( extractFormsInProg )
import C2C.TransFlatCurryForms       ( copyTransFlatCurry )
import C2C.TransTypedFlatCurryForms  ( copyTransTypedFlatCurry )

------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  (opts0,prog) <- processOptions args
  opts <- checkCurrySystem opts0
  setCurryPath opts
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
    error "Cannot determine kind of Curry system (pakcs,kics2,curry2go)!"
  let sysname = filter (not . isSpace) out
  case sysname of
    "pakcs"    -> return opts { optTypedFlat = False }
    "kics2"    -> return opts { optTypedFlat = True  }
    "curry2go" -> return opts { optTypedFlat = False }
    _          -> do putStrLn $ "Unknown Curry system '" ++ sysname ++ "'."
                     exitWith 1

-- Sets the environment variable `CURRYPATH` to the value computed by
-- `cypm deps --path` if it was not already set.
-- Hence, `curry2cgi` can be invoked without the explicit use of `cypm exec`.
setCurryPath :: Options -> IO ()
setCurryPath opts = do
  let cpm = optCPM opts
  cp <- getEnv "CURRYPATH"
  when (null cp) $ do
    putStrLnInfo opts $ "Computing CURRYPATH with '" ++ cpm ++ "'..."
    (rc,out,err) <- evalCmd cpm ["deps","--path"] ""
    if rc==0
      then do let cpath = strip out
              putStrLnDetail opts $ "CURRYPATH=" ++ cpath
              setEnv "CURRYPATH" cpath
      else putStrLn $ "ERROR during computing CURRYPATH with 'cypm':\n" ++
                      out ++ err
 where
  -- Remove leading and trailing whitespace
  strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace


-- Generate the main program containing the wrapper for all forms
-- and compile it into a CGI binary.
compileCGI :: Options -> [String] -> String -> IO ()
compileCGI opts transmods mname = do
  putStrLnInfo opts $ "Wrapping '" ++ mname ++ "' to generate CGI binary..."
  pid <- getPID
  let dot2us c = if c == '.' then '_' else c
      mainmod  = map dot2us mname ++ "_CGIMAIN_" ++ show pid
      maincall = "main_cgi_9999_" ++ show pid
      cgifile  = if null (optOutput opts) then mname ++ ".cgi"
                                          else optOutput opts
      cgidir   = takeDirectory cgifile
  createDirectoryIfMissing True cgidir
  let mainprog = genMainProg opts mname mainmod maincall
  when (optVerb opts > 1) $ putStr $ unlines
    [line, "GENERATED MAIN PROGRAM:", mainprog, line]
  writeFile (mainmod ++ ".curry") mainprog
  unless (null transmods) $ precompile mainmod
  -- compile main module:
  let curryverb  = if optVerb opts == 2 then 1 else optVerb opts
      compilecmd = unwords $
        [ optSystem opts </> "bin" </> "curry" , "--nocypm" ] ++
        map (\rcopts -> "-D" ++ rcopts) (optCurryRC opts) ++
        [ ":set", 'v' : show curryverb ] ++
        optCurryOpts opts ++
        [ ":load", mainmod, ":save", maincall, ":quit" ]
  putStrLnInter opts $ "Executing: " ++ compilecmd
  cf <- system compilecmd
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
  putStrLnInfo opts $
    "New files \"" ++ cgifile ++ "*\" with compiled cgi script generated."
 where
  precompile mainmod = do
    putStrLnInter opts $ "Modules transformed by setting form IDs:\n" ++
                         unwords transmods
    putStrLnInfo opts $ "Pre-compiling " ++ mainmod ++ "..."
    if optTypedFlat opts
      then do readTypeAnnotatedFlatCurry mainmod
              mapM_ (copyTransTypedFlatCurry opts) transmods
      else do readFlatCurry mainmod
              mapM_ (copyTransFlatCurry opts) transmods

  cleanMain mainmod = do
    system $ unwords [optSystem opts </> "bin" </> "cleancurry", mainmod]
    system $ "/bin/rm -f " ++ mainmod ++ ".curry"

-- Generates the small cgi shell script that actually calls the executable.
genShellScript :: Options -> String -> IO ()
genShellScript opts cgifile = do
  system $ "/bin/rm -f " ++ cgifile
  langenv <- getEnv "LANG"
  cgibase <- if optAbsolute opts then getAbsolutePath cgifile
                                 else return $ "./" ++ takeFileName cgifile
  let limit = optLimit opts
      script = unlines $
                 ["#!/bin/sh"] ++
                 (if null langenv then []
                                  else ["LANG=" ++ langenv, "export LANG"]) ++
                 (if null limit then [] else ["ulimit " ++ limit]) ++
                 ["exec " ++ cgibase ++ ".bin 2>> " ++ cgibase ++ ".log"]
  writeFile cgifile script
  system $ unwords ["chmod", "755", cgifile]
  return ()

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

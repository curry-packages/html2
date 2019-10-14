------------------------------------------------------------------------------
--- Script to compile a Curry program implementing a web script
--- using the package `html2` and the library `HTML.Base`
--- into a cgi script to be placed in a server-accessible directory
--- for executing cgi scripts.
---
--- @author Michael Hanus
--- @version October 2019
------------------------------------------------------------------------------

module Curry2CGI
 where

import Directory    ( createDirectoryIfMissing, doesFileExist )
import Distribution ( installDir )
import FileGoodies
import FilePath     ( (</>) )
import GetOpt
import List         ( intercalate, isPrefixOf, nub )
import ReadNumeric  ( readNat )
import System
import Time         ( calendarTimeToString, getLocalTime )

import AbstractCurry.Types ( QName )
import System.CurryPath    ( stripCurrySuffix )

import ExtractForms        ( extractFormsInProg, showQName )

main :: IO ()
main = do
  args <- getArgs
  (opts,prog) <- processOptions args
  checkCurrySystem (optSystem opts)
  formops <- mapM (extractFormsInProg (optVerb opts) (optSystem opts))
                  (optFormMods opts)
  compileCGI (opts { optForms = nub (concat formops) }) prog

checkCurrySystem :: String -> IO ()
checkCurrySystem currydir = do
  let currybin = currydir </> "bin" </> "curry"
  isexec <- doesFileExist currybin
  unless isexec $
    error $ "Curry system executable '" ++ currybin ++ "' does not exist!"

compileCGI :: Options -> String -> IO ()
compileCGI opts mname = do
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
-- Option processing for the script.

data Options = Options
  { optVerb      :: Int   -- verbosity (0: quiet, 1: status, 2: interm, 3: all)
  , optHelp      :: Bool     -- if help info should be printed
  , optOutput    :: String   -- name of the cgi program file (with suffix .cgi)
  , optMain      :: String   -- the main expression
  , optForms     :: [QName]  -- qualified names of form operations
  , optFormMods  :: [String] -- names of modules containing form operations
  , optSystem    :: String   -- path to root of Curry system
  , optCPM       :: String   -- command to invoke Curry Package Manager
  , optCurryRC   :: [String] -- curryrc options
  , optCurryOpts :: [String] -- options passed to the Curry compiler
  , optLimit     :: String   -- ulimit settings for the cgi program
  }

defaultOptions :: Options
defaultOptions =
  Options 1 False "" "" [] [] installDir "cypm exec"
          [] [":set -time", ":set -interactive"]
          "-t 120"

--- Process the actual command line argument and return the options
--- and the name of the main program.
processOptions :: [String] -> IO (Options,String)
processOptions argv = do
  let (funopts, args, opterrors) = getOpt Permute options argv
      opts = foldl (flip id) defaultOptions funopts
  unless (null opterrors)
         (putStr (unlines opterrors) >> printUsage >> exitWith 1)
  when (optHelp opts) (printUsage >> exitWith 0)
  case args of
    [p] -> let mname = stripCurrySuffix p
               opts1 = opts { optFormMods = nub (optFormMods opts ++ [mname])
                            , optMain = if null (optMain opts)
                                          then mname ++ ".main"
                                          else optMain opts }
           in return (opts1, mname)
    []  -> error $ "Name of main module missing!"
    _   -> error $ "Please provide only one main module!"
 where
  printUsage = putStrLn usageText

-- Usage text
usageText :: String
usageText =
  usageInfo ("Usage: curry2cgi [options] <module name>\n") options

-- Definition of actual command line options.
options :: [OptDescr (Options -> Options)]
options =
  [ Option "h?" ["help"]
           (NoArg (\opts -> opts { optHelp = True }))
           "print help and exit"
  , Option "v" ["verb"]
            (OptArg (maybe (checkVerb 2) (safeReadNat checkVerb)) "<n>")
            "verbosity level:\n0: quiet (same as `-q')\n1: show status messages (default)\n2: show intermediate results (same as `-v')\n3: show all details"
  , Option "m" ["main"]
            (ReqArg (\s opts -> opts { optMain = s }) "<m>")
            ("Curry expression (of type IO HtmlPage) computing\n" ++
             "the HTML page\n(default: main)")
  , Option "o" ["output"]
            (ReqArg (\s opts -> opts { optOutput = s }) "<o>")
            ("name of the file (with suffix .cgi) where the cgi\n" ++
             "program should be stored (default: <curry>.cgi)")
  , Option "i" ["include"]
            (ReqArg (\s opts -> opts { optFormMods = optFormMods opts ++ [s] })
                    "<i>")
            ("Additional Curry module for which all public\n" ++
             "form handlers should be generated")
  , Option "s" ["system"]
            (ReqArg (\s opts -> opts { optSystem = s }) "<s>")
            ("set path to the root of Curry system\n" ++
             "(then 'path/bin/curry' is invoked to compile script)")
  , Option "" ["cpmexec"]
            (ReqArg (\s opts -> opts { optCPM = s }) "<c>")
            ("set the command to execute programs with the\n" ++
             "Curry Package Manager (default: 'cypm exec')")
  , Option "D" []
            (ReqArg (\s opts -> opts { optCurryRC = optCurryRC opts ++ [s] })
                    "name=val")
            "define (curry)rc property 'name' as 'val'"
  , Option "u" ["ulimit"]
            (ReqArg (\s opts -> opts { optLimit = s }) "<l>")
            ("set 'ulimit <l>' when executing the cgi program\n" ++
             "(default: '-t 120')")
  ]
 where
  safeReadNat opttrans s opts =
   let numError = error "Illegal number argument (try `-h' for help)"
   in maybe numError
            (\ (n,rs) -> if null rs then opttrans n opts else numError)
            (readNat s)

  checkVerb n opts = if n>=0 && n<4
                     then opts { optVerb = n }
                     else error "Illegal verbosity level (try `-h' for help)"

putStrLnIfNQ :: Options -> String -> IO ()
putStrLnIfNQ opts s = unless (optVerb opts == 0) $ putStrLn s

line :: String
line = take 78 (repeat '-')

-------------------------------------------------------------------------

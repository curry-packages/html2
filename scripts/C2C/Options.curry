------------------------------------------------------------------------------
--- Option processing for the `curry2cgi` script.
---
--- @author Michael Hanus
--- @version March 2023
------------------------------------------------------------------------------

module C2C.Options
 where

import Control.Monad               ( when, unless )
import Curry.Compiler.Distribution ( installDir )
import Data.List                   ( nub )
import Numeric                     ( readNat )
import System.Process              ( exitWith, system )
import System.Console.GetOpt

import AbstractCurry.Types ( QName )
import System.CurryPath    ( stripCurrySuffix )

------------------------------------------------------------------------

--- The script banner.
banner :: String
banner = unlines [bannerLine,bannerText,bannerLine]
 where
  bannerText = "Compile Curry programs with HTML forms to CGI executables " ++
               "(Version of 18/03/23)"
  bannerLine = take (length bannerText) (repeat '=')

------------------------------------------------------------------------------
-- Option processing for the script.

data Options = Options
  { optVerb      :: Int   -- verbosity (0: quiet, 1: status, 2: interm, 3: all)
  , optHelp      :: Bool     -- if help info should be printed
  , optOutput    :: String   -- name of the cgi program file (with suffix .cgi)
  , optAbsolute  :: Bool     -- generate cgi script with absolute file names?
  , optMain      :: String   -- the main expression
  , optForms     :: [QName]  -- qualified names of form operations
  , optFormMods  :: [String] -- names of modules containing form operations
  , optSystem    :: String   -- path to root of Curry system
  , optTypedFlat :: Bool     -- does the Curry compiler require TypedFlatCurry?
  , optCPM       :: String   -- command to invoke Curry Package Manager
  , optCurryRC   :: [String] -- curryrc options
  , optCurryOpts :: [String] -- options passed to the Curry compiler
  , optLimit     :: String   -- ulimit settings for the cgi program
  }

defaultOptions :: Options
defaultOptions =
  Options 1 False "" False "" [] [] installDir False "cypm exec"
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
  printUsage = putStrLn (banner ++ "\n" ++ usageText)

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
  , Option "q" ["quiet"]
           (NoArg (\opts -> opts { optVerb = 0 }))
           "run quietly (no output, only exit code)"
  , Option "v" ["verb"]
            (OptArg (maybe (checkVerb 2) (safeReadNat checkVerb)) "<n>")
            ("verbosity level:\n" ++
             "0: quiet (same as `-q')\n" ++
             "1: show status messages (default)\n" ++
             "2: show intermediate results (same as `-v')\n" ++
             "3: show all details")
  , Option "m" ["main"]
            (ReqArg (\s opts -> opts { optMain = s }) "<m>")
            ("Curry expression (of type IO HtmlPage) computing\n" ++
             "the HTML page\n(default: main)")
  , Option "o" ["output"]
            (ReqArg (\s opts -> opts { optOutput = s }) "<o>")
            ("name of the file (with suffix .cgi) where the cgi\n" ++
             "program should be stored (default: <curry>.cgi)")
  , Option "a" ["absolute"]
           (NoArg (\opts -> opts { optAbsolute = True }))
           "generate script with absolute file names"
  , Option "i" ["include"]
            (ReqArg (\s opts -> opts { optFormMods = optFormMods opts ++ [s] })
                    "<i>")
            ("Additional Curry module for which all public\n" ++
             "form handlers should be generated")
  , Option "s" ["system"]
            (ReqArg (\s opts -> opts { optSystem = s }) "<s>")
            ("set path to the root of Curry system so that\n" ++
             "'<s>/bin/curry' is invoked to compile script and\n" ++
             "'<s>/bin/cleancurry' is invoked to clean itermediate files\n" ++
             "(default: '" ++ installDir ++ "')")
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
  safeReadNat opttrans s opts = case readNat s of
   [(n,"")] -> opttrans n opts
   _        -> error "Illegal number argument (try `-h' for help)"

  checkVerb n opts = if n>=0 && n<4
                     then opts { optVerb = n }
                     else error "Illegal verbosity level (try `-h' for help)"

putStrLnInfo :: Options -> String -> IO ()
putStrLnInfo opts s = when (optVerb opts > 0) $ putStrLn s

putStrLnInter :: Options -> String -> IO ()
putStrLnInter opts s = when (optVerb opts > 1) $ putStrLn s

putStrLnDetail :: Options -> String -> IO ()
putStrLnDetail opts s = when (optVerb opts > 2) $ putStrLn s

line :: String
line = take 78 (repeat '-')

------------------------------------------------------------------------------
-- Some auxiliaries.

--- The name of the form definition type.
formDefTypeName :: (String,String)
formDefTypeName = ("HTML.Base","HtmlFormDef")

--- Shows a qualified name.
showQName :: (String,String) -> String
showQName (mn,fn) = mn ++ "." ++ fn

--- Executes a command and show the command if verbosity is detailed.
execVerbCommand :: Options -> String -> IO Int
execVerbCommand opts cmd = do
  when (optVerb opts > 2) $ putStrLn $ "EXECUTING: " ++ cmd
  system cmd

-------------------------------------------------------------------------

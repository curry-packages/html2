------------------------------------------------------------------------------
--- Compute infos about all `HtmlFormDef` operations occurring in a module.
---
--- @author Michael Hanus
--- @version September 2019
------------------------------------------------------------------------------

module ExtractForms ( extractFormsInProg )
 where

import FilePath     ( (</>) )
import List         ( intercalate )
import System       ( exitWith, getArgs, getPID, system )

import FlatCurry.Files
import FlatCurry.Goodies
import FlatCurry.Types
import HTML.Base
import System.CurryPath  ( stripCurrySuffix )

extractFormsInProg :: String -> String -> IO [QName]
extractFormsInProg curryroot mname = do
  putStrLn $ "Extracting and checking forms contained in module '" ++
             mname ++ "'..."
  intcurry <- readFlatCurryInt mname
  let formnames = extractFormOps intcurry
  putStrLn $ "Form operations found: " ++ unwords (map snd formnames)
  checkFormIDsInProg curryroot mname formnames
  return formnames

extractFormOps :: Prog -> [QName]
extractFormOps prog = map funcName (filter isPublicFormDef (progFuncs prog))
 where
  isPublicFormDef fdecl =
    funcVisibility fdecl == Public &&
    isFormDefType (funcType fdecl)

  isFormDefType t = case t of
    TCons tc _ -> tc == ("HTML.Base","HtmlFormDef")
    _          -> False


-- Test whether all `HtmlFormDef` identifiers in a module are correct,
-- i.e., are identical to the string representation of their defining
-- operations.
checkFormIDsInProg :: String -> String -> [QName] -> IO ()
checkFormIDsInProg curryroot mname formnames = do
  pid <- getPID
  let testprogname = "TESTFORMPROG_" ++ show pid
  writeFile (testprogname ++ ".curry") $ unlines
    [ "import " ++ mname
    , "import HTML.Base"
    , "import CheckFormIDs"
    , "main :: IO ()"
    , "main = sequence_ [" ++
                intercalate "," (map genFormCall formnames) ++ "]"
    ]
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

------------------------------------------------------------------------------
--- Transforms a FlatCurry file by setting correct IDs in all form definitions.
---
--- @author Michael Hanus
--- @version November 2020
------------------------------------------------------------------------------

module C2C.TransFlatCurryForms ( setFormIDsInFlatCurry, copyTransFlatCurry )
 where

import Control.Monad     ( when, unless )
import System.FilePath   ( (</>) )
import System.Process    ( exitWith, system )

import FlatCurry.Files
import FlatCurry.Types hiding ( showQName )
import System.CurryPath  ( lookupModuleSourceInLoadPath )

import C2C.Options

------------------------------------------------------------------------------
--- Transforms a FlatCurry file by setting IDs in all form definitions.
setFormIDsInFlatCurry :: Options -> String -> IO ()
setFormIDsInFlatCurry opts mname = do
  lookupModuleSourceInLoadPath mname >>=
    maybe (error $ "Module '" ++ mname ++ "' not found in load path!")
          attachFormIDsInProg
 where
  attachFormIDsInProg (mdir,_) = do
    when (optVerb opts > 1) $
      putStr $ "Reading FlatCurry of module '" ++ mname ++ "'..."
    (Prog name imps types funcs ops) <- readFlatCurry mname
    putStrLnInter opts "done!"
    let newflatname = flatCurryFileName (mdir </> mname) ++ transSuffix
        tprog       = Prog name imps types (map transFunc funcs) ops
    putStrLnInter opts $ "Writing transformed FlatCurry file..."
    writeFlatCurryFile newflatname tprog
    copyFlatCurryInDir opts mdir mname

  transFunc fd@(Func fn ar vis te rl) =
    if isFormDefType te
      then Func fn ar vis te (addID rl)
      else fd
   where
    addID (External _) = error "Externally defined HTML form!"
    addID (Rule vs exp) =
      Rule vs (Comb FuncCall ("HTML.Base","setFormDefId")
                    [string2FC (showQName fn), exp])

  isFormDefType texp = case texp of
    TCons tc [_] -> tc == formDefTypeName
    _            -> False


string2FC :: String -> Expr
string2FC []     = Comb ConsCall ("Prelude","[]") []
string2FC (c:cs) =
  Comb ConsCall ("Prelude",":") [Lit (Charc c), string2FC cs]

--- Copies transformed FlatCurry files.
copyTransFlatCurry :: Options -> String -> IO ()
copyTransFlatCurry opts mname = do
  lookupModuleSourceInLoadPath mname >>=
    maybe (error $ "Module '" ++ mname ++ "' not found in load path!")
          (\ (mdir,_) -> copyFlatCurryInDir opts mdir mname)

copyFlatCurryInDir :: Options -> String -> String -> IO ()
copyFlatCurryInDir opts mdir mname = do
  let flatname    = flatCurryFileName (mdir </> mname)
      newflatname = flatCurryFileName (mdir </> mname) ++ transSuffix
  putStrLnInter opts $ "Replacing original FlatCurry file..."
  rc <- execVerbCommand opts $
          "/bin/cp \"" ++ newflatname ++ "\" \"" ++ flatname ++ "\""
  unless (rc == 0) $ exitWith 1

transSuffix :: String
transSuffix = ":SETFORMIDS"

------------------------------------------------------------------------------

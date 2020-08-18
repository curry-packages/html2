------------------------------------------------------------------------------
--- Transforms a Typed FlatCurry file by setting correct IDs
--- in all form definitions.
---
--- @author Michael Hanus
--- @version August 2020
------------------------------------------------------------------------------

module C2C.TransTypedFlatCurryForms
  ( setFormIDsInTypedFlatCurry, copyTransTypedFlatCurry )
 where

import FilePath     ( (</>), (<.>) )
import System       ( exitWith, system )

import FlatCurry.Annotated.Files
import FlatCurry.Annotated.Types
import System.CurryPath  ( lookupModuleSourceInLoadPath )

import C2C.Options

------------------------------------------------------------------------------
--- Transforms a FlatCurry file by setting IDs in all form definitions.
setFormIDsInTypedFlatCurry :: Options -> String -> IO ()
setFormIDsInTypedFlatCurry opts mname = do
  lookupModuleSourceInLoadPath mname >>=
    maybe (error $ "Module '" ++ mname ++ "' not found in load path!")
          attachFormIDsInProg
 where
  attachFormIDsInProg (mdir,_) = do
    when (optVerb opts > 1) $ putStr $
      "Reading TypedFlatCurry of module '" ++ mname ++ "'..."
    (AProg name imps types funcs ops) <- readTypedFlatCurry mname
    putStrLnInter opts "done!"
    let newflatname = typedFlatCurryFileName (mdir </> mname) ++ transSuffix
        tprog       = AProg name imps types (map transFunc funcs) ops
    putStrLnInter opts $ "Writing transformed FlatCurry file..."
    writeTypedFlatCurryFile newflatname tprog
    copyFlatCurryInDir opts mdir mname

  transFunc fd@(AFunc fn ar vis te rl) =
    if isFormDefType te
      then AFunc fn ar vis te (addID rl)
      else fd
   where
    addID (AExternal _ _) = error "Externally defined HTML form!"
    addID (ARule rt vs exp) =
      let fdtype = TCons formDefTypeName [formDefTypeArg te]
      in ARule rt vs (AComb fdtype FuncCall
                            (("HTML.Base","setFormDefId"),
                             FuncType stringType (FuncType fdtype fdtype))
                            [string2FC (showQName fn), exp])

    formDefTypeArg (TCons _ [ta]) = ta

  isFormDefType texp = case texp of
    TCons tc [_] -> tc == formDefTypeName
    _            -> False

string2FC :: String -> AExpr TypeExpr
string2FC []     = AComb stringType ConsCall (("Prelude","[]"), stringType) []
string2FC (c:cs) =
  AComb stringType ConsCall 
        (("Prelude",":"), FuncType charType (FuncType stringType stringType))
        [ALit charType (Charc c), string2FC cs]

charType :: TypeExpr
charType = TCons ("Prelude","Char") []

stringType :: TypeExpr
stringType = TCons ("Prelude","[]") [charType]

--- Copies transformed TypedFlatCurry files.
copyTransTypedFlatCurry :: Options -> String -> IO ()
copyTransTypedFlatCurry opts mname = do
  lookupModuleSourceInLoadPath mname >>=
    maybe (error $ "Module '" ++ mname ++ "' not found in load path!")
          (\ (mdir,_) -> copyFlatCurryInDir opts mdir mname)

copyFlatCurryInDir :: Options -> String -> String -> IO ()
copyFlatCurryInDir opts mdir mname = do
  let flatname    = typedFlatCurryFileName (mdir </> mname)
      newflatname = typedFlatCurryFileName (mdir </> mname) ++ transSuffix
  putStrLnInter opts $ "Replacing original TypedFlatCurry file..."
  cprc <- execVerbCommand opts $
            "/bin/cp \"" ++ newflatname ++ "\" \"" ++ flatname ++ "\""
  -- delete existing Haskell target files for this module:
  rmrc <- execVerbCommand opts $ "/bin/rm -f " ++ kics2DestFile mdir mname
  unless (cprc + rmrc == 0) $ exitWith 1

-- Computes the Haskell target file pattern for a module in some directory.
kics2DestFile :: String -> String -> String
kics2DestFile mdir mname =
  mdir </> ".curry" </> "kics2" </>
  foldr1 (</>) (addCurryInLast (splitModuleIdentifiers mname)) <.> "*"
 where
  splitModuleIdentifiers s = let (pref, rest) = break (== '.') s in
    pref : case rest of []     -> []
                        _ : s' -> splitModuleIdentifiers s'

  addCurryInLast []           = error "addCurryInLast: empty list"
  addCurryInLast [x]          = ["Curry_" ++ x]
  addCurryInLast (x:xs@(_:_)) = x : addCurryInLast xs


transSuffix :: String
transSuffix = ":SETFORMIDS"

------------------------------------------------------------------------------

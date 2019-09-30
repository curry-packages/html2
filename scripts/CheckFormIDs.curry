------------------------------------------------------------------------------
--- Auxiliary definitions used by the form checker.
---
--- @author Michael Hanus
--- @version September 2019
------------------------------------------------------------------------------

import System ( exitWith )
import HTML.Base

checkFormID :: (HtmlFormDef a, String) -> IO ()
checkFormID (fd, s) =
  unless (formDefId fd == s) $ do
    putStrLn $ "ERROR: form operation '" ++ s ++ "' has non-matching ID!"
    exitWith 1

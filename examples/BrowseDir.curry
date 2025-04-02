------------------------------------------------------------------------------
-- Example for HTML programming in Curry:
--
-- A form to browse the structure of a directory.
-- The form is parameterized by the (URL-encoded) name of the directory.
-- Subdirectories are presented as links to browse them.
--
-- @author Michael Hanus
-- @version April 2025
------------------------------------------------------------------------------

import System.Directory -- from package `directory`

import HTML.Base
import Network.URL ( string2urlencoded, urlencoded2string )

main :: IO HtmlPage
main = do
  param <- getUrlParameter
  let dir = if null param then "." else urlencoded2string param
  entries <- getDirectoryContents dir
  hexps <- mapM (entry2html dir) entries
  return $ page "Browse Directory"
                [h1 [htxt $ "Directory: " ++ dir], ulist hexps]

-- Transform directory and entry in this directory into a link
-- (if it is a directory) or a text:
entry2html :: String -> String -> IO [BaseHtml]
entry2html dir e = do
  direx <- doesDirectoryExist (dir ++ "/" ++ e)
  if direx
    then return [href ("?" ++ string2urlencoded (dir ++ "/" ++ e))
                      [htxt e]]
    else return [htxt e]

-- Install with:
-- > curry2cgi -o ~/public_html/cgi-bin/browsedir.cgi BrowseDir
--
-- Call with: http://...CGIBINDIR/browsedir.cgi?<directory (urlencoded)>

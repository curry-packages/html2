------------------------------------------------------------------------------
--- This module contains some operations to support the execution
--- of CGI scripts defined with the library `HTML.Base`.
--- These operations are used by the script `curry2cgi`
--- to compile Curry CGI scripts into executables.
---
--- @author Michael Hanus
--- @version September 2019
------------------------------------------------------------------------------

module HTML.CGI.Exec ( showFormPageAction, showFormAction )
 where

import IO          ( hPutStrLn, stderr )
import List        ( intercalate )
import ReadNumeric ( readHex, readNat )
import System      ( getEnviron )
import Time        ( calendarTimeToString, getLocalTime )

import HTML.Base

------------------------------------------------------------------------------

--- Shows the HTML page generated from the parameter action
--- as a string on stdout.
--- The forms possibly contained in the HTML page are passed as parameters,
--- where the elements are usually `(formid, showFormAction formdef)`.
--- This operation is used by the script to compile web scripts
--- into executables.
showFormPageAction :: [(String, [(String,String)] -> IO ())] -> IO HtmlPage
                   -> IO ()
showFormPageAction formmap genpage = catchFormErrors $ do
  cgivars <- getFormVariables
  maybe (genpage >>= showPage)
        (\formid -> maybe (showPage (formNotCompiledPage formid))
                          (\f -> f cgivars)
                          (lookup formid formmap))
        (lookup "FORMID" cgivars)

--- Processes a submitted HTML form by reading the data and
--- construct the initial form to find the corresponding event handler.
--- The list of CGI variables/values if passed as the second argument.
showFormAction :: HtmlFormDef a -> [(String,String)] -> IO ()
showFormAction (HtmlFormDef _ readact formgen) cgivars = catchFormErrors $ do
  val <- readact
  let (iform,_) = instCgiRefs (formgen val) 0
  let cenv = cgiGetValue cgivars
  p <- maybe (return noHandlerPage) (\h -> h cenv) (findHandler cenv iform)
  showPage p
  --let fenv = unlines $ map (\ (x,y) -> x ++ "=" ++ y) cgivars
  --putStrLn ("Content-type: text/plain\n\n" ++ fenv)

--- Catches run-time errors, print them on stderr and also
--- in a specific webpages.
catchFormErrors :: IO () -> IO ()
catchFormErrors formact = catch formact showFormError
 where
  showFormError err = do
    let errstr = showError err
    cdate <- getLocalTime >>= return . calendarTimeToString
    hPutStrLn stderr $ cdate ++ ": " ++ errstr
    showPage $ page "Run-time exception"
      [h1 [htxt "Run-time exception occurred"],
       par [htxt "An error occurred during the execution of the web script."],
       par [htxt $ "Error message: " ++ errstr]]

--- Shows a HTML page on stdout.
showPage :: HtmlPage -> IO ()
showPage p = do
  let (cookiestring,hpage) = extractCookies p
  putStrLn $ cookiestring ++
             "Content-type: text/html\n\n" ++ showHtmlPage hpage

-- Extract the cookies contained in a HTML page and return the
-- "set cookie" string and the HTML page without the cookies:
extractCookies :: HtmlPage -> (String,HtmlPage)
extractCookies (HtmlPage title params hexp) =
  let cookiestring = if null cookies
                     then ""
                     else "Cache-control: no-cache=\"set-cookie\"\n" ++
                          concatMap ((++"\n") . formatCookie) cookies
   in (cookiestring, HtmlPage title otherparams hexp)
 where
   (cookies,otherparams) = splitFormParams params

   splitFormParams []           = ([],[])
   splitFormParams (fparam:fps) =
     let (cs,ops) = splitFormParams fps
      in case fparam of
           PageCookie n v ps -> ((n,v,ps) : cs, ops)
           _                 -> (cs, fparam:ops)


-- Generates HTML page to show in illegal invocation of a form.
noHandlerPage :: HtmlPage
noHandlerPage = page "Illegal Form Submission"
  [h1 [htxt "Error: illegal form submission"],
   par [htxt $ "Your request cannot be processed since the form is not " ++
               "invoked by a submit request!"]]

-- Generates HTML page to show in illegal invocation of a form.
formNotCompiledPage :: String -> HtmlPage
formNotCompiledPage formid = page "Form Not Compiled"
  [h1 [htxt $ "Error: Form \"" ++ formid ++ "\" not compiled!"],
   par [htxt $ "The form with the identifier above was not compiled! " ++
               "Please re-compile the web application with all forms!"]]

-- Transforms a CGI variable mapping into a CGI environment.
cgiGetValue :: [(String,String)] -> CgiEnv
cgiGetValue cgivars cgiref =
  intercalate "\n" (map snd (filter (((idOfCgiRef cgiref) ==) . fst) cgivars))

-- Find the handler corresponding to the variables in the CGI environment.
findHandler :: CgiEnv -> [HtmlExp] -> Maybe HtmlPageHandler
findHandler _ [] = Nothing
findHandler cenv (HtmlText _ : hexps) = findHandler cenv hexps
findHandler cenv (HtmlStruct _ _ hexps1 : hexps2) =
  findHandler cenv (hexps1 ++ hexps2)
findHandler cenv (HtmlEvent _ _ : hexps) = findHandler cenv hexps
findHandler cenv (HtmlRefEvent _ cgiref handler : hexps) =
  if null (cenv cgiref)
    then findHandler cenv hexps
    else Just handler
findHandler cenv (HtmlCRef hexp _ : hexps) = findHandler cenv (hexp : hexps)

------------------------------------------------------------------------------
--- Gets the list of variable/value pairs sent from the browser for the
--- current CGI script.
--- Used for the implementation of HTML event handlers.
getFormVariables :: IO [(String,String)]
getFormVariables = do
  clen <- getEnviron "CONTENT_LENGTH"
  cont <- getNChar (maybe 0 fst (readNat clen))
  return (includeCoordinates (parseCgiEnv cont))

-- translate a string of cgi environment bindings into list of binding pairs:
parseCgiEnv :: String -> [(String,String)]
parseCgiEnv s | s == ""   = []
              | otherwise = map ufield2field
                             (map (\(n,v)->(n,utf2latin (urlencoded2string v)))
                                  (map (splitChar '=') (split (=='&') s)))
 where
   ufield2field (n,v) = if take 7 n == "UFIELD_"
                        then (tail n, utf2latin (urlencoded2string v))
                        else (n,v)

   -- split a string at particular character:
   splitChar c xs = let (ys,zs) = break (==c) xs
                    in if zs==[] then (ys,zs) else (ys,tail zs)

   -- split a string at all positions of a particular character:
   split p xs =
    let (ys,zs) = break p xs
    in if zs==[] then [ys]
                 else ys : split p (tail zs)

--- Translates urlencoded string into equivalent ASCII string.
urlencoded2string :: String -> String
urlencoded2string [] = []
urlencoded2string (c:cs)
  | c == '+'  = ' ' : urlencoded2string cs
  | c == '%'  = chr (maybe 0 fst (readHex (take 2 cs)))
                 : urlencoded2string (drop 2 cs)
  | otherwise = c : urlencoded2string cs

--- Transforms a string with UTF-8 umlauts into a string with latin1 umlauts.
utf2latin :: String -> String
utf2latin [] = []
utf2latin [c] = [c]
utf2latin (c1:c2:cs)
 | ord c1 == 195 = chr (ord c2 + 64) : utf2latin cs
 | otherwise     = c1 : utf2latin (c2:cs)

includeCoordinates :: [(String,String)] -> [(String,String)]
includeCoordinates [] = []
includeCoordinates ((tag,val):cenv) 
  = case break (=='.') tag of
      (_,[]) -> (tag,val):includeCoordinates cenv
      (event,['.','x']) -> ("x",val):(event,val):includeCoordinates cenv
      (_,['.','y']) -> ("y",val):includeCoordinates cenv
      _ -> error "includeCoordinates: unexpected . in url parameter"
   

-- get n chars from stdin:
getNChar :: Int -> IO String
getNChar n = if n<=0 then return ""
                     else do c <- getChar
                             cs <- getNChar (n-1)
                             return (c:cs)

------------------------------------------------------------------------------

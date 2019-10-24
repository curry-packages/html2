------------------------------------------------------------------------------
--- This module contains some operations to support the execution
--- of CGI scripts defined with the library `HTML.Base`.
--- These operations are used by the script `curry2cgi`
--- to compile Curry CGI scripts into executables.
---
--- @author Michael Hanus
--- @version October 2019
------------------------------------------------------------------------------

module HTML.CGI.Exec ( printMainPage, execFormDef )
 where

import IO          ( hPutStrLn, stderr )
import List        ( intercalate, split )
import ReadNumeric ( readHex, readNat )
import System      ( getEnviron )
import Time        ( calendarTimeToString, getLocalTime )

import HTML.Base

------------------------------------------------------------------------------

--- Shows the HTML page generated from the second parameter
--- as a string on stdout.
--- The forms possibly contained in the HTML page are passed as parameters,
--- where the elements are usually of the form
--- `(formid, execFormDef formdef)`.
--- This operation is used by the script `curry2gi` to compile web scripts
--- into executables.
printMainPage :: [(String, [(String,String)] -> IO ())] -> IO HtmlPage -> IO ()
printMainPage formmap genpage = catchFormErrors $ do
  cgivars <- getFormVariables
  maybe (genpage >>= execPage >>= printPage)
        (\formid -> maybe (printPage (formNotCompiledPage formid))
                          (\f -> f cgivars)
                          (lookup formid formmap))
        (lookup "FORMID" cgivars)

--- Translates a form definition into an operation which takes
--- CGI variables and their values and produces the HTML text on
--- standard output.
--- The generated operation processes reads the required data (by executing
--- the read action of the form definition) and
--- constructs the initial form to find the corresponding event handler
--- contained in this form.
--- The list of CGI variables/values is passed as the second argument.
execFormDef :: HtmlFormDef a -> [(String,String)] -> IO ()
execFormDef (HtmlFormDef _ readact formgen) cgivars = catchFormErrors $ do
  val <- readact
  hexps <- mapM execHtml (formgen val)
  let (iform,_) = instCgiRefs hexps 0
  let cenv = cgiGetValue cgivars
  p <- maybe (return noHandlerPage) (\h -> h cenv) (findHandler cenv iform)
  execPage p >>= printPage
  -- for debugging:
  --let fenv = unlines $ map (\ (x,y) -> x ++ "=" ++ y) cgivars
  --putStrLn ("Content-type: text/plain\n\n" ++ fenv)

--- Catches run-time errors, print them on stderr and also
--- in a specific web page.
catchFormErrors :: IO () -> IO ()
catchFormErrors formact = catch formact showFormError
 where
  showFormError err = do
    let errstr = showError err
    cdate <- getLocalTime >>= return . calendarTimeToString
    hPutStrLn stderr $ cdate ++ ": " ++ errstr
    printPage $ page "Run-time exception"
      [h1 [htxt "Run-time exception occurred"],
       par [htxt "An error occurred during the execution of the web script."],
       par [htxt $ "Error message: " ++ errstr]]

--- Shows a HTML page on stdout.
printPage :: HtmlPage -> IO ()
printPage (HtmlAnswer ctype cont) = do
  putStr $ "Content-Length: " ++ show (length cont) ++
           "\nContent-Type: " ++ ctype ++ "\n\n" ++ cont
printPage p@(HtmlPage _ _ _) = do
  let (headerstring,hpage) = extractHeader p
  putStrLn $ headerstring ++
             "Content-type: text/html\n\n" ++ showHtmlPage hpage

-- Extract the headers contained in a form as well as the cookies
-- and return a string for the HTTP header of the page
-- if any cookie is set, also return the cache-control header
extractHeader :: HtmlPage -> (String, HtmlPage)
extractHeader (HtmlAnswer ctype cont) = ("",HtmlAnswer ctype cont)
extractHeader (HtmlPage title params hexp) = 
  (headerstring ++ cookiestring, HtmlPage title otherparams hexp)
 where 
  headerstring = concatMap (++"\n") headers

  cookiestring = if null cookies
                   then ""
                   else "Cache-control: no-cache=\"set-cookie\"\n" ++
                        concatMap ((++"\n") . formatCookie) cookies

  (headers, cookies, otherparams) = splitPageParams params

  splitPageParams []           = ([],[],[])
  splitPageParams (fparam:fps) =
    let (hs,cs,ops) = splitPageParams fps
    in case fparam of
         PageCookie n v ps -> (hs, (n,v,ps) : cs, ops)
         HttpHeader k v    -> ((k ++ ": " ++ v):hs, cs, ops)
         _                 -> (hs, cs, fparam:ops)


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
findHandler :: CgiEnv -> [HtmlExp] -> Maybe HtmlHandler
findHandler _ [] = Nothing
findHandler cenv (HtmlText _ : hexps) = findHandler cenv hexps
findHandler cenv (HtmlStruct _ _ hexps1 : hexps2) =
  findHandler cenv (hexps1 ++ hexps2)
findHandler cenv (HtmlEvent _ cgiref handler : hexps) =
  if null (cenv cgiref)
    then findHandler cenv hexps
    else Just handler
findHandler cenv (HtmlCRef hexp _ : hexps) = findHandler cenv (hexp : hexps)
findHandler _ (HtmlAction _    : _) =
  error "HTML.CGI.Exec: HtmlAction occurred"

------------------------------------------------------------------------------
-- Executes an HTML page, i.e., execute all HtmlActions contained
-- in the HTML expressions and replaces them by their computed results.
execPage :: HtmlPage -> IO HtmlPage
execPage (HtmlPage title params hexps) =
  mapM execHtml hexps >>= return . HtmlPage title params
execPage (HtmlAnswer t c) = return (HtmlAnswer t c)

-- Executes an HTML expression, i.e., execute all HtmlActions contained
-- in it and replaces them by their computed results.
execHtml :: HtmlExp -> IO HtmlExp
execHtml htmlexp = case htmlexp of
  HtmlText   _           -> return htmlexp
  HtmlStruct tag ats hes -> mapM execHtml hes >>= return . HtmlStruct tag ats
  HtmlCRef   he cref     -> do hexp <- execHtml he
                               return (HtmlCRef hexp cref)
  HtmlEvent  he cref hdl -> do hexp <- execHtml he
                               return (HtmlEvent hexp cref hdl)
  HtmlAction act         -> act >>= execHtml

------------------------------------------------------------------------------
--- Gets the list of variable/value pairs sent from the browser to the
--- current CGI script.
--- Used for the implementation of HTML event handlers.
getFormVariables :: IO [(String,String)]
getFormVariables = do
  clen <- getEnviron "CONTENT_LENGTH"
  cont <- getNChar (maybe 0 fst (readNat clen))
  return (includeCoordinates (parseCgiEnv cont))

-- translate a string of cgi environment bindings into list of binding pairs:
parseCgiEnv :: String -> [(String,String)]
parseCgiEnv s
  | s == ""   = []
  | otherwise = map (\ (n,v) -> (n, utf2latin (urlencoded2string v)))
                    (map (splitChar '=') (split (=='&') s))
 where
   -- split a string at the first position of a given character:
   splitChar c xs = let (ys,zs) = break (==c) xs
                    in if zs==[] then (ys,zs) else (ys,tail zs)

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
utf2latin []  = []
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

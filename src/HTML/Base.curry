------------------------------------------------------------------------------
--- Library for constructing static and dynamic HTML pages.
--- [This paper](http://www.informatik.uni-kiel.de/~mh/papers/PADL01.html)
--- contains a description of the basic ideas behind this library.
---
--- A cgi script written with this library can be installed
--- by the command
---
---     cypm exec curry2cgi -m mainPage -o /home/joe/public_html/prog.cgi Prog
---
--- where `Prog` is the name of the Curry program with
--- the cgi script, `/home/joe/public_html/prog.cgi` is
--- the desired location of the
--- compiled cgi script, and `mainPage` is the Curry expression
--- (of type `IO HtmlPage`) computing the HTML page (where `cypm`
--- is the command calling the Curry Package Manager).
---
--- @author Michael Hanus (with extensions by Bernd Brassel and Marco Comini)
--- @version July 2020
------------------------------------------------------------------------------

{-# OPTIONS_CYMAKE -Wno-incomplete-patterns #-}

module HTML.Base
 ( HtmlExp(..), textOf,
   HtmlPage(..), PageParam(..),
   HtmlFormDef(..), formDefWithID, formDefId,
   CookieParam(..),
   CgiRef, idOfCgiRef, instCgiRefs, CgiEnv, HtmlHandler,
   defaultEncoding,
   answerText, answerEncText,
   getCookies,
   page, standardPage,
   pageEnc, pageCookie, pageCSS, pageMetaInfo,
   pageLinkInfo, pageBodyAttr, addPageParam, addCookies, addHttpHeader,
   htxt, htxts, hempty, nbsp, h1, h2, h3, h4, h5, h6,
   par, section, header, footer, emphasize, strong, bold, italic, nav, code,
   center, blink, teletype, pre, verbatim, address, href, anchor,
   ulist, ulistWithClass, ulistWithItemClass,
   olist, olistWithClass, olistWithItemClass,
   litem, dlist,
   table, tableWithClass, headedTable, addHeadings,
   hrule, breakline, image,
   styleSheet, style, textstyle, blockstyle, inline, block,
   redirectPage, expires,
   formExp,
   button, resetButton, imageButton, coordinates,
   textField, password, textArea, checkBox, checkedBox,
   radioMain, radioMainOff, radioOther,
   selection, selectionInitial, multipleSelection,
   hiddenField, htmlQuote, htmlIsoUmlauts, addAttr, addAttrs, addClass,
   showHtmlExps, showHtmlExp, showHtmlPage,
   htmlPrelude, htmlTagAttrs,
   getUrlParameter, urlencoded2string, string2urlencoded,
   formatCookie
 ) where

import Char        ( isAlphaNum, isSpace )
import ReadNumeric ( readNat, readHex )
import System      ( getEnviron )
import Time        ( CalendarTime(..), ClockTime, toTimeString, toUTCTime )

infixl 0 `addAttr`
infixl 0 `addAttrs`
infixl 0 `addClass`
infixl 0 `addPageParam`

------------------------------------------------------------------------------
--- The default encoding used in generated web pages.
defaultEncoding :: String
defaultEncoding = "utf-8" --"iso-8859-1"

------------------------------------------------------------------------------
--- The (abstract) data type for representing references to input elements
--- in HTML forms.
data CgiRef = CgiRef String

--- Internal identifier of a CgiRef (intended only for internal use in other
--- libraries!).
idOfCgiRef :: CgiRef -> String
idOfCgiRef (CgiRef i) = i

--- The type for representing cgi environments
--- (i.e., mappings from cgi references to the corresponding values of
--- the input elements).
type CgiEnv = CgiRef -> String

--- The type of event handlers occurring in HTML forms.
type HtmlHandler = CgiEnv -> IO HtmlPage

--- The data type for representing HTML expressions.
--- @cons HtmlText s - a text string without any further structure
--- @cons HtmlStruct t as hs - a structure with a tag, attributes, and
---                            HTML expressions inside the structure
--- @cons HtmlCRef h ref - an input element (described by the first argument)
---                        with a cgi reference
--- @cons HtmlEvent h ref hdlr - an input element (first arg) identified
---                              by a cgi reference with an associated
---                              event handler (typically, a submit button)
--- @cons HtmlAction act - an action that computes an HTML expression
---                      which will be inserted when the HTML document is shown
data HtmlExp =
    HtmlText   String
  | HtmlStruct String [(String,String)] [HtmlExp]
  | HtmlCRef   HtmlExp CgiRef
  | HtmlEvent  HtmlExp CgiRef HtmlHandler
  | HtmlAction (IO HtmlExp)

--- Extracts the textual contents of a list of HTML expressions.
--- For instance,
---
---      textOf [HtmlText "xy", HtmlStruct "a" [] [HtmlText "bc"]] == "xy bc"
---
textOf :: [HtmlExp] -> String
textOf = unwords . filter (not . null) . map textOfHtmlExp
 where
   textOfHtmlExp (HtmlText s) = s
   textOfHtmlExp (HtmlStruct _ _ hs)   = textOf hs
   textOfHtmlExp (HtmlCRef   hexp _)   = textOf [hexp]
   textOfHtmlExp (HtmlEvent  hexp _ _) = textOf [hexp]
   textOfHtmlExp (HtmlAction _)        = ""

------------------------------------------------------------------------------
--- The data type for representing HTML forms embedded into HTML pages.
---
--- A form definition consists of a unique identifier of form (usually,
--- the qualified name of the operation defining the form),
--- a (reading!) IO action and a mapping from data
--- into an HTML expression (which usually contains event handlers
--- to produce the form answers).
--- It is assumed that the IO action reads only data and does not
--- change it, since it is applied twice when executing a form.
data HtmlFormDef a = HtmlFormDef String (IO a) (a -> [HtmlExp])

--- A definition of a form with a unique identifier of form (usually,
--- the qualified name of the operation defining the form).
--- A form contains a (reading!) IO action and a mapping from data
--- into an HTML expression (which usually contains event handlers
--- to produce the form answers).
--- It is assumed that the IO action reads only data and does not
--- change it, since it is applied twice when executing a form.
formDefWithID :: String -> IO a -> (a -> [HtmlExp]) -> HtmlFormDef a
formDefWithID = HtmlFormDef

--- Returns the identifier of a form definition.
formDefId :: HtmlFormDef a -> String
formDefId (HtmlFormDef s _ _) = s

-- Auxiliary operations for executing forms.

--- Computes the initial form of a form definition.
genInitForm :: HtmlFormDef a -> IO [HtmlExp]
genInitForm (HtmlFormDef _ readact formgen) = readact >>= return . formgen

--- Instantiates all CgiRefs with a unique tag in HTML expressions.
--- Only internally used.
instCgiRefs :: [HtmlExp] -> Int -> ([HtmlExp],Int)
-- arguments: HTMLExps, number for cgi-refs
-- result: translated HTMLExps, new number for cgi-refs
instCgiRefs [] i = ([],i)
instCgiRefs (HtmlText s : hexps) i =
  case instCgiRefs hexps i of
    (nhexps,j) -> (HtmlText s : nhexps, j)
instCgiRefs (HtmlStruct tag attrs hexps1 : hexps2) i =
  case instCgiRefs hexps1 i of
    (nhexps1,j) -> case instCgiRefs hexps2 j of
                     (nhexps2,k) -> (HtmlStruct tag attrs nhexps1 : nhexps2, k)
instCgiRefs (HtmlEvent (HtmlStruct tag attrs hes) cgiref handler : hexps) i
  | idOfCgiRef cgiref =:= ("FIELD_" ++ show i)
  = case instCgiRefs hexps (i+1) of
      (nhexps,j) ->
         (HtmlEvent (HtmlStruct tag attrs hes) cgiref handler : nhexps, j)
instCgiRefs (HtmlCRef hexp cgiref : hexps) i
  | idOfCgiRef cgiref =:= ("FIELD_" ++ show i)
  = case instCgiRefs [hexp] (i+1) of
      ([nhexp],j) -> case instCgiRefs hexps j of
                       (nhexps,k) -> (nhexp : nhexps, k)
instCgiRefs (HtmlAction _ : _) _ =
  error "HTML.Base.instCgiRefs: HtmlAction occurred"

------------------------------------------------------------------------------
--- The data type for representing HTML pages.
--- @cons HtmlPage t ps hs - an HTML page with title t, optional parameters
---         (e.g., cookies) ps, and contents hs
--- @cons HtmlAnswer t c - an answer in an arbitrary format where t
---         is the content type (e.g., "text/plain") and c is the contents
data HtmlPage = HtmlPage String [PageParam] [HtmlExp]
              | HtmlAnswer String String

--- The possible parameters of an HTML page.
--- The parameters of a cookie (`PageCookie`) are its name and value and
--- optional parameters (expiration date, domain, path (e.g., the path "/"
--- makes the cookie valid for all documents on the server), security) which
--- are collected in a list.
--- @cons PageEnc - the encoding scheme of this page
--- @cons PageCookie name value params - a cookie to be sent to the
---                                      client's browser
--- @cons PageCSS s - a URL for a CSS file for this page
--- @cons HttpHeader key value - additional HTTP header included in this page
--- @cons PageJScript s - a URL for a Javascript file for this page
--- @cons PageMeta as - meta information (in form of attributes) for this page
--- @cons PageLink as - link information (in form of attributes) for this page
--- @cons PageHeadInclude he - HTML expression to be included in page header
--- @cons PageBodyAttr attr - optional attribute for the body element of the
---                           page (more than one occurrence is allowed)
data PageParam = PageEnc         String
               | PageCookie      String String [CookieParam]
               | PageCSS         String
               | HttpHeader      String String
               | PageJScript     String
               | PageMeta        [(String,String)]
               | PageLink        [(String,String)]
               | PageHeadInclude HtmlExp
               | PageBodyAttr    (String,String)

--- An encoding scheme for a HTML page.
pageEnc :: String -> PageParam
pageEnc enc = PageEnc enc

--- A cookie to be sent to the client's browser when a HTML page is
--- requested.
pageCookie :: (String,String) -> PageParam
pageCookie (n,v) = PageCookie n v []

--- A URL for a CSS file for a HTML page.
pageCSS :: String -> PageParam
pageCSS css = PageCSS css

--- A header to be sent to the client's browser when a HTML page is
--- requested.
httpHeader :: String -> String -> PageParam
httpHeader k v = HttpHeader k v

--- Meta information for a HTML page. The argument is a list of
--- attributes included in the `meta`-tag in the header for this page.
pageMetaInfo :: [(String,String)] -> PageParam
pageMetaInfo attrs = PageMeta attrs

--- Link information for a HTML page. The argument is a list of
--- attributes included in the `link`-tag in the header for this page.
pageLinkInfo :: [(String,String)] -> PageParam
pageLinkInfo attrs = PageLink attrs

--- Optional attribute for the body element of the web page.
--- More than one occurrence is allowed, i.e., all such attributes are
--- collected.
pageBodyAttr :: (String,String) -> PageParam
pageBodyAttr attr = PageBodyAttr attr

--- A basic HTML web page with the default encoding.
--- @param title - the title of the page
--- @param hexps - the page's body (list of HTML expressions)
--- @return an HTML page
page :: String -> [HtmlExp] -> HtmlPage
page title hexps = HtmlPage title [PageEnc defaultEncoding] hexps

--- A standard HTML web page where the title is included
--- in the body as the first header.
--- @param title - the title of the page
--- @param hexps - the page's body (list of HTML expressions)
--- @return an HTML page with the title as the first header
standardPage :: String -> [HtmlExp] -> HtmlPage
standardPage title hexps = page title (h1 [htxt title] : hexps)

--- Adds a parameter to an HTML page.
--- @param page - a page
--- @param param - a page's parameter
--- @return an HTML page
addPageParam :: HtmlPage -> PageParam -> HtmlPage
addPageParam (HtmlPage title params hexps) param =
  HtmlPage title (param:params) hexps
addPageParam hexp@(HtmlAnswer _ _) _ = hexp

--- Add simple cookie to an HTML page.
--- The cookies are sent to the client's browser together with this page.
--- @param cs - the cookies as a list of name/value pairs
--- @param form - the form to add cookies to
--- @return a new HTML page
addCookies :: [(String,String)] -> HtmlPage -> HtmlPage
addCookies cs (HtmlPage title params hexps) =
  HtmlPage title (map pageCookie cs ++ params) hexps
addCookies _ (HtmlAnswer _ _) =
  error "addCookies: cannot add cookie to HTML answer"

--- Add a HTTP header to a HTML page.
--- Headers are sent to the client's browser together with the page.
--- @param key   - the name of the HTTP header field
--- @param value - the value of the HTTP header field
--- @param page  - the page to which the header is added
--- @return a new HTML page
addHttpHeader :: String -> String -> HtmlPage -> HtmlPage
addHttpHeader key value (HtmlPage t fas hs) =
  HtmlPage t (HttpHeader key value : fas) hs
addHttpHeader _ _ (HtmlAnswer _ _) =
  error "addHttpHeader: cannot add HTTP header to HTML answer"

------------------------------------------------------------------------------
--- The possible parameters of a cookie.
data CookieParam = CookieExpire ClockTime
                 | CookieDomain String
                 | CookiePath   String
                 | CookieSecure

-- Shows the cookie in standard syntax:
formatCookie :: (String,String,[CookieParam]) -> String
formatCookie (name,value,params) =
  "Set-Cookie: " ++ name ++ "=" ++ string2urlencoded value ++
  concatMap (\p->"; "++formatCookieParam p) params

-- Formats a cookie parameter:
formatCookieParam :: CookieParam -> String
formatCookieParam (CookieExpire e) = "expires=" ++ toCookieDateString e
formatCookieParam (CookieDomain d) = "domain="  ++ d
formatCookieParam (CookiePath   p) = "path="    ++ p
formatCookieParam CookieSecure     = "secure"

-- Formats a clock time into a date string for cookies:
toCookieDateString :: ClockTime -> String
toCookieDateString time =
 let (CalendarTime y mo d h mi s tz) = toUTCTime time
  in (show d ++ "-" ++ shortMonths!!(mo-1) ++ "-" ++ show y ++ " " ++
         toTimeString (CalendarTime y mo d h mi s tz) ++ " UTC")
  where shortMonths = ["Jan","Feb","Mar","Apr","May","Jun",
                       "Jul","Aug","Sep","Oct","Nov","Dec"]


--- A textual result instead of an HTML page as a result for active web pages.
--- @param txt - the contents of the result page
--- @return an HTML answer page
answerText :: String -> HtmlPage
answerText = HtmlAnswer "text/plain"

--- A textual result instead of an HTML page as a result for active web pages
--- where the encoding is given as the first parameter.
--- @param enc - the encoding of the text(e.g., "utf-8" or "iso-8859-1")
--- @param txt - the contents of the result page
--- @return an HTML answer page
answerEncText :: String -> String -> HtmlPage
answerEncText enc = HtmlAnswer ("text/plain; charset="++enc)

--- Generates a redirection page to a given URL.
--- This is implemented via the HTTP response header `Location` (see also
--- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Location>).
--- @param url - The URL target of the redirection
--- @param page - The redirection page
redirectPage :: String -> HtmlPage
redirectPage url = addHttpHeader "Location" url $ page "Redirect" []

--- Adds expire time to given HTML page.
--- @param secs - Number of seconds before document expires
--- @param page - The page to add the header information to
expires :: Int -> HtmlPage -> HtmlPage
expires secs hpage =
  hpage `addPageParam`
    PageHeadInclude (HtmlStruct "meta" [("http-equiv","expires"),
                                        ("content",show secs)] [])

------------------------------------------------------------------------------
-- some useful abbreviations:

--- Basic text as HTML expression.
--- The text may contain special HTML chars (like &lt;,&gt;,&amp;,&quot;)
--- which will be quoted so that they appear as in the parameter string.
htxt   :: String -> HtmlExp
htxt s = HtmlText (htmlQuote s)

--- A list of strings represented as a list of HTML expressions.
--- The strings may contain special HTML chars that will be quoted.
htxts :: [String] -> [HtmlExp]
htxts = map htxt

--- An empty HTML expression.
hempty :: HtmlExp
hempty = HtmlText ""

--- Non breaking Space
nbsp   :: HtmlExp
nbsp = HtmlText "&nbsp;"

--- Header 1
h1      :: [HtmlExp] -> HtmlExp
h1 hexps = HtmlStruct "h1" [] hexps

--- Header 2
h2      :: [HtmlExp] -> HtmlExp
h2 hexps = HtmlStruct "h2" [] hexps

--- Header 3
h3      :: [HtmlExp] -> HtmlExp
h3 hexps = HtmlStruct "h3" [] hexps

--- Header 4
h4      :: [HtmlExp] -> HtmlExp
h4 hexps = HtmlStruct "h4" [] hexps

--- Header 5
h5      :: [HtmlExp] -> HtmlExp
h5 hexps = HtmlStruct "h5" [] hexps

--- Header 6
h6      :: [HtmlExp] -> HtmlExp
h6 hexps = HtmlStruct "h6" [] hexps

--- Paragraph
par      :: [HtmlExp] -> HtmlExp
par hexps = HtmlStruct "p" [] hexps

--- Section
section :: [HtmlExp] -> HtmlExp
section hexps = HtmlStruct "section" [] hexps

--- Header
header :: [HtmlExp] -> HtmlExp
header hexps = HtmlStruct "header" [] hexps

--- Footer
footer :: [HtmlExp] -> HtmlExp
footer hexps = HtmlStruct "footer" [] hexps

--- Emphasize
emphasize      :: [HtmlExp] -> HtmlExp
emphasize hexps = HtmlStruct "em" [] hexps

--- Strong (more emphasized) text.
strong      :: [HtmlExp] -> HtmlExp
strong hexps = HtmlStruct "strong" [] hexps

--- Boldface
bold      :: [HtmlExp] -> HtmlExp
bold hexps = HtmlStruct "b" [] hexps

--- Italic
italic      :: [HtmlExp] -> HtmlExp
italic hexps = HtmlStruct "i" [] hexps

--- Navigation
nav :: [HtmlExp] -> HtmlExp
nav doc = HtmlStruct "nav" [] doc

--- Program code
code      :: [HtmlExp] -> HtmlExp
code hexps = HtmlStruct "code" [] hexps

--- Centered text
center      :: [HtmlExp] -> HtmlExp
center hexps = HtmlStruct "center" [] hexps

--- Blinking text
blink      :: [HtmlExp] -> HtmlExp
blink hexps = HtmlStruct "blink" [] hexps

--- Teletype font
teletype      :: [HtmlExp] -> HtmlExp
teletype hexps = HtmlStruct "tt" [] hexps

--- Unformatted input, i.e., keep spaces and line breaks and
--- don't quote special characters.
pre      :: [HtmlExp] -> HtmlExp
pre hexps = HtmlStruct "pre" [] hexps

--- Verbatim (unformatted), special characters (&lt;,&gt;,&amp;,&quot;)
--- are quoted.
verbatim  :: String -> HtmlExp
verbatim s = HtmlStruct "pre" [] [HtmlText (htmlQuote s)]

--- Address
address       :: [HtmlExp] -> HtmlExp
address hexps = HtmlStruct "address" [] hexps

--- Hypertext reference
href           :: String -> [HtmlExp] -> HtmlExp
href ref hexps = HtmlStruct "a" [("href",ref)] hexps

--- An anchored text with a hypertext reference inside a document.
anchor           :: String -> [HtmlExp] -> HtmlExp
anchor anc hexps = HtmlStruct "span" [("id",anc)] hexps

--- Unordered list.
--- @param items - the list items where each item is a list of HTML expressions
ulist       :: [[HtmlExp]] -> HtmlExp
ulist items = HtmlStruct "ul" [] (map litem items)

--- An unordered list with classes for the entire list and the list elements.
--- The class annotation will be ignored if it is empty.
--- @param listclass - the class for the entire list structure
--- @param itemclass - the class for the list items
--- @param items - the list items where each item is a list of HTML expressions
ulistWithClass :: String -> String -> [[HtmlExp]] -> HtmlExp
ulistWithClass listclass itemclass items =
  HtmlStruct "ul" [] (map litemWC items) `addClass` listclass
 where
  litemWC i = litem i `addClass` itemclass

--- An unordered list with classes for the entire list
--- individual classes for the list elements.
--- The class annotation will be ignored if it is empty.
--- @param listclass - the class for the entire list structure
--- @param classitems - the list items together with their classes
ulistWithItemClass :: String -> [(String,[HtmlExp])] -> HtmlExp
ulistWithItemClass listclass classeditems =
  HtmlStruct "ul" [] (map litemWC classeditems) `addClass` listclass
 where
  litemWC (c,i) = litem i `addClass` c

--- Ordered list.
--- @param items - the list items where each item is a list of HTML expressions
olist :: [[HtmlExp]] -> HtmlExp
olist items = HtmlStruct "ol" [] (map litem items)

--- An ordered list with classes for the entire list and the list elements.
--- The class annotation will be ignored if it is empty.
--- @param listclass - the class for the entire list structure
--- @param itemclass - the class for the list items
--- @param items - the list items where each item is a list of HTML expressions
olistWithClass :: String -> String -> [[HtmlExp]] -> HtmlExp
olistWithClass listclass itemclass items =
  HtmlStruct "ol" [] (map litemWC items) `addClass` listclass
 where
  litemWC i = litem i `addClass` itemclass

--- An ordered list with classes for the entire list
--- individual classes for the list elements.
--- The class annotation will be ignored if it is empty.
--- @param listclass - the class for the entire list structure
--- @param classitems - the list items together with their classes
olistWithItemClass :: String -> [(String,[HtmlExp])] -> HtmlExp
olistWithItemClass listclass classeditems =
  HtmlStruct "ol" [] (map litemWC classeditems) `addClass` listclass
 where
  litemWC (c,i) = litem i `addClass` c

--- A single list item (usually not explicitly used)
litem :: [HtmlExp] -> HtmlExp
litem hexps = HtmlStruct "li" [] hexps

--- Description list
--- @param items - a list of (title/description) pairs (of HTML expressions)
dlist       :: [([HtmlExp],[HtmlExp])] -> HtmlExp
dlist items = HtmlStruct "dl" [] (concatMap ditem items)
 where
  ditem (hexps1,hexps2) = [HtmlStruct "dt" [] hexps1,
                           HtmlStruct "dd" [] hexps2]

--- Table with a matrix of items where each item is a list of HTML expressions.
table :: [[[HtmlExp]]] -> HtmlExp
table items = HtmlStruct "table" []
 (map (\row->HtmlStruct "tr" []
                 (map (\item -> HtmlStruct "td" [] item) row)) items)

--- Table with a matrix of items (each item is a list of HTML expressions)
--- with classes for the entire table, each row, and each data element.
--- The class annotation will be ignored if it is empty.
--- @param tableclass - the class for the entire table structure
--- @param rowclass   - the class for the table rows
--- @param dataclass  - the class for the table data items
--- @param items - the matrix of table items where each item is a
---                list of HTML expressions
tableWithClass :: String -> String -> String -> [[[HtmlExp]]] -> HtmlExp
tableWithClass tableclass rowclass dataclass items =
 HtmlStruct "table" []
   (map (\row -> HtmlStruct "tr" []
                   (map (\d -> HtmlStruct "td" [] d `addClass` dataclass) row)
                   `addClass` rowclass)
        items) `addClass` tableclass

--- Similar to <code>table</code> but introduces header tags for the first row.
headedTable :: [[[HtmlExp]]] -> HtmlExp
headedTable = withinTable . table
 where
  withinTable (HtmlStruct "table" attrs (HtmlStruct "tr" rowAttrs row:rows)) =
      HtmlStruct "table" attrs
        (HtmlStruct "tr" rowAttrs (map addTh row):rows)
  addTh x = case x of
             (HtmlStruct "td" attrs conts) -> HtmlStruct "th" attrs conts
             other -> other

--- Add a row of items (where each item is a list of HTML expressions)
--- as headings to a table. If the first argument is not a table,
--- the headings are ignored.
addHeadings :: HtmlExp -> [[HtmlExp]] -> HtmlExp
addHeadings htable headings = case htable of
   HtmlStruct "table" attrs rows ->
      HtmlStruct "table" attrs
         (HtmlStruct "tr" [] (map (\item->HtmlStruct "th" [] item) headings):rows)
   _ -> htable


--- Horizontal rule
hrule :: HtmlExp
hrule = HtmlStruct "hr" [] []

--- Break a line
breakline :: HtmlExp
breakline = HtmlStruct "br" [] []

--- Image
--- @param src - the URL of the image
--- @param alt - the alternative text shown instead of the image
image :: String -> String -> HtmlExp
image src alt = HtmlStruct "img" [("src",src),("alt",htmlQuote alt)] []


-------------- styles and document structuring:
--- Defines a style sheet to be used in this HTML document.
--- @param css - a string in CSS format
styleSheet :: String -> HtmlExp
styleSheet css = HtmlStruct "style" [("type","text/css")] [HtmlText css]

--- Provides a style for HTML elements.
--- The style argument is the name of a style class defined in a
--- style definition (see `styleSheet`) or in an external
--- style sheet (see form and page parameters `FormCSS` and `PageCSS`).
--- @param st - name of a style class
--- @param hexps - list of HTML expressions
style :: String -> [HtmlExp] -> HtmlExp
style st hexps = HtmlStruct "span" [("class",st)] hexps

--- Provides a style for a basic text.
--- The style argument is the name of a style class defined in an
--- external style sheet.
--- @param st - name of a style class
--- @param txt - a string (special characters will be quoted)
textstyle :: String -> String -> HtmlExp
textstyle st txt = HtmlStruct "span" [("class",st)] [htxt txt]

--- Provides a style for a block of HTML elements.
--- The style argument is the name of a style class defined in an
--- external style sheet. This element is used (in contrast to "style")
--- for larger blocks of HTML elements since a line break is placed
--- before and after these elements.
--- @param st - name of a style class
--- @param hexps - list of HTML expressions
blockstyle :: String -> [HtmlExp] -> HtmlExp
blockstyle st hexps = HtmlStruct "div" [("class",st)] hexps

--- Joins a list of HTML elements into a single HTML element.
--- Although this construction has no rendering, it is sometimes useful
--- for programming when several HTML elements must be put together.
--- @param hexps - list of HTML expressions
inline :: [HtmlExp] -> HtmlExp
inline hexps = HtmlStruct "span" [] hexps

--- Joins a list of HTML elements into a block.
--- A line break is placed before and after these elements.
--- @param hexps - list of HTML expressions
block :: [HtmlExp] -> HtmlExp
block hexps = HtmlStruct "div" [] hexps

------------------------------------------------------------------------------
-- Forms and input fields:

--- A form embedded in an HTML expression.
--- The parameter is a form defined as an exported top-level operation
--- in the CGI program so that it can be accessed by the main program.
--- The URL of the generated form is the same as the main page, i.e.,
--- the current URL parameter is passed to the form (which is
--- useful for REST-based programming with URL parameters).
--- The form uses a hidden field named `FORMID` to identify the form
--- in the submitted form controller.
formExp :: HtmlFormDef a -> HtmlExp
formExp formspec = HtmlAction htmlAction
 where
  htmlAction = do
    urlparam <- getUrlParameter
    he       <- genInitForm formspec
    return $
       HtmlStruct "form" [("method","post"),("action",'?' : urlparam)]
         (hiddenField "FORMID" (formDefId formspec) : fst (instCgiRefs he 0))

--- A button to submit a form with a label string and an event handler.
button :: String -> HtmlHandler -> HtmlExp
button label handler
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlEvent (HtmlStruct "input" [("type","submit"), ("name",ref),
                                   ("value",htmlQuote label)] [])
              cref handler
 where
  cref,ref free

--- Reset button with a label string
resetButton :: String -> HtmlExp
resetButton label =
    HtmlStruct "input" [("type","reset"),("value",htmlQuote label)] []

--- Submit button in form of an imag.
--- @param src - url of the image
--- @param handler - event handler
imageButton :: String -> HtmlHandler -> HtmlExp
imageButton src handler
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlEvent
       (HtmlStruct "input" [("type","image"),("name",ref),("src",src)] [])
       cref handler
 where
  cref,ref free

--- Input text field with a reference and an initial contents
textField :: CgiRef -> String -> HtmlExp
textField cref contents
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef
       (HtmlStruct "input" [("type","text"),("name",ref),
                            ("value",htmlQuote contents)] [])
       cref
 where ref free

--- Input text field (where the entered text is obscured) with a reference
password :: CgiRef -> HtmlExp
password cref
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef
       (HtmlStruct "input" [("type","password"),("name",ref)] [])
       cref
 where
   ref free

--- Input text area with a reference, height/width, and initial contents
textArea :: CgiRef -> (Int,Int) -> String -> HtmlExp
textArea cref (height,width) contents
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef
       (HtmlStruct "textarea" [("name",ref),
                                ("rows",show height),("cols",show width)]
                               [htxt contents])
       cref
 where
   ref free

--- A checkbox with a reference and a value.
--- The value is returned if checkbox is on, otherwise "" is returned.
checkBox :: CgiRef -> String -> HtmlExp
checkBox cref value
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef
       (HtmlStruct "input" [("type","checkbox"),("name",ref),
                            ("value",htmlQuote value)] [])
       cref
 where
   ref free

--- A checkbox that is initially checked with a reference and a value.
--- The value is returned if checkbox is on, otherwise "" is returned.
checkedBox :: CgiRef -> String -> HtmlExp
checkedBox cref value
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef
       (HtmlStruct "input" [("type","checkbox"),("name",ref),
                            ("value",htmlQuote value),("checked","checked")] [])
       cref
 where
   ref free

--- A main button of a radio (initially "on") with a reference and a value.
--- The value is returned of this button is on.
--- A complete radio button suite always consists of a main button
--- (radio_main) and some further buttons (radio_others) with the
--- same reference. Initially, the main button is selected
--- (or nothing is selected if one uses radio_main_off instead of radio_main).
--- The user can select another button but always at most one button
--- of the radio can be selected. The value corresponding to the
--- selected button is returned in the environment for this radio reference.
radioMain :: CgiRef -> String -> HtmlExp
radioMain cref value
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef
       (HtmlStruct "input" [("type","radio"),("name",ref),
                            ("value",htmlQuote value),("checked","yes")] [])
       cref
 where
   ref free

--- A main button of a radio (initially "off") with a reference and a value.
--- The value is returned of this button is on.
radioMainOff :: CgiRef -> String -> HtmlExp
radioMainOff cref value
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef
       (HtmlStruct "input" [("type","radio"),("name",ref),
                            ("value",htmlQuote value)] [])
       cref
 where
   ref free

--- A further button of a radio (initially "off") with a reference (identical
--- to the main button of this radio) and a value.
--- The value is returned of this button is on.
radioOther :: CgiRef -> String -> HtmlExp
radioOther cref value
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlStruct "input"
               [("type","radio"),("name",ref),("value",htmlQuote value)] []
 where
   ref free

--- A selection button with a reference and a list of name/value pairs.
--- The names are shown in the selection and the value is returned
--- for the selected name.
selection :: CgiRef -> [(String,String)] -> HtmlExp
selection cref menue
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef
       (HtmlStruct "select" [("name",ref)]
         ((concat . map (\(n,v)->[HtmlStruct "option" [("value",v)] [htxt n]]))
          menue))
       cref
 where
   ref free

--- A selection button with a reference, a list of name/value pairs,
--- and a preselected item in this list.
--- The names are shown in the selection and the value is returned
--- for the selected name.
--- @param ref - a CGI reference
--- @param nvs - list of name/value pairs
--- @param sel - the index of the initially selected item in the list nvs
--- @return an HTML expression representing the selection button
selectionInitial :: CgiRef -> [(String,String)] -> Int -> HtmlExp
selectionInitial cref sellist sel
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef (HtmlStruct "select" [("name",ref)] (selOption sellist sel))
             cref
 where
   ref free

   selOption [] _ = []
   selOption ((n,v):nvs) i =
      HtmlStruct "option"
        ([("value",v)] ++ if i==0 then [("selected","selected")] else [])
        [htxt n] : selOption nvs (i-1)

--- A selection button with a reference and a list of name/value/flag pairs.
--- The names are shown in the selection and the value is returned
--- if the corresponding name is selected. If flag is True, the
--- corresonding name is initially selected. If more than one name
--- has been selected, all values are returned in one string
--- where the values are separated by newline (`'\n'`) characters.
multipleSelection :: CgiRef -> [(String,String,Bool)] -> HtmlExp
multipleSelection cref sellist
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef (HtmlStruct "select" [("name",ref),("multiple","multiple")]
                                  (map selOption sellist))
            cref
 where
   ref free

   selOption (n,v,flag) =
      HtmlStruct "option"
        ([("value",v)] ++ if flag then [("selected","selected")] else [])
        [htxt n]

--- A hidden field to pass a value referenced by a fixed name.
--- This function should be used with care since it may cause
--- conflicts with the CGI-based implementation of this library.
hiddenField :: String -> String -> HtmlExp
hiddenField name value =
    HtmlStruct "input" [("type","hidden"),("name",name),("value",value)] []


------------------------------------------------------------------------------
--- Quotes special characters (`<`,`>`,`&`,`"`, umlauts) in a string
--- as HTML special characters.
htmlQuote :: String -> String
htmlQuote [] = []
htmlQuote (c:cs) | c=='<' = "&lt;"   ++ htmlQuote cs
                 | c=='>' = "&gt;"   ++ htmlQuote cs
                 | c=='&' = "&amp;"  ++ htmlQuote cs
                 | c=='"' = "&quot;" ++ htmlQuote cs
                 | otherwise = htmlIsoUmlauts [c] ++ htmlQuote cs

--- Translates umlauts in iso-8859-1 encoding into HTML special characters.
htmlIsoUmlauts :: String -> String
htmlIsoUmlauts [] = []
htmlIsoUmlauts (c:cs) | oc==228 = "&auml;"  ++ htmlIsoUmlauts cs
                      | oc==246 = "&ouml;"  ++ htmlIsoUmlauts cs
                      | oc==252 = "&uuml;"  ++ htmlIsoUmlauts cs
                      | oc==196 = "&Auml;"  ++ htmlIsoUmlauts cs
                      | oc==214 = "&Ouml;"  ++ htmlIsoUmlauts cs
                      | oc==220 = "&Uuml;"  ++ htmlIsoUmlauts cs
                      | oc==223 = "&szlig;" ++ htmlIsoUmlauts cs
                      | oc==197 = "&Aring;" ++ htmlIsoUmlauts cs
                      | oc==250 = "&uacute;"++ htmlIsoUmlauts cs
                      | oc==237 = "&iacute;"++ htmlIsoUmlauts cs
                      | oc==225 = "&aacute;"++ htmlIsoUmlauts cs
                      | otherwise = c : htmlIsoUmlauts cs
  where oc = ord c

------------------------------------------------------------------------------
--- Adds an attribute (name/value pair) to an HTML element.
addAttr :: HtmlExp -> (String,String) -> HtmlExp
addAttr hexp attr = addAttrs hexp [attr]

--- Adds a list of attributes (name/value pair) to an HTML element.
addAttrs :: HtmlExp -> [(String,String)] -> HtmlExp
addAttrs (HtmlText s) _ = HtmlText s  -- strings have no attributes
addAttrs (HtmlStruct tag attrs hexps) newattrs =
    HtmlStruct tag (attrs ++ newattrs) hexps
addAttrs (HtmlEvent hexp cref handler) attrs =
    HtmlEvent (addAttrs hexp attrs) cref handler
addAttrs (HtmlCRef  hexp cref) attrs =
    HtmlCRef (addAttrs hexp attrs) cref
addAttrs (HtmlAction act) _ = HtmlAction act

--- Adds a class attribute to an HTML element
--- (if the class attribute is not empty).
addClass :: HtmlExp -> String -> HtmlExp
addClass hexp cls | null cls  = hexp
                  | otherwise = addAttr hexp ("class",cls)

------------------------------------------------------------------------------
-- Auxiliaries for faster show (could be later put into a standard library)

type ShowS = String -> String

showString :: String -> String -> String
showString s = (s++)

showChar :: Char -> String -> String
showChar c = (c:)

nl :: String -> String
nl = showChar '\n'

concatS :: [a -> a] -> a -> a
concatS [] = id
concatS xs@(_:_) = foldr1 (\ f g -> f . g) xs

------------------------------------------------------------------------------
--- Transforms a list of HTML expressions into string representation.
showHtmlExps :: [HtmlExp] -> String
showHtmlExps hexps = showsHtmlExps 0 hexps ""

-- get the string contents of an HTML expression:
getText :: HtmlExp -> String
getText (HtmlText   s)      = s
getText (HtmlStruct _ _ _)  = ""
getText (HtmlEvent  he _ _) = getText he
getText (HtmlCRef   he _)   = getText he
getText (HtmlAction _)      = ""

-- get the (last) tag of an HTML expression:
getTag :: HtmlExp -> String
getTag (HtmlText   _)       = ""
getTag (HtmlStruct tag _ _) = tag
getTag (HtmlEvent  he _ _)  = getTag he
getTag (HtmlCRef   he _)    = getTag he
getTag (HtmlAction  _)      = ""

-- is this a tag where a line break can be safely added?
tagWithLn :: String -> Bool
tagWithLn t = t/="" &&
              t `elem` ["br","p","li","ul","ol","dl","dt","dd","hr",
                        "h1","h2","h3","h4","h5","h6","div",
                        "html","title","head","body","link","meta","script",
                        "form","table","tr","td"]


--- Transforms a single HTML expression into string representation.
showHtmlExp :: HtmlExp -> String
showHtmlExp hexp = showsHtmlExp 0 hexp ""

--- HTML tags that have no end tag in HTML:
noEndTags :: [String]
noEndTags = ["img","input","link","meta"]

showsHtmlExp :: Int -> HtmlExp -> ShowS
showsHtmlExp _ (HtmlText s) = showString s
showsHtmlExp i (HtmlStruct tag attrs hexps) =
  let maybeLn j = if tagWithLn tag then nl . showTab j else id
   in maybeLn i .
      (if null hexps && (null attrs || tag `elem` noEndTags)
         then showsHtmlOpenTag tag attrs "/>"
         else showsHtmlOpenTag tag attrs ">" . maybeLn (i+2) . showExps hexps .
              maybeLn i . showString "</" . showString tag . showChar '>'
      ) . maybeLn i
 where
  showExps = if tag=="pre"
               then concatS . map (showsHtmlExp 0)
               else showsHtmlExps (i+2)
showsHtmlExp i (HtmlEvent hexp _ _) = showsHtmlExp i hexp
showsHtmlExp i (HtmlCRef  hexp _)   = showsHtmlExp i hexp
showsHtmlExp _ (HtmlAction  _)      =
  error "HTML.Base.showsHtmlExp: HtmlAction occurred"

showsHtmlExps :: Int -> [HtmlExp] -> ShowS
showsHtmlExps _ [] = id
showsHtmlExps i (he:hes) = showsWithLnPrefix he . showsHtmlExps i hes
 where
   showsWithLnPrefix hexp = let s = getText hexp
                            in if s/="" && isSpace (head s)
                                 then nl . showTab i . showString (tail s)
                                 else showsHtmlExp i hexp

showTab :: Int -> String -> String
showTab n = showString (take n (repeat ' '))

showsHtmlOpenTag :: String -> [(String,String)] -> String -> ShowS
showsHtmlOpenTag tag attrs close =
  showChar '<' . showString tag .
  concatS (map attr2string attrs) . showString close
 where
    attr2string (attr,value) = showChar ' ' . showString attr .
         showString "=\"" . encodeQuotes value . showChar '"'

    -- encode double quotes as "&quot;":
    encodeQuotes [] = id
    encodeQuotes (c:cs) | c=='"'    = showString "&quot;" . encodeQuotes cs
                        | otherwise = showChar c . encodeQuotes cs


------------------------------------------------------------------------------
--- Transforms HTML page into string representation.
--- @param page - the HTML page
--- @return string representation of the HTML document
showHtmlPage :: HtmlPage -> String
showHtmlPage (HtmlAnswer _ cont)            = cont
showHtmlPage (HtmlPage   title params html) =
  htmlPrelude ++
  showHtmlExp (HtmlStruct "html" htmlTagAttrs
                  [HtmlStruct "head" []
                       ([HtmlStruct "title" [] [HtmlText (htmlQuote title)]] ++
                       concatMap param2html params),
                   HtmlStruct "body" bodyattrs html])
 where
  param2html (PageEnc enc) =
     [HtmlStruct "meta" [("http-equiv","Content-Type"),
                         ("content","text/html; charset="++enc)] []]
  param2html (PageCookie _ _ _) = [] -- cookies are differently processed
  param2html (PageCSS css) =
     [HtmlStruct "link" [("rel","stylesheet"),("type","text/css"),("href",css)]
                 []]
  param2html (HttpHeader _ _) = [] -- page headers are differently processed
  param2html (PageJScript js) =
     [HtmlStruct "script" [("type","text/javascript"),("src",js)] []]
  param2html (PageMeta attrs) = [HtmlStruct "meta" attrs []]
  param2html (PageLink attrs) = [HtmlStruct "link" attrs []]
  param2html (PageHeadInclude hexp) = [hexp]
  param2html (PageBodyAttr _) = [] -- these attributes are separately processed

  bodyattrs = [attr | (PageBodyAttr attr) <- params]

--- Standard header for generated HTML pages.
htmlPrelude :: String
htmlPrelude = "<!DOCTYPE html>\n"

--- Standard attributes for element "html".
htmlTagAttrs :: [(String,String)]
htmlTagAttrs = [("lang","en")]

------------------------------------------------------------------------------
--- Gets the parameter attached to the URL of the script.
--- For instance, if the script is called with URL
--- "http://.../script.cgi?parameter", then "parameter" is
--- returned by this I/O action.
--- Note that an URL parameter should be "URL encoded" to avoid
--- the appearance of characters with a special meaning.
--- Use the functions "urlencoded2string" and "string2urlencoded"
--- to decode and encode such parameters, respectively.

getUrlParameter :: IO String
getUrlParameter = getEnviron "QUERY_STRING"

--- Translates urlencoded string into equivalent ASCII string.
urlencoded2string :: String -> String
urlencoded2string [] = []
urlencoded2string (c:cs)
  | c == '+'  = ' ' : urlencoded2string cs
  | c == '%'  = chr (maybe 0 fst (readHex (take 2 cs)))
                 : urlencoded2string (drop 2 cs)
  | otherwise = c : urlencoded2string cs

--- Translates arbitrary strings into equivalent urlencoded string.
string2urlencoded :: String -> String
string2urlencoded [] = []
string2urlencoded (c:cs)
  | isAlphaNum c = c : string2urlencoded cs
  | c == ' '     = '+' : string2urlencoded cs
  | otherwise = let oc = ord c
    in '%' : int2hex(oc `div` 16) : int2hex(oc `mod` 16) : string2urlencoded cs
 where
   int2hex i = if i<10 then chr (ord '0' + i)
                       else chr (ord 'A' + i - 10)


------------------------------------------------------------------------------
--- Gets the cookies sent from the browser for the current CGI script.
--- The cookies are represented in the form of name/value pairs since
--- no other components are important here.
getCookies :: IO [(String,String)]
getCookies = do
  cookiestring <- getEnviron "HTTP_COOKIE"
  return $ parseCookies cookiestring

-- translate a string of cookies (of the form "NAME1=VAL1; NAME2=VAL")
-- into a list of name/value pairs:
parseCookies :: String -> [(String,String)]
parseCookies str =
  if null str
    then []
    else let (c1,cs) = break (==';') str
         in parseCookie c1 :
            parseCookies (dropWhile (==' ') (if cs=="" then "" else tail cs))
 where
  parseCookie s = let (name,evalue) = break (=='=') s in
      (name, if evalue=="" then "" else urlencoded2string (tail evalue))

--- For image buttons: retrieve the coordinates where the user clicked
--- within the image.
coordinates :: CgiEnv -> Maybe (Int,Int)
coordinates env = let x = env (CgiRef "x")
                      y = env (CgiRef "y")
                  in if x/="" && y/=""
                       then Just (tryReadNat 0 x, tryReadNat 0 y)
                       else Nothing
 where
  tryReadNat d s = maybe d (\ (i,rs) -> if null rs then i else d) (readNat s)

------------------------------------------------------------------------------

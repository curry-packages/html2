------------------------------------------------------------------------------
--- Library for constructing static and dynamic HTML pages.
--- [This paper](https://www.informatik.uni-kiel.de/~mh/papers/PADL01.html)
--- contains a description of the basic ideas behind an old version
--- of this library.
---
--- An application written with this library can be transformed into
--- a cgi script by the command
---
---     > cypm exec curry2cgi -m mainPage -o /home/joe/cgi-bin/prog.cgi Prog
---
--- where `Prog` is the name of the Curry program with
--- the cgi script, `/home/joe/cgi-bin/prog.cgi` is
--- the desired location of the
--- compiled cgi script, and `mainPage` is the Curry expression
--- (of type `IO HtmlPage`) computing the HTML page (where `cypm`
--- is the command calling the Curry Package Manager).
---
--- @author Michael Hanus (with extensions by Bernd Brassel and Marco Comini)
--- @version October 2020
------------------------------------------------------------------------------

{-# OPTIONS_CYMAKE -Wno-incomplete-patterns #-}

module HTML.Base
 ( HTML, htmlText, htmlStruct, hStruct, updAttrs,
   HtmlExp(..), BaseHtml(..), toHtmlExp, fromHtmlExp, textOf,
   HtmlPage(..), PageParam(..),
   FormReader, fromFormReader, toFormReader,
   HtmlFormDef, simpleFormDef, simpleFormDefWithID, formDef, formDefWithID,
   formDefId, setFormDefId, formDefRead, formDefView,
   CookieParam(..),
   CgiRef, idOfCgiRef, instCgiRefs, CgiEnv, HtmlHandler,
   defaultEncoding,
   answerText, answerEncText,
   getCookies,
   page, headerPage,
   pageEnc, pageCookie, pageCSS, pageMetaInfo,
   pageLinkInfo, pageBodyAttr, addPageParam, addCookies, addHttpHeader,
   htxt, htxts, hempty, nbsp, h1, h2, h3, h4, h5, h6,
   par, section, header, footer, emphasize, strong, bold, italic, nav, code,
   center, blink, teletype, pre, verbatim, address, href, anchor,
   ulist, ulistWithClass, ulistWithItemClass,
   olist, olistWithClass, olistWithItemClass,
   litem, dlist,
   table, tableWithClass, headedTable,
   hrule, breakline, image,
   styleSheet, style, textstyle, blockstyle, inline, block, hiddenField,
   redirectPage, expires,
   formElem,
   button, resetButton, imageButton, coordinates,
   textField, password, textArea, checkBox, checkedBox,
   radioMain, radioMainOff, radioOther,
   selection, selectionInitial, multipleSelection,
   htmlQuote, htmlIsoUmlauts, addAttr, addAttrs, addClass,
   showBaseHtmls, showBaseHtml, showHtmlPage,
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
--- The default encoding used in generated HTML documents.
defaultEncoding :: String
defaultEncoding = "utf-8" --"iso-8859-1"

------------------------------------------------------------------------------
-- Basic types for (static) HTML documents.

--- The attributes for HTML structures consists of a list of
--- name/value pairs.
type Attrs = [(String,String)]

--- The data type to represent static HTML expressions.
--- @cons BaseText s         - a text string without any further structure
--- @cons BaseStruct t as hs - a structure with a tag, attributes, and
---                             HTML expressions inside the structure
--- @cons BaseAction act     - an action that computes a general HTML expression
---                            which will be inserted when the HTML document
---                            is shown (used to implement form expressions)
data BaseHtml =
    BaseText   String
  | BaseStruct String Attrs [BaseHtml]
  | BaseAction (IO HtmlExp)

--- Updates the attributes in a basic HTML expression.
updBaseAttrs :: (Attrs -> Attrs) -> BaseHtml -> BaseHtml
updBaseAttrs _ (BaseText s)                 = BaseText s
updBaseAttrs f (BaseStruct tag attrs hexps) = BaseStruct tag (f attrs) hexps
updBaseAttrs _ (BaseAction act)             = BaseAction act

--- A type is an instance of class `HTML` if it has operations to construct
--- HTML documents, i.e., constructors for basic text strings and
--- structures with tags and attributes.
class HTML a where
  htmlText   :: String -> a
  htmlStruct :: String -> Attrs -> [a] -> a
  updAttrs   :: (Attrs -> Attrs) -> a -> a

--- An HTML structure with a given tag and no attributes.
hStruct :: HTML h => String -> [h] -> h
hStruct tag = htmlStruct tag []

--- The type of basic HTML expressions is an instance of class `HTML`.
instance HTML BaseHtml where
  htmlText   = BaseText
  htmlStruct = BaseStruct
  updAttrs   = updBaseAttrs

------------------------------------------------------------------------------
-- CGI references and environments.

--- The (abstract) data type for representing references to input elements
--- in HTML forms.
data CgiRef = CgiRef String

--- Internal identifier of a CgiRef (intended only for internal use in other
--- libraries!).
idOfCgiRef :: CgiRef -> String
idOfCgiRef (CgiRef i) = i

--- The type for representing cgi environments, i.e., mappings
--- from cgi references to the corresponding values of the input elements.
type CgiEnv = CgiRef -> String

--- The type of event handlers occurring in HTML forms.
type HtmlHandler = CgiEnv -> IO HtmlPage

------------------------------------------------------------------------------
--- The data type for representing HTML expressions with input elements,
--- i.e., all elements which might occur inside a form.
--- @cons HtmlText s           - a text string without any further structure
--- @cons HtmlStruct t as hs   - a structure with a tag, attributes, and
---                              HTML expressions inside the structure
--- @cons HtmlCRef ref h       - an input element (described by the second
---                              argument) with a cgi reference
--- @cons HtmlEvent h ref hdlr - an input element (first arg) identified
---                              by a cgi reference with an associated
---                              event handler (typically, a submit button)
--- @cons HtmlAction act       - an action that computes an HTML expression
---                              which will be inserted when the HTML document
---                              is shown (used to implement form expressions)
data HtmlExp =
    HtmlText   String
  | HtmlStruct String Attrs [HtmlExp]
  | HtmlAction (IO HtmlExp)
  | HtmlCRef   CgiRef HtmlExp
  | HtmlEvent  CgiRef HtmlHandler HtmlExp

--- Updates the attributes in an HTML expression.
updHtmlAttrs :: (Attrs -> Attrs) -> HtmlExp -> HtmlExp
updHtmlAttrs _ (HtmlText s)                 = HtmlText s
updHtmlAttrs f (HtmlStruct tag attrs hexps) = HtmlStruct tag (f attrs) hexps
updHtmlAttrs f (HtmlEvent ref handler he)   =
  HtmlEvent ref handler (updHtmlAttrs f he)
updHtmlAttrs f (HtmlCRef ref he)            = HtmlCRef ref (updHtmlAttrs f he)
updHtmlAttrs _ (HtmlAction act)             = HtmlAction act

--- The type of HTML expressions is an instance of class `HTML`.
instance HTML HtmlExp where
  htmlText   = HtmlText
  htmlStruct = HtmlStruct
  updAttrs   = updHtmlAttrs

--- Transforms a static into a dynamic HTML document.
toHtmlExp :: BaseHtml -> HtmlExp
toHtmlExp (BaseText s)         = HtmlText s
toHtmlExp (BaseStruct t ps hs) = HtmlStruct t ps (map toHtmlExp hs)
toHtmlExp (BaseAction a)       = HtmlAction a

--- Transforms a dynamic HTML into a static one by dropping references
--- and event handlers.
fromHtmlExp :: HtmlExp -> BaseHtml
fromHtmlExp (HtmlText   s)       = BaseText s
fromHtmlExp (HtmlStruct t ps hs) = BaseStruct t ps (map fromHtmlExp hs)
fromHtmlExp (HtmlAction a)       = BaseAction a
fromHtmlExp (HtmlEvent  _ _ hs)  = fromHtmlExp hs
fromHtmlExp (HtmlCRef   _ hs)    = fromHtmlExp hs

------------------------------------------------------------------------------
--- Extracts the textual contents of a list of HTML expressions.
--- For instance,
---
---      textOf [BaseText "xy", BaseStruct "a" [] [BaseText "bc"]] == "xy bc"
---
textOf :: [BaseHtml] -> String
textOf = unwords . filter (not . null) . map textOfBaseHtml
 where
  textOfBaseHtml (BaseText s)          = s
  textOfBaseHtml (BaseStruct _ _ hs)   = textOf hs
  textOfBaseHtml (BaseAction _)        = ""

------------------------------------------------------------------------------
-- HTML forms.

--- The type `FormReader` is a monad with operations to read data
--- to invoke an HTML form.
--- It is assumed that a `FormReader` action reads only data and does not
--- change the environment, since the action is applied twice
--- when executing a form.
--- A typical action of this kind is `HTML.Session.getSessionData`.
--- 
--- The `FormReader` type encapsulates IO actions in order to enforce
--- the correct use of forms.
data FormReader a = FormReader (IO a)

--- Transforms a `FormReader` action into a standard IO action.
fromFormReader :: FormReader a -> IO a
fromFormReader (FormReader a) = a

--- Transforms an IO action into a `FormReader` action.
--- This operation should be used with care since it must be
--- ensured that the action only reads data and does not
--- change the environment, since the action is applied twice
--- when executing a form.
toFormReader :: IO a -> FormReader a
toFormReader = FormReader

instance Monad FormReader where
  return x = FormReader (return x)
  a >>= f = FormReader (fromFormReader a >>= \x -> fromFormReader (f x))

--- The data type for representing HTML forms embedded into HTML pages.
---
--- A form definition consists of a unique identifier of form (usually,
--- the qualified name of the operation defining the form),
--- a `FormReader` action and a mapping from data
--- into an HTML expression (which usually contains event handlers
--- to produce the form answers).
data HtmlFormDef a = HtmlFormDef String (FormReader a) (a -> [HtmlExp])

--- A definition of a simple form which does not require session data.
---
--- The unique identifier required for the implementation of forms
--- is added by the `curry2cgi` translator.
simpleFormDef :: [HtmlExp] -> HtmlFormDef ()
simpleFormDef hexps = HtmlFormDef "" (return ()) (const hexps)

--- A definition of a simple form, which does not require session data,
--- with a unique identifier (usually, the qualified name of the
--- operation defining the form).
simpleFormDefWithID :: String -> [HtmlExp] -> HtmlFormDef ()
simpleFormDefWithID fid hexps = HtmlFormDef fid (return ()) (const hexps)

--- A definition of a form which consists of a `FormReader` action
--- and a mapping from data into an HTML expression
--- (which usually contains event handlers to produce the form answers).
--- It is assumed that the `FormReader` action reads only data and does not
--- change it, since it is applied twice when executing a form.
---
--- The unique identifier required for the implementation of forms
--- is added by the `curry2cgi` translator.
formDef :: FormReader a -> (a -> [HtmlExp]) -> HtmlFormDef a
formDef = HtmlFormDef ""

--- A definition of a form with a unique identifier (usually,
--- the qualified name of the operation defining the form).
--- A form contains a `FormReader` action and a mapping from data
--- into an HTML expression (which usually contains event handlers
--- to produce the form answers).
--- It is assumed that the `FormReader` action reads only data and does not
--- change it, since it is applied twice when executing a form.
formDefWithID :: String -> FormReader a -> (a -> [HtmlExp]) -> HtmlFormDef a
formDefWithID = HtmlFormDef

--- Returns the identifier of a form definition.
formDefId :: HtmlFormDef a -> String
formDefId (HtmlFormDef s _ _) = s

--- Sets the identifier of a form definition.
--- Only intended for internal use in the `curry2cgi` translator.
setFormDefId :: String -> HtmlFormDef a -> HtmlFormDef a
setFormDefId fid (HtmlFormDef _ readact formgen) =
  HtmlFormDef fid readact formgen

--- Returns the `FormReader` action of a form definition.
formDefRead :: HtmlFormDef a -> IO a
formDefRead (HtmlFormDef _ ra _) = fromFormReader ra

--- Returns the view operation of a form definition.
formDefView :: HtmlFormDef a -> (a -> [HtmlExp])
formDefView (HtmlFormDef _ _ v) = v


-- Auxiliary operations for executing forms.

--- Computes the initial form of a form definition.
genInitForm :: HtmlFormDef a -> IO [HtmlExp]
genInitForm (HtmlFormDef _ readact formgen) =
  fromFormReader readact >>= return . formgen

--- Instantiates all CgiRefs with a unique tag in HTML expressions.
--- Only internally used.
--- Parameters: HTML expressions, number for cgi-refs
--- Result: translated HTML expressions, new number for cgi-refs
instCgiRefs :: [HtmlExp] -> Int -> ([HtmlExp],Int)
instCgiRefs [] i = ([],i)
instCgiRefs (HtmlText s : hexps) i =
  case instCgiRefs hexps i of
    (nhexps,j) -> (HtmlText s : nhexps, j)
instCgiRefs (HtmlStruct tag attrs hexps1 : hexps2) i =
  case instCgiRefs hexps1 i of
    (nhexps1,j) -> case instCgiRefs hexps2 j of
                     (nhexps2,k) -> (HtmlStruct tag attrs nhexps1 : nhexps2, k)
instCgiRefs (HtmlEvent cgiref handler (HtmlStruct tag attrs hes) : hexps) i
  | idOfCgiRef cgiref =:= ("FIELD_" ++ show i)
  = case instCgiRefs hexps (i+1) of
      (nhexps,j) ->
         (HtmlEvent cgiref handler (HtmlStruct tag attrs hes) : nhexps, j)
instCgiRefs (HtmlCRef cgiref hexp : hexps) i
  | idOfCgiRef cgiref =:= ("FIELD_" ++ show i)
  = case instCgiRefs [hexp] (i+1) of
      ([nhexp],j) -> case instCgiRefs hexps j of
                       (nhexps,k) -> (nhexp : nhexps, k)
instCgiRefs (HtmlAction _ : _) _ =
  error "HTML.Base.instCgiRefs: HtmlAction occurred"

------------------------------------------------------------------------------
--- The data type for representing HTML pages. Since the HTML document
--- shown in this page is a base HTML expression, it is ensured that
--- input elements and event handlers occur only in embedded forms.
--- @cons HtmlPage t ps hs - an HTML page with title t, optional parameters
---                          (e.g., cookies) ps, and contents hs
--- @cons HtmlAnswer t c - an answer in an arbitrary format where t
---         is the content type (e.g., "text/plain") and c is the contents
data HtmlPage = HtmlPage String [PageParam] [BaseHtml]
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
               | PageHeadInclude BaseHtml
               | PageBodyAttr    (String,String)

--- An encoding scheme for a HTML page.
pageEnc :: String -> PageParam
pageEnc = PageEnc

--- A cookie to be sent to the client's browser when a HTML page is
--- requested.
pageCookie :: (String,String) -> PageParam
pageCookie (n,v) = PageCookie n v []

--- A URL for a CSS file for a HTML page.
pageCSS :: String -> PageParam
pageCSS = PageCSS

--- A header to be sent to the client's browser when a HTML page is
--- requested.
httpHeader :: String -> String -> PageParam
httpHeader = HttpHeader

--- Meta information for a HTML page. The argument is a list of
--- attributes included in the `meta`-tag in the header for this page.
pageMetaInfo :: [(String,String)] -> PageParam
pageMetaInfo = PageMeta

--- Link information for a HTML page. The argument is a list of
--- attributes included in the `link`-tag in the header for this page.
pageLinkInfo :: [(String,String)] -> PageParam
pageLinkInfo = PageLink

--- Optional attribute for the body element of the web page.
--- More than one occurrence is allowed, i.e., all such attributes are
--- collected.
pageBodyAttr :: (String,String) -> PageParam
pageBodyAttr = PageBodyAttr

--- A basic HTML web page with the default encoding.
--- @param title - the title of the page
--- @param hexps - the page's body (list of HTML expressions)
--- @return an HTML page
page :: String -> [BaseHtml] -> HtmlPage
page title hexps = HtmlPage title [PageEnc defaultEncoding] hexps

--- A standard HTML web page where the title is included
--- in the body as the first header.
--- @param title - the title of the page
--- @param hexps - the page's body (list of HTML expressions)
--- @return an HTML page with the title as the first header
headerPage :: String -> [BaseHtml] -> HtmlPage
headerPage title hexps = page title (h1 [htxt title] : hexps)

--- Adds a parameter to an HTML page.
--- @param page - a page
--- @param param - a page's parameter
--- @return an HTML page
addPageParam :: HtmlPage -> PageParam -> HtmlPage
addPageParam (HtmlPage title params hexps) param =
  HtmlPage title (param:params) hexps
addPageParam hexp@(HtmlAnswer _ _) _ = hexp

--- Adds simple cookie to an HTML page.
--- The cookies are sent to the client's browser together with this page.
--- @param cs - the cookies as a list of name/value pairs
--- @param form - the form to add cookies to
--- @return a new HTML page
addCookies :: [(String,String)] -> HtmlPage -> HtmlPage
addCookies cs (HtmlPage title params hexps) =
  HtmlPage title (map pageCookie cs ++ params) hexps
addCookies _ (HtmlAnswer _ _) =
  error "addCookies: cannot add cookie to HTML answer"

--- Adds a HTTP header to a HTML page.
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
answerEncText enc = HtmlAnswer ("text/plain; charset=" ++ enc)

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
    PageHeadInclude (BaseStruct "meta" [("http-equiv","expires"),
                                        ("content",show secs)] [])

------------------------------------------------------------------------------
-- some useful abbreviations:

--- Basic text as HTML expression.
--- The text may contain special HTML chars (like &lt;,&gt;,&amp;,&quot;)
--- which will be quoted so that they appear as in the parameter string.
htxt :: HTML h => String -> h
htxt s = htmlText (htmlQuote s)

--- A list of strings represented as a list of HTML expressions.
--- The strings may contain special HTML chars that will be quoted.
htxts :: HTML h => [String] -> [h]
htxts = map htxt

--- An empty HTML expression.
hempty :: HTML h => h
hempty = htmlText ""

--- Non breaking Space
nbsp :: HTML h => h
nbsp = htmlText "&nbsp;"

--- Header 1
h1 :: HTML h => [h] -> h
h1 = hStruct "h1"

--- Header 2
h2 :: HTML h => [h] -> h
h2 = hStruct "h2"

--- Header 3
h3 :: HTML h => [h] -> h
h3 = hStruct "h3"

--- Header 4
h4 :: HTML h => [h] -> h
h4 = hStruct "h4"

--- Header 5
h5 :: HTML h => [h] -> h
h5 = hStruct "h5"

--- Header 6
h6 :: HTML h => [h] -> h
h6 = hStruct "h6"

--- Paragraph
par :: HTML h => [h] -> h
par = hStruct "p"

--- Section
section :: HTML h => [h] -> h
section = hStruct "section"

--- Header
header :: HTML h => [h] -> h
header = hStruct "header"

--- Footer
footer :: HTML h => [h] -> h
footer = hStruct "footer"

--- Emphasize
emphasize :: HTML h => [h] -> h
emphasize = hStruct "em"

--- Strong (more emphasized) text.
strong :: HTML h => [h] -> h
strong = hStruct "strong"

--- Boldface
bold :: HTML h => [h] -> h
bold = hStruct "b"

--- Italic
italic :: HTML h => [h] -> h
italic = hStruct "i"

--- Navigation
nav :: HTML h => [h] -> h
nav = hStruct "nav"

--- Program code
code :: HTML h => [h] -> h
code = hStruct "code"

--- Centered text
center :: HTML h => [h] -> h
center = hStruct "center"

--- Blinking text
blink :: HTML h => [h] -> h
blink = hStruct "blink"

--- Teletype font
teletype :: HTML h => [h] -> h
teletype = hStruct "tt"

--- Unformatted input, i.e., keep spaces and line breaks and
--- don't quote special characters.
pre :: HTML h => [h] -> h
pre = hStruct "pre"

--- Verbatim (unformatted), special characters (&lt;,&gt;,&amp;,&quot;)
--- are quoted.
verbatim :: HTML h => String -> h
verbatim s = hStruct "pre" [htmlText (htmlQuote s)]

--- Address
address :: HTML h => [h] -> h
address = hStruct "address"

--- Hypertext reference
href :: HTML h => String -> [h] -> h
href ref = htmlStruct "a" [("href",ref)]

--- An anchored text with a hypertext reference inside a document.
anchor :: HTML h => String -> [h] -> h
anchor anc = htmlStruct "span" [("id",anc)]

--- Unordered list.
--- @param items - the list items where each item is a list of HTML expressions
ulist :: HTML h => [[h]] -> h
ulist items = hStruct "ul" (map litem items)

--- An unordered list with classes for the entire list and the list elements.
--- The class annotation will be ignored if it is empty.
--- @param listclass - the class for the entire list structure
--- @param itemclass - the class for the list items
--- @param items - the list items where each item is a list of HTML expressions
ulistWithClass :: HTML h => String -> String -> [[h]] -> h
ulistWithClass listclass itemclass items =
  hStruct "ul" (map litemWC items) `addClass` listclass
 where
  litemWC i = litem i `addClass` itemclass

--- An unordered list with classes for the entire list
--- individual classes for the list elements.
--- The class annotation will be ignored if it is empty.
--- @param listclass - the class for the entire list structure
--- @param classitems - the list items together with their classes
ulistWithItemClass :: HTML h => String -> [(String,[h])] -> h
ulistWithItemClass listclass classeditems =
  hStruct "ul" (map litemWC classeditems) `addClass` listclass
 where
  litemWC (c,i) = litem i `addClass` c

--- Ordered list.
--- @param items - the list items where each item is a list of HTML expressions
olist :: HTML h => [[h]] -> h
olist items = hStruct "ol" (map litem items)

--- An ordered list with classes for the entire list and the list elements.
--- The class annotation will be ignored if it is empty.
--- @param listclass - the class for the entire list structure
--- @param itemclass - the class for the list items
--- @param items - the list items where each item is a list of HTML expressions
olistWithClass :: HTML h => String -> String -> [[h]] -> h
olistWithClass listclass itemclass items =
  hStruct "ol" (map litemWC items) `addClass` listclass
 where
  litemWC i = litem i `addClass` itemclass

--- An ordered list with classes for the entire list
--- individual classes for the list elements.
--- The class annotation will be ignored if it is empty.
--- @param listclass - the class for the entire list structure
--- @param classitems - the list items together with their classes
olistWithItemClass :: HTML h => String -> [(String,[h])] -> h
olistWithItemClass listclass classeditems =
  hStruct "ol" (map litemWC classeditems) `addClass` listclass
 where
  litemWC (c,i) = litem i `addClass` c

--- A single list item (usually not explicitly used)
litem :: HTML h => [h] -> h
litem = hStruct "li"

--- Description list
--- @param items - a list of (title/description) pairs (of HTML expressions)
dlist :: HTML h => [([h],[h])] -> h
dlist items = hStruct "dl" (concatMap ditem items)
 where
  ditem (hexps1,hexps2) = [htmlStruct "dt" [] hexps1,
                           htmlStruct "dd" [] hexps2]

--- Table with a matrix of items where each item is a list of HTML expressions.
table :: HTML h => [[[h]]] -> h
table = hStruct "table" . map (\row -> hStruct "tr" (map (hStruct "td") row))

--- Table with a matrix of items (each item is a list of HTML expressions)
--- with classes for the entire table, each row, and each data element.
--- The class annotation will be ignored if it is empty.
--- @param tableclass - the class for the entire table structure
--- @param rowclass   - the class for the table rows
--- @param dataclass  - the class for the table data items
--- @param items - the matrix of table items where each item is a
---                list of HTML expressions
tableWithClass :: HTML h => String -> String -> String -> [[[h]]] -> h
tableWithClass tableclass rowclass dataclass items =
 hStruct "table"
   (map (\row -> hStruct "tr"
                   (map (\d -> hStruct "td" d `addClass` dataclass) row)
                   `addClass` rowclass)
        items) `addClass` tableclass

--- Similar to `table` but introduces header tags for the first row.
headedTable :: HTML h => [[[h]]] -> h
headedTable = hStruct "table" . headedItems
 where
  headedItems []         = []
  headedItems (row:rows) = hStruct "tr" (map (hStruct "th") row) :
                           map (\r -> hStruct "tr" (map (hStruct "td") r)) rows

--- Horizontal rule
hrule :: HTML h => h
hrule = hStruct "hr" []

--- Break a line
breakline :: HTML h => h
breakline = hStruct "br" []

--- Image
--- @param src - the URL of the image
--- @param alt - the alternative text shown instead of the image
image :: HTML h => String -> String -> h
image src alt = htmlStruct "img" [("src",src),("alt",htmlQuote alt)] []


-------------- styles and document structuring:
--- Defines a style sheet to be used in this HTML document.
--- @param css - a string in CSS format
styleSheet :: HTML h => String -> h
styleSheet css = htmlStruct "style" [("type","text/css")] [htmlText css]

--- Provides a style for HTML elements.
--- The style argument is the name of a style class defined in a
--- style definition (see `styleSheet`) or in an external
--- style sheet (see form and page parameters `FormCSS` and `PageCSS`).
--- @param st - name of a style class
--- @param hexps - list of HTML expressions
style :: HTML h => String -> [h] -> h
style st = htmlStruct "span" [("class",st)]

--- Provides a style for a basic text.
--- The style argument is the name of a style class defined in an
--- external style sheet.
--- @param st - name of a style class
--- @param txt - a string (special characters will be quoted)
textstyle :: HTML h => String -> String -> h
textstyle st txt = htmlStruct "span" [("class",st)] [htxt txt]

--- Provides a style for a block of HTML elements.
--- The style argument is the name of a style class defined in an
--- external style sheet. This element is used (in contrast to "style")
--- for larger blocks of HTML elements since a line break is placed
--- before and after these elements.
--- @param st - name of a style class
--- @param hexps - list of HTML expressions
blockstyle :: HTML h => String -> [h] -> h
blockstyle st = htmlStruct "div" [("class",st)]

--- Joins a list of HTML elements into a single HTML element.
--- Although this construction has no rendering, it is sometimes useful
--- for programming when several HTML elements must be put together.
--- @param hexps - list of HTML expressions
inline :: HTML h => [h] -> h
inline = hStruct "span"

--- Joins a list of HTML elements into a block.
--- A line break is placed before and after these elements.
--- @param hexps - list of HTML expressions
block :: HTML h => [h] -> h
block = hStruct "div"

--- A hidden field to pass a value referenced by a fixed name.
--- This function should be used with care since it may cause
--- conflicts with the CGI-based implementation of this library.
hiddenField :: HTML h => String -> String -> h
hiddenField name value =
  htmlStruct "input" [("type","hidden"),("name",name),("value",value)] []

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
--- 
--- Since form elements can not be nested, see
--- [HTML](https://html.spec.whatwg.org/multipage/forms.html#the-form-element),
--- the form element itself is a static HTML expression.
formElem :: HtmlFormDef a -> BaseHtml
formElem formspec = BaseAction formAction
 where
  formAction = do
    urlparam <- getUrlParameter
    he       <- genInitForm formspec
    return $
       HtmlStruct "form" [("method", "post"), ("action", '?' : urlparam)]
         (hiddenField "FORMID" (formDefId formspec) : fst (instCgiRefs he 0))

--- A button to submit a form with a label string and an event handler.
button :: String -> HtmlHandler -> HtmlExp
button label handler
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlEvent cref handler
              (HtmlStruct "input" [("type","submit"), ("name",ref),
                                   ("value",htmlQuote label)] [])
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
  = HtmlEvent cref handler
       (HtmlStruct "input" [("type","image"),("name",ref),("src",src)] [])
 where
  cref,ref free

--- Input text field with a reference and an initial contents
textField :: CgiRef -> String -> HtmlExp
textField cref contents
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef cref
      (HtmlStruct "input" [("type","text"),("name",ref),
                           ("value",htmlQuote contents)] [])
 where ref free

--- Input text field (where the entered text is obscured) with a reference
password :: CgiRef -> HtmlExp
password cref
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef cref
       (HtmlStruct "input" [("type","password"),("name",ref)] [])
 where
   ref free

--- Input text area with a reference, height/width, and initial contents
textArea :: CgiRef -> (Int,Int) -> String -> HtmlExp
textArea cref (height,width) contents
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef cref
       (HtmlStruct "textarea" [("name",ref),
                                ("rows",show height),("cols",show width)]
                               [htxt contents])
 where
   ref free

--- A checkbox with a reference and a value.
--- The value is returned if checkbox is on, otherwise "" is returned.
checkBox :: CgiRef -> String -> HtmlExp
checkBox cref value
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef cref
       (HtmlStruct "input" [("type","checkbox"),("name",ref),
                            ("value",htmlQuote value)] [])
 where
   ref free

--- A checkbox that is initially checked with a reference and a value.
--- The value is returned if checkbox is on, otherwise "" is returned.
checkedBox :: CgiRef -> String -> HtmlExp
checkedBox cref value
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef cref
       (HtmlStruct "input" [("type","checkbox"),("name",ref),
                            ("value",htmlQuote value),("checked","checked")] [])
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
  = HtmlCRef cref
       (HtmlStruct "input" [("type","radio"),("name",ref),
                            ("value",htmlQuote value),("checked","yes")] [])
 where
   ref free

--- A main button of a radio (initially "off") with a reference and a value.
--- The value is returned of this button is on.
radioMainOff :: CgiRef -> String -> HtmlExp
radioMainOff cref value
  | cref =:= CgiRef ref -- instantiate cref argument
  = HtmlCRef cref
       (HtmlStruct "input" [("type","radio"),("name",ref),
                            ("value",htmlQuote value)] [])
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
  = HtmlCRef cref
       (HtmlStruct "select" [("name",ref)]
         ((concat . map (\(n,v)->[HtmlStruct "option" [("value",v)] [htxt n]]))
          menue))
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
  = HtmlCRef cref (HtmlStruct "select" [("name",ref)] (selOption sellist sel))
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
  = HtmlCRef cref
             (HtmlStruct "select" [("name",ref),("multiple","multiple")]
                                   (map selOption sellist))
 where
   ref free

   selOption (n,v,flag) =
      HtmlStruct "option"
        ([("value",v)] ++ if flag then [("selected","selected")] else [])
        [htxt n]

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
addAttr :: HTML h => h -> (String,String) -> h
addAttr hexp attr = addAttrs hexp [attr]

--- Adds a list of attributes (name/value pair) to an HTML element.
addAttrs :: HTML h => h -> Attrs -> h
addAttrs h newattrs = updAttrs (++newattrs) h

--- Adds a class attribute to an HTML element
--- (if the class attribute is not empty).
addClass :: HTML h => h -> String -> h
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
showBaseHtmls :: [BaseHtml] -> String
showBaseHtmls hexps = showsBaseHtmls 0 hexps ""

-- is this a tag where a line break can be safely added?
tagWithLn :: String -> Bool
tagWithLn t = t/="" &&
              t `elem` ["br","p","li","ul","ol","dl","dt","dd","hr",
                        "h1","h2","h3","h4","h5","h6","div",
                        "html","title","head","body","link","meta","script",
                        "form","table","tr","td"]

--- Transforms a single HTML expression into string representation.
showBaseHtml :: BaseHtml -> String
showBaseHtml hexp = showsBaseHtml 0 hexp ""

--- HTML tags that have no end tag in HTML:
noEndTags :: [String]
noEndTags = ["img","input","link","meta"]

showsBaseHtml :: Int -> BaseHtml -> ShowS
showsBaseHtml _ (BaseText s) = showString s
showsBaseHtml i (BaseStruct tag attrs hexps) =
  let maybeLn j = if tagWithLn tag then nl . showTab j else id
   in maybeLn i .
      (if null hexps && (null attrs || tag `elem` noEndTags)
         then showsHtmlOpenTag tag attrs "/>"
         else showsHtmlOpenTag tag attrs ">" . maybeLn (i+2) . showExps hexps .
              maybeLn i . showString "</" . showString tag . showChar '>'
      ) . maybeLn i
 where
  showExps = if tag=="pre"
               then concatS . map (showsBaseHtml 0)
               else showsBaseHtmls (i+2)
showsBaseHtml _ (BaseAction  _)      =
  error "HTML.Base.showsBaseHtml: BaseAction occurred"

showsBaseHtmls :: Int -> [BaseHtml] -> ShowS
showsBaseHtmls _ [] = id
showsBaseHtmls i (he:hes) = showsWithLnPrefix he . showsBaseHtmls i hes
 where
  showsWithLnPrefix hexp = let s = textOfBaseHtml hexp
                           in if s /= "" && isSpace (head s)
                                then nl . showTab i . showString (tail s)
                                else showsBaseHtml i hexp

  -- get the string contents of an HTML expression:
  textOfBaseHtml :: BaseHtml -> String
  textOfBaseHtml (BaseText   s)        = s
  textOfBaseHtml (BaseStruct _ _ _)    = ""
  textOfBaseHtml (BaseAction _)        = ""

showTab :: Int -> String -> String
showTab n = showString (take n (repeat ' '))

showsHtmlOpenTag :: String -> Attrs -> String -> ShowS
showsHtmlOpenTag tag attrs close =
  showChar '<' . showString tag .
  concatS (map attr2string attrs) . showString close
 where
    attr2string (attr,value) =
      showChar ' ' . showString attr .
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
  showBaseHtml (BaseStruct "html" htmlTagAttrs
                  [BaseStruct "head" []
                       ([BaseStruct "title" [] [BaseText (htmlQuote title)]] ++
                        concatMap pageParam2HTML params),
                   BaseStruct "body" bodyattrs html])
 where
  bodyattrs = [attr | (PageBodyAttr attr) <- params]

--- Translates page parameters into HTML expressions.
--- Used to show HTML pages.
pageParam2HTML :: PageParam -> [BaseHtml]
pageParam2HTML (PageEnc enc) =
   [BaseStruct "meta" [("http-equiv","Content-Type"),
                       ("content","text/html; charset="++enc)] []]
pageParam2HTML (PageCookie _ _ _) = [] -- cookies are differently processed
pageParam2HTML (PageCSS css) =
   [BaseStruct "link" [("rel","stylesheet"),("type","text/css"),("href",css)]
               []]
pageParam2HTML (HttpHeader _ _) = [] -- page headers are differently processed
pageParam2HTML (PageJScript js) =
   [BaseStruct "script" [("type","text/javascript"),("src",js)] []]
pageParam2HTML (PageMeta attrs) = [BaseStruct "meta" attrs []]
pageParam2HTML (PageLink attrs) = [BaseStruct "link" attrs []]
pageParam2HTML (PageHeadInclude hexp) = [hexp]
pageParam2HTML (PageBodyAttr _) = [] --these attributes are separately processed

--- Standard header for generated HTML pages.
htmlPrelude :: String
htmlPrelude = "<!DOCTYPE html>\n"

--- Standard attributes for element "html".
htmlTagAttrs :: Attrs
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

--- Translates an URL encoded string into equivalent ASCII string.
urlencoded2string :: String -> String
urlencoded2string [] = []
urlencoded2string (c:cs)
  | c == '+'  = ' ' : urlencoded2string cs
  | c == '%'  = chr (maybe 0 fst (readHex (take 2 cs)))
                 : urlencoded2string (drop 2 cs)
  | otherwise = c : urlencoded2string cs

--- Translates arbitrary strings into equivalent URL encoded strings.
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

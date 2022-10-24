------------------------------------------------------------------------------
--- This module contains a very simple parser for HTML documents.
---
--- @author Michael Hanus
--- @version October 2022
------------------------------------------------------------------------------
{-# OPTIONS_CYMAKE -Wno-incomplete-patterns #-}

module HTML.Parser
  ( StaticHtml(..), fromStaticHtml, toStaticHtml
  , readHtmlFile, readFileWithHtml, parseHtmlString, parseHtml )
 where

import Data.Char
import HTML.Base

------------------------------------------------------------------------------
--- The data type to represent HTML expressions parsed from a string.
--- It is similar to type `HTML.Base.BaseHtml` except that there
--- is no constructor `BaseAction` so this type has instances
--- for standard classes like `Eq`, `Data`, `Read`, and `Show`.
--- @cons HText s         - a text string without any further structure
--- @cons HStruct t as hs - a structure with a tag, attributes, and
---                         HTML expressions inside the structure
data StaticHtml =
    HText   String
  | HStruct String Attrs [StaticHtml]
 deriving (Eq, Read, Show)

--- Updates the attributes in a basic HTML expression.
updStaticAttrs :: (Attrs -> Attrs) -> StaticHtml -> StaticHtml
updStaticAttrs _ (HText s)                 = HText s
updStaticAttrs f (HStruct tag attrs hexps) = HStruct tag (f attrs) hexps

--- Transforms a `StaticHtml` expression into a `BaseHtml` expression.
fromStaticHtml :: StaticHtml -> BaseHtml
fromStaticHtml (HText s)            = BaseText s
fromStaticHtml (HStruct t attrs hs) = BaseStruct t attrs (map fromStaticHtml hs)

--- Transforms a `BaseHtml` expression into a `StaticHtml` expression
--- provided that `BaseAction` constructors do not occur (otherwise,
--- an error is raised).
toStaticHtml :: BaseHtml -> StaticHtml
toStaticHtml (BaseText s)           = HText s
toStaticHtml (BaseStruct t atts hs) = HStruct t atts (map toStaticHtml hs)
toStaticHtml (BaseAction _)         =
  error "HTML.Parser.toStaticHtml: BaseAction occurred in base HTML expression"

--- The type of static HTML expressions is an instance of class `HTML`.
instance HTML StaticHtml where
  htmlText   = HText
  htmlStruct = HStruct
  updAttrs   = updStaticAttrs
  toBaseHtml = fromStaticHtml

------------------------------------------------------------------------------

--- Reads a file with HTML text and returns the corresponding HTML expressions.
--- @param file - the name of a file containing HTML text
--- @return a list of HTML expressions (if the file contains exactly one
---         HTML document, this list should contain exactly one element)
readHtmlFile :: String -> IO [BaseHtml]
readHtmlFile file = readFile file >>= return . parseHtmlString

--- Reads a file containing HTML text and returns the corresponding
--- `StaticHtml` expressions.
--- @param file - the name of a file containing HTML text
--- @return a list of `StaticHtml` expressions (if the file contains exactly
---         one HTML document, this list should contain exactly one element)
readFileWithHtml :: String -> IO [StaticHtml]
readFileWithHtml file = readFile file >>= return . parseHtml

------------------------------------------------------------------------------
--- Transforms an HTML string into a list of `BaseHTML` expressions.
--- If the HTML string is a well structured document, the list
--- of HTML expressions should contain exactly one element.
parseHtmlString :: String -> [BaseHtml]
parseHtmlString = map fromStaticHtml . parseHtml

--- Transforms an HTML string into a list of `StaticHTML` expressions.
--- If the HTML string is a well structured document, the list
--- of HTML expressions should contain exactly one element.
parseHtml :: String -> [StaticHtml]
parseHtml s = reverse (parseHtmlTokens [] (scanHtmlString s))

------------------------------------------------------------------------------

--- The data type for representing HTML tokens.
data HtmlToken = HTText String | HTElem String [(String,String)]

-- parse a list of HTML tokens into list of HTML expressions:
-- (first argument "helems" is a stack of already read tokens)
parseHtmlTokens :: [StaticHtml] -> [HtmlToken] -> [StaticHtml]
parseHtmlTokens helems []              = helems
parseHtmlTokens helems (HTText s : hs) =
 parseHtmlTokens (HText s : helems) hs
parseHtmlTokens helems (HTElem (t:ts) args : hs) =
  if t == '/'
    then let (structargs,elems,rest) = splitHtmlElems ts helems
         in parseHtmlTokens ([HStruct ts structargs elems] ++ rest) hs
    else parseHtmlTokens (HStruct (t:ts) args [] : helems) hs


-- split the HTML token stack up to a particular token:
splitHtmlElems :: String -> [StaticHtml]
               -> ([(String,String)],[StaticHtml],[StaticHtml])
splitHtmlElems _ [] = ([],[],[])
splitHtmlElems tag (HText s : hs) =
  let (largs,elems,rest) = splitHtmlElems tag hs
  in (largs, elems ++ [HText s], rest)
splitHtmlElems tag (HStruct s args cont@(_:_) : hs) =
  let (largs,elems,rest) = splitHtmlElems tag hs
  in (largs, elems ++ [HStruct s args cont], rest)
splitHtmlElems tag (HStruct s args []: hs) =
  if tag==s
    then (args,[],hs)
    else let (largs,elems,rest) = splitHtmlElems tag hs
         in  (largs, elems ++ [HStruct s args []], rest)


-- scan an HTML string into list of HTML tokens:
scanHtmlString :: String -> [HtmlToken]
scanHtmlString s = scanHtml s
 where
  scanHtml []     = []
  scanHtml (c:cs) =
    if c=='<'
    then if take 3 cs == "!--"
         then scanHtmlComment cs
         else if take 4 (map toLower cs) == "pre>"
              then scanHtmlPre "" (skipFirstNewLine (drop 4 cs))
              else scanHtmlElem [] cs
    else let (initxt,remtag) = break (=='<') (c:cs)
          in HTText initxt : scanHtml remtag

-- scan an HTML element
scanHtmlElem :: String -> String -> [HtmlToken]
scanHtmlElem ct [] = [HTText ("&lt;"++ct)] -- incomplete element
scanHtmlElem ct (c:cs)
  | c=='>'    = (if null ct
                 then HTText "&lt;&gt;" -- invalid HTML, but we accept it...
                 else HTElem ct [])  : scanHtmlString cs
  | isSpace c =
     if null ct
     then HTText "&lt; " : scanHtmlString cs -- invalid HTML, but we accept it...
     else let (args,rest) = splitAtElement (=='>') (dropWhile isSpace cs)
              revargs = reverse args
           in if null args || head revargs /= '/'
              then HTElem ct (string2args args) : scanHtmlString rest
              else HTElem ct (string2args (reverse (tail revargs)))
                    : HTElem ('/':ct) [] : scanHtmlString rest
  | c=='/' && head cs == '>' = HTElem ct [] : HTElem ('/':ct) []
                                           : scanHtmlString (tail cs)
  | otherwise = scanHtmlElem (ct++[toLower c]) cs

-- scan an HTML comment
scanHtmlComment :: String -> [HtmlToken]
scanHtmlComment [] = []
scanHtmlComment (c:cs) =
  if c=='-' && take 2 cs == "->"
  then scanHtmlString (drop 2 cs)
  else scanHtmlComment cs

-- scan an HTML preformatted element
scanHtmlPre :: String -> String -> [HtmlToken]
scanHtmlPre _ [] = []  -- errorneous incomplete element
scanHtmlPre pre (c:cs) =
  if c=='<' && take 5 (map toLower cs) == "/pre>"
  then HTElem "pre" [] : HTText (reverse pre) : HTElem "/pre" []
       : scanHtmlString (drop 5 cs)
  else scanHtmlPre (c:pre) cs

-- split a string into blank separated list of strings:
string2args :: String -> [(String,String)]
string2args [] = []
string2args (c:cs) =
   let (arg1,rest) = splitAtElement isSpace (c:cs)
   in  deleteApo (splitAtElement (=='=') arg1)
        : string2args (dropWhile isSpace rest)

deleteApo :: (String,String) -> (String,String)
deleteApo (tag,[]) = (map toLower tag,[])
deleteApo (tag,c:cs) | c=='"'    = (map toLower tag, deleteLastApo cs)
                     | c=='\''   = (map toLower tag, deleteLastApo cs)
                     | otherwise = (map toLower tag, c:cs)

deleteLastApo :: String -> String
deleteLastApo [] = []
deleteLastApo [c] = if c=='"' || c=='\'' then [] else [c]
deleteLastApo (c1:c2:cs) = c1 : deleteLastApo (c2:cs)


-- split a list at the first element satisfying a predicate:
splitAtElement :: (a -> Bool) -> [a] -> ([a],[a])
splitAtElement _ [] = ([],[])
splitAtElement p (c:cs) =
  if p c then ([],cs)
         else let (first,rest) = splitAtElement p cs in (c:first,rest)

skipFirstNewLine :: String -> String
skipFirstNewLine [] = []
skipFirstNewLine (c:cs) = 
  if c=='\n' then cs
             else if isSpace c then skipFirstNewLine cs else c:cs

-- end of HTML parser

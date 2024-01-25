------------------------------------------------------------------------------
-- Example for CGI programming in Curry:
-- A web page with a form containing a text input field and two submit buttons
-- to reverse and duplicate the input string.
------------------------------------------------------------------------------

module RevDup ( main, revDupForm ) where

import HTML.Base

-- Example: a form with a text input field and two submit buttons.
revDupForm :: HtmlFormDef ()
revDupForm = simpleFormDefWithID "RevDup.revDupForm"
  [ htxt "Enter a string: ", textField ref ""
  , hrule
  , button "Reverse string"   revHandler
  , button "Duplicate string" dupHandler
  ]
 where
  ref free

  revHandler env = return $ page "Answer"
    [ h1 [ htxt $ "Reversed input: " ++ reverse (env ref)] ]

  dupHandler env = return $ page "Answer"
    [ h1 [ htxt $ "Duplicated input: " ++ env ref ++ env ref] ]

-- main HTML page containing the form
main :: IO HtmlPage
main = return $ page "Question"
  [ h1 [htxt "This is an example form"], formElem revDupForm ]

-- Install with:
-- > curry2cgi -o ~/public_html/cgi-bin/revdup.cgi RevDup

-------------------------------------------------------------------------

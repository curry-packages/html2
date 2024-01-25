------------------------------------------------------------------------------
-- Example for CGI programming in Curry:
-- A form with a text input field and two submit buttons to reverse
-- and duplicate the input string together with a "time" form imported
-- from another module.
------------------------------------------------------------------------------

import HTML.Base
import TimeForm ( timeForm )

-- Example: a form with a text input field and two submit buttons.
revDupForm :: HtmlFormDef ()
revDupForm = simpleFormDef
  [ htxt "Enter a string: ", textField ref ""
  , hrule
  , button "Reverse string" revHandler
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
  [ h1 [htxt "This is an example form"]
  , formElem revDupForm
  , hrule
  , formElem timeForm
  ]

-- Install with (note that we need to include forms from `TimeForm`!):
-- > curry2cgi -i TimeForm -o ~/public_html/cgi-bin/revduptime.cgi RevDupTime

-------------------------------------------------------------------------

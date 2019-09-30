------------------------------------------------------------------------------
-- Example for CGI programming in Curry:
-- A form with a text input field and two submit buttons to reverse
-- and duplicate the input string together with a "time" form imported
-- from another module.
------------------------------------------------------------------------------

import Time
import HTML.Base
import TimeForm ( timeForm )

-- Example: a form with a text input field and two submit buttons.
revDupForm :: HtmlFormDef String
revDupForm = HtmlFormDef "RevDupTime.revDupForm" (return "") formHtml
 where
  formHtml _ =
    [ htxt "Enter a string: ", textfield ref ""
    , hrule
    , formButton "Reverse string" revHandler
    , formButton "Duplicate string" dupHandler
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
  , formExp revDupForm
  , hrule
  , formExp timeForm
  ]

-- Install with (note that we need to include forms from `TimeForm`!):
-- > cypm exec curry2cgi -i TimeForm -o ~/public_html/cgi-bin/revduptime.cgi RevDupTime

-------------------------------------------------------------------------

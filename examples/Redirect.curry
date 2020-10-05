------------------------------------------------------------------------------
-- Example for a form to redirect to another URL typed in a text input field.
-- The example exploits the HTTP header field `Location` for redirection.
------------------------------------------------------------------------------

import HTML.Base

-- Example: a form with a text input field and two submit buttons.
redirectForm :: HtmlFormDef ()
redirectForm = simpleFormDef
  [ htxt "Enter a URL: ", textField ref "http://www.google.com"
  , hrule
  , button "Go to the URL" (\env -> return $ redirectPage (env ref))
  ]
 where
  ref free

-- main HTML page containing the form
main :: IO HtmlPage
main = return $ page "Redirection"
  [ h1 [htxt "This is simple example for redirection"],
    formElem redirectForm ]

-- Install with:
-- > cypm exec curry2cgi -o ~/public_html/cgi-bin/redirect.cgi Redirect

-------------------------------------------------------------------------

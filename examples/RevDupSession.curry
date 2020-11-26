------------------------------------------------------------------------------
-- Example for CGI programming in Curry:
-- A web page with a form containing a text input field and two submit buttons
-- to reverse and duplicate the input string.
-- Here we use session data to store the string typed into the input
-- field in order to use it for the subsequent form.
------------------------------------------------------------------------------

import Global

import HTML.Base
import HTML.Session

--- The data stored in a session is the string typed into the input field.
rdInput :: Global (SessionStore String)
rdInput = global emptySessionStore (Persistent (inSessionDataDir "rdInput"))

-- Example: a form with a text input field and two submit buttons.
revDupForm :: HtmlFormDef String
revDupForm = formDef readInfo formHtml
 where
  readInfo = getSessionData rdInput ""

  formHtml s =
    [ htxt "Enter a string: ", textField ref s
    , hrule
    , button "Reverse string" revHandler
    , button "Duplicate string" dupHandler
    ]
   where
    ref free

    revHandler env = do
      putSessionData rdInput (env ref)
      withSessionCookie $ page "Answer"
        [ h1 [ htxt $ "Reversed input: " ++ reverse (env ref)], hrule
        , formElem revDupForm ]

    dupHandler env = do
      putSessionData rdInput (env ref)
      withSessionCookie $ page "Answer"
        [ h1 [ htxt $ "Duplicated input: " ++ env ref ++ env ref], hrule
        , formElem revDupForm ]

-- main HTML page containing the form
main :: IO HtmlPage
main = withSessionCookieInfo $ page "Question"
  [ h1 [htxt "This is an example form"]
  , formElem revDupForm
  ]

-- Install with:
-- > cypm exec curry2cgi -o ~/public_html/cgi-bin/revdup.cgi RevDupSession

-------------------------------------------------------------------------

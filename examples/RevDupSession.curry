------------------------------------------------------------------------------
-- Example for CGI programming in Curry:
-- A web page with a form containing a text input field and two submit buttons
-- to reverse and duplicate the input string.
-- Here we use session data to store the string typed into the input
-- field in order to use it for a subsequent form.
------------------------------------------------------------------------------

import FilePath ( (</>) )
import Global

import HTML.Base
import HTML.Session

--- The data stored in a session is the string typed into the input field.
rdInput :: Global (SessionStore String)
rdInput = global emptySessionStore (Persistent ("." </> "rdInput"))

-- Example: a form with a text input field and two submit buttons.
revDupForm :: HtmlFormDef String
revDupForm = HtmlFormDef "RevDupSession.revDupForm" readInfo formHtml
 where
  readInfo = getSessionData rdInput ""

  formHtml s =
    [ htxt "Enter a string: ", textfield ref s
    , hrule
    , formButton "Reverse string" revHandler
    , formButton "Duplicate string" dupHandler
    ]
   where
    ref free

    revHandler env = do
      putSessionData rdInput (env ref)
      withSessionCookie $ page "Answer"
        [ h1 [ htxt $ "Reversed input: " ++ reverse (env ref)], hrule
        , formExp revDupForm ]

    dupHandler env = do
      putSessionData rdInput (env ref)
      withSessionCookie $ page "Answer"
        [ h1 [ htxt $ "Duplicated input: " ++ env ref ++ env ref], hrule
        , formExp revDupForm ]

-- main HTML page containing the form
main :: IO HtmlPage
main = withSessionCookie $ page "Question"
  [ h1 [htxt "This is an example form"]
  , formExp revDupForm
  ]

-- Install with:
-- > cypm exec curry2cgi -o ~/public_html/cgi-bin/revdup.cgi RevDupSession

-------------------------------------------------------------------------

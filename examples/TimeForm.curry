------------------------------------------------------------------------------
-- Example for CGI programming in Curry:
-- a form with button to show the current time
------------------------------------------------------------------------------

module TimeForm where

import Data.Time -- from package `time`
import HTML.Base

-- Example: a form with button to show the current time.
timeForm :: HtmlFormDef ()
timeForm = simpleFormDef [ button "Show time" timeHandler ]
 where
  timeHandler _ = do
    ltime <- getLocalTime
    return $ page "Answer"
      [ h1 [ htxt $ "Local time: " ++ calendarTimeToString ltime ]
      , hrule
      , formElem timeForm ]

-- main HTML page containing the form
main :: IO HtmlPage
main = return $ page "Time"
  [ h1 [htxt "This is an example form to show the current time"]
  , hrule
  , formElem timeForm
  ]

-- Install with:
-- > curry2cgi -o ~/public_html/cgi-bin/time.cgi TimeForm

-------------------------------------------------------------------------

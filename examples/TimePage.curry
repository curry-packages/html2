------------------------------------------------------------------------------
-- Example for CGI programming in Curry:
-- a dynamic HTML page  to show the current time
------------------------------------------------------------------------------

import Data.Time -- from package `time`
import HTML.Base

-- Example: an HTML page to show the current time.
timePage :: IO HtmlPage
timePage = do
  time <- getLocalTime
  return $ headerPage "Current Server Time"
    [htxt $ "Current time: " ++ calendarTimeToString time]

main :: IO HtmlPage
main = timePage

-- Install with:
-- > curry2cgi -o ~/public_html/cgi-bin/time.cgi TimePage


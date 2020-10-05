------------------------------------------------------------------------------
-- Example for CGI programming in Curry:
-- a dynamic HTML page  to show the current time
------------------------------------------------------------------------------

import Time
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
-- > cypm exec curry2cgi -o ~/public_html/cgi-bin/time.cgi TimePage


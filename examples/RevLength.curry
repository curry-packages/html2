------------------------------------------------------------------------------
-- Example for CGI programming in Curry:
-- A web page with a form containing a text input field and two submit buttons
-- to compute the length of text or to reverse the input string.
------------------------------------------------------------------------------

import HTML.Base

-- Example: a form with a text input field and two submit buttons.
lengthRevForm :: [HtmlExp]
lengthRevForm =
  [htxt "Enter a string: ", textField ref "",
   button "Length" lengthHandler, button "Reverse" revHandler]
 where
  ref free

  lengthHandler env = return $ page "Answer"
    [h1 [htxt $ "String length: " ++ show (length (env ref))]]

  revHandler env = return $ page "Answer"
    [h1 [htxt $ "Reversed input: " ++ reverse (env ref)]]

-- Example: a form with a text input field and two submit buttons.
lengthRevFormDef :: HtmlFormDef ()
lengthRevFormDef = simpleFormDef lengthRevForm

-- main HTML page containing the form
main :: IO HtmlPage
main = return $ headerPage "String input" [formElem lengthRevFormDef]

-- Install with:
-- > cypm exec curry2cgi -o ~/public_html/cgi-bin/revlen.cgi RevLength

-------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Example for CGI programming in Curry:
-- A web page with a form containing a text input field and a submit button
-- to compute the length of the text together with a recursive form.
------------------------------------------------------------------------------

import HTML.Base

-- Example: a recursive form to compute the length of a string.
lengthForm :: HtmlFormDef ()
lengthForm = simpleFormDef
  [htxt "Enter a string: ", textField ref "", button "Length"
    (\env -> return $ page "Answer"
               [h1 [htxt $ "Length: " ++ show (length (env ref))],
                hrule, formElem lengthForm])]
 where ref free

-- main HTML page containing the form
main :: IO HtmlPage
main = return $ headerPage "String length" [formElem lengthForm]

-- Install with:
-- > curry2cgi -o ~/public_html/cgi-bin/length.cgi Length

-------------------------------------------------------------------------

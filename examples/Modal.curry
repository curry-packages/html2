------------------------------------------------------------------------------
-- Example for a form with a modal dialog with Bootstrap style.
------------------------------------------------------------------------------

import HTML.Base
import HTML.Styles.Bootstrap4

-- Example: a page with a button to launch the modal dialog.
-- The modal dialog is also shown when the page is loaded
-- (see call to `scriptShowModal`).
main :: IO HtmlPage
main = return $ mainPage "Redirection"
  [ h1 [htxt "This is simple example for a modal dialog"]]
  [ hrule
  , cookieModal, modalLaunchPrimButton "cookieModal" "Launch modal dialog"
  , hrule ] `addPageBody` scriptShowModal "cookieModal"

cookieModal :: HTML h => h
cookieModal = stdModal True "cookieModal"
  [htxt "Cookie Information"]
  [htxt "This web site uses cookies for navigation and user inputs."]
  [htxt "Close to proceed: ", modalClosePrimButton "Close"]

-------------------------------------------------------------------------
-- A basic Bootstrap page.
-- It is assumed that Bootstrap style are available in directory `../bt4`.
mainPage :: String -> [BaseHtml] -> [BaseHtml] -> HtmlPage
mainPage title header contents =
  bootstrapPage favIcon cssIncludes jsIncludes title homePage
                [] [] 0 [] header contents []
 where
  favIcon = "../bt4/img/favicon.ico"

  cssIncludes =
    map (\n -> "../bt4/css/" ++ n ++ ".css") ["bootstrap.min"]

  jsIncludes = ["https://code.jquery.com/jquery-3.4.1.slim.min.js",
                "../bt4/js/bootstrap.bundle.min.js"]

  homePage = ("http://www.curry-lang.org",[htxt "Curry"])

-------------------------------------------------------------------------

-- Install with:
-- > curry2cgi -o ~/public_html/cgi-bin/modal.cgi Modal

-------------------------------------------------------------------------

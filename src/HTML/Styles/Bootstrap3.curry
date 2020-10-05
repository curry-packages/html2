----------------------------------------------------------------------------
--- This library contains some operations to generate web pages
--- rendered with [Bootstrap](http://twitter.github.com/bootstrap/)
---
--- @author Michael Hanus
--- @version September 2020
----------------------------------------------------------------------------

module HTML.Styles.Bootstrap3
 ( bootstrapPage, titledSideMenu
 , defaultButton, smallButton, primButton
 , hrefDefaultButton, hrefSmallButton, hrefPrimButton
 , hrefInfoButton, hrefSuccessButton, hrefWarningButton, hrefDangerButton
 , hrefButton, hrefBlock, hrefInfoBlock
 , glyphicon, homeIcon, userIcon, loginIcon, logoutIcon
 ) where

import HTML.Base

----------------------------------------------------------------------------
--- An HTML page rendered with bootstrap.
--- @param rootdir - the root directory to find styles, fonts, scripts
---                  (in subdirectories `css`, `fonts`, `js`) and the
---                  `favicon.ico`
---                  of the root) and images (in subdirectory `img` of the root)
--- @param styles - the style files to be included (typically,
---                 `bootstrap` and `bootstrap-responsive`), stored in
---                 `rootdir/css` with suffix `.css`)
--- @param title - the title of the form
--- @lefttopmenu - the menu shown in the left side of the top navigation bar
--- @righttopmenu - the menu shown in the right side of the top navigation bar
---                 (could be empty)
--- @param columns - number of columns for the left-side menu
---                  (if columns==0, then the left-side menu is omitted)
--- @param sidemenu - the menu shown at the left-side of the main document
---                   (maybe created with 'titledSideMenu')
--- @param header   - the main header (rendered with jumbotron style)
--- @param contents - the main contents of the document
--- @param footer   - the footer of the document
bootstrapPage :: String -> [String] -> String -> (String,[BaseHtml])
              -> [[BaseHtml]] -> [[BaseHtml]] -> Int -> [BaseHtml] -> [BaseHtml]
              -> [BaseHtml] -> [BaseHtml] -> HtmlPage
bootstrapPage rootdir styles title brandurltitle lefttopmenu righttopmenu
              leftcols sidemenu header contents footer =
  HtmlPage title
           ([pageEnc "utf-8",responsiveView,icon] ++
             map (\n -> pageCSS (rootdir++"/css/"++n++".css")) styles)
           (bootstrapBody rootdir brandurltitle lefttopmenu righttopmenu
                          leftcols sidemenu header contents footer)
 where
  -- for a better view on handheld devices:
  responsiveView =
    pageMetaInfo [("name","viewport"),
                  ("content","width=device-width, initial-scale=1.0")]

  icon = pageLinkInfo [("rel","shortcut icon"),
                       ("href",rootdir++"/favicon.ico")]

--- Create body of HTML page. Used by bootstrapForm and bootstrapPage.
bootstrapBody ::
  HTML h => String -> (String,[h]) -> [[h]] -> [[h]] -> Int -> [h] -> [h]
         -> [h] -> [h] -> [h]
bootstrapBody rootdir brandurltitle lefttopmenu righttopmenu
              leftcols sidemenu header contents footerdoc =
  topNavigationBar brandurltitle lefttopmenu righttopmenu ++
  [blockstyle "container-fluid"
   ([blockstyle "row"
      (if leftcols==0
       then [blockstyle (bsCols 12)
              (headerRow ++ contents)]
       else [blockstyle (bsCols leftcols)
              [blockstyle "well nav-sidebar" sidemenu],
             blockstyle (bsCols (12-leftcols))
              (headerRow ++ contents)])] ++
     if null footerdoc
       then []
       else [hrule, footer footerdoc]),
   -- JavaScript includes placed at the end so page loads faster:
   htmlStruct "script" [("src",rootdir++"/js/jquery.min.js")] [],
   htmlStruct "script" [("src",rootdir++"/js/bootstrap.min.js")] []]
 where
  bsCols n = "col-sm-" ++ show n ++ " " ++ "col-md-" ++ show n
  
  -- header row:
  headerRow = if null header
                then []
                else [htmlStruct "header" [("class","jumbotron")] header]


-- Navigation bar at the top. The first argument is a header element
-- put at the left, the second and third arguments are the left
-- and right menus which will be collapsed if the page is two small.
topNavigationBar :: HTML h => (String,[h]) -> [[h]] -> [[h]] -> [h]
topNavigationBar (brandurl,brandtitle) leftmenu rightmenu =
  [blockstyle "navbar navbar-inverse navbar-fixed-top"
    [blockstyle "container-fluid"
      [blockstyle "navbar-header"
         [htmlStruct "button"
           [("type","button"),("class","navbar-toggle collapsed"),
            ("data-toggle","collapse"),("data-target","#topnavbar"),
            ("aria-expanded","false"),("aria-controls","topnavbar")]
           [textstyle "sr-only" "Toggle navigation",
            textstyle "icon-bar" "",
            textstyle "icon-bar" "",
            textstyle "icon-bar" ""],
          href brandurl brandtitle `addClass` "navbar-brand"],
        htmlStruct "div" [("id","topnavbar"),
                          ("class","collapse navbar-collapse")]
         ([ulist leftmenu `addClass` "nav navbar-nav"] ++
          if null rightmenu then []
          else [ulist rightmenu `addClass` "nav navbar-nav navbar-right"])]]]

-- Create a side menu containing a title and a list of items:
titledSideMenu :: HTML h => String -> [[h]] -> [h]
titledSideMenu title items =
  (if null title
     then []
     else [htmlStruct "small" [] [htxt title]]) ++
  [ulist items `addClass` "nav nav-sidebar"]

----------------------------------------------------------------------------
-- Some buttons:

--- Default input button.
defaultButton :: String -> HtmlHandler -> HtmlExp
defaultButton label handler =
  button label handler `addClass` "btn btn-default"

--- Small input button.
smallButton :: String -> HtmlHandler -> HtmlExp
smallButton label handler =
  button label handler `addClass` "btn btn-sm btn-default"

--- Primary input button.
primButton :: String -> HtmlHandler -> HtmlExp
primButton label handler =
  button label handler `addClass` "btn btn-primary"

--- Hypertext reference rendered as a default button.
hrefDefaultButton :: HTML h => String -> [h] -> h
hrefDefaultButton ref hexps =
  href ref hexps `addClass` "btn btn-default"

--- Hypertext reference rendered as a small button.
hrefSmallButton :: HTML h => String -> [h] -> h
hrefSmallButton ref hexps =
  href ref hexps `addClass` "btn btn-sm btn-default"

hrefButton :: HTML h => String -> [h] -> h
hrefButton = hrefSmallButton

--- Hypertext reference rendered as a primary button.
hrefPrimButton :: HTML h => String -> [h] -> h
hrefPrimButton ref hexps =
  href ref hexps `addClass` "btn btn-primary"

--- Hypertext reference rendered as an info button.
hrefInfoButton :: HTML h => String -> [h] -> h
hrefInfoButton ref hexps =
  href ref hexps `addClass` "btn btn-info"

--- Hypertext reference rendered as a success button.
hrefSuccessButton :: HTML h => String -> [h] -> h
hrefSuccessButton ref hexps =
  href ref hexps `addClass` "btn btn-success"

--- Hypertext reference rendered as a warning button.
hrefWarningButton :: HTML h => String -> [h] -> h
hrefWarningButton ref hexps =
  href ref hexps `addClass` "btn btn-warning"

--- Hypertext reference rendered as a danger button.
hrefDangerButton :: HTML h => String -> [h] -> h
hrefDangerButton ref hexps =
  href ref hexps `addClass` "btn btn-danger"

--- Hypertext reference rendered as a block level button.
hrefBlock :: HTML h => String -> [h] -> h
hrefBlock ref hexps =
  href ref hexps `addClass` "btn btn-sm btn-block"

--- Hypertext reference rendered as an info block level button.
hrefInfoBlock :: HTML h => String -> [h] -> h
hrefInfoBlock ref hexps =
  href ref hexps `addClass` "btn btn-info btn-block"

----------------------------------------------------------------------------
-- Some icons:

glyphicon :: HTML h => String -> h
glyphicon n = textstyle ("glyphicon glyphicon-"++n) ""

homeIcon :: HTML h => h
homeIcon   = glyphicon "home"

userIcon :: HTML h => h
userIcon   = glyphicon "user"

loginIcon :: HTML h => h
loginIcon  = glyphicon "log-in"

logoutIcon :: HTML h => h
logoutIcon = glyphicon "log-out"

----------------------------------------------------------------------------

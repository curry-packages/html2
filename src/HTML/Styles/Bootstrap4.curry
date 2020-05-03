----------------------------------------------------------------------------
--- This library contains some operations to generate web pages
--- rendered with [Bootstrap version 4](https://getbootstrap.com/).
---
--- @author Michael Hanus
--- @version May 2020
----------------------------------------------------------------------------

module HTML.Styles.Bootstrap4
 ( bootstrapPage, titledSideMenu
 , primButton, primSmButton, scndButton, scndSmButton, infoButton, infoSmButton
 , hrefPrimButton, hrefPrimSmButton, hrefScndButton, hrefScndSmButton
 , hrefInfoButton, hrefSuccButton, hrefWarnButton, hrefDangerButton
 , hrefInfoBlock, hrefPrimSmBlock, hrefScndSmBlock
 , hrefPrimBadge, hrefScndBadge, hrefSuccBadge, hrefWarnBadge
 , ehrefPrimBadge, ehrefScndBadge, ehrefSuccBadge, ehrefWarnBadge
 , hrefNav, hrefNavActive, ehrefNav, ehref
 , kbdInput
 ) where

import HTML.Base

----------------------------------------------------------------------------
--- An HTML page rendered with bootstrap with a fixed top navigation bar.
--- @param favicon   - the icon file `favicon.ico` (when empty not included)
--- @param styles    - the style files to be included (typically,
---                    `css/bootstrap.min.css`)
--- @param jsincludes - the JavaScript files to be included (typically,
---                     `.../jquery.js`, `js/bootstrap.min.js`)
--- @param title   - the title of the form
--- @param brand   - the brand shown top left (a URL/title pair)
--- @lefttopmenu   - the menu shown in the left side of the top navigation bar
--- @righttopmenu  - the menu shown in the right side of the top navigation bar
---                  (could be empty)
--- @param columns - number of columns for the left-side menu
---                  (if columns==0, then the left-side menu is omitted)
--- @param sidemenu - the menu shown at the left-side of the main document
---                   (maybe created with 'titledSideMenu')
--- @param header   - the main header (will be rendered with jumbotron style)
--- @param contents - the main contents of the document
--- @param footer   - the footer of the document
bootstrapPage :: String -> [String] -> [String] -> String -> (String,[HtmlExp])
              -> [[HtmlExp]] -> [[HtmlExp]] -> Int -> [HtmlExp] -> [HtmlExp]
              -> [HtmlExp] -> [HtmlExp] -> HtmlPage
bootstrapPage favicon styles jsincludes title brandurltitle lefttopmenu
              righttopmenu leftcols sidemenu header contents footer =
  HtmlPage title
           ([pageEnc "utf-8", responsiveView] ++ icon ++
             map pageCSS styles)
           (bootstrapBody jsincludes brandurltitle lefttopmenu righttopmenu
                          leftcols sidemenu header contents footer)
 where
  -- for a better view on handheld devices:
  responsiveView =
    pageMetaInfo
      [("name","viewport"),
       ("content","width=device-width, initial-scale=1, shrink-to-fit=no")]

  icon = if null favicon
           then []
           else [pageLinkInfo [("rel","shortcut icon"), ("href",favicon)]]

--- Create body of HTML page. Used by `bootstrapPage`.
bootstrapBody :: [String] -> (String,[HtmlExp]) -> [[HtmlExp]]
              -> [[HtmlExp]] -> Int -> [HtmlExp] -> [HtmlExp]
              -> [HtmlExp] -> [HtmlExp] -> [HtmlExp]
bootstrapBody jsincludes brandurltitle lefttopmenu righttopmenu
              leftcols sidemenu header contents footer =
  topNavigationBar brandurltitle lefttopmenu righttopmenu ++
  [blockstyle "container-fluid"
     ([blockstyle "row"
        (if leftcols==0
           then [blockstyle (bsCols 12)
                   (headerRow ++ contents)]
           else [blockstyle (bsCols leftcols)
                   [blockstyle "card" sidemenu],
                 blockstyle (bsCols (12-leftcols))
                   (headerRow ++ contents)])] ++
       if null footer
        then []
        else [hrule, HtmlStruct "footer" [] footer])] ++
   -- JavaScript includes placed at the end so page loads faster:
  map (\n -> HtmlStruct "script" [("src",n)] []) jsincludes
 where
  bsCols n = "col-sm-" ++ show n ++ " " ++ "col-md-" ++ show n
  
  -- header row:
  headerRow = if null header
                then []
                else [HtmlStruct "header" [("class","jumbotron")] header]


-- Navigation bar at the top. The first argument is a header element
-- put at the left, the second and third arguments are the left
-- and right menus which will be collapsed if the page is two small.
topNavigationBar :: (String,[HtmlExp]) -> [[HtmlExp]] -> [[HtmlExp]]
                 -> [HtmlExp]
topNavigationBar (brandurl,brandtitle) leftmenu rightmenu =
  [HtmlStruct "nav"
    [("class","navbar navbar-expand-md navbar-dark fixed-top bg-dark")]
    [href brandurl brandtitle `addClass` "navbar-brand",
     HtmlStruct "button"
       [("type","button"),("class","navbar-toggler"),
        ("data-toggle","collapse"),
        ("data-target","#topnavbar"),
        ("aria-expanded","false"),
        ("aria-controls","topnavbar"),
        ("aria-label","Toggle navigation")]
       [textstyle "navbar-toggler-icon" ""],
     HtmlStruct "div" [("id","topnavbar"),
                       ("class","collapse navbar-collapse")] $
       [ulistWithClass "navbar-nav mr-auto" "nav-item" leftmenu] ++
       if null rightmenu
         then []
         else [ulistWithClass "navbar-nav navbar-right" "nav-item" rightmenu]]]

-- Create a side menu containing a title and a list of items:
titledSideMenu :: String -> [[HtmlExp]] -> [HtmlExp]
titledSideMenu title items =
  (if null title
     then []
     else [HtmlStruct "h5" [] [htxt title]]) ++
  [ulistWithClass "nav flex-column" "nav-item" items]

----------------------------------------------------------------------------
-- Some buttons:

--- Primary input button.
primButton :: String -> HtmlHandler -> HtmlExp
primButton label handler =
  button label handler `addClass` "btn btn-primary"

--- Small primary input button.
primSmButton :: String -> HtmlHandler -> HtmlExp
primSmButton label handler =
  button label handler `addClass` "btn btn-sm btn-primary"

--- Secondary input button.
scndButton :: String -> HtmlHandler -> HtmlExp
scndButton label handler =
  button label handler `addClass` "btn btn-secondary"

--- Small secondary input button.
scndSmButton :: String -> HtmlHandler -> HtmlExp
scndSmButton label handler =
  button label handler `addClass` "btn btn-sm btn-secondary"

--- Info input button.
infoButton :: String -> HtmlHandler -> HtmlExp
infoButton label handler =
  button label handler `addClass` "btn btn-info"

--- Small info input button.
infoSmButton :: String -> HtmlHandler -> HtmlExp
infoSmButton label handler =
  button label handler `addClass` "btn btn-sm btn-info"

----------------------------------------------------------------------------
-- Renderings for hypertext references.

--- Hypertext reference rendered as a primary button.
hrefPrimButton :: String -> [HtmlExp] -> HtmlExp
hrefPrimButton ref hexps =
  href ref hexps `addClass` "btn btn-primary"

--- Hypertext reference rendered as a primary small button.
hrefPrimSmButton :: String -> [HtmlExp] -> HtmlExp
hrefPrimSmButton ref hexps =
  href ref hexps `addClass` "btn btn-sm btn-primary"

--- Hypertext reference rendered as a secondary button.
hrefScndButton :: String -> [HtmlExp] -> HtmlExp
hrefScndButton ref hexps =
  href ref hexps `addClass` "btn btn-secondary"

--- Hypertext reference rendered as a secondary small button.
hrefScndSmButton :: String -> [HtmlExp] -> HtmlExp
hrefScndSmButton ref hexps =
  href ref hexps `addClass` "btn btn-sm btn-secondary"

--- Hypertext reference rendered as an info button.
hrefInfoButton :: String -> [HtmlExp] -> HtmlExp
hrefInfoButton ref hexps =
  href ref hexps `addClass` "btn btn-info"

--- Hypertext reference rendered as a success button.
hrefSuccButton :: String -> [HtmlExp] -> HtmlExp
hrefSuccButton ref hexps =
  href ref hexps `addClass` "btn btn-success"

--- Hypertext reference rendered as a warning button.
hrefWarnButton :: String -> [HtmlExp] -> HtmlExp
hrefWarnButton ref hexps =
  href ref hexps `addClass` "btn btn-warning"

--- Hypertext reference rendered as a danger button.
hrefDangerButton :: String -> [HtmlExp] -> HtmlExp
hrefDangerButton ref hexps =
  href ref hexps `addClass` "btn btn-danger"

--- Hypertext reference rendered as an info block button.
hrefInfoBlock :: String -> [HtmlExp] -> HtmlExp
hrefInfoBlock ref hexps =
  href ref hexps `addClass` "btn btn-info btn-block"

-- Hypertext reference rendered as a small primary block button.
hrefPrimSmBlock :: String -> [HtmlExp] -> HtmlExp
hrefPrimSmBlock ref hexps =
  href ref hexps `addClass` "btn btn-sm btn-primary btn-block"

-- Hypertext reference rendered as a small secondary block button.
hrefScndSmBlock :: String -> [HtmlExp] -> HtmlExp
hrefScndSmBlock ref hexps =
  href ref hexps `addClass` "btn btn-sm btn-secondary btn-block"

-- Hypertext reference rendered as a primary badge.
hrefPrimBadge :: String -> [HtmlExp] -> HtmlExp
hrefPrimBadge ref hexps = href ref hexps `addClass` "badge badge-primary"

-- Hypertext reference rendered as a secondary badge.
hrefScndBadge :: String -> [HtmlExp] -> HtmlExp
hrefScndBadge ref hexps = href ref hexps `addClass` "badge badge-secondary"

-- Hypertext reference rendered as a success badge.
hrefSuccBadge :: String -> [HtmlExp] -> HtmlExp
hrefSuccBadge ref hexps = href ref hexps `addClass` "badge badge-success"

-- Hypertext reference rendered as a warning badge.
hrefWarnBadge :: String -> [HtmlExp] -> HtmlExp
hrefWarnBadge ref hexps = href ref hexps `addClass` "badge badge-warning"


-- External hypertext reference rendered as a primary badge.
ehrefPrimBadge :: String -> [HtmlExp] -> HtmlExp
ehrefPrimBadge ref hexps = ehref ref hexps `addClass` "badge badge-primary"

-- External hypertext reference rendered as a secondary badge.
ehrefScndBadge :: String -> [HtmlExp] -> HtmlExp
ehrefScndBadge ref hexps = ehref ref hexps `addClass` "badge badge-secondary"

-- External hypertext reference rendered as a success badge.
ehrefSuccBadge :: String -> [HtmlExp] -> HtmlExp
ehrefSuccBadge ref hexps = ehref ref hexps `addClass` "badge badge-success"

-- External hypertext reference rendered as a warning badge.
ehrefWarnBadge :: String -> [HtmlExp] -> HtmlExp
ehrefWarnBadge ref hexps = ehref ref hexps `addClass` "badge badge-warning"


--- Hypertext reference in navigations.
hrefNav :: String -> [HtmlExp] -> HtmlExp
hrefNav url hexp = href url hexp `addClass` "nav-link"

-- Active hypertext reference in navigations.
hrefNavActive :: String -> [HtmlExp] -> HtmlExp
hrefNavActive url hexp = ehref url hexp `addClass` "nav-link active"

-- External hypertext reference in navigations.
ehrefNav :: String -> [HtmlExp] -> HtmlExp
ehrefNav url hexp = ehref url hexp `addClass` "nav-link"

-- An external hypertext reference which opens on a new page.
ehref :: String -> [HtmlExp] -> HtmlExp
ehref ref hexp = href ref hexp `addAttr` ("target","_blank")

----------------------------------------------------------------------------

--- Render an HTML expression as keyboard or user input.
kbdInput :: [HtmlExp] -> HtmlExp
kbdInput = HtmlStruct "kbd" []

----------------------------------------------------------------------------

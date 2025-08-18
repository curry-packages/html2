----------------------------------------------------------------------------
--- This library contains some operations to generate web pages
--- rendered with [Bootstrap version 4](https://getbootstrap.com/).
---
--- @author Michael Hanus
--- @version August 2025
----------------------------------------------------------------------------

module HTML.Styles.Bootstrap4
 ( bootstrapPage, bootstrapPageExtended, bootstrapPage2, BodyOptions(..)
 , titledSideMenu, primButton, primSmButton, scndButton, scndSmButton
 , infoButton, infoSmButton
 , hrefPrimButton, hrefPrimSmButton, hrefScndButton, hrefScndSmButton
 , hrefInfoButton, hrefInfoSmButton, hrefSuccButton, hrefSuccSmButton
 , hrefWarnButton, hrefWarnSmButton, hrefDangButton, hrefDangSmButton
 , hrefLightButton, hrefLightSmButton, hrefDarkButton, hrefDarkSmButton
 , hrefPrimBlock, hrefScndBlock, hrefInfoBlock
 , hrefPrimSmBlock, hrefScndSmBlock, hrefInfoSmBlock
 , hrefPrimBadge, hrefScndBadge, hrefSuccBadge, hrefInfoBadge
 , hrefWarnBadge, hrefDangBadge, hrefLightBadge, hrefDarkBadge
 , ehrefPrimBadge, ehrefScndBadge, ehrefSuccBadge, ehrefInfoBadge
 , ehrefWarnBadge, ehrefDangBadge, ehrefLightBadge, ehrefDarkBadge
 , hrefNav, hrefNavActive, ehrefNav, ehref, eTarget
 , kbdInput
 , staticButton
 , stdModal, modalLaunchPrimButton, modalClosePrimButton, scriptShowModal
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
bootstrapPage :: String -> [String] -> [String] -> String -> (String,[BaseHtml])
              -> [[BaseHtml]] -> [[BaseHtml]] -> Int -> [BaseHtml] -> [BaseHtml]
              -> [BaseHtml] -> [BaseHtml] -> HtmlPage
bootstrapPage favicon styles jsincludes title brandurltitle lefttopmenu
              righttopmenu leftcols sidemenu header contents footer =
  bootstrapPage2 favicon styles jsincludes title brandurltitle
                 (addNavItemClass lefttopmenu) (addNavItemClass righttopmenu)
                 leftcols sidemenu header contents footer
 where
  addNavItemClass = map (\i -> ("nav-item", i))

--- An HTML page rendered with bootstrap with a fixed top navigation bar,
--- with extended configuration options for the body.
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
--- @param opts     - configuration options for the body of the page
--- @param sidemenu - the menu shown at the left-side of the main document
---                   (maybe created with 'titledSideMenu')
--- @param header   - the main header (will be rendered with jumbotron style)
--- @param contents - the main contents of the document
--- @param footer   - the footer of the document
bootstrapPageExtended :: String -> [String] -> [String] -> String -> (String,[BaseHtml])
              -> [[BaseHtml]] -> [[BaseHtml]] -> BodyOptions -> [BaseHtml] -> [BaseHtml]
              -> [BaseHtml] -> [BaseHtml] -> HtmlPage
bootstrapPageExtended favicon styles jsincludes title brandurltitle lefttopmenu
              righttopmenu opts sidemenu header contents footer =
  bootstrapPageInternal favicon styles jsincludes title brandurltitle
                        (addNavItemClass lefttopmenu)
                        (addNavItemClass righttopmenu)
                        opts sidemenu header contents footer
 where
  addNavItemClass = map (\i -> ("nav-item", i))

--- An HTML page rendered with bootstrap with a fixed top navigation bar
--- and individual classes for the top menu items.
--- @param favicon   - the icon file `favicon.ico` (when empty not included)
--- @param styles    - the style files to be included (typically,
---                    `css/bootstrap.min.css`)
--- @param jsincludes - the JavaScript files to be included (typically,
---                     `.../jquery.js`, `js/bootstrap.min.js`)
--- @param title   - the title of the form
--- @param brand   - the brand shown top left (a URL/title pair)
--- @lefttopmenu   - the menu shown in the left side of the top navigation bar
---                  (with class attribute for the menu items)
--- @righttopmenu  - the menu shown in the right side of the top navigation bar
---                  (with class attribute for the menu items, could be empty)
--- @param columns - number of columns for the left-side menu
---                  (if columns==0, then the left-side menu is omitted)
--- @param sidemenu - the menu shown at the left-side of the main document
---                   (maybe created with 'titledSideMenu')
--- @param header   - the main header (will be rendered with jumbotron style)
--- @param contents - the main contents of the document
--- @param footer   - the footer of the document
bootstrapPage2 :: String -> [String] -> [String] -> String
               -> (String,[BaseHtml]) -> [(String,[BaseHtml])]
               -> [(String,[BaseHtml])] -> Int -> [BaseHtml] -> [BaseHtml]
               -> [BaseHtml] -> [BaseHtml] -> HtmlPage
bootstrapPage2 favicon styles jsincludes title brandurltitle
  lefttopmenu righttopmenu leftcols sidemenu header contents footer =
  bootstrapPageInternal favicon styles jsincludes title brandurltitle
                        lefttopmenu righttopmenu opts sidemenu header
                        contents footer
 where
  opts = BodyOptions { leftCols = leftcols, hideNavbar = False
                     , container = "container-fluid" }

--- An HTML page rendered with bootstrap with a fixed top navigation bar
--- and individual classes for the top menu items, parametrized by
--- configuration options for the body.
bootstrapPageInternal :: String -> [String] -> [String] -> String
               -> (String,[BaseHtml]) -> [(String,[BaseHtml])]
               -> [(String,[BaseHtml])] -> BodyOptions -> [BaseHtml] -> [BaseHtml]
               -> [BaseHtml] -> [BaseHtml] -> HtmlPage
bootstrapPageInternal favicon styles jsincludes title brandurltitle
  lefttopmenu righttopmenu opts sidemenu header contents footer =
  HtmlPage title
           ([pageEnc "utf-8", responsiveView] ++ icon ++
             map pageCSS styles)
           (bootstrapBody jsincludes brandurltitle
                          lefttopmenu righttopmenu
                          opts
                          sidemenu header contents footer)
 where
  -- for a better view on handheld devices:
  responsiveView =
    pageMetaInfo
      [("name","viewport"),
       ("content","width=device-width, initial-scale=1, shrink-to-fit=no")]

  icon = if null favicon
           then []
           else [pageLinkInfo [("rel","shortcut icon"), ("href",favicon)]]

--- Configuration options for the body of a Bootstrap page.
data BodyOptions = BodyOptions
  { leftCols   :: Int    -- ^ Number of columns for the left-side menu
                         --   (if columns==0, then the left-side menu is omitted)
  , hideNavbar :: Bool   -- ^ Hide the left side menu on small screens
  , container  :: String -- ^ Bootstrap container class, e.g., 
                         --   "container-fluid" or "container"
  } 
 deriving (Show, Eq)

--- Creates body of HTML page. Used by `bootstrapPage`.
bootstrapBody ::
  HTML h => [String] -> (String,[h]) -> [(String,[h])] -> [(String,[h])]
         -> BodyOptions -> [h] -> [h] -> [h] -> [h] -> [h]
bootstrapBody jsincludes brandurltitle lefttopmenu righttopmenu
              opts sidemenu header contents footerdoc =
  topNavigationBar brandurltitle lefttopmenu righttopmenu ++
  [blockstyle (container opts)
     ([blockstyle "row"
        (if leftCols opts ==0
           then [blockstyle (bsCols 12)
                   (headerRow ++ contents)]
           else [blockstyle (bsCols $ leftCols opts)
                   [blockstyle "card" sidemenu],
                 blockstyle (bsCols (12 - leftCols opts) ++ colLg)
                   (headerRow ++ contents)])] ++
       if null footerdoc
         then []
         else [hrule, footer footerdoc])] ++
   -- JavaScript includes placed at the end so page loads faster:
  map (\n -> htmlStruct "script" [("src",n)] []) jsincludes
 where
  bsCols n = "col-sm-" ++ show n ++ " " ++ "col-md-" ++ show n
  colLg = if hideNavbar opts
            then "col-lg-12"
            else ""
  
  -- header row:
  headerRow = if null header
                then []
                else [htmlStruct "header" [("class","jumbotron")] header]


-- Navigation bar at the top. The first argument is a header element
-- put at the left, the second and third arguments are the left
-- and right menus which will be collapsed if the page is two small.
topNavigationBar ::
  HTML h =>  (String,[h]) -> [(String,[h])] -> [(String,[h])] -> [h]
topNavigationBar (brandurl,brandtitle) leftmenu rightmenu =
  [htmlStruct "nav"
    [("class","navbar navbar-expand-md navbar-dark fixed-top bg-dark")]
    [href brandurl brandtitle `addClass` "navbar-brand",
     htmlStruct "button"
       [("type","button"),("class","navbar-toggler"),
        ("data-toggle","collapse"),
        ("data-target","#topnavbar"),
        ("aria-expanded","false"),
        ("aria-controls","topnavbar"),
        ("aria-label","Toggle navigation")]
       [textstyle "navbar-toggler-icon" ""],
     htmlStruct "div" [("id","topnavbar"),
                       ("class","collapse navbar-collapse")] $
       [ulistWithItemClass "navbar-nav mr-auto" leftmenu] ++
       if null rightmenu
         then []
         else [ulistWithItemClass "navbar-nav navbar-right" rightmenu]]]

--- Create a side menu containing a title and a list of items:
titledSideMenu :: HTML h => String -> [[h]] -> [h]
titledSideMenu title items =
  (if null title
     then []
     else [h5 [htxt title]]) ++
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
hrefPrimButton :: HTML h => String -> [h] -> h
hrefPrimButton ref hexps =
  href ref hexps `addClass` "btn btn-primary"

--- Hypertext reference rendered as a small primary button.
hrefPrimSmButton :: HTML h => String -> [h] -> h
hrefPrimSmButton ref hexps =
  href ref hexps `addClass` "btn btn-sm btn-primary"

--- Hypertext reference rendered as a secondary button.
hrefScndButton :: HTML h => String -> [h] -> h
hrefScndButton ref hexps =
  href ref hexps `addClass` "btn btn-secondary"

--- Hypertext reference rendered as a small secondary button.
hrefScndSmButton :: HTML h => String -> [h] -> h
hrefScndSmButton ref hexps =
  href ref hexps `addClass` "btn btn-sm btn-secondary"

--- Hypertext reference rendered as an info button.
hrefInfoButton :: HTML h => String -> [h] -> h
hrefInfoButton ref hexps =
  href ref hexps `addClass` "btn btn-info"

--- Hypertext reference rendered as a small secondary button.
hrefInfoSmButton :: HTML h => String -> [h] -> h
hrefInfoSmButton ref hexps =
  href ref hexps `addClass` "btn btn-sm btn-info"

--- Hypertext reference rendered as a success button.
hrefSuccButton :: HTML h => String -> [h] -> h
hrefSuccButton ref hexps =
  href ref hexps `addClass` "btn btn-success"

--- Hypertext reference rendered as a small success button.
hrefSuccSmButton :: HTML h => String -> [h] -> h
hrefSuccSmButton ref hexps =
  href ref hexps `addClass` "btn btn-sm btn-success"

--- Hypertext reference rendered as a warning button.
hrefWarnButton :: HTML h => String -> [h] -> h
hrefWarnButton ref hexps =
  href ref hexps `addClass` "btn btn-warning"

--- Hypertext reference rendered as a small warning button.
hrefWarnSmButton :: HTML h => String -> [h] -> h
hrefWarnSmButton ref hexps =
  href ref hexps `addClass` "btn btn-sm btn-warning"

--- Hypertext reference rendered as a danger button.
hrefDangButton :: HTML h => String -> [h] -> h
hrefDangButton ref hexps =
  href ref hexps `addClass` "btn btn-danger"

--- Hypertext reference rendered as a small danger button.
hrefDangSmButton :: HTML h => String -> [h] -> h
hrefDangSmButton ref hexps =
  href ref hexps `addClass` "btn btn-sm btn-danger"

--- Hypertext reference rendered as a light button.
hrefLightButton :: HTML h => String -> [h] -> h
hrefLightButton ref hexps =
  href ref hexps `addClass` "btn btn-light"

--- Hypertext reference rendered as a small light button.
hrefLightSmButton :: HTML h => String -> [h] -> h
hrefLightSmButton ref hexps =
  href ref hexps `addClass` "btn btn-sm btn-light"

--- Hypertext reference rendered as a dark button.
hrefDarkButton :: HTML h => String -> [h] -> h
hrefDarkButton ref hexps =
  href ref hexps `addClass` "btn btn-dark"

--- Hypertext reference rendered as a small dark button.
hrefDarkSmButton :: HTML h => String -> [h] -> h
hrefDarkSmButton ref hexps =
  href ref hexps `addClass` "btn btn-sm btn-dark"


-- Hypertext reference rendered as a primary block button.
hrefPrimBlock :: HTML h => String -> [h] -> h
hrefPrimBlock ref hexps =
  href ref hexps `addClass` "btn btn-primary btn-block"

-- Hypertext reference rendered as a secondary block button.
hrefScndBlock :: HTML h => String -> [h] -> h
hrefScndBlock ref hexps =
  href ref hexps `addClass` "btn btn-secondary btn-block"

--- Hypertext reference rendered as an info block button.
hrefInfoBlock :: HTML h => String -> [h] -> h
hrefInfoBlock ref hexps =
  href ref hexps `addClass` "btn btn-info btn-block"

-- Hypertext reference rendered as a small primary block button.
hrefPrimSmBlock :: HTML h => String -> [h] -> h
hrefPrimSmBlock ref hexps =
  href ref hexps `addClass` "btn btn-sm btn-primary btn-block"

-- Hypertext reference rendered as a small secondary block button.
hrefScndSmBlock :: HTML h => String -> [h] -> h
hrefScndSmBlock ref hexps =
  href ref hexps `addClass` "btn btn-sm btn-secondary btn-block"

-- Hypertext reference rendered as a small info block button.
hrefInfoSmBlock :: HTML h => String -> [h] -> h
hrefInfoSmBlock ref hexps =
  href ref hexps `addClass` "btn btn-sm btn-info btn-block"

----------------------------------------------------------------------------
-- Badges

--- Hypertext reference rendered as a primary badge.
hrefPrimBadge :: HTML h => String -> [h] -> h
hrefPrimBadge ref hexps = href ref hexps `addClass` "badge badge-primary"

--- Hypertext reference rendered as a secondary badge.
hrefScndBadge :: HTML h => String -> [h] -> h
hrefScndBadge ref hexps = href ref hexps `addClass` "badge badge-secondary"

--- Hypertext reference rendered as a success badge.
hrefSuccBadge :: HTML h => String -> [h] -> h
hrefSuccBadge ref hexps = href ref hexps `addClass` "badge badge-success"

--- Hypertext reference rendered as an info badge.
hrefInfoBadge :: HTML h => String -> [h] -> h
hrefInfoBadge ref hexps = href ref hexps `addClass` "badge badge-info"

--- Hypertext reference rendered as a warning badge.
hrefWarnBadge :: HTML h => String -> [h] -> h
hrefWarnBadge ref hexps = href ref hexps `addClass` "badge badge-warning"

--- Hypertext reference rendered as a danger badge.
hrefDangBadge :: HTML h => String -> [h] -> h
hrefDangBadge ref hexps = href ref hexps `addClass` "badge badge-danger"

--- Hypertext reference rendered as a light badge.
hrefLightBadge :: HTML h => String -> [h] -> h
hrefLightBadge ref hexps = href ref hexps `addClass` "badge badge-light"

--- Hypertext reference rendered as a dark badge.
hrefDarkBadge :: HTML h => String -> [h] -> h
hrefDarkBadge ref hexps = href ref hexps `addClass` "badge badge-dark"

--- External hypertext reference rendered as a primary badge.
ehrefPrimBadge :: HTML h => String -> [h] -> h
ehrefPrimBadge ref hexps = eTarget $ hrefPrimBadge ref hexps

--- External hypertext reference rendered as a secondary badge.
ehrefScndBadge :: HTML h => String -> [h] -> h
ehrefScndBadge ref hexps = eTarget $ hrefScndBadge ref hexps

--- External hypertext reference rendered as a success badge.
ehrefSuccBadge :: HTML h => String -> [h] -> h
ehrefSuccBadge ref hexps = eTarget $ hrefSuccBadge ref hexps

--- External hypertext reference rendered as an info badge.
ehrefInfoBadge :: HTML h => String -> [h] -> h
ehrefInfoBadge ref hexps = eTarget $ hrefInfoBadge ref hexps

--- External hypertext reference rendered as a warning badge.
ehrefWarnBadge :: HTML h => String -> [h] -> h
ehrefWarnBadge ref hexps = eTarget $ hrefWarnBadge ref hexps

--- External hypertext reference rendered as a danger badge.
ehrefDangBadge :: HTML h => String -> [h] -> h
ehrefDangBadge ref hexps = eTarget $ hrefDangBadge ref hexps

--- External hypertext reference rendered as a light badge.
ehrefLightBadge :: HTML h => String -> [h] -> h
ehrefLightBadge ref hexps = eTarget $ hrefLightBadge ref hexps

--- External hypertext reference rendered as a dark badge.
ehrefDarkBadge :: HTML h => String -> [h] -> h
ehrefDarkBadge ref hexps = eTarget $ hrefDarkBadge ref hexps

----------------------------------------------------------------------------
-- Navigation links

--- Hypertext reference in navigations.
hrefNav :: HTML h => String -> [h] -> h
hrefNav url hexp = href url hexp `addClass` "nav-link"

--- Active hypertext reference in navigations.
hrefNavActive :: HTML h => String -> [h] -> h
hrefNavActive url hexp = ehref url hexp `addClass` "nav-link active"

--- External hypertext reference in navigations.
ehrefNav :: HTML h => String -> [h] -> h
ehrefNav url hexp = ehref url hexp `addClass` "nav-link"

--- An external hypertext reference which opens on a new page.
ehref :: HTML h => String -> [h] -> h
ehref ref hexp = eTarget $ href ref hexp

--- Transforms a hypertext reference into an external one
--- which opens on a new page.
--- Basically, the attribute `target="_blank"` is added.
eTarget :: HTML h => h -> h
eTarget hexp = hexp `addAttr` ("target","_blank")

--------------------------------------------------------------------------

--- Render an HTML expression as keyboard or user input.
kbdInput :: HTML h => [h] -> h
kbdInput = htmlStruct "kbd" []

----------------------------------------------------------------------------
-- Support for modal dialogs.

-- A static button (i.e., not a submit button of a form) of type `button`.
staticButton :: HTML h => [h] -> h
staticButton = htmlStruct "button" [("type","button")]

-- The basic modal structure:
modal :: HTML h => String -> String -> [h] -> h
modal modalId labelId = htmlStruct "div"
  [("class","modal fade")
  ,("id",modalId)
  ,("tabindex","-1")
  ,("role","dialog")
  ,("aria-labelledby",labelId)
  ,("aria-hidden","true")]

modalDialog :: HTML h => [h] -> h
modalDialog = htmlStruct "div" [("class","modal-dialog")]

modalContent :: HTML h => [h] -> h
modalContent = htmlStruct "div" [("class","modal-content")]

modalHeader :: HTML h => [h] -> h
modalHeader = htmlStruct "div" [("class","modal-header")]

modalBody :: HTML h => [h] -> h
modalBody = htmlStruct "div" [("class","modal-body")]

modalFooter :: HTML h => [h] -> h
modalFooter = htmlStruct "div" [("class","modal-footer")]

modalDismiss :: (String,String)
modalDismiss = ("data-dismiss","modal")

modalToggle :: (String,String)
modalToggle = ("data-toggle","modal")

stdModalClose :: HTML h => h
stdModalClose =
  staticButton [htmlText "&times;"]
   `addAttrs` [ ("class","close"), modalDismiss, ("aria-hidden","true")]

--- Defining a modal dialog where a modal id, the title, body, and footer
--- HTML expressions are provided.
--- If the first argument is `True`, then the modal dialog
--- will not close when clicking outside it.
stdModal :: HTML h => Bool -> String -> [h] -> [h] -> [h] -> h
stdModal staticbackdrop modalId title body footer =
  modal modalId labelId
    [modalDialog
      [modalContent
        [modalHeader
          [ htmlStruct "h5" [("class","modal-title")] title
              `addAttr` ("id",labelId)
          , stdModalClose]
        , modalBody body
        , modalFooter footer]]]
   `addAttrs`
     (if staticbackdrop
        then [("data-backdrop","static"), ("data-keyboard","false")]
        else [])
 where labelId = modalId ++ "Label"

--- A primary button to launch a modal dialog where the modal id and
--- the button text are provided.
modalLaunchPrimButton :: HTML h => String -> String -> h
modalLaunchPrimButton modalid btitle =
  staticButton [htxt btitle]
    `addAttrs` [ ("class","btn btn-primary"), ("data-toggle", "modal")
               , ("data-target", '#':modalid)]

--- A primary button to close a modal dialog to be used
--- inside the modal definition. The argument is the button text.
modalClosePrimButton :: HTML h => String -> h
modalClosePrimButton s =
  staticButton [ htxt s]
    `addAttrs` [ ("class","btn btn-primary"), ("data-dismiss","modal") ]

--- A JavaScript element which can be put at the end of the page body
--- in order to show a modal defined in the page after the page is loaded.
scriptShowModal :: String -> [BaseHtml]
scriptShowModal modalid =
  [hStruct "script"
    [htmlText $ "$(document).ready(function(){ $(\"#" ++ modalid ++
                "\").modal('show'); });"
    ]
  ]

----------------------------------------------------------------------------

-- Generate an HMTL page containing a list of all multiplications of digits

import HTML.Base

-- Generate list of digit multiplications:
multiplications :: [(Int,Int,Int)]
multiplications = [ (x,y,x*y) | x <- [1..10], y <- [1..x] ]

-- Generate HTML expression for a single multplication:
mult2html :: (Int,Int,Int) -> [BaseHtml]
mult2html (x,y,z) =
 [htxt $ show x ++ " * " ++ show y ++ " = ",
  strong [htxt $ show z], hrule]

-- The complete HTML document:
multPage :: HtmlPage
multPage = headerPage "Multiplication of Digits" $
  concatMap mult2html [ (x,y,x*y) | x <- [1..10], y <- [1..x] ]


-- A form that generates the HTML document on demand:
getMultPage :: IO HtmlPage
getMultPage = return multPage

-- Install the CGI program by:
-- curry2cgi -o ~/public_html/cgi-bin/multdigits.cgi -m getMultPage MultTable

------------------------------------------------------------------------------
-- An example to demonstrate a file upload form with the help
-- of a simple Python script.
------------------------------------------------------------------------------

import System.Directory ( doesFileExist ) -- from package `directory`
import HTML.Base

-- The upload form is defined as a raw search form since the handler
-- is written in Python. The handler script must be stored in file
-- `upload-handler.cgi`.
-- The arguments are the base name of the file where the contents
-- of the uploaded file is stored, and the URL loaded after
-- uploading the file.
uploadForm :: String -> String -> BaseHtml
uploadForm uploadfilename redirecturl =
  htmlStruct "form" [("method","post"), ("action","upload-handler.cgi"),
                     ("enctype","multipart/form-data")]
    [hiddenField "UPLOADNAME" uploadfilename,
     hiddenField "REDIRECT" redirecturl,
     htmlStruct "input" [("type","file"), ("name","FILENAME")] [],
     htmlStruct "input" [("type","submit"), ("value", "Upload!")] []
    ]

-- main HTML page containing the form
main :: IO HtmlPage
main = do
  uparam <- getUrlParameter
  if null uparam -- Initial call
    then return $ page "Upload Form"
           [ h3 [htxt "Please select a file to upload:"],
             uploadForm upfile "uploadform.cgi?next" ]
    else do
      exfile <- doesFileExist upfile
      if exfile
        then do cnt <- readFile upfile
                return $ page "UploadForm"
                  [h3 [htxt "Contents of the uploaded file:"],
                   verbatim cnt]
        else return $ page "UploadForm" [h3 [htxt "No file uploaded!"]]
 where
  upfile = "UploadedFile"

-- Install with:
-- > curry2cgi -o ~/public_html/cgi-bin/uploadform.cgi UploadFile
-- > cp -p upload-handler.cgi ~/public_html/cgi-bin/

-------------------------------------------------------------------------

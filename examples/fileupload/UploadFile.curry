------------------------------------------------------------------------------
-- An example to demonstrate a file upload form with the help
-- of a simple Python script.
------------------------------------------------------------------------------

import Directory ( doesFileExist )
import HTML.Base

-- The upload form is defined as a raw search form since the handler
-- is written in Python. The handler script must be stored in file
-- `upload-handler.cgi`.
-- The arguments are the base name of the file where the contents
-- of the uploaded file is stored, and the URL loaded after
-- uploading the file.
uploadForm :: String -> String -> HtmlExp
uploadForm uploadfilename redirecturl =
  HtmlStruct "form" [("method","post"), ("action","upload-handler.cgi"),
                     ("enctype","multipart/form-data")]
    [hiddenField "UPLOADNAME" uploadfilename,
     hiddenField "REDIRECT" redirecturl,
     HtmlStruct "input" [("type","file"), ("name","FILENAME")] [],
     HtmlStruct "input" [("type","submit"), ("value", "Upload!")] []
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
-- > cypm exec curry2cgi -o ~/public_html/cgi-bin/uploadform.cgi UploadFile
-- > cp -p upload-handler.cgi ~/public_html/cgi-bin/

-------------------------------------------------------------------------
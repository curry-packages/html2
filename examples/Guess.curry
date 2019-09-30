------------------------------------------------------------------------------
-- Example for HTML programming in Curry:
--
-- A recursive form for a number guessing game
-- which also counts the number of guesses
------------------------------------------------------------------------------

import Global

import HTML.Base
import HTML.Session

--- The data stored in the session is the number of guesses.
guessNr :: Global (SessionStore Int)
guessNr = global emptySessionStore (Persistent "guessNr")

guessInputForm :: HtmlFormDef Int
guessInputForm = HtmlFormDef "Guess.guessInputForm" readGuesses formHtml
 where
  readGuesses = getSessionData guessNr 0 -- read session data

  formHtml n =
    (if n>0 then [h4 [htxt $ show (n+1) ++ ". attempt:"]] else []) ++
    [htxt "Guess a natural number: ", textfield nref "",
     formButton "Check" guessHandler]
   where
    nref free

    guessHandler env = do
      let nr = read (env nref) :: Int
      if nr==42
        then do
          putSessionData guessNr 0
          return $ page "Answer" $
            [h1 [htxt $ "Right! You needed " ++ show (n+1) ++ " guesses!"]]
        else do
          putSessionData guessNr (n+1)
          return $ page "Answer" $
            [h1 [htxt $ if nr<42 then "Too small!"
                                 else "Too large!"],
             hrule, formExp guessInputForm]

-- main HTML page containing the form
main :: IO HtmlPage
main = do
  cookie <- sessionCookie  -- be sure that there is a cookie for the session
  putSessionData guessNr 0  -- initialize session state
  return (standardPage "Number Guessing Game"
    [ formExp guessInputForm ] `addPageParam` cookie)

-- Install the CGI script in user homepage by:
-- > cypm exec curry2cgi -o ~/public_html/cgi-bin/guess.cgi Guess


-- |
-- Module     : Simulation.Aivika.Experiment.HtmlWriter
-- Copyright  : Copyright (c) 2012, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.4.1
--
-- This is an utility module that provides an HTML writer.
--

module Simulation.Aivika.Experiment.HtmlWriter 
       (HtmlWriter,
        runHtmlWriter,
        composeHtml,
        writeHtml,
        writeHtmlLn,
        writeHtmlText,
        writeHtmlParagraph,
        writeHtmlParagraphWithId,
        writeHtmlHeader1,
        writeHtmlHeader1WithId,
        writeHtmlHeader2,
        writeHtmlHeader2WithId,
        writeHtmlHeader3,
        writeHtmlHeader3WithId,
        writeHtmlHeader4,
        writeHtmlHeader4WithId,
        writeHtmlHeader5,
        writeHtmlHeader5WithId,
        writeHtmlHeader6,
        writeHtmlHeader6WithId,
        writeHtmlBreak,
        writeHtmlLink,
        writeHtmlImage,
        writeHtmlList,
        writeHtmlDocumentWithTitle,
        encodeHtmlText) where

import Control.Monad
import Control.Monad.Trans

import Network.URI

-- | It writes fast an HTML code.
newtype HtmlWriter a = 
  HtmlWriter { runHtmlWriter :: ShowS -> IO (a, ShowS)
               -- ^ Run the HTML writer monad.
             }

instance Monad HtmlWriter where
  
  return a = HtmlWriter $ \f -> return (a, f)
  
  (HtmlWriter m) >>= k = HtmlWriter $ \f ->
    do (a, f') <- m f
       let HtmlWriter m' = k a
       m' f'
       
instance MonadIO HtmlWriter where       
  
  liftIO m = HtmlWriter $ \f ->
    do x <- m
       return (x, f)
       
-- | Write the HTML code.
writeHtml :: String -> HtmlWriter ()
writeHtml code = 
  HtmlWriter $ \f -> return ((), f . (code ++))
                     
-- | Write the HTML code.
writeHtmlLn :: String -> HtmlWriter ()
writeHtmlLn code = 
  do writeHtml code
     writeHtml "\n"
                     
-- | Write the text in HTML.                     
writeHtmlText :: String -> HtmlWriter ()                     
writeHtmlText text =
  HtmlWriter $ \f -> return ((), f . (encodeHtmlText text ++))
                     
-- | Compose the HTML applying the corresponded transformation.                     
composeHtml :: ShowS -> HtmlWriter ()                     
composeHtml g =
  HtmlWriter $ \f -> return ((), f . g)

-- | Write the HTML link with the specified URI and contents.
writeHtmlLink :: String -> HtmlWriter () -> HtmlWriter ()
writeHtmlLink uri inner =
  do writeHtml "<a href=\""
     writeHtml $ escapeURIString isUnescapedInURI uri
     writeHtml "\">"
     inner
     writeHtml "</a>"
     
-- | Write the HTML image with the specified URI.
writeHtmlImage :: String -> HtmlWriter ()
writeHtmlImage uri =
  do writeHtml "<img src=\""
     writeHtml $ escapeURIString isUnescapedInURI uri
     writeHtml "\" />"

-- | Write the @\<p\>@ element with the specified contents.     
writeHtmlParagraph :: HtmlWriter () -> HtmlWriter ()     
writeHtmlParagraph inner =
  do writeHtml "<p>"
     inner
     writeHtml "</p>"
     
-- | Write the @\<h1\>@ element with the specified contents.     
writeHtmlHeader1 :: HtmlWriter () -> HtmlWriter ()     
writeHtmlHeader1 inner =
  do writeHtml "<h1>"
     inner
     writeHtml "</h1>"
     
-- | Write the @\<h2\>@ element with the specified contents.     
writeHtmlHeader2 :: HtmlWriter () -> HtmlWriter ()     
writeHtmlHeader2 inner =
  do writeHtml "<h2>"
     inner
     writeHtml "</h2>"
     
-- | Write the @\<h3\>@ element with the specified contents.     
writeHtmlHeader3 :: HtmlWriter () -> HtmlWriter ()     
writeHtmlHeader3 inner =
  do writeHtml "<h3>"
     inner
     writeHtml "</h3>"
     
-- | Write the @\<h4\>@ element with the specified contents.     
writeHtmlHeader4 :: HtmlWriter () -> HtmlWriter ()     
writeHtmlHeader4 inner =
  do writeHtml "<h4>"
     inner
     writeHtml "</h4>"
     
-- | Write the @\<h5\>@ element with the specified contents.     
writeHtmlHeader5 :: HtmlWriter () -> HtmlWriter ()     
writeHtmlHeader5 inner =
  do writeHtml "<h5>"
     inner
     writeHtml "</h5>"
     
-- | Write the @\<h6\>@ element with the specified contents.     
writeHtmlHeader6 :: HtmlWriter () -> HtmlWriter ()     
writeHtmlHeader6 inner =
  do writeHtml "<h6>"
     inner
     writeHtml "</h6>"
     
-- | Write the @\<p\>@ element with the specified id and contents.     
writeHtmlParagraphWithId :: String -> HtmlWriter () -> HtmlWriter ()     
writeHtmlParagraphWithId id inner =
  do writeHtml "<p id=\""
     writeHtml id
     writeHtml "\">"
     inner
     writeHtml "</p>"
     
-- | Write the @\<h1\>@ element with the specified id and contents.     
writeHtmlHeader1WithId :: String -> HtmlWriter () -> HtmlWriter ()     
writeHtmlHeader1WithId id inner =
  do writeHtml "<h1 id=\""
     writeHtml id
     writeHtml "\">"
     inner
     writeHtml "</h1>"
     
-- | Write the @\<h2\>@ element with the specified id and contents.     
writeHtmlHeader2WithId :: String -> HtmlWriter () -> HtmlWriter ()     
writeHtmlHeader2WithId id inner =
  do writeHtml "<h2 id=\""
     writeHtml id
     writeHtml "\">"
     inner
     writeHtml "</h2>"
     
-- | Write the @\<h3\>@ element with the specified id and contents.     
writeHtmlHeader3WithId :: String -> HtmlWriter () -> HtmlWriter ()     
writeHtmlHeader3WithId id inner =
  do writeHtml "<h3 id=\""
     writeHtml id
     writeHtml "\">"
     inner
     writeHtml "</h3>"
     
-- | Write the @\<h4\>@ element with the specified id and contents.     
writeHtmlHeader4WithId :: String -> HtmlWriter () -> HtmlWriter ()     
writeHtmlHeader4WithId id inner =
  do writeHtml "<h4 id=\""
     writeHtml id
     writeHtml "\">"
     inner
     writeHtml "</h4>"
     
-- | Write the @\<h5\>@ element with the specified id and contents.     
writeHtmlHeader5WithId :: String -> HtmlWriter () -> HtmlWriter ()     
writeHtmlHeader5WithId id inner =
  do writeHtml "<h5 id=\""
     writeHtml id
     writeHtml "\">"
     inner
     writeHtml "</h5>"
     
-- | Write the @\<h6\>@ element with the specified id and contents.     
writeHtmlHeader6WithId :: String -> HtmlWriter () -> HtmlWriter ()     
writeHtmlHeader6WithId id inner =
  do writeHtml "<h6 id=\""
     writeHtml id
     writeHtml "\">"
     inner
     writeHtml "</h6>"
     
-- | Write the @\<br\>@ element.
writeHtmlBreak :: HtmlWriter ()     
writeHtmlBreak =
  writeHtml "<br />"
     
-- | Write the list of items.  
writeHtmlList :: [a] -> (a -> HtmlWriter ()) -> HtmlWriter ()
writeHtmlList xs action =
  do writeHtml "<ul>"
     forM_ xs $ \x -> 
       do writeHtml "<li>"
          action x 
          writeHtml "</li>"
     writeHtml "</ul>"

-- | Write the HTML document with the specified title and contents
writeHtmlDocumentWithTitle :: String -> HtmlWriter () -> HtmlWriter ()
writeHtmlDocumentWithTitle title inner =
  do writeHtml "<html>"
     writeHtml "<head>"
     writeHtml "<title>"
     writeHtmlText title
     writeHtml "</title>"
     writeHtmlCss 
     writeHtml "</head>"
     writeHtml "<body>"
     writeHtmlHeader1 $ 
       writeHtmlText title
     writeHtml "</h1>"
     inner
     writeHtml "<br /><p><font size=\"-1\">Automatically generated by "
     writeHtml "<a href=\"https://github.com/dsorokin/aivika-experiment\">"
     writeHtml "Aivika Experiment</a>"
     writeHtml "</font></p>"
     writeHtml "</body>"
     writeHtml "</html>"

-- | Escape special HTML characters in the 'String'.
-- It is based on one function from package Web-Encodings,
-- which is licensed under BSD3 but obsolete now.
encodeHtmlText :: String -> String
encodeHtmlText x = join $ map encodeHtmlChar x

-- | Escape a character.
encodeHtmlChar :: Char -> String
encodeHtmlChar '<'  = "&lt;"
encodeHtmlChar '>'  = "&gt;"
encodeHtmlChar '&'  = "&amp;"
encodeHtmlChar '"'  = "&quot;"
encodeHtmlChar '\'' = "&#39;"
encodeHtmlChar c    = [c]

-- | Write the CSS styles
writeHtmlCss :: HtmlWriter ()
writeHtmlCss =
  do writeHtmlLn "<style type=\"text/css\">"
     writeHtmlLn "* { margin: 0; padding: 0 }"
     writeHtmlLn ""
     writeHtmlLn "html {"
     writeHtmlLn "  background-color: white;"
     writeHtmlLn "  width: 100%;"
     writeHtmlLn "  height: 100%;"
     writeHtmlLn "}"
     writeHtmlLn ""
     writeHtmlLn "body {"
     writeHtmlLn "  background: white;"
     writeHtmlLn "  color: black;"
     writeHtmlLn "  text-align: left;"
     writeHtmlLn "  min-height: 100%;"
     writeHtmlLn "  position: relative;"
     writeHtmlLn "}"
     writeHtmlLn ""
     writeHtmlLn "p {"
     writeHtmlLn "  margin: 0.8em 0;"
     writeHtmlLn "}"
     writeHtmlLn ""
     writeHtmlLn "ul, ol {"
     writeHtmlLn "  margin: 0.8em 0 0.8em 2em;"
     writeHtmlLn "}"
     writeHtmlLn ""
     writeHtmlLn "dl {"
     writeHtmlLn "  margin: 0.8em 0;"
     writeHtmlLn "}"
     writeHtmlLn ""
     writeHtmlLn "dt {"
     writeHtmlLn "  font-weight: bold;"
     writeHtmlLn "}"
     writeHtmlLn ""
     writeHtmlLn "dd {"
     writeHtmlLn "  margin-left: 2em;"
     writeHtmlLn "}"
     writeHtmlLn ""
     writeHtmlLn "a { text-decoration: none; }"
     writeHtmlLn "a[href]:link { color: rgb(196,69,29); }"
     writeHtmlLn "a[href]:visited { color: rgb(171,105,84); }"
     writeHtmlLn "a[href]:hover { text-decoration:underline; }"
     writeHtmlLn ""
     writeHtmlLn "body {"
     writeHtmlLn "  font:13px/1.4 sans-serif;"
     writeHtmlLn "  *font-size:small; /* for IE */"
     writeHtmlLn "  *font:x-small; /* for IE in quirks mode */"
     writeHtmlLn "}"
     writeHtmlLn ""
     writeHtmlLn "h1 { font-size: 146.5%; /* 19pt */ } "
     writeHtmlLn "h2 { font-size: 131%;   /* 17pt */ }"
     writeHtmlLn "h3 { font-size: 116%;   /* 15pt */ }"
     writeHtmlLn "h4 { font-size: 100%;   /* 13pt */ }"
     writeHtmlLn "h5 { font-size: 100%;   /* 13pt */ }"
     writeHtmlLn ""
     writeHtmlLn "select, input, button, textarea {"
     writeHtmlLn "  font:99% sans-serif;"
     writeHtmlLn "}"
     writeHtmlLn ""
     writeHtmlLn "table {"
     writeHtmlLn "  font-size:inherit;"
     writeHtmlLn "  font:100%;"
     writeHtmlLn "}"
     writeHtmlLn ""
     writeHtmlLn "pre, code, kbd, samp, tt, .src {"
     writeHtmlLn "  font-family:monospace;"
     writeHtmlLn "  *font-size:108%;"
     writeHtmlLn "  line-height: 124%;"
     writeHtmlLn "}"
     writeHtmlLn ""
     writeHtmlLn ".links, .link {"
     writeHtmlLn "  font-size: 85%; /* 11pt */"
     writeHtmlLn "}"
     writeHtmlLn ".info  {"
     writeHtmlLn "  font-size: 85%; /* 11pt */"
     writeHtmlLn "}"
     writeHtmlLn ""
     writeHtmlLn ".caption, h1, h2, h3, h4, h5, h6 { "
     writeHtmlLn "  font-weight: bold;"
     writeHtmlLn "  color: rgb(78,98,114);"
     writeHtmlLn "  margin: 0.8em 0 0.4em;"
     writeHtmlLn "}"
     writeHtmlLn ""
     writeHtmlLn "* + h1, * + h2, * + h3, * + h4, * + h5, * + h6 {"
     writeHtmlLn "  margin-top: 2em;"
     writeHtmlLn "}"
     writeHtmlLn ""
     writeHtmlLn "h1 + h2, h2 + h3, h3 + h4, h4 + h5, h5 + h6 {"
     writeHtmlLn "  margin-top: inherit;"
     writeHtmlLn "}"
     writeHtmlLn ""
     writeHtmlLn "ul.links {"
     writeHtmlLn "  list-style: none;"
     writeHtmlLn "  text-align: left;"
     writeHtmlLn "  float: right;"
     writeHtmlLn "  display: inline-table;"
     writeHtmlLn "  margin: 0 0 0 1em;"
     writeHtmlLn "}"
     writeHtmlLn ""
     writeHtmlLn "ul.links li {"
     writeHtmlLn "  display: inline;"
     writeHtmlLn "  border-left: 1px solid #d5d5d5; "
     writeHtmlLn "  white-space: nowrap;"
     writeHtmlLn "  padding: 0;"
     writeHtmlLn "}"
     writeHtmlLn ""
     writeHtmlLn "ul.links li a {"
     writeHtmlLn "  padding: 0.2em 0.5em;"
     writeHtmlLn "}"
     writeHtmlLn "</style>"

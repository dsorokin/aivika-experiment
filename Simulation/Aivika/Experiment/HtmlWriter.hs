
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
     
-- | Write the HTML document with the specified title and contents
writeHtmlDocumentWithTitle :: String -> HtmlWriter () -> HtmlWriter ()
writeHtmlDocumentWithTitle title inner =
  do writeHtml "<html>"
     writeHtml "<head>"
     writeHtml "<title>"
     writeHtmlText title
     writeHtml "</title>"
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

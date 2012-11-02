
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
        writeHtmlLink,
        writeHtmlRelativeLink,
        encodeHtmlText) where

import Control.Monad
import Control.Monad.Trans

import System.FilePath

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

-- | Write the HTML link by the specified path and text.
writeHtmlLink :: FilePath -> String -> HtmlWriter ()
writeHtmlLink path text =
  do writeHtml "<a href=\""
     writeHtml $ escapeURIString isUnescapedInURI path
     writeHtml "\">"
     writeHtmlText text
     writeHtml "</a>"
         
-- | Write the HTML link using the specified relative path (the
-- first argument relative to the second) and text (the third argument). 
writeHtmlRelativeLink :: FilePath -> FilePath -> String -> HtmlWriter ()
writeHtmlRelativeLink path1 path2 text =
  do writeHtml "<a href=\""
     writeHtml $
       escapeURIString isUnescapedInURI $
       makeRelative path1 path2
     writeHtml "\">"
     writeHtmlText text
     writeHtml "</a>"
     
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

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-- | Provide the user with a rich text editor.
module MyNic (myNicHtmlField) where

import Yesod.Core
import Yesod.Form
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Text.Hamlet (shamlet)
import Text.Julius (julius, rawJS)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Text (Text, pack)
import Data.Maybe (listToMaybe)

myNicHtmlField :: Field (HandlerT site IO) Html
myNicHtmlField = Field
    { fieldParse = \e _ -> return . Right . fmap (preEscapedToMarkup . sanitizeBalance) . listToMaybe $ e
    , fieldView = \theId name attrs val _isReq -> do
        toWidget [shamlet|
$newline never
    <textarea id="#{theId}" *{attrs} name="#{name}" .html>#{showVal val}
|]        
        toWidget [julius|
(function(){new nicEditor({fullPanel:true}).panelInstance("#{rawJS theId}")})();
|]
    , fieldEnctype = UrlEncoded
    }
  where
    showVal = either id (pack . renderHtml)
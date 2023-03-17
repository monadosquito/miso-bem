{-# LANGUAGE GADTs #-}


-- | Miso HTML elements helpers
module Bem.Miso.View.Html where


import Miso


{- |
the wrapper around Miso HTML element
to decide which contents type to accept further
-}
data HtmlElem a c where
         NonVoidHtmlElem
             :: ([Attribute a] -> [View a] -> View a)
             -> HtmlElem a ([Attribute a], [View a])
         VoidHtmlElem :: ([Attribute a] -> View a) -> HtmlElem a [Attribute a]

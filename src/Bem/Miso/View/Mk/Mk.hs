{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}


-- | Make views.
module Bem.Miso.View.Mk.Mk
           ( module Bem.Miso.View.Html
           , module Bem.Miso.View.Mk.Mk
           ) where


import Bem.Miso.View.Html

import Bem.Cls.Gen.Gen
import Bem.Utl.Intr
import Control.Monad.Reader
import Miso
import Miso.String


{- |
the wrapper
through that the <Bem.Utl.Intr.FromBlkElem> type can be passed
into a non-function type constructor
-}
newtype BlkElem a = BlkElem (FromBlkElem (View a))


{- |
Make a view having a class of a block and element along with their modifiers.
-}
blkElem :: HtmlElem a c -> c -> FromBlkElem (View a)
blkElem (NonVoidHtmlElem nonVoidHtmlElem) (attrs, views)
    =
    \blk blkMods prnt elem' elemMods
    ->
    nonVoidHtmlElem
        ((class_ . ms $ genBlkElem blk blkMods prnt elem' elemMods) : attrs)
        views
blkElem (VoidHtmlElem voidHtmlElem) attrs
    =
    \blk blkMods prnt elem' elemMods
    ->
    voidHtmlElem
        $ (class_ . ms $ genBlkElem blk blkMods prnt elem' elemMods) : attrs

-- | Make a view having a class of a element along with its modifiers.
elem :: HtmlElem a c -> c -> forall b . FromFullElem b (View a)
elem (NonVoidHtmlElem nonVoidHtmlElem) (attrs, views)
    =
    \prnt elem' elemMods
    ->
    nonVoidHtmlElem ((class_ . ms $ genElem prnt elem' elemMods) : attrs) views
elem (VoidHtmlElem voidHtmlElem) attrs
    =
    \prnt elem' elemMods
    ->
    voidHtmlElem $ (class_ . ms $ genElem prnt elem' elemMods) : attrs

{- |
Make a view
having access to the model
and a class of a block
and element along with their modifiers.
-}
mkBlkElem :: HtmlElem a c -> c -> Reader m (BlkElem a)
mkBlkElem (NonVoidHtmlElem nonVoidHtmlElem) (attrs, views)
    =
    return
        $ BlkElem
        $ \blk blkMods prnt elem' elemMods
          ->
          nonVoidHtmlElem
              ((class_ . ms $ genBlkElem blk blkMods prnt elem' elemMods)
               : attrs
              )
              views
mkBlkElem (VoidHtmlElem voidHtmlElem) attrs
    =
    return
        $ BlkElem
        $ \blk blkMods prnt elem' elemMods
          ->
          voidHtmlElem
              $ (class_ . ms $ genBlkElem blk blkMods prnt elem' elemMods)
              : attrs

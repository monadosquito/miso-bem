{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}


-- | high-level extra utilities
module Bem.Miso.Utl.Utl where


import Bem.Utl.Intr
import Miso

import Bem.Miso.View.Html

import Bem.Cls.Gen.Gen
import Bem.Utl.Utl
import Control.Monad.Reader
import Miso.String


{- |
the wrapper
through that the <Bem.Utl.Intr.FromBlkElem> type can be passed
into a non-function type constructor
-}
newtype BlkElem a = BlkElem (FromBlkElem (View a))

{- |
the wrapper
through that the Bem.Utl.Intr.NoModsBlkElem type can be passed
into the non-function type constructor
-}
newtype NoModsBlkElem a = NoModsBlkElem (FromNoModsBlkElem (View a))

{- |
the wrapper
through that the Bem.Utl.Intr.FromBlkNoModsElem type can be passed
into the non-function type constructor
-}
newtype BlkNoModsElem a = BlkNoModsElem (FromBlkNoModsElem (View a))

{- |
the wrapper
through that the Bem.Utl.Intr.FromNoModsBlkNoModsElem type can be passed
into the non-function type constructor
-}
newtype NoModsBlkNoModsElem a = NoModsBlkNoModsElem
                                    (FromNoModsBlkNoModsElem (View a))


{- |
Make a view
having a class of a block along with its modifiers
and element without its modifiers.
-}
blkNoModsElem :: HtmlElem a c -> c -> FromBlkNoModsElem (View a)
blkNoModsElem (NonVoidHtmlElem nonVoidHtmlElem) (attrs, views)
    =
    \blk blkMods prnt elem'
    ->
    nonVoidHtmlElem
        ((class_ . ms $ genNoElemModsBlk blk blkMods prnt elem') : attrs)
        views
blkNoModsElem (VoidHtmlElem voidHtmlElem) attrs
    =
    \blk blkMods prnt elem'
    ->
    voidHtmlElem
        $ (class_ . ms $ genNoElemModsBlk blk blkMods prnt elem') : attrs

{- |
Make a view
having access to the model
and a class of a block along with its modifiers
and element without its modifiers.
-}
mkBlkNoModsElem :: HtmlElem a c -> c -> Reader m (BlkNoModsElem a)
mkBlkNoModsElem (NonVoidHtmlElem nonVoidHtmlElem) (attrs, views)
    =
    return
        $ BlkNoModsElem
        $ \blk blkMods prnt elem'
          ->
          nonVoidHtmlElem
              ((class_ . ms $ genNoElemModsBlk blk blkMods prnt elem') : attrs)
              views
mkBlkNoModsElem (VoidHtmlElem voidHtmlElem) attrs
    =
    return
        $ BlkNoModsElem
        $ \blk blkMods prnt elem'
          ->
          voidHtmlElem
              $ (class_ . ms $ genNoElemModsBlk blk blkMods prnt elem') : attrs

{- |
Make a view
having access to the model
and a class of a block without its modifiers
and element along with its modifiers.
-}
mkNoModsBlkElem :: HtmlElem a c -> c -> Reader m (NoModsBlkElem a)
mkNoModsBlkElem (NonVoidHtmlElem nonVoidHtmlElem) (attrs, views)
    =
    return
        $ NoModsBlkElem
        $ \blk prnt elem' elemMods
          ->
          nonVoidHtmlElem
              ((class_ . ms $ genNoBlkModsBlk blk prnt elem' elemMods) : attrs)
              views
mkNoModsBlkElem (VoidHtmlElem voidHtmlElem) attrs
    =
    return
        $ NoModsBlkElem
        $ \blk prnt elem' elemMods
          ->
          voidHtmlElem
              $ (class_ . ms $ genNoBlkModsBlk blk prnt elem' elemMods) : attrs

{- |
Make a view
having access to the model
and a class of a block
and element without their modifiers.
-}
mkNoModsBlkNoModsElem :: HtmlElem a c -> c -> Reader m (NoModsBlkNoModsElem a)
mkNoModsBlkNoModsElem (NonVoidHtmlElem nonVoidHtmlElem) (attrs, views)
    =
    return
        $ NoModsBlkNoModsElem
        $ \blk prnt elem'
          ->
          nonVoidHtmlElem
              ((class_ . ms $ genNoModsBlk blk prnt elem') : attrs)
              views
mkNoModsBlkNoModsElem (VoidHtmlElem voidHtmlElem) attrs
    =
    return
        $ NoModsBlkNoModsElem
        $ \blk prnt elem'
          ->
          voidHtmlElem $ (class_ . ms $ genNoModsBlk blk prnt elem') : attrs

{- |
Make a view
having a class of a block without its modifiers
and element along with its modifiers.
-}
noModsBlkElem :: HtmlElem a c -> c -> FromNoModsBlkElem (View a)
noModsBlkElem (NonVoidHtmlElem nonVoidHtmlElem) (attrs, views)
    =
    \blk prnt elem' elemMods
    ->
    nonVoidHtmlElem
        ((class_ . ms $ genNoBlkModsBlk blk prnt elem' elemMods) : attrs)
        views
noModsBlkElem (VoidHtmlElem voidHtmlElem) attrs
    =
    \blk prnt elem' elemMods
    ->
    voidHtmlElem
        $ (class_ . ms $ genNoBlkModsBlk blk prnt elem' elemMods) : attrs

-- | Make a view having a class of a block and element without their modifiers.
noModsBlkNoModsElem :: HtmlElem a c -> c -> FromNoModsBlkNoModsElem (View a)
noModsBlkNoModsElem (NonVoidHtmlElem nonVoidHtmlElem) (attrs, views)
    =
    \blk prnt elem'
    ->
    nonVoidHtmlElem
        ((class_ . ms $ genNoModsBlk blk prnt elem') : attrs)
        views
noModsBlkNoModsElem (VoidHtmlElem voidHtmlElem) attrs
    =
    \blk prnt elem'
    ->
    voidHtmlElem $ (class_ . ms $ genNoModsBlk blk prnt elem') : attrs

-- | Make a view having a class of an element without its modifiers.
noModsElem :: HtmlElem a c -> c -> forall b . FromElem b (View a)
noModsElem (NonVoidHtmlElem nonVoidHtmlElem) (attrs, views)
    =
    \prnt elem'
    ->
    nonVoidHtmlElem
        ((class_ . ms $ genElem prnt elem' []) : attrs)
        views
noModsElem (VoidHtmlElem nonVoidHtmlElem) attrs
    =
    \prnt elem'
    ->
    nonVoidHtmlElem $ (class_ . ms $ genElem prnt elem' []) : attrs

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}


-- | high-level extra utilities
module Bem.Miso.Utl.Utl where


import Bem.Utl.Intr
import Miso

import Bem.Miso.View.Html

import Bem.Utl.Utl
import Control.Monad.Reader
import Miso.String
import Bem.Cfg.Cfg
import qualified Bem.Utl.Utl as Bem


{- |
the wrapper
through that the <Bem.Utl.Intr.FromBlkElem> type can be passed
into a non-function type constructor
-}
newtype BlkElem a = BlkElem (FromBlkElem (View a))

{- |
the wrapper
through that the <Bem.Utl.Intr.NoModsBlkElem> type can be passed
into the non-function type constructor
-}
newtype NoModsBlkElem a = NoModsBlkElem (FromNoModsBlkElem (View a))

{- |
the wrapper
through that the <Bem.Utl.Intr.FromBlkNoModsElem> type can be passed
into the non-function type constructor
-}
newtype BlkNoModsElem a = BlkNoModsElem (FromBlkNoModsElem (View a))

{- |
the wrapper
through that the <Bem.Utl.Intr.FromNoModsBlkNoModsElem> type can be passed
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
blkNoModsElem = _blkNoModsElem $ Bem.Miso.Utl.Utl.init defCfg

{- |
Make a view
having access to the model
and a class of a block along with its modifiers
and element without its modifiers.
-}
mkBlkNoModsElem :: HtmlElem a c -> c -> Reader m (BlkNoModsElem a)
mkBlkNoModsElem = _mkBlkNoModsElem $ Bem.Miso.Utl.Utl.init defCfg

{- |
Make a view
having access to the model
and a class of a block without its modifiers
and element along with its modifiers.
-}
mkNoModsBlkElem :: HtmlElem a c -> c -> Reader m (NoModsBlkElem a)
mkNoModsBlkElem = _mkNoModsBlkElem $ Bem.Miso.Utl.Utl.init defCfg

{- |
Make a view
having access to the model
and a class of a block
and element without their modifiers.
-}
mkNoModsBlkNoModsElem :: HtmlElem a c -> c -> Reader m (NoModsBlkNoModsElem a)
mkNoModsBlkNoModsElem = _mkNoModsBlkNoModsElem $ Bem.Miso.Utl.Utl.init defCfg

{- |
Make a view
having a class of a block without its modifiers
and element along with its modifiers.
-}
noModsBlkElem :: HtmlElem a c -> c -> FromNoModsBlkElem (View a)
noModsBlkElem = _noModsBlkElem $ Bem.Miso.Utl.Utl.init defCfg

-- | Make a view having a class of a block and element without their modifiers.
noModsBlkNoModsElem :: HtmlElem a c -> c -> FromNoModsBlkNoModsElem (View a)
noModsBlkNoModsElem = _noModsBlkNoModsElem $ Bem.Miso.Utl.Utl.init defCfg

-- | Make a view having a class of an element without its modifiers.
noModsElem :: HtmlElem a c -> c -> forall b . FromElem b (View a)
noModsElem = _noModsElem $ Bem.Miso.Utl.Utl.init defCfg


-- | a view having a class of a block and element along with their modifiers
type BlkElem' a = FromBlkElem (View a)

-- | a view having a class of a element along with its modifiers
type Elem' a = forall b . FromFullElem b (View a)

{- |
a view having access to the model
and a class of a block and element along with their modifiers
-}
type MkBlkElem' a m = Reader m (BlkElem a)


-- | the configurable partial view makers
data Mks a m = Mks { _blkElem :: forall c . HtmlElem a c -> c -> BlkElem' a
                   , _blkNoModsElem
                         :: forall c
                         . HtmlElem a c
                         -> c
                         -> FromBlkNoModsElem (View a)
                   , _elem :: forall c . HtmlElem a c -> c -> Elem' a
                   , _mkBlkElem
                         :: forall c
                         . HtmlElem a c
                         -> c
                         -> MkBlkElem' a m
                   , _mkBlkNoModsElem
                         :: forall c
                         . HtmlElem a c
                         -> c
                         -> Reader m (BlkNoModsElem a)
                   , _mkNoModsBlkElem
                         :: forall c
                         . HtmlElem a c
                         -> c
                         -> Reader m (NoModsBlkElem a)
                   , _mkNoModsBlkNoModsElem
                         :: forall c
                         . HtmlElem a c
                         -> c
                         -> Reader m (NoModsBlkNoModsElem a)
                   , _noModsBlkElem
                         :: forall c
                         . HtmlElem a c
                         -> c
                         -> FromNoModsBlkElem (View a)
                   , _noModsBlkNoModsElem
                         :: forall c
                         . HtmlElem a c
                         -> c
                         -> FromNoModsBlkNoModsElem (View a)
                   , _noModsElem
                         :: forall c
                         . HtmlElem a c
                         -> c
                         -> forall b . FromElem b (View a)
                   }

-- | Initialise the configurable partial views makers using a configuration.
init :: Cfg -> Mks a m
init cfg
    =
    Mks { _blkElem
              =
              \case
                  NonVoidHtmlElem nonVoidHtmlElem
                     ->
                     \(attrs, views)
                     ->
                     \blk blkMods prnt elem' elemMods
                     ->
                     nonVoidHtmlElem
                         ((class_ . ms $ _genBlk blk blkMods prnt elem' elemMods
                          )
                          : attrs
                         )
                         views
                  VoidHtmlElem voidHtmlElem
                      ->
                      \attrs
                      ->
                      \blk blkMods prnt elem' elemMods
                      ->
                      voidHtmlElem
                          $ (class_ . ms
                                 $ _genBlk blk blkMods prnt elem' elemMods
                            )
                          : attrs
        , _blkNoModsElem
              =
              \case
                  NonVoidHtmlElem nonVoidHtmlElem
                      ->
                      \(attrs, views)
                      ->
                      \blk blkMods prnt elem'
                      ->
                      nonVoidHtmlElem
                          ((class_ . ms
                                $ _genNoElemModsBlk blk blkMods prnt elem'
                           )
                           : attrs
                          )
                          views
                  VoidHtmlElem voidHtmlElem
                      ->
                      \attrs
                      ->
                      \blk blkMods prnt elem'
                      ->
                      voidHtmlElem
                          $ (class_ . ms
                                 $ _genNoElemModsBlk blk blkMods prnt elem'
                            )
                          : attrs
        , _elem
              =
              \case
                  NonVoidHtmlElem nonVoidHtmlElem
                      ->
                      \(attrs, views)
                      ->
                      \prnt elem' elemMods
                      ->
                      nonVoidHtmlElem
                          ((class_ . ms $ _genElem prnt elem' elemMods) : attrs)
                          views
                  VoidHtmlElem voidHtmlElem
                      ->
                      \attrs
                      ->
                      \prnt elem' elemMods
                      ->
                      voidHtmlElem
                          $ (class_ . ms $ _genElem prnt elem' elemMods) : attrs
        , _mkBlkElem
              =
              \case
                  NonVoidHtmlElem nonVoidHtmlElem
                      ->
                      \(attrs, views)
                      ->
                      return
                          $ BlkElem
                          $ \blk blkMods prnt elem' elemMods
                            ->
                            nonVoidHtmlElem
                                ((class_
                                  . ms
                                  $ _genBlk blk blkMods prnt elem' elemMods
                                 )
                                 : attrs
                                )
                                views
                  VoidHtmlElem voidHtmlElem
                      ->
                      \attrs
                      ->
                      return
                          $ BlkElem
                          $ \blk blkMods prnt elem' elemMods
                            ->
                            voidHtmlElem
                                $ (class_ . ms
                                       $ _genBlk blk blkMods prnt elem' elemMods
                                  )
                                : attrs
        , _mkBlkNoModsElem
              =
              \case
                  NonVoidHtmlElem nonVoidHtmlElem
                      ->
                      \(attrs, views)
                      ->
                      return
                          $ BlkNoModsElem
                          $ \blk blkMods prnt elem'
                            ->
                            nonVoidHtmlElem
                                ((class_ . ms
                                      $ _genNoElemModsBlk blk blkMods prnt elem'
                                 )
                                 : attrs
                                )
                                views
                  VoidHtmlElem voidHtmlElem
                      ->
                      \attrs
                      ->
                      return
                          $ BlkNoModsElem
                          $ \blk blkMods prnt elem'
                            ->
                            voidHtmlElem
                                $ (class_ . ms
                                       $ _genNoElemModsBlk
                                             blk
                                             blkMods
                                             prnt
                                             elem'
                                  )
                                : attrs
        , _noModsBlkElem
              =
              \case
                  NonVoidHtmlElem nonVoidHtmlElem
                      ->
                      \(attrs, views)
                      ->
                      \blk prnt elem' elemMods
                      ->
                      nonVoidHtmlElem
                          ((class_ . ms
                                $ _genNoBlkModsBlk blk prnt elem' elemMods
                           )
                           : attrs
                          )
                          views
                  VoidHtmlElem voidHtmlElem
                      ->
                      \attrs
                      ->
                      \blk prnt elem' elemMods
                      ->
                      voidHtmlElem
                          $ (class_ . ms
                                 $ _genNoBlkModsBlk blk prnt elem' elemMods
                            )
                          : attrs
        , _noModsBlkNoModsElem
              =
              \case
                  NonVoidHtmlElem nonVoidHtmlElem
                      ->
                      \(attrs, views)
                      ->
                      \blk prnt elem'
                      ->
                      nonVoidHtmlElem
                          ((class_ . ms $ _genNoModsBlk blk prnt elem')
                          : attrs
                          )
                          views
                  VoidHtmlElem voidHtmlElem
                      ->
                      \attrs
                      ->
                      \blk prnt elem'
                      ->
                      voidHtmlElem
                          $ (class_ . ms $ _genNoModsBlk blk prnt elem')
                          : attrs
        , _noModsElem
              =
              \case
                  NonVoidHtmlElem nonVoidHtmlElem
                      ->
                      \(attrs, views)
                      ->
                      \prnt elem'
                      ->
                      nonVoidHtmlElem
                          ((class_ . ms $ _genNoModsElem prnt elem') : attrs)
                          views
                  VoidHtmlElem voidHtmlElem
                      ->
                      \attrs
                      ->
                      \prnt elem'
                      ->
                      voidHtmlElem
                          $ (class_ . ms $ _genNoModsElem prnt elem') : attrs
        , _mkNoModsBlkElem
              =
              \case
                  NonVoidHtmlElem nonVoidHtmlElem
                      ->
                      \(attrs, views)
                      ->
                      return
                          $ NoModsBlkElem
                          $ \blk prnt elem' elemMods
                            ->
                            nonVoidHtmlElem
                                ((class_ . ms
                                      $ _genNoBlkModsBlk blk prnt elem' elemMods
                                 )
                                 : attrs
                                )
                                views
                  VoidHtmlElem voidHtmlElem
                      ->
                      \attrs
                      ->
                      return
                          $ NoModsBlkElem
                          $ \blk prnt elem' elemMods
                          ->
                          voidHtmlElem
                              $ (class_ . ms
                                     $ _genNoBlkModsBlk blk prnt elem' elemMods
                                )
                              : attrs
        , _mkNoModsBlkNoModsElem
              =
              \case
                  NonVoidHtmlElem nonVoidHtmlElem
                      ->
                      \(attrs, views)
                      ->
                      return
                          $ NoModsBlkNoModsElem
                          $ \blk prnt elem'
                            ->
                            nonVoidHtmlElem
                                ((class_ . ms $ _genNoModsBlk blk prnt elem')
                                 : attrs
                                )
                                views
                  VoidHtmlElem voidHtmlElem
                      ->
                      \attrs
                      ->
                      return
                          $ NoModsBlkNoModsElem
                          $ \blk prnt elem'
                            ->
                            voidHtmlElem
                                $ (class_ . ms $ _genNoModsBlk blk prnt elem')
                                : attrs
        }
  where
    Bem.Gens {..} = Bem.init cfg

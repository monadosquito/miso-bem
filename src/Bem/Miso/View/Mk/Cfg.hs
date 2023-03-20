{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}


-- | Make views using custom Bem class decorations.
module Bem.Miso.View.Mk.Cfg where


import qualified Bem.Miso.Utl.Utl as Utl
import Bem.Miso.View.Html

import Bem.Cfg.Cfg
import Miso
import Miso.String
import qualified Bem.Utl.Utl as Bem


-- | the configurable view makers
data Mks a m = Mks { _blkElem :: forall c . HtmlElem a c -> c -> Utl.BlkElem' a
                   , _elem :: forall c . HtmlElem a c -> c -> Utl.Elem' a
                   , _mkBlkElem
                         :: forall c
                         . HtmlElem a c
                         -> c
                         -> Utl.MkBlkElem' a m
                   }

-- | Initialise the configurable view makers using a configuration.
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
                      \blk blkMods prnt elem' elemMods
                      ->
                      voidHtmlElem
                          $ (class_
                             . ms
                             $ _genBlk blk blkMods prnt elem' elemMods
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
                          $ (class_ . ms $ _genElem prnt elem' elemMods)
                          : attrs
        , _mkBlkElem
              =
              \case
                  NonVoidHtmlElem nonVoidHtmlElem
                      ->
                      \(attrs, views)
                      ->
                      return
                          $ Utl.BlkElem
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
                          $ Utl.BlkElem
                          $ \blk blkMods prnt elem' elemMods
                            ->
                            voidHtmlElem
                                $ (class_ . ms
                                       $ _genBlk blk blkMods prnt elem' elemMods
                                  )
                                : attrs
        }
  where
    Bem.Gens {..} = Bem.init cfg

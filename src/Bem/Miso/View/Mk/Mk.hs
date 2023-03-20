{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}


-- | Make views using the default Bem class decorations.
module Bem.Miso.View.Mk.Mk
           ( module Bem.Miso.View.Html
           , module Bem.Miso.View.Mk.Mk
           , Utl.BlkElem
           ) where


import Bem.Miso.View.Html

import qualified Bem.Miso.Utl.Utl as Utl
import Bem.Miso.Utl.Intr
import qualified Bem.Miso.View.Mk.Cfg as Cfg


{- |
Make a view having a class of a block and element along with their modifiers.
-}
blkElem :: HtmlElem a c -> c -> Utl.BlkElem' a
blkElem = Cfg._blkElem defMks

-- | Make a view having a class of a element along with its modifiers.
elem :: HtmlElem a c -> c -> Utl.Elem' a
elem = Cfg._elem defMks

{- |
Make a view
having access to the model
and a class of a block
and element along with their modifiers.
-}
mkBlkElem :: HtmlElem a c -> c -> Utl.MkBlkElem' a m
mkBlkElem = Cfg._mkBlkElem defMks

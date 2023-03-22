{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}


-- | Make views using the default Bem class decorations.
module Bem.Miso.View.Mk.Mk
           ( module Bem.Miso.View.Html
           , module Bem.Miso.View.Mk.Mk
           , BlkElem
           ) where


import Bem.Miso.View.Html

import Bem.Miso.Utl.Utl
import Bem.Miso.Utl.Intr
import Bem.Miso.View.Mk.Cfg


{- |
Make a view having a class of a block and element along with their modifiers.
-}
blkElem :: HtmlElem a c -> c -> BlkElem' a
blkElem = _blkElem defMks

-- | Make a view having a class of a element along with its modifiers.
elem :: HtmlElem a c -> c -> Elem' a
elem = _elem defMks

{- |
Make a view
having access to the model
and a class of a block
and element along with their modifiers.
-}
mkBlkElem :: HtmlElem a c -> c -> MkBlkElem' a m
mkBlkElem = _mkBlkElem defMks

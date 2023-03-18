{-# LANGUAGE RankNTypes #-}


-- | high-level extra utilities
module Bem.Miso.Utl.Utl where


import Bem.Utl.Intr
import Miso


{- |
the wrapper
through that the <Bem.Utl.Intr.FromBlkElem> type can be passed
into a non-function type constructor
-}
newtype BlkElem a = BlkElem (FromBlkElem (View a))

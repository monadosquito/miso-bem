-- | low-level extra utilities
module Bem.Miso.Utl.Intr where


import qualified Bem.Miso.View.Mk.Cfg as Cfg

import Bem.Cfg.Cfg


-- | configurable view makers configured using the default configuration
defMks :: Cfg.Mks a m
defMks = Cfg.init defCfg

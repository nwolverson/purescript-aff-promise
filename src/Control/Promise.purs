module Control.Promise (fromAff, Promise()) where

import Control.Monad.Eff (Eff)
import Prelude (Unit)
import Control.Monad.Aff (Aff, runAff)

foreign import data Promise :: * -> *

foreign import promise :: forall eff a b.
  ((a -> Eff eff Unit) -> (b -> Eff eff Unit) -> Eff eff Unit) -> Eff eff (Promise a)

fromAff :: forall eff a. Aff eff a -> Eff eff (Promise a)
fromAff aff = promise (\succ err -> runAff err succ aff)

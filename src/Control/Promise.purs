module Control.Promise (fromAff, fromPromise, Promise()) where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Aff (makeAff, Aff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foreign (Foreign, unsafeReadTagged)
import Data.Foreign.Class (read)

foreign import data Promise :: * -> *

foreign import promise :: forall eff a b.
  ((a -> Eff eff Unit) -> (b -> Eff eff Unit) -> Eff eff Unit) -> Eff eff (Promise a)
foreign import thenImpl :: forall a b e.
  Promise a -> (Foreign -> Eff e b) -> (a -> Eff e b) -> Eff e Unit

fromAff :: forall eff a. Aff eff a -> Eff eff (Promise a)
fromAff aff = promise (\succ err -> void $ runAff err succ aff)

coerce :: Foreign -> Error
coerce fn =
  either (\_ -> error "Promise failed, couldn't extract JS Error or String")
         id
         (runExcept ((unsafeReadTagged "Error" fn) <|> (error <$> read fn)))

fromPromise :: forall eff a. Promise a -> Aff eff a
fromPromise p = makeAff (\errCB succCB -> thenImpl p (errCB <<< coerce) succCB)

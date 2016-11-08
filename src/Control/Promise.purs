module Control.Promise (fromAff, toAff, Promise()) where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Aff (makeAff, Aff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foreign (Foreign, unsafeReadTagged)
import Data.Foreign.Class (read)

-- | Type of JavaScript Promises (with particular return type)
-- | Effects are not traced in the Promise type, as they form part of the Eff that
-- | results in the promise.
foreign import data Promise :: * -> *

foreign import promise :: forall eff a b.
  ((a -> Eff eff Unit) -> (b -> Eff eff Unit) -> Eff eff Unit) -> Eff eff (Promise a)
foreign import thenImpl :: forall a b e.
  Promise a -> (Foreign -> Eff e b) -> (a -> Eff e b) -> Eff e Unit

-- | Convert an Aff into a Promise.
fromAff :: forall eff a. Aff eff a -> Eff eff (Promise a)
fromAff aff = promise (\succ err -> void $ runAff err succ aff)

coerce :: Foreign -> Error
coerce fn =
  either (\_ -> error "Promise failed, couldn't extract JS Error or String")
         id
         (runExcept ((unsafeReadTagged "Error" fn) <|> (error <$> read fn)))

-- | Convert a Promise into an Aff.
-- | When the promise rejects, we attempt to
-- | coerce the error value into an actual JavaScript Error object. We can do this
-- | with Error objects or Strings. Anything else gets a "dummy" Error object.
toAff :: forall eff a. Promise a -> Aff eff a
toAff p = makeAff (\errCB succCB -> thenImpl p (errCB <<< coerce) succCB)

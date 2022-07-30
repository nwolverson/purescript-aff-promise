module Control.Promise
  ( module Control.Promise
  , module Web.Promise
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), either, hush)
import Data.Maybe (fromMaybe')
import Effect (Effect)
import Effect.Aff (Aff, makeAff, runAff_)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)
import Foreign (readString, unsafeToForeign)
import Web.Promise (Promise, Rejection)
import Web.Promise as Promise
import Web.Promise.Rejection as Rejection

-- | Convert an Aff into a Promise.
fromAff :: forall a. Promise.Flatten a a ⇒ Aff a -> Effect (Promise a)
fromAff aff = Promise.new (\succ err -> runAff_ (either (err <<< Rejection.fromError) succ) aff)

coerce :: Rejection -> Error
coerce rej =
  fromMaybe'
    (\_ → error "Promise failed, couldn't extract JS Error or String")
    (Rejection.toError rej <|> map error (hush (runExcept (readString (unsafeToForeign rej)))))

-- | Convert a Promise into an Aff.
-- | When the promise rejects, we attempt to
-- | coerce the error value into an actual JavaScript Error object. We can do this
-- | with Error objects or Strings. Anything else gets a "dummy" Error object.
toAff :: forall a. Promise a -> Aff a
toAff = toAff' coerce

-- | Convert a Promise into an Aff with custom Error coercion.
-- | When the promise rejects, we attempt to coerce the error value into an
-- | actual JavaScript Error object using the provided function.
toAff' :: forall a. (Rejection -> Error) -> Promise a -> Aff a
toAff' customCoerce p = makeAff \cb →
  mempty <$
    Promise.thenOrCatch
      (\a -> Promise.resolve <$> cb (Right a))
      (\e -> Promise.resolve <$> cb (Left (customCoerce e)))
      p

-- | Utility to convert an Effect returning a Promise into an Aff (i.e. the inverse of fromAff)
toAffE :: forall a. Effect (Promise a) -> Aff a
toAffE f = liftEffect f >>= toAff

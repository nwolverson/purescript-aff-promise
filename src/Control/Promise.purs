module Promise.Aff
  ( module Promise.Aff
  , module Exports
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
import Promise (Promise) as Exports
import Promise as Promise
import Promise.Rejection as Rejection

-- | Convert an Aff into a Promise.
fromAff :: forall a. Promise.Flatten a a => Aff a -> Effect (Promise.Promise a)
fromAff aff = Promise.new (\succ err -> runAff_ (either (err <<< Rejection.fromError) succ) aff)

coerce :: Promise.Rejection -> Error
coerce rej =
  fromMaybe'
    (\_ -> error "Promise failed, couldn't extract JS Error or String")
    (Rejection.toError rej <|> map error (hush (runExcept (readString (unsafeToForeign rej)))))

-- | Convert a Promise into an Aff.
-- | When the promise rejects, we attempt to
-- | coerce the error value into an actual JavaScript Error object. We can do this
-- | with Error objects or Strings. Anything else gets a "dummy" Error object.
toAff :: forall a. Promise.Promise a -> Aff a
toAff = toAff' coerce

-- | Convert a Promise into an Aff with custom Error coercion.
-- | When the promise rejects, we attempt to coerce the error value into an
-- | actual JavaScript Error object using the provided function.
toAff' :: forall a. (Promise.Rejection -> Error) -> Promise.Promise a -> Aff a
toAff' customCoerce p = makeAff \cb ->
  mempty <$
    Promise.thenOrCatch
      (\a -> Promise.resolve <$> cb (Right a))
      (\e -> Promise.resolve <$> cb (Left (customCoerce e)))
      p

-- | Utility to convert an Effect returning a Promise into an Aff (i.e. the inverse of fromAff)
toAffE :: forall a. Effect (Promise.Promise a) -> Aff a
toAffE f = liftEffect f >>= toAff

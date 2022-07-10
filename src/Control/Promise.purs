module Control.Promise (fromAff, toAff, toAff', toAffE, module PromiseReexport) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), either)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, runAff_)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)
import Foreign (Foreign, readString, unsafeReadTagged, unsafeToForeign)
import Web.Promise (class Flatten, Promise, resolve)
import Web.Promise (class Flatten, Promise) as PromiseReexport
import Web.Promise as Promise
import Web.Promise.Rejection as Rejection

-- | Convert an Aff into a Promise.
fromAff :: forall a b. (Flatten a b) => Aff a -> Effect (Promise b)
fromAff aff = Promise.new $
  \succ err -> runAff_ (either (err <<< Rejection.fromError) succ) aff

coerce :: Foreign -> Error
coerce fn =
  either (\_ -> error "Promise failed, couldn't extract JS Error or String")
         identity
         (runExcept ((unsafeReadTagged "Error" fn) <|> (error <$> readString fn)))

-- | Convert a Promise into an Aff.
-- | When the promise rejects, we attempt to
-- | coerce the error value into an actual JavaScript Error object. We can do this
-- | with Error objects or Strings. Anything else gets a "dummy" Error object.
toAff :: forall a. Promise a -> Aff a
toAff = toAff' coerce

-- | Convert a Promise into an Aff with custom Error coercion.
-- | When the promise rejects, we attempt to coerce the error value into an
-- | actual JavaScript Error object using the provided function.
toAff' :: forall a. (Foreign -> Error) -> Promise a -> Aff a
toAff' customCoerce promise = makeAff $ \cb -> do
  withContinuation <- Promise.then_ (map resolve <<< cb <<< Right) promise
  _ <- Promise.catch (map resolve <<< cb <<< Left <<< customCoerce <<< unsafeToForeign) withContinuation
  pure mempty

-- | Utility to convert an Effect returning a Promise into an Aff (i.e. the inverse of fromAff)
toAffE :: forall a. Effect (Promise a) -> Aff a
toAffE f = liftEffect f >>= toAff

module Test.Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Control.Monad.Reader.Class (class MonadReader, ask, local)
import Control.Monad.Reader.Trans (runReaderT)
import Data.Either (either)
import Data.Int (toNumber)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, delay, launchAff_, parallel, sequential, throwError)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Exception (error, message)
import Foreign (readString, unsafeToForeign)
import Foreign.Index (readProp)
import Promise.Aff (Promise)
import Promise.Aff as Promise
import Test.Assert as Assert

suite :: forall m. MonadReader String m => MonadEffect m => String -> m Unit -> m Unit
suite msg runTest = do
  prev <- ask
  liftEffect $ log prev
  local (_ <> msg) runTest

test :: forall m. MonadReader String m => MonadEffect m => MonadAff m => String -> Aff Unit -> m Unit
test msg runTest = do
  prev <- ask
  liftEffect $ log prev
  local (_ <> msg) (liftAff runTest)

assert :: forall m. MonadEffect m => String -> Boolean -> m Unit
assert msg bool = liftEffect $ Assert.assert' msg bool

shouldEqual :: forall m a. MonadEffect m => Eq a => Show a => a -> a -> m Unit
shouldEqual expected actual =
  liftEffect $ Assert.assertEqual { expected, actual }

makeTimeout :: forall a. Int -> Aff a
makeTimeout time = do
  delay $ Milliseconds $ toNumber time
  throwError $ error $ "test timed out after " <> show time <> "ms"

timeout :: Int -> Aff Unit -> Aff Unit
timeout time theTest = do
  r <- sequential $ parallel (attempt $ makeTimeout time) <|> parallel (attempt theTest)
  either throwError (const (pure unit)) r

foreign import helloPromise :: Promise String

foreign import errPromise :: Promise String

foreign import customErrPromise :: Promise String

foreign import goodbyePromise :: Promise String

main :: Effect Unit
main = launchAff_ $ flip runReaderT "" do
  suite "ffi" do
    test "Hello" do
      s <- Promise.toAff helloPromise
      shouldEqual "Hello" s
    test "err" do
      res <- attempt $ Promise.toAff errPromise
      shouldEqual "err" $ either message (const "-") res
    test "customErr" do
      res <- attempt $ Promise.toAff' errorCodeCoerce customErrPromise
      shouldEqual "err" $ either message (const "-") res
    test "Goodbye" do
      res <- attempt $ Promise.toAff goodbyePromise
      shouldEqual "Goodbye" $ either message (const "-") res
  suite "round-trip" do
    test "success" do
      timeout 100 $ do
        promise <- liftEffect $ Promise.fromAff $ pure 42
        res <- Promise.toAff promise
        assert "round-trip result is 42" $ res == 42
    test "toAffE" do
      timeout 100 do
        res <- Promise.toAffE $ Promise.fromAff $ pure 123
        assert "round-trip result for toAffE is 123" $ res == 123
    test "error" do
      promise <- liftEffect $ Promise.fromAff (throwError (error "err123") :: Aff Unit)
      res <- attempt $ Promise.toAff promise
      shouldEqual "err123" $ either message (const "-") res
  where
  errorCodeCoerce v =
    either (\_ -> error "fail") error
      (runExcept $ readProp "code" (unsafeToForeign v) >>= readString)

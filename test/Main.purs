module Test.Main where

import Prelude

import Control.Monad.Except (runExcept, throwError)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Either (either)
import Effect (Effect)
import Effect.Aff (attempt)
import Effect.Class (liftEffect)
import Effect.Exception (error, message)
import Foreign (readString, unsafeFromForeign)
import Foreign.Index (readProp)
import Test.Unit (suite, test, timeout)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

foreign import helloPromise :: Promise String

foreign import errPromise :: Promise String

foreign import customErrPromise :: Promise String

foreign import goodbyePromise :: Promise String

main :: Effect Unit
main = runTest do
  suite "ffi" do
    test "Hello" do
      s <- Promise.toAff helloPromise
      Assert.equal "Hello" s
    test "err" do
      res <- attempt $ Promise.toAff errPromise
      Assert.equal "err" $ either message (const "-") res
    test "customErr" do
      res <- attempt $ Promise.toAff' errorCodeCoerce customErrPromise
      Assert.equal "err" $ either message (const "-") res
    test "Goodbye" do
      res <- attempt $ Promise.toAff goodbyePromise
      Assert.equal "Goodbye" $ either message (const "-") res
  suite "round-trip" do
    test "success" do
      timeout 100 $ do
        promise <- liftEffect $ Promise.fromAff $ pure 42
        res <- Promise.toAff promise
        Assert.assert "round-trip result is 42" $ res == 42
    test "toAffE" do
      timeout 100 do
        res <- Promise.toAffE $ Promise.fromAff $ pure 123
        Assert.assert "round-trip result for toAffE is 123" $ res == 123
    test "error" do
      promise <- liftEffect $ Promise.fromAff $ throwError $ error "err123"
      res <- attempt $ Promise.toAff promise
      Assert.equal "err123" $ either message (const "-") res
  where
    errorCodeCoerce v = either (\_ -> error "fail") error $
                          (runExcept $ readProp "code" (unsafeFromForeign v) >>= readString)

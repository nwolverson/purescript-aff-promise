module Test.Main where

import Prelude
import Control.Promise as Promise
import Control.Promise (Promise)
import Test.Unit.Assert as Assert
import Control.Monad.Aff (attempt)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (message, error)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Error.Class (throwError)
import Data.Either (either)
import Test.Unit (suite, test, timeout)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

foreign import helloPromise :: Promise String

foreign import errPromise :: Promise String

foreign import goodbyePromise :: Promise String

main :: forall e. Eff ( console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR, timer :: TIMER | e) Unit
main = runTest do
  suite "ffi" do
    test "Hello" do
      s <- Promise.toAff helloPromise
      Assert.equal "Hello" s
    test "err" do
      res <- attempt $ Promise.toAff errPromise
      Assert.equal "err" $ either message (const "-") res
    test "Goodbye" do
      res <- attempt $ Promise.toAff goodbyePromise
      Assert.equal "Goodbye" $ either message (const "-") res
  suite "round-trip" do
    test "success" do
      timeout 100 $ do
        promise <- liftEff $ Promise.fromAff $ pure 42
        res <- Promise.toAff promise
        Assert.assert "round-trip result is 42" $ res == 42
    test "error" do
      promise <- liftEff $ Promise.fromAff $ throwError $ error "err123"
      res <- attempt $ Promise.toAff promise
      Assert.equal "err123" $ either message (const "-") res

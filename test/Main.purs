module Test.Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Erl.Test.EUnit (suite, test)
import Erl.Test.EUnit as EUnit
import Test.Assert (assertEqual)
import Test.QuickCheck ((===))
import Test.QuickCheck.Helpers (property)

main :: Effect Unit
main = do
  void $ EUnit.runTests do
    suite "Test suite" do
      test "tests things" do
        assertEqual { expected: 1, actual: 1 }

        property "All numbers are equal to themselves" \(x :: Int) ->
          x === x

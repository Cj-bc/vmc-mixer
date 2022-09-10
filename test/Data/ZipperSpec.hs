module Data.ZipperSpec where
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Data.Zipper (next, previous, Zipper(Zipper))
import Lens.Micro ((^.))
import Lens.Micro.Extras (view)
import Data.Maybe (isNothing, isJust)

spec = do
  describe "next" $ do
    it "should return Nothing if it is peeking last value" (isNothing $ view next (Zipper 0 [1..] []))
    it "should return new Zipper if it isn't peeking last value" (isJust $ view next (Zipper 0 [1..] [1..]))

    -- prop "next should not modify its contents" $ \zipper ->
    --   (toList $ zipper^.next) == (toList zipper)

  describe "previous" $ do
    it "should return Nothing if it is peeking the first value" (isNothing $ view previous  (Zipper 0 [] [1..]))
    it "should return new Zipper if it isn't peeking the first value" (isJust $ view previous  (Zipper 0 [1..] [1..]))

  describe "next and previous" $ do
    let zipper = Zipper 4 [5,6,7,8] [3,2,1,0] :: Zipper Int
    it "If zipper isn't peeking first nor last value, next . previous == previous . next" $
      (view previous zipper >>= view next) == (view next zipper >>= view previous)

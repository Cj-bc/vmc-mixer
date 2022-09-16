{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module VMCMixer.Backend.FilterSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import VMCMixer.Backend.Filter (filterLayerInitialState, FilterLayerState (FilterLayerState), applyFilter', applyFilter, SenderCmd(Packet))
import VMCMixer.Types (Performer(Performer), MarionetteMsgAddresses(Time, RootTransform), Filter(Filter))
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HMap
import qualified Data.VMCP.Marionette as Marionette
import Data.VMCP.Marionette (MarionetteMsg)
import Control.Monad.State.Strict (execStateT, evalStateT)
import Pipes.Prelude (toListM)
import Pipes ((>->), each)
import Control.Monad.Writer (runWriter)

spec = do
  describe "applyFilter'" $ do
    let sampleEmptyState = filterLayerInitialState (Performer 0 (Just "Fallback"))

    it "should pass anything if no filter is specified" $ pending
    prop "should pass anything if 'previousPerformer' is empty" $ \(targetP :: Performer) ->
      let s = FilterLayerState (Filter (Performer 0 Nothing)
                                $ HMap.fromList [(RootTransform, [Performer 1 Nothing])])
              HMap.empty
      in applyFilter' targetP RootTransform s `shouldBe` True

    it "shuold not pass if higher pripritized 'Performer' is in 'previousPerformer'" $ forAll
      (arbitrary `suchThat` (/= Performer 1 Nothing)) $ \targetP ->
                 let s = FilterLayerState (Filter (Performer 0 Nothing)
                                           $ HMap.fromList [(RootTransform, [Performer 1 Nothing])])
                                          (HMap.fromList [(RootTransform, [Performer 1 Nothing])])
                 in applyFilter' targetP RootTransform s `shouldBe` False

    it "should treat fallback as lowest priority" $
      let s = FilterLayerState (Filter (Performer 0 Nothing)
                                $ HMap.fromList [(RootTransform, [Performer 1 Nothing, Performer 2 Nothing, Performer 3 Nothing])])
              (HMap.fromList [(RootTransform, [Performer 3 Nothing])])
      in applyFilter' (Performer 0 Nothing) RootTransform s `shouldBe` False

    it "should treat Performers that aren't on list as lowest priority" $
      let s = FilterLayerState (Filter (Performer 0 Nothing)
                                $ HMap.fromList [(RootTransform, [Performer 1 Nothing, Performer 2 Nothing, Performer 3 Nothing])])
              (HMap.fromList [(RootTransform, [Performer 3 Nothing])])
      in applyFilter' (Performer 4 Nothing) RootTransform s `shouldBe` False
    
  describe "applyFilter" $ do
    it "should not pass lower prioritized packets when higher prioritized packets are shipped" $
      let p1 = Performer 1 (Just "Performer 1")
          p2 = Performer 2 (Just "Performer 2")
          fallbackP = Performer 0 (Just "Fallback")
          filter_ = FilterLayerState (Filter fallbackP $ HMap.fromList [(Time, [p1])]) HMap.empty
          result :: [MarionetteMsg]
          (result, _) = runWriter $ flip evalStateT filter_ . toListM $
                        each [Packet p1 (Marionette.Time 0)
                             ,Packet p2 (Marionette.Time 1)
                             ,Packet p2 (Marionette.Time 2)
                             ,Packet p2 (Marionette.Time 3)
                             ,Packet p2 (Marionette.Time 4)
                             ,Packet p1 (Marionette.Time 5)
                             ,Packet p1 (Marionette.Time 6)
                             ,Packet p2 (Marionette.Time 7)
                             ]  >-> applyFilter
      in result `shouldBe` [Marionette.Time 0, Marionette.Time 5, Marionette.Time 6]

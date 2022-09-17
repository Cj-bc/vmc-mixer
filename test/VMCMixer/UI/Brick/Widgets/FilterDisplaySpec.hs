{-# LANGUAGE OverloadedStrings #-}
module VMCMixer.UI.Brick.Widgets.FilterDisplaySpec where

import Test.Hspec
import Test.QuickCheck (Arbitrary (arbitrary))
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HMap
import VMCMixer.UI.Brick.Widgets.FilterDisplay
import VMCMixer.Types (Performer(Performer), MarionetteMsgAddresses(..), Filter(Filter))
import Lens.Micro ((^.), over, each, _2)
import Data.VRM (BlendShapeExpression(..))
import Data.UnityEditor (HumanBodyBones(..))
import Data.Zipper (Zipper(Zipper))
import qualified Data.Zipper as Z

spec = do
  describe "toFilter" $ do
    it "should not modify contents" $
      let name = ()
          filters = [(RootTransform, V.fromList [Performer 1 Nothing, Performer 2 Nothing])
                    ,(Time, V.fromList [Performer 3 Nothing])
                    ]
          zipper = Zipper (head filters) (tail filters) []
          fbkName = ()
          fd = FilterDisplay name zipper
      in toFilter fd == (Filter . HMap.fromList $ over (each._2) V.toList filters)

  describe "filterAdd" $ do
    let sampleFD = FilterDisplay () (Zipper (Available, V.fromList [(Performer 0 Nothing)]) [] [])

    it "should add given Performer successfully" $
      let expectedZipper = Zipper (Available, V.fromList [Performer 0 Nothing, Performer 1 Nothing]) [] []
      in (filterAdd (Performer 1 Nothing) sampleFD)^.containedFilters == expectedZipper

    it "should not modify other address filters" $
      (filterAdd (Performer 1 Nothing) sampleFD)^.containedFilters.Z.before == sampleFD^.containedFilters.Z.before
      && (filterAdd (Performer 1 Nothing) sampleFD)^.containedFilters.Z.after == sampleFD^.containedFilters.Z.after

  describe "filterRemove" $ do
    let sampleFD = FilterDisplay () (Zipper (Available, V.fromList [(Performer 0 Nothing)]) [] [])

    it "should remove specified Performer successfully" $
      let expectedZipper = Zipper (Available, V.empty) [] []
      in (filterRemove (Performer 0 Nothing) sampleFD)^.containedFilters == expectedZipper

    it "should not modify other address filters" $
      (filterRemove (Performer 0 Nothing) sampleFD)^.containedFilters.Z.before == sampleFD^.containedFilters.Z.before
      && (filterRemove (Performer 0 Nothing) sampleFD)^.containedFilters.Z.after == sampleFD^.containedFilters.Z.after

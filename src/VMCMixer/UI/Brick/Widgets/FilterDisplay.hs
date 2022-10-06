{- |
Module      :  VMCMixer.UI.Brick.Widgets.FilterDisplay
Description :  Brick widget to display filters
Copyright   :  (c) Cj.bc-sd a.k.a Cj-bc
License     :  GPL-3
Maintainer  :  cj.bc-sd@outlook.jp
Stability   :  experimental
Portability :  portable

This file is part of vmc-mixer.

vmc-mixer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

vmc-mixer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with vmc-mixer. If not, see <https://www.gnu.org/licenses/>.

-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
module VMCMixer.UI.Brick.Widgets.FilterDisplay (
  -- * FilterDisplay
  FilterDisplay(..)
  -- ** Lenses
, displayName
, containedFilters
  -- ** Modification
, toFilter
, filterAdd
, filterRemove

  -- * Brick
, handleFilterDisplayEvent

  -- ** Widget rendering
, filterDisplay
, renderPerformer    
, renderFilterDisplay
, renderFilterInfoRow

  -- ** Attributes
, filterDisplayAttr
, filterDisplayPeekedAttr
) where
import Brick.Widgets.Core (Named(..), vBox, hBox, str, txt, clickable, padLeft, withAttr)
import Brick.Widgets.Border (hBorder)
import Brick.Types (Widget, Padding(Pad), EventM)
import qualified Data.HashMap.Strict as HMap
import VMCMixer.Types (MarionetteMsgAddresses, Performer, performerName, performerPort, Filter(Filter))
import Lens.Micro ((^.), (%~), (&), _2, (.~), over, each)
import Lens.Micro.Extras (view)
import Lens.Micro.TH (makeLenses)
import qualified Graphics.Vty as Vty
import Brick (AttrName)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Data.Zipper


data FilterDisplay n = FilterDisplay { _displayName :: n
                                     , _containedFilters :: Zipper (MarionetteMsgAddresses, V.Vector Performer)
                                     }
makeLenses ''FilterDisplay

toFilter :: FilterDisplay n -> Filter
toFilter d = Filter $ HMap.fromList
             . over (each._2) V.toList
             $ d^.containedFilters.before ++ [d^.containedFilters.peeked] ++ d^.containedFilters.after

-- | Add new filter spec to peeked address
filterAdd :: Performer -> FilterDisplay n -> FilterDisplay n
filterAdd p display = display&containedFilters.peeked._2%~(`V.snoc` p)

-- | Remove specified filter spec from peeked address
filterRemove :: Performer -> FilterDisplay n -> FilterDisplay n
filterRemove p display = display&containedFilters.peeked._2%~V.filter (/= p)

-- | Helper function to create
--
-- Fixme: It inserts Filter for 'Time' temporary to create 'Zipper'
-- Consider switch to use other data structure in 'FilterDisplay' as
-- filter could be empty
filterDisplay :: n -> [(MarionetteMsgAddresses, V.Vector Performer)] -> FilterDisplay n
filterDisplay n [] = FilterDisplay n (Zipper (Time, V.empty) [] [])
filterDisplay n (peeked':rest) = FilterDisplay n (Zipper peeked' [] rest)

instance Named (FilterDisplay n) n where
  getName = view displayName

-- | Widget to display one 'Performer'
renderPerformer :: (Ord n, Show n) => Performer -> Widget n
renderPerformer performer = hBox [txt . maybe "" id $ view performerName performer
                                , str $ " (" ++ (show $ view performerPort performer) ++ ")"
                                ]

-- | Main renderer function for 'FilterDisplay'
--
-- Uses will needs to use this
renderFilterDisplay :: (Ord n, Show n) => Bool -> FilterDisplay n -> Widget n
renderFilterDisplay isFocused map =
  let filters = mconcat [(False,) <$> map^.containedFilters.before
                        , [(True, map^.containedFilters.peeked)]
                        , (False,) <$> map^.containedFilters.after
                        ]
  in vBox $ hBorder:(uncurry renderFilterInfoRow <$> filters)

-- | Widget to display one 'MarionetteMsgAddresses'
renderFilterInfoRow :: (Ord n, Show n) => Bool -> (MarionetteMsgAddresses, V.Vector Performer) -> Widget n
renderFilterInfoRow isFocused (addr, ls) = withAttr atr $ vBox [str $ show addr
                                                               , padLeft (Pad 2) . vBox . V.toList $ renderPerformer <$> ls
                                                               ]
  where
    atr = if isFocused then filterDisplayPeekedAttr else filterDisplayAttr

-- | The top-level attribute used for entire filterDisplay widget
filterDisplayAttr :: AttrName
filterDisplayAttr = "filterDisplay"

-- | The attribute used for 'Filter' that is currently peeked
filterDisplayPeekedAttr :: AttrName
filterDisplayPeekedAttr = filterDisplayAttr <> "Peeked"


handleFilterDisplayEvent :: Vty.Event -> FilterDisplay n -> EventM n (FilterDisplay n)
handleFilterDisplayEvent (Vty.EvKey (Vty.KChar 'j') []) d = return $ d&containedFilters%~(\d -> fromMaybe d $ view next d)
handleFilterDisplayEvent (Vty.EvKey (Vty.KChar 'k') []) d = return $ d&containedFilters%~(\d -> fromMaybe d $ view previous d)
handleFilterDisplayEvent _ d = return d
